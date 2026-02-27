module RWSTest exposing (..)

import Expect
import Shared exposing (..)
import Stax.RWS as S exposing (RWS)
import Test exposing (..)


type alias Stack a =
    RWS Config LogEntry State a


scenario : String -> List { given : Stack a, expect : ( State, List LogEntry, a ) } -> Test
scenario descr expectedList =
    test descr <|
        \_ ->
            Expect.all
                (List.map (|>) expectedList)
                (\{ given, expect } ->
                    given
                        |> S.runWith config initialState
                        |> Expect.equal expect
                )


numberOfNumbers : number
numberOfNumbers =
    30000


lotsOfNumbers : List Int
lotsOfNumbers =
    List.range 1 numberOfNumbers


suite : Test
suite =
    let
        updateCount f =
            S.modify <| \m -> { m | count = f m.count }

        add1ToCount =
            updateCount <| (+) 1
    in
    describe "RWS"
        [ scenario "pure returns the value given"
            [ { given = S.pure -10
              , expect = ( initialState, [], -10 )
              }
            , { given = S.pure 99
              , expect = ( initialState, [], 99 )
              }
            ]
        , scenario "doNothing returns unit"
            [ { given = S.doNothing
              , expect = ( initialState, [], () )
              }
            ]
        , describe "Functor"
            [ scenario "map transforms the result"
                [ { given = S.map ((+) 1) (S.pure 41)
                  , expect = ( initialState, [], 42 )
                  }
                ]
            , scenario "map2 lifts a two-argument function"
                [ { given = S.map2 Tuple.pair S.ask S.get
                  , expect = ( initialState, [], ( config, initialState ) )
                  }
                ]
            ]
        , describe "Conditional"
            [ scenario "when runs computation only when True"
                [ { given = S.when True (S.tell [ "x" ])
                  , expect = ( initialState, [ "x" ], () )
                  }
                , { given = S.when False (S.tell [ "x" ])
                  , expect = ( initialState, [], () )
                  }
                ]
            , scenario "whenJust runs when Maybe has a value"
                [ { given = S.whenJust (Just 1) (S.tellSingle << String.fromInt)
                  , expect = ( initialState, [ "1" ], () )
                  }
                , { given = S.whenJust Nothing (\_ -> S.tell [ "never" ])
                  , expect = ( initialState, [], () )
                  }
                ]
            , scenario "whenOk runs when Result is Ok"
                [ { given = S.whenOk (Ok 1) (S.tellSingle << String.fromInt)
                  , expect = ( initialState, [ "1" ], () )
                  }
                , { given = S.whenOk (Err "fail") (\_ -> S.tell [ "never" ])
                  , expect = ( initialState, [], () )
                  }
                ]
            ]
        , describe "Reader"
            [ scenario "can return configuration"
                [ { given = S.ask
                  , expect = ( initialState, [], config )
                  }
                , { given =
                        S.doNothing
                            |> S.andThen_ S.ask
                  , expect = ( initialState, [], config )
                  }
                , { given =
                        S.doNothing
                            |> S.andThen_ (updateCount <| (+) 3)
                            |> S.andThen_ (S.tell [ "Some stuff" ])
                            |> S.andThen_ S.ask
                  , expect = ( { count = 3 }, [ "Some stuff" ], config )
                  }
                ]
            , scenario "can use the configuration in the middle"
                [ { given =
                        S.doNothing
                            |> S.andThen_ S.ask
                            |> S.andThen (\cfg -> S.tell [ String.fromInt cfg.maxWidgets ])
                            |> S.andThen_ S.ask
                            |> S.andThen (\cfg -> updateCount <| (+) cfg.maxWidgets)
                  , expect = ( { count = 10 }, [ "10" ], () )
                  }
                ]
            ]
        , describe "Writer"
            [ scenario "Can accumulate multiple logs"
                [ { given =
                        S.doNothing
                            |> S.andThen_ (S.tell [ "First" ])
                            |> S.andThen_ (S.tell [ "Second" ])
                            |> S.andThen_ (S.tell [ "Third", "Fourth" ])
                  , expect = ( initialState, [ "First", "Second", "Third", "Fourth" ], () )
                  }
                ]
            ]
        , describe "State"
            [ scenario "Can get"
                [ { given = S.get
                  , expect = ( initialState, [], initialState )
                  }
                ]
            , scenario "Can gets"
                [ { given = S.gets .count
                  , expect = ( initialState, [], 0 )
                  }
                , { given =
                        S.put { count = 7 }
                            |> S.andThen_ (S.gets .count)
                  , expect = ( { count = 7 }, [], 7 )
                  }
                ]
            , scenario "Can put"
                [ { given = S.put { count = 99 }
                  , expect = ( { count = 99 }, [], () )
                  }
                ]
            , scenario "Can get and put"
                [ let
                    while_ : () -> Stack ()
                    while_ _ =
                        S.map2 Tuple.pair (S.asks .maxWidgets) S.get
                            |> S.andThen
                                (\( maxWidgets, { count } ) ->
                                    S.when (count < maxWidgets) <|
                                        (S.put { count = count + 1 }
                                            |> S.andThen_ (S.tell [ "*" ])
                                            |> S.andThen while_
                                        )
                                )
                  in
                  { given = while_ ()
                  , expect = ( { count = 10 }, [ "*", "*", "*", "*", "*", "*", "*", "*", "*", "*" ], () )
                  }
                ]
            ]
        , describe "Convenience"
            [ scenario "askGet returns config and state"
                [ { given = S.askGet
                  , expect = ( initialState, [], ( config, initialState ) )
                  }
                ]
            , scenario "andTell logs based on result, config, and state"
                [ { given =
                        S.pure 7
                            |> S.andTell (\( a, cfg, s ) -> [ String.fromInt a ++ ":" ++ String.fromInt cfg.maxWidgets ++ ":" ++ String.fromInt s.count ])
                  , expect = ( initialState, [ "7:10:0" ], 7 )
                  }
                ]
            , scenario "andModify updates state from result, config, and state"
                [ { given =
                        S.pure 5
                            |> S.andModify (\( s, a, cfg ) -> { s | count = s.count + a + cfg.maxWidgets })
                  , expect = ( { count = 15 }, [], 5 )
                  }
                ]
            ]
        , describe "Traversal"
            [ describe "combineMap"
                [ scenario "pure"
                    [ { given = S.combineMap (\v -> add1ToCount |> S.andThen_ (S.pure v)) lotsOfNumbers
                      , expect = ( { count = numberOfNumbers }, [], lotsOfNumbers )
                      }
                    ]
                , scenario "for_ runs computation for each element"
                    [ { given = S.for_ [ 1, 2 ] (S.tellSingle << String.fromInt)
                      , expect = ( initialState, [ "1", "2" ], () )
                      }
                    ]
                ]
            , scenario "combine sequences a list of stacks"
                [ { given = S.combine [ S.pure 1, S.pure 2 ]
                  , expect = ( initialState, [], [ 1, 2 ] )
                  }
                ]
            ]
        , describe "Transform"
            [ test "liftStateLens runs child computation and updates parent" <|
                \_ ->
                    let
                        parentState : Parent
                        parentState =
                            { child = { n = 0 } }

                        stack : RWS Config LogEntry Parent ()
                        stack =
                            S.liftStateLens childOfParent (S.modify (\c -> { c | n = c.n + 1 }))
                    in
                    S.runWith config parentState stack
                        |> Expect.equal ( { child = { n = 1 } }, [], () )
            , test "liftStateOptional runs child when present, onNoMatch otherwise" <|
                \_ ->
                    let
                        parentWithChild : IndecisiveParent
                        parentWithChild =
                            { maybeChild = Just { n = 5 } }

                        parentWithoutChild : IndecisiveParent
                        parentWithoutChild =
                            { maybeChild = Nothing }

                        stack : RWS Config LogEntry IndecisiveParent ()
                        stack =
                            S.liftStateOptional
                                (S.modify (\p -> { p | maybeChild = Just { n = 99 } }))
                                childOfIndecisiveParent
                                (S.modify (\c -> { c | n = c.n + 1 }))
                    in
                    Expect.all
                        [ \_ -> S.runWith config parentWithChild stack |> Expect.equal ( { maybeChild = Just { n = 6 } }, [], () )
                        , \_ -> S.runWith config parentWithoutChild stack |> Expect.equal ( { maybeChild = Just { n = 99 } }, [], () )
                        ]
                        ()
            , test "liftStatePrism runs child when constructor matches, onNoMatch otherwise" <|
                \_ ->
                    let
                        stackMatch : RWS Config LogEntry AOrB Int
                        stackMatch =
                            S.liftStatePrism
                                (S.pure 0)
                                aOfAOrB
                                (S.modify (\n -> n + 10) |> S.andThen_ S.get |> S.andThen (\n -> S.pure (n + 1)))

                        stackNoMatch : RWS Config LogEntry AOrB Int
                        stackNoMatch =
                            S.liftStatePrism
                                (S.tell [ "no match" ] |> S.andThen_ (S.pure 42))
                                aOfAOrB
                                (S.pure 0)
                    in
                    Expect.all
                        [ \_ -> S.runWith config (A 3) stackMatch |> Expect.equal ( A 13, [], 14 )
                        , \_ -> S.runWith config B stackNoMatch |> Expect.equal ( B, [ "no match" ], 42 )
                        ]
                        ()
            ]
        , describe "Foldable"
            [ describe "foldM"
                [ scenario "can be used to build a list"
                    [ { given =
                            S.foldM
                                (\runningList b ->
                                    S.pure <| b :: runningList
                                )
                                []
                                lotsOfNumbers
                      , expect = ( initialState, [], List.reverse lotsOfNumbers )
                      }
                    ]
                ]
            ]
        ]
