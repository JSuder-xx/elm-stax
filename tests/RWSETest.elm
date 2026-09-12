module RWSETest exposing (..)

import Expect
import Shared exposing (..)
import Stax.RWSE as S exposing (RWSE)
import Test exposing (..)
import Triple.Extra exposing (triple)


type Error
    = BigDeal String
    | NoBigDeal Int


type alias Stack a =
    RWSE Config LogEntry State Error a


scenario : String -> List { given : Stack a, expect : ( State, List LogEntry, Result Error a ) } -> Test
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

        inc =
            add1ToCount |> S.andThen_ S.get
    in
    describe "RWSE"
        [ scenario "pure returns the value given"
            [ { given = S.pure -10
              , expect = ( initialState, [], Ok -10 )
              }
            , { given = S.pure 99
              , expect = ( initialState, [], Ok 99 )
              }
            ]
        , describe "Functor"
            [ scenario "map transforms the result"
                [ { given = S.map ((+) 1) (S.pure 41)
                  , expect = ( initialState, [], Ok 42 )
                  }
                ]
            , scenario "map2 lifts a two-argument function"
                [ { given = S.map2 Tuple.pair S.ask S.get
                  , expect = ( initialState, [], Ok ( config, initialState ) )
                  }
                ]
            , scenario "map2 runs stateful computations left-to-right"
                [ { given = S.map2 Tuple.pair inc inc
                  , expect = ( { count = 2 }, [], Ok ( { count = 1 }, { count = 2 } ) )
                  }
                ]
            , scenario "map3 runs stateful computations left-to-right"
                [ { given = S.map3 triple inc inc inc
                  , expect = ( { count = 3 }, [], Ok ( { count = 1 }, { count = 2 }, { count = 3 } ) )
                  }
                ]
            ]
        , describe "Conditional"
            [ scenario "when runs computation only when True"
                [ { given = S.when True (S.tell [ "x" ])
                  , expect = ( initialState, [ "x" ], Ok () )
                  }
                , { given = S.when False (S.tell [ "x" ])
                  , expect = ( initialState, [], Ok () )
                  }
                ]
            , scenario "whenJust runs when Maybe has a value"
                [ { given = S.whenJust (Just 1) (S.tellSingle << String.fromInt)
                  , expect = ( initialState, [ "1" ], Ok () )
                  }
                , { given = S.whenJust Nothing (\_ -> S.tell [ "never" ])
                  , expect = ( initialState, [], Ok () )
                  }
                ]
            , scenario "whenOk runs when Result is Ok"
                [ { given = S.whenOk (Ok 1) (S.tellSingle << String.fromInt)
                  , expect = ( initialState, [ "1" ], Ok () )
                  }
                , { given = S.whenOk (Err (BigDeal "fail")) (\_ -> S.tell [ "never" ])
                  , expect = ( initialState, [], Ok () )
                  }
                ]
            ]
        , describe "Reader"
            [ scenario "can return configuration"
                [ { given = S.ask
                  , expect = ( initialState, [], Ok config )
                  }
                , { given =
                        S.doNothing
                            |> S.andThen_ S.ask
                  , expect = ( initialState, [], Ok config )
                  }
                , { given =
                        S.doNothing
                            |> S.andThen_ (updateCount <| (+) 3)
                            |> S.andThen_ (S.tell [ "Some stuff" ])
                            |> S.andThen_ S.ask
                  , expect = ( { count = 3 }, [ "Some stuff" ], Ok config )
                  }
                ]
            , scenario "can use the configuration in the middle"
                [ { given =
                        S.doNothing
                            |> S.andThen_ S.ask
                            |> S.andThen (\cfg -> S.tell [ String.fromInt cfg.maxWidgets ])
                            |> S.andThen_ S.ask
                            |> S.andThen (\cfg -> updateCount <| (+) cfg.maxWidgets)
                  , expect = ( { count = 10 }, [ "10" ], Ok () )
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
                  , expect = ( initialState, [ "First", "Second", "Third", "Fourth" ], Ok () )
                  }
                ]
            ]
        , describe "State"
            [ scenario "Can get"
                [ { given = S.get
                  , expect = ( initialState, [], Ok initialState )
                  }
                ]
            , scenario "Can put"
                [ { given = S.put { count = 99 }
                  , expect = ( { count = 99 }, [], Ok () )
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
                  , expect = ( { count = 10 }, [ "*", "*", "*", "*", "*", "*", "*", "*", "*", "*" ], Ok () )
                  }
                ]
            ]
        , describe "Except"
            [ scenario "immediately returns error"
                [ { given = S.throw <| NoBigDeal -10
                  , expect = ( initialState, [], Err <| NoBigDeal -10 )
                  }
                , { given = S.throw <| BigDeal "Oh crud!"
                  , expect = ( initialState, [], Err <| BigDeal "Oh crud!" )
                  }
                ]
            , scenario "stops execution of everything following the throw"
                [ { given =
                        S.pure
                            ()
                            |> S.andThen_ (updateCount <| (+) 3)
                            |> S.andThen_ (S.tell [ "Before Error" ])
                            |> S.andThen_ (S.throw <| BigDeal "Darn")
                            |> S.andThen_ (updateCount <| (+) 2)
                            |> S.andThen_ (S.tell [ "After Error" ])
                  , expect = ( { count = 3 }, [ "Before Error" ], Err <| BigDeal "Darn" )
                  }
                ]
            , scenario "can be recovered with catch" <|
                let
                    catcher error =
                        case error of
                            BigDeal "On no!!!" ->
                                S.pure 10

                            _ ->
                                S.throw error
                in
                [ { given =
                        S.throw (BigDeal "On no!!!")
                            |> S.catch catcher
                  , expect = ( initialState, [], Ok 10 )
                  }
                , { given =
                        S.throw (NoBigDeal 10)
                            |> S.catch catcher
                  , expect = ( initialState, [], Err (NoBigDeal 10) )
                  }
                ]
            , scenario "can bring a failed result into the stack with try"
                [ { given = S.try (S.throw (BigDeal "On no!!!"))
                  , expect = ( initialState, [], Ok (Err <| BigDeal "On no!!!") )
                  }
                ]
            , scenario "can bring a successful result into the stack with try"
                [ { given = S.try (S.pure 10)
                  , expect = ( initialState, [], Ok (Ok 10) )
                  }
                ]
            , scenario "fromResult lifts Ok to success and Err to throw"
                [ { given = S.fromResult (Ok 42)
                  , expect = ( initialState, [], Ok 42 )
                  }
                , { given = S.fromResult (Err (BigDeal "fail"))
                  , expect = ( initialState, [], Err (BigDeal "fail") )
                  }
                ]
            ]
        , describe "Convenience"
            [ scenario "askGet returns config and state"
                [ { given = S.askGet
                  , expect = ( initialState, [], Ok ( config, initialState ) )
                  }
                ]
            , scenario "andTell logs based on result, config, and state"
                [ { given =
                        S.pure 7
                            |> S.andTell (\( a, cfg, s ) -> [ String.fromInt a ++ ":" ++ String.fromInt cfg.maxWidgets ++ ":" ++ String.fromInt s.count ])
                  , expect = ( initialState, [ "7:10:0" ], Ok 7 )
                  }
                ]
            , scenario "andModify updates state from result, config, and state"
                [ { given =
                        S.pure 5
                            |> S.andModify (\( s, a, cfg ) -> { s | count = s.count + a + cfg.maxWidgets })
                  , expect = ( { count = 15 }, [], Ok 5 )
                  }
                ]
            ]
        , describe "Traversal"
            [ describe "combineMap"
                [ scenario "pure"
                    [ { given = S.combineMap (\v -> add1ToCount |> S.andThen_ (S.pure v)) lotsOfNumbers
                      , expect = ( { count = numberOfNumbers }, [], Ok lotsOfNumbers )
                      }
                    ]
                , scenario "for_ runs computation for each element"
                    [ { given = S.for_ [ 1, 2 ] (S.tellSingle << String.fromInt)
                      , expect = ( initialState, [ "1", "2" ], Ok () )
                      }
                    ]
                , scenario "stops early"
                    [ { given =
                            S.combineMap
                                (\v ->
                                    if v < 10 then
                                        add1ToCount |> S.andThen_ (S.pure v)

                                    else
                                        S.throw <| BigDeal "Yikes"
                                )
                                lotsOfNumbers
                      , expect = ( { count = 9 }, [], Err <| BigDeal "Yikes" )
                      }
                    ]
                ]
            , scenario "combine sequences a list of stacks"
                [ { given = S.combine [ S.pure 1, S.pure 2 ]
                  , expect = ( initialState, [], Ok [ 1, 2 ] )
                  }
                ]
            ]
        , describe "Transform"
            [ scenario "mapError transforms the error type"
                [ { given =
                        S.throw 42
                            |> S.mapError (\n -> BigDeal (String.fromInt n))
                  , expect = ( initialState, [], Err (BigDeal "42") )
                  }
                ]
            ]
        , describe "Foldable"
            [ describe "foldM"
                [ scenario "stops computation early on error"
                    [ { given =
                            S.foldM
                                (\_ b ->
                                    if b < 10 then
                                        add1ToCount

                                    else
                                        S.throw <| BigDeal "Uh oh"
                                )
                                ()
                                lotsOfNumbers
                      , expect = ( { count = 9 }, [], Err <| BigDeal "Uh oh" )
                      }
                    ]
                , scenario "can be used to build a list"
                    [ { given =
                            S.foldM
                                (\runningList b ->
                                    S.pure <| b :: runningList
                                )
                                []
                                lotsOfNumbers
                      , expect = ( initialState, [], Ok <| List.reverse lotsOfNumbers )
                      }
                    ]
                ]
            ]
        ]
