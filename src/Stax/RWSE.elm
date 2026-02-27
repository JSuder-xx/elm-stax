module Stax.RWSE exposing
    ( RWSE, rwse, run, runWith, pure, fromResult, doNothing
    , andMap, andThen, andThen_, map, mapWith, map2, map3, map4, map5, when, whenJust, whenOk
    , combine, combineMap, for_, foldM
    , ask, asks
    , tell, andTell, tellSingle
    , get, modify, put, andModify
    , catch, throw, try
    , liftStateLens, liftStateOptional, liftStatePrism, mapError, ignore
    , askGet
    )

{-| A Reader Writer State Except monad stack. Useful when pure computations can fail—interpreters, parsers, compilers.

  - The "Reader" part lets you thread configuration through computations (e.g., compiler flags). It is like functional dependency injection with the configuration applied LAST.
  - The "Writer" part lets you thread logging through computations. You `tell` to log; messages are read at the end.
  - The "State" part lets you thread state through computations (e.g., variable aliases when unifying, symbols encountered).
  - The "Except" part gives you fail-fast semantics. It is based on `Result` but the API gives you `throw`, `catch`, and `try` which looks like imperative programming.

Use RWSE over RWS when you need fail-fast error handling.


## Example

Declare small stacks at the top level, then compose them. Use `throw` and `catch` when a step can fail:


    type Error
        = ExceededMax Int

    type alias Config =
        { step : Int, maxTotal : Int }

    type alias State =
        { total : Int }

    addStep : RWSE Config String State Error ()
    addStep =
        ask
            |> andThen (\cfg -> modify (\s -> { s | total = s.total + cfg.step }))
            |> andThen_ (tell [ "addStep" ])

    double : RWSE Config String State Error ()
    double =
        modify (\s -> { s | total = s.total * 2 })
            |> andThen_ (tell [ "double" ])

    checkMax : RWSE Config String State Error ()
    checkMax =
        askGet
            |> andThen
                (\( cfg, s ) ->
                    if s.total > cfg.maxTotal then
                        throw (ExceededMax s.total)

                    else
                        tell [ "within limit " ++ String.fromInt s.total ]
                )

    computation : RWSE Config String State Error ()
    computation =
        addStep
            |> andThen_ double
            |> andThen_ checkMax
            |> andThen_ addStep

    -- runWith { step = 1, maxTotal = 5 } { total = 0 } computation
    -- => ( { total = 3 }, [ "addStep", "double", "within limit", "addStep" ], Ok () )


## Unneeded Facets

Just use unit `()` if you don't need a facet of this. For example,

  - If you declared `type alias MyStack a = RWSE () () () () a` that would basically just be `Result`.
  - And you could still
      - `get`, `modify`, and `put` the state but it would be unit so uninteresting.
      - `ask` for the configuration but it would be unit so uninteresting.
      - `tell` to log but the result would be a list of units which is uninteresting.


## Functionality By Category


### Type, Construct, Run

@docs RWSE, rwse, run, runWith, pure, fromResult, doNothing


### Functor/Applicative/Monad

@docs andMap, andThen, andThen_, map, mapWith, map2, map3, map4, map5, when, whenJust, whenOk


### Foldable/Traversable

@docs combine, combineMap, for_, foldM


### Configuration (Reader)

@docs ask, asks


### Logging (Writer)

@docs tell, andTell, tellSingle


### State

@docs get, modify, put, andModify


### Exceptions

@docs catch, throw, try


### Transformations

@docs liftStateLens, liftStateOptional, liftStatePrism, mapError, ignore


### Miscellaneous

@docs askGet

-}

import Internal.Optics as O exposing (Lens, Optional, Prism)
import Maybe.Extra
import Result.Extra
import Triple.Extra as Triple


{-| A Reader Writer State Except monad stack. Useful when pure computations can fail—interpreters, parsers, compilers.

  - The "Reader" part lets you thread configuration through computations (e.g., compiler flags). It is like functional dependency injection with the configuration applied LAST.
  - The "Writer" part lets you thread logging through computations. You `tell` to log; messages are read at the end.
  - The "State" part lets you thread state through computations (e.g., variable aliases when unifying, symbols encountered).
  - The "Except" part gives you fail-fast semantics. It is based on `Result` but the API gives you `throw`, `catch`, and `try` which looks like imperative programming.

Use RWSE over RWS when you need fail-fast error handling.

-}
type RWSE config log state error a
    = RWSE (config -> state -> ( state, List log, Result error a ))


{-| Construct a stack from a raw function. Low-level; prefer `pure`, `andThen`, and the other combinators.
-}
rwse : (config -> state -> ( state, List log, Result error a )) -> RWSE config log state error a
rwse =
    RWSE


{-| Execute the stack with config and state. Returns `(finalState, logs, Result error a)`.
-}
run : RWSE config log state error a -> config -> state -> ( state, List log, Result error a )
run (RWSE f) =
    f


{-| Same as `run` but takes config and state first. Pipeline-friendly: `runWith config state myStack`.
-}
runWith : config -> state -> RWSE config log state error a -> ( state, List log, Result error a )
runWith config state s =
    run s config state


{-| Wrap a value in the stack.
-}
pure : a -> RWSE config log state error a
pure a =
    RWSE (\_ state -> ( state, [], Ok a ))


{-| Create a stack from a Result. `Ok` becomes success; `Err` becomes a thrown error.

    fromResult (Ok 42) -- succeeds with 42

    fromResult (Err "fail") -- throws, halting the computation

-}
fromResult : Result error a -> RWSE config log state error a
fromResult =
    Result.Extra.unpack throw pure


{-| No-op. Useful with `andThen_` when you need a unit-producing computation.
-}
doNothing : RWSE config log state error ()
doNothing =
    pure ()


{-| Throw away a value.
-}
ignore : RWSE config log state error a -> RWSE config log state error ()
ignore =
    map (always ())


{-| Transform the result of a stack.
-}
map : (a -> b) -> RWSE config log state error a -> RWSE config log state error b
map f (RWSE inner) =
    RWSE (\config state -> inner config state |> Triple.mapThird (Result.map f))


{-| Map the result of a stack with knowledge of the config and state.
-}
mapWith : (( a, config, state ) -> b) -> RWSE config log state error a -> RWSE config log state error b
mapWith f (RWSE inner) =
    RWSE <|
        \config state ->
            inner config state
                |> Triple.mapThird (Result.map <| \a -> f ( a, config, state ))


{-| Shorthand for andMapping.

    map2 Tuple.pair ask get -- RWSE config log state error ( config, state )

-}
map2 : (a -> b -> c) -> RWSE config log state error a -> RWSE config log state error b -> RWSE config log state error c
map2 f a b =
    pure f |> andMap a |> andMap b


{-| Shorthand for andMapping.

    map3 f a b c

    -- is equivalent to
    pure f |> andMap a |> andMap b |> andMap c

-}
map3 : (a -> b -> c -> d) -> RWSE config log state error a -> RWSE config log state error b -> RWSE config log state error c -> RWSE config log state error d
map3 f a b c =
    pure f |> andMap a |> andMap b |> andMap c


{-| Shorthand for andMapping.

    map4 f a b c d

    -- is equivalent to
    pure f |> andMap a |> andMap b |> andMap c |> andMap d

-}
map4 : (a -> b -> c -> d -> e) -> RWSE config log state error a -> RWSE config log state error b -> RWSE config log state error c -> RWSE config log state error d -> RWSE config log state error e
map4 f a b c d =
    pure f |> andMap a |> andMap b |> andMap c |> andMap d


{-| Shorthand for andMapping.

    map5 f a b c d e

    -- is equivalent to
    pure f |> andMap a |> andMap b |> andMap c |> andMap d |> andMap e

-}
map5 : (a -> b -> c -> d -> e -> f) -> RWSE config log state error a -> RWSE config log state error b -> RWSE config log state error c -> RWSE config log state error d -> RWSE config log state error e -> RWSE config log state error f
map5 f a b c d e =
    pure f |> andMap a |> andMap b |> andMap c |> andMap d |> andMap e


{-| Applicative apply. `pure f |> andMap a |> andMap b` lifts a two-argument function over the stack.
-}
andMap : RWSE config log state error a -> RWSE config log state error (a -> b) -> RWSE config log state error b
andMap aM abM =
    aM |> andThen (\a -> map ((|>) a) abM)


{-| Iterate over a list with an accumulator. Like `List.foldl` but the step runs in the stack (config, state, logging, fail-fast).

    foldM (\acc x -> pure (acc + x)) 0 [ 1, 2, 3 ]

-}
foldM : (localState -> a -> RWSE config log state error localState) -> localState -> List a -> RWSE config log state error localState
foldM step initialAcc originalList =
    let
        go : config -> ( state, List log, Result error localState ) -> List a -> ( state, List log, Result error localState )
        go config ( currentState, currentLog, accResult ) lst =
            case ( lst, accResult ) of
                ( [], _ ) ->
                    ( currentState, currentLog, accResult )

                ( _, Err _ ) ->
                    ( currentState, currentLog, accResult )

                ( head :: tail, Ok acc ) ->
                    go config (run (step acc head) config currentState) tail
    in
    RWSE
        (\config state ->
            go config ( state, [], Ok initialAcc ) originalList
        )


{-| Map each element through the stack and collect results. Also known as traverse. Fails fast on first error.
-}
combineMap : (a -> RWSE config log state error b) -> List a -> RWSE config log state error (List b)
combineMap f originalList =
    let
        go : config -> ( state, List log, Result error (List b) ) -> List a -> ( state, List log, Result error (List b) )
        go config ( currentState, currentLog, accResult ) lst =
            case ( lst, accResult ) of
                ( [], _ ) ->
                    ( currentState, currentLog, Result.map List.reverse accResult )

                ( _, Err _ ) ->
                    ( currentState, currentLog, accResult )

                ( head :: tail, Ok acc ) ->
                    let
                        ( newState, newLog, result ) =
                            run (f head) config currentState
                    in
                    case result of
                        Err err ->
                            ( newState, newLog, Err err )

                        Ok item ->
                            go config ( newState, currentLog ++ newLog, Ok <| item :: acc ) tail
    in
    RWSE <| \config state -> go config ( state, [], Ok [] ) originalList


{-| Like a for loop: run a computation for each element, discarding results. Fails fast on first error.

    for_ [ 1, 2, 3 ] (\n -> tell [ "processing " ++ String.fromInt n ])

-}
for_ : List a -> (a -> RWSE config log state error ()) -> RWSE config log state error ()
for_ lst f =
    combineMap f lst |> map (always ())


{-| ALSO KNOWN AS sequence
-}
combine : List (RWSE config log state error a) -> RWSE config log state error (List a)
combine =
    combineMap identity


{-| Monadic bind. Chain computations; each step receives the result of the previous.
-}
andThen : (a -> RWSE config log state error b) -> RWSE config log state error a -> RWSE config log state error b
andThen f (RWSE inner) =
    RWSE
        (\config state ->
            let
                ( s1, w1, aResult ) =
                    inner config state
            in
            case aResult of
                Err error ->
                    ( s1, w1, Err error )

                Ok a ->
                    run (f a) config s1 |> Triple.mapSecond (List.append w1)
        )


{-| Convenience function for where you have a computation that should happen after another where the preceding produces no value. It reads a lot nicer.

    -- for example
    pure ()
        |> andThen_ (tell [ "Hello" ])

    -- is nicer than
    pure ()
        |> andThen (\_ -> tell [ "Hello" ])

-}
andThen_ : RWSE config log state error b -> RWSE config log state error () -> RWSE config log state error b
andThen_ second first =
    first |> andThen (always second)


{-| Execute a computation only when the condition is true; otherwise do nothing.

    when (count > 0) (modify (\s -> { s | count = s.count - 1 }))

-}
when : Bool -> RWSE config log state error () -> RWSE config log state error ()
when condition whenTrue =
    if condition then
        whenTrue

    else
        doNothing


{-| Execute a computation only when the Maybe has a value.

    whenJust (Just 42) (\n -> tell [ "got " ++ String.fromInt n ])

-}
whenJust : Maybe a -> (a -> RWSE config log state error ()) -> RWSE config log state error ()
whenJust a_ just =
    Maybe.Extra.unwrap doNothing just a_


{-| Execute a computation only when the Result is Ok.

    whenOk (Ok 42) (\n -> tell [ "success: " ++ String.fromInt n ])

-}
whenOk : Result e a -> (a -> RWSE config log state error ()) -> RWSE config log state error ()
whenOk a_ ok =
    case a_ of
        Err _ ->
            doNothing

        Ok a ->
            ok a



-- ExceptT


{-| Throw an error and halt all computations. Stops chains of `andThen`, `combine`, `combineMap`, `for_`, and `foldM`.

    throw (ParseError "unexpected token")
        |> catch (\err -> pure 0)  -- recovers with 0

-}
throw : error -> RWSE config log state error a
throw error =
    RWSE (\_ state -> ( state, [], Err error ))


{-| Recover from an error. If the stack failed, run the handler with the error; otherwise pass through the success.

    throw (ParseError "bad")
        |> catch (\err -> pure (defaultValue err))

-}
catch : (error -> RWSE config log state error a) -> RWSE config log state error a -> RWSE config log state error a
catch errorCatch (RWSE inner) =
    RWSE
        (\config state ->
            let
                ( s1, w1, aResult ) =
                    inner config state
            in
            aResult
                |> Result.Extra.unpack
                    (\error -> run (errorCatch error) config s1 |> Triple.mapSecond (List.append w1))
                    (\a -> ( s1, w1, Ok a ))
        )


{-| Turn a failed stack into a successful one by wrapping the result in `Result`. Errors become `Err`, successes become `Ok`.

    try (throw (ParseError "oops")) -- Ok (Err (ParseError "oops"))

    try (pure 42) -- Ok (Ok 42)

-}
try : RWSE config log state error a -> RWSE config log state error (Result error a)
try (RWSE inner) =
    RWSE (\config state -> inner config state |> Triple.mapThird Ok)


{-| Convenience function to load the configuration and the state all in one fell swoop.
-}
askGet : RWSE config log state error ( config, state )
askGet =
    map2 Tuple.pair ask get



-- WriterT


{-| Append to the log. All `tell` output is collected and returned at the end of `run`.
-}
tell : List log -> RWSE config log state error ()
tell log =
    RWSE (\_ state -> ( state, log, Ok () ))


{-| Append a single value to the log. Equivalent to `tell [ x ]`.
-}
tellSingle : log -> RWSE config log state error ()
tellSingle =
    List.singleton >> tell


{-| This convenience function allows you to log based on the result of a prior computation, the configuration, and the current state and then return the original value. So it acts like
a kind of sink.
-}
andTell : (( a, config, state ) -> List log) -> RWSE config log state error a -> RWSE config log state error a
andTell f first =
    first
        |> andThen
            (\a ->
                askGet
                    |> andThen (\( config, state ) -> tell (f ( a, config, state )))
                    |> map (always a)
            )



-- StateT


{-| Read the state into the value returned by the state. Only use this if you genuinely need to read the state for some reason.

  - If you want to modify the state (read it, apply a function, and write it) use modify.
  - If you want to modify the state based on the result of a prior computation, the configuration, and the current state use `andModify`.
  - If you want to tell based on the result of a prior computation, the configuration, and the current state use `andTell`.

-}
get : RWSE config log state error state
get =
    RWSE (\_ state -> ( state, [], Ok state ))


{-| Replace the state entirely.
-}
put : state -> RWSE config log state error ()
put state =
    RWSE (\_ _ -> ( state, [], Ok () ))


{-| Update the state by applying an update function.

See `andModify` for a more powerful version that includes the current state, the result of a prior computation, and the configuration.

-}
modify : (state -> state) -> RWSE config log state error ()
modify f =
    RWSE (\_ state -> ( f state, [], Ok () ))


{-| This convenience function supports modifying the state based on the state, the result of a prior computation, and the configuration.
-}
andModify : (( state, a, config ) -> state) -> RWSE config log state error a -> RWSE config log state error a
andModify f first =
    first
        |> andThen
            (\a ->
                askGet |> andThen (\( config, state ) -> put (f ( state, a, config )) |> map (always a))
            )



-- ReaderT


{-| Load the configuration as the value.

PLEASE see the following before using this

  - `andModify` if you want to modify the state based on the result of a prior computation, the configuration, and the current state.
  - `andTell` if you want to log based on the result of a prior computation, the configuration, and the current state.
  - `mapWith` if you want to map the value with knowledge of the configuration and the current state.

-}
ask : RWSE config log state error config
ask =
    RWSE (\config state -> ( state, [], Ok config ))


{-| Load one fact of the configuration as the value.

PLEASE see the following before using this

  - `andModify` if you want to modify the state based on the result of a prior computation, the configuration, and the current state.
  - `andTell` if you want to log based on the result of a prior computation, the configuration, and the current state.
  - `mapWith` if you want to map the value with knowledge of the configuration and the current state.

-}
asks : (config -> a) -> RWSE config log state error a
asks f =
    RWSE (\config state -> ( state, [], Ok <| f config ))



-- Transformations


{-| Transform the error type. Useful when unifying error types from different subcomputations.

    mapError (\e -> Wrapper e) (parseExpr ...)

-}
mapError : (ea -> eb) -> RWSE config log state ea a -> RWSE config log state eb a
mapError f (RWSE inner) =
    RWSE <| \config state -> inner config state |> Triple.mapThird (Result.mapError f)


{-| Lift a computation that works on a child state to work on a parent state. Accepts record-shaped optics compatible with [Monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/); Elm's structural typing means any record with `get : parent -> child` and `set : child -> parent -> parent` works.

    lens = { get = .count, set = \v s -> { s | count = v } }
    liftStateLens lens (modify ((+) 1))

-}
liftStateLens : Lens parentState childState -> RWSE config log childState error a -> RWSE config log parentState error a
liftStateLens lens child =
    RWSE <| \config parentState -> run child config (lens.get parentState) |> Triple.mapFirst (O.andSet lens parentState)


{-| Lift a computation to work on a parent state when the child may be missing (Optional/AffineTraversal). The first argument runs if the child is missing; it uses the parent state.
-}
liftStateOptional : RWSE config log parentState error a -> Optional parentState childCaseState -> RWSE config log childCaseState error a -> RWSE config log parentState error a
liftStateOptional onNoMatch optional child =
    RWSE <|
        \config parentState ->
            optional.getOption parentState
                |> Maybe.Extra.unpack
                    (\_ -> run onNoMatch config parentState)
                    (run child config >> Triple.mapFirst (O.andSet optional parentState))


{-| Lift a computation to work on a custom type when focusing on one constructor via a Prism. The first argument runs if the constructor does not match.
-}
liftStatePrism : RWSE config log customType error a -> Prism customType constructorInfo -> RWSE config log constructorInfo error a -> RWSE config log customType error a
liftStatePrism onNoMatch optional child =
    RWSE <|
        \config parentState ->
            optional.getOption parentState
                |> Maybe.Extra.unpack
                    (\_ -> run onNoMatch config parentState)
                    (run child config >> Triple.mapFirst optional.reverseGet)
