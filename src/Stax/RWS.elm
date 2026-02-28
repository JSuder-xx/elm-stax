module Stax.RWS exposing
    ( RWS, rws, run, runWith, pure, doNothing
    , andMap, andThen, andThen_, map, mapWith, map2, map3, map4, map5, when, whenJust, whenOk
    , combine, combineMap, for_, foldM
    , ask, asks
    , tell, andTell, tellSingle
    , get, gets, modify, put, andModify
    , askGet
    , liftStateLens, liftStateOptional, liftStatePrism, ignore
    )

{-| A Reader Writer State monad stack. This stack cannot fail. Useful for pure computations that need configuration, logging, and mutable state—such as interpreters.

  - The "Reader" part lets you thread configuration through computations. It is like functional dependency injection with the configuration applied LAST.
  - The "Writer" part lets you thread logging through computations. You `tell` to log.
  - The "State" part lets you thread state through computations.

Use RWS when you never need to fail; use `Stax.RWSE` when you need fail-fast error handling.


## Example

Declare small stacks at the top level, then compose them:


    type alias Config =
        { step : Int }

    type alias State =
        { total : Int }

    addStep : RWS Config String State ()
    addStep =
        ask
            |> andThen (\cfg -> modify (\s -> { s | total = s.total + cfg.step }))
            |> andThen_ (tell [ "addStep" ])

    double : RWS Config String State ()
    double =
        modify (\s -> { s | total = s.total * 2 })
            |> andThen_ (tell [ "double" ])

    computation : RWS Config String State ()
    computation =
        addStep
            |> andThen_ double
            |> andThen_ addStep

    -- runWith { step = 1 } { total = 0 } computation
    -- => ( { total = 3 }, [ "addStep", "double", "addStep" ], () )


## Unneeded Facets

Just use unit `()` if you don't need a facet of this. For example,

  - If you declared `type alias MyStack a = RWS () () () a` that would basically do nothing.
  - You could still
      - `get`, `modify`, and `put` the state but it would be unit so uninteresting.
      - `ask` for the configuration but it would be unit so uninteresting.
      - `tell` to log but the result would be a list of units which is uninteresting.


## Functionality By Category


### Type, Construct, Run

@docs RWS, rws, run, runWith, pure, doNothing


### Functor/Applicative/Monad

@docs andMap, andThen, andThen_, map, mapWith, map2, map3, map4, map5, when, whenJust, whenOk


### Foldable/Traversable

@docs combine, combineMap, for_, foldM


### Configuration (Reader)

@docs ask, asks


### Logging (Writer)

@docs tell, andTell, tellSingle


### State

@docs get, gets, modify, put, andModify


### Miscellaneous

@docs askGet


### Transformations

@docs liftStateLens, liftStateOptional, liftStatePrism, ignore

-}

import Internal.Optics as O exposing (Lens, Optional, Prism)
import Maybe.Extra as Maybe
import Triple.Extra as Triple


{-| A Reader Writer State monad stack. This stack cannot fail. Useful for pure computations that need configuration, logging, and mutable state—such as interpreters.

  - The "Reader" part lets you thread configuration through computations. It is like functional dependency injection with the configuration applied LAST.
  - The "Writer" part lets you thread logging through computations. You `tell` to log.
  - The "State" part lets you thread state through computations.

Use RWS when you never need to fail; use `Stax.RWSE` when you need fail-fast error handling.

-}
type RWS config log state a
    = RWS (config -> state -> ( state, List log, a ))


{-| Construct a stack from a raw function. Low-level; prefer `pure`, `andThen`, and the other combinators.
-}
rws : (config -> state -> ( state, List log, a )) -> RWS config log state a
rws =
    RWS


{-| Execute the stack with config and state. Returns `(finalState, logs, result)`.
-}
run : RWS config log state a -> config -> state -> ( state, List log, a )
run (RWS f) =
    f


{-| Same as `run` but takes config and state first. Pipeline-friendly: `runWith config state myStack`.
-}
runWith : config -> state -> RWS config log state a -> ( state, List log, a )
runWith config state s =
    run s config state


{-| Wrap a value in the stack.
-}
pure : a -> RWS config log state a
pure a =
    RWS (\_ state -> ( state, [], a ))


{-| No-op. Useful with `andThen_` when you need a unit-producing computation.
-}
doNothing : RWS config log state ()
doNothing =
    pure ()


{-| Throw away a value.
-}
ignore : RWS config log state a -> RWS config log state ()
ignore =
    map (always ())


{-| Transform the result of a stack.
-}
map : (a -> b) -> RWS config log state a -> RWS config log state b
map f (RWS inner) =
    RWS (\config state -> inner config state |> Triple.mapThird f)


{-| Lift a two-argument function over the stack. Equivalent to `pure f |> andMap a |> andMap b`.
-}
map2 : (a -> b -> c) -> RWS config log state a -> RWS config log state b -> RWS config log state c
map2 f a b =
    pure f |> andMap a |> andMap b


{-| Lift a three-argument function over the stack.
-}
map3 : (a -> b -> c -> d) -> RWS config log state a -> RWS config log state b -> RWS config log state c -> RWS config log state d
map3 f a b c =
    pure f |> andMap a |> andMap b |> andMap c


{-| Map the result of a stack with knowledge of the config and state.
-}
mapWith : (( a, config, state ) -> b) -> RWS config log state a -> RWS config log state b
mapWith f (RWS inner) =
    RWS <|
        \config state ->
            inner config state
                |> Triple.mapThird (\a -> f ( a, config, state ))


{-| Shorthand for andMapping.

    map4 f a b c d

    -- is equivalent to
    pure f |> andMap a |> andMap b |> andMap c |> andMap d

-}
map4 : (a -> b -> c -> d -> e) -> RWS config log state a -> RWS config log state b -> RWS config log state c -> RWS config log state d -> RWS config log state e
map4 f a b c d =
    pure f |> andMap a |> andMap b |> andMap c |> andMap d


{-| Shorthand for andMapping.

    map5 f a b c d e

    -- is equivalent to
    pure f |> andMap a |> andMap b |> andMap c |> andMap d |> andMap e

-}
map5 : (a -> b -> c -> d -> e -> f) -> RWS config log state a -> RWS config log state b -> RWS config log state c -> RWS config log state d -> RWS config log state e -> RWS config log state f
map5 f a b c d e =
    pure f |> andMap a |> andMap b |> andMap c |> andMap d |> andMap e


{-| Applicative apply. `pure f |> andMap a |> andMap b` lifts a two-argument function over the stack.
-}
andMap : RWS config log state a -> RWS config log state (a -> b) -> RWS config log state b
andMap aM abM =
    aM |> andThen (\a -> map ((|>) a) abM)


{-| Iterate over a list with an accumulator. Like `List.foldl` but the step runs in the stack (config, state, logging).

    foldM (\acc x -> pure (acc + x)) 0 [ 1, 2, 3 ]

-}
foldM : (localState -> a -> RWS config log state localState) -> localState -> List a -> RWS config log state localState
foldM step initialAcc originalList =
    let
        go : config -> ( state, List log, localState ) -> List a -> ( state, List log, localState )
        go config ( currentState, currentLog, acc ) lst =
            case lst of
                [] ->
                    ( currentState, currentLog, acc )

                head :: tail ->
                    let
                        ( newState, newLog, newAcc ) =
                            run (step acc head) config currentState
                    in
                    go config ( newState, currentLog ++ newLog, newAcc ) tail
    in
    RWS
        (\config state ->
            go config ( state, [], initialAcc ) originalList
        )


{-| Map each element through the stack and collect results. Also known as traverse.
-}
combineMap : (a -> RWS config log state b) -> List a -> RWS config log state (List b)
combineMap f originalList =
    let
        go : config -> ( state, List log, List b ) -> List a -> ( state, List log, List b )
        go config ( currentState, currentLog, acc ) lst =
            case lst of
                [] ->
                    ( currentState, currentLog, List.reverse acc )

                head :: tail ->
                    let
                        ( newState, newLog, result ) =
                            run (f head) config currentState
                    in
                    go config ( newState, currentLog ++ newLog, result :: acc ) tail
    in
    RWS <| \config state -> go config ( state, [], [] ) originalList


{-| Like a for loop: run a computation for each element, discarding results.

    for_ [ 1, 2, 3 ] (\n -> tell [ "processing " ++ String.fromInt n ])

-}
for_ : List a -> (a -> RWS config log state ()) -> RWS config log state ()
for_ lst f =
    combineMap f lst |> map (always ())


{-| Run a list of stacks and collect results. Also known as sequence. Equivalent to `combineMap identity`.
-}
combine : List (RWS config log state a) -> RWS config log state (List a)
combine =
    combineMap identity


{-| Monadic bind. Chain computations; each step receives the result of the previous.
-}
andThen : (a -> RWS config log state b) -> RWS config log state a -> RWS config log state b
andThen f (RWS inner) =
    RWS
        (\config state ->
            let
                ( s1, w1, a ) =
                    inner config state
            in
            run (f a) config s1 |> Triple.mapSecond (List.append w1)
        )


{-| Chain when the first computation produces `()`. Cleaner than `andThen (\_ -> second)`.

    pure () |> andThen_ (tell [ "Hello" ])

-}
andThen_ : RWS config log state b -> RWS config log state () -> RWS config log state b
andThen_ second first =
    first |> andThen (always second)


{-| Execute a computation only when the condition is true; otherwise do nothing.

    when (count > 0) (modify (\s -> { s | count = s.count - 1 }))

-}
when : Bool -> RWS config log state () -> RWS config log state ()
when condition whenTrue =
    if condition then
        whenTrue

    else
        doNothing


{-| Execute a computation only when the Maybe has a value.

    whenJust (Just 42) (\n -> tell [ "got " ++ String.fromInt n ])

-}
whenJust : Maybe a -> (a -> RWS config log state ()) -> RWS config log state ()
whenJust a_ just =
    Maybe.unwrap doNothing just a_


{-| Execute a computation only when the Result is Ok.

    whenOk (Ok 42) (\n -> tell [ "success: " ++ String.fromInt n ])

-}
whenOk : Result e a -> (a -> RWS config log state ()) -> RWS config log state ()
whenOk a_ ok =
    case a_ of
        Err _ ->
            doNothing

        Ok a ->
            ok a



-- WriterT


{-| Append to the log. All `tell` output is collected and returned at the end of `run`.
-}
tell : List log -> RWS config log state ()
tell log =
    RWS (\_ state -> ( state, log, () ))


{-| Convenience function to write a single thing.
-}
tellSingle : log -> RWS config log state ()
tellSingle =
    List.singleton >> tell


{-| This convenience function allows you to log based on the result of a prior computation, the configuration, and the current state and then return the original value. So it acts like
a kind of sink.
-}
andTell : (( a, config, state ) -> List log) -> RWS config log state a -> RWS config log state a
andTell f first =
    first
        |> andThen
            (\a ->
                askGet
                    |> andThen (\( config, state ) -> tell (f ( a, config, state )))
                    |> map (always a)
            )



-- StateT


{-| Read the current state into the value. Prefer `modify` or `andModify` when you only need to update.
-}
get : RWS config log state state
get =
    RWS (\_ state -> ( state, [], state ))


{-| Read a projection of the state. Equivalent to `get |> map f`.
-}
gets : (state -> a) -> RWS config log state a
gets f =
    RWS (\_ state -> ( state, [], f state ))


{-| Replace the state entirely.
-}
put : state -> RWS config log state ()
put state =
    RWS (\_ _ -> ( state, [], () ))


{-| Update the state by applying a function. See `andModify` for a version that also has the prior result and config.
-}
modify : (state -> state) -> RWS config log state ()
modify f =
    RWS (\_ state -> ( f state, [], () ))


{-| This convenience function supports modifying the state based on the state, the result of a prior computation, and the configuration.
-}
andModify : (( state, a, config ) -> state) -> RWS config log state a -> RWS config log state a
andModify f first =
    first
        |> andThen
            (\a ->
                askGet |> andThen (\( config, state ) -> put (f ( state, a, config )) |> map (always a))
            )



-- ReaderT


{-| Read the configuration as the value. See `andModify`, `andTell`, and `mapWith` for alternatives that avoid separate ask/get.
-}
ask : RWS config log state config
ask =
    RWS (\config state -> ( state, [], config ))


{-| Read a projection of the configuration. Equivalent to `ask |> map f`.
-}
asks : (config -> a) -> RWS config log state a
asks f =
    RWS (\config state -> ( state, [], f config ))



-- Miscellaneous


{-| Want to read the configuration and the state all in one fell swoop? This is it.
-}
askGet : RWS config log state ( config, state )
askGet =
    map2 Tuple.pair ask get


{-| Lift a computation that works on a child state to work on a parent state. Accepts record-shaped optics compatible with [Monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest/); Elm's structural typing means any record with `get : parent -> child` and `set : child -> parent -> parent` works.

    lens = { get = .count, set = \v s -> { s | count = v } }
    liftStateLens lens (modify ((+) 1))

-}
liftStateLens : Lens parentState childState -> RWS config log childState a -> RWS config log parentState a
liftStateLens lens child =
    RWS <| \config parentState -> run child config (lens.get parentState) |> Triple.mapFirst (O.andSet lens parentState)


{-| Lift a computation to work on a parent state when the child may be missing (Optional/AffineTraversal). The first argument runs if the child is missing; it uses the parent state.
-}
liftStateOptional : RWS config log parentState a -> Optional parentState childCaseState -> RWS config log childCaseState a -> RWS config log parentState a
liftStateOptional onNoMatch optional child =
    RWS <|
        \config parentState ->
            optional.getOption parentState
                |> Maybe.unpack
                    (\_ -> run onNoMatch config parentState)
                    (run child config >> Triple.mapFirst (O.andSet optional parentState))


{-| Lift a computation to work on a custom type when focusing on one constructor via a Prism. The first argument runs if the constructor does not match.
-}
liftStatePrism : RWS config log customType a -> Prism customType constructorInfo -> RWS config log constructorInfo a -> RWS config log customType a
liftStatePrism onNoMatch prism child =
    RWS <|
        \config parentState ->
            prism.getOption parentState
                |> Maybe.unpack
                    (\_ -> run onNoMatch config parentState)
                    (run child config >> Triple.mapFirst prism.reverseGet)
