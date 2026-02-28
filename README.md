# Stax — Monad Stacks for Elm

Pure RWS and RWSE monad stacks for Elm. Thread configuration (Reader), logging (Writer), and state through computations—with optional fail-fast error handling (Except) in RWSE. Useful for interpreters, compilers, and other pure computations that need config, mutable state, and logging while producing a value.

## When to Use

- **Stax.RWS**: Use when your computation never needs to fail. Reader + Writer + State.
- **Stax.RWSE**: Use when you need fail-fast error handling. Reader + Writer + State + Except.

## Representing ReaderT, WriterT, StateT, ExceptT, MaybeT

These two stacks cover common combinations of ReaderT, WriterT, StateT, ExceptT, and MaybeT:

- **Choose RWSE** if you need ExceptT or MaybeT (i.e., any fail-fast semantics). Otherwise use **RWS**.
- Use the unit type `()` for any facet you don’t need:
  - `RWS () log state a` — no Reader
  - `RWS config () state a` — no Writer
  - `RWS config log () a` — no State
  - The same applies in RWSE for config, log, and state.

For MaybeT-style behavior (failure with no error payload), use **RWSE with `()` in the error position**: `RWSE config log state () a`. `Err ()` plays the role of `Nothing` and `Ok a` the role of `Just a`.

## Example

Declare small stacks at the top level, then compose them with `andThen` and `andThen_`:

```elm
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
```

The goal is to provide concrete monad stacks you can snap together. Use `pure` and `andThen` to build computations; use `run` or `runWith` to execute them.

## Modules

- [Stax.RWS](https://package.elm-lang.org/packages/jsuder-xx/elm-stax/latest/Stax-RWS) — Reader Writer State (no errors)
- [Stax.RWSE](https://package.elm-lang.org/packages/jsuder-xx/elm-stax/latest/Stax-RWSE) — Reader Writer State Except (fail-fast errors)

## Installation

```bash
elm install jsuder-xx/elm-stax
```
