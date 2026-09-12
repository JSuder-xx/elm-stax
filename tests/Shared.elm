module Shared exposing
    ( Config, config
    , LogEntry
    , State, initialState
    )

{-|

@docs Config, config
@docs LogEntry
@docs State, initialState

-}


type alias State =
    { count : Int }


type alias Config =
    { maxWidgets : Int }


type alias LogEntry =
    String


config : Config
config =
    { maxWidgets = 10 }


initialState : State
initialState =
    { count = 0 }
