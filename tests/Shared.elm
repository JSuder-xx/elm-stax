module Shared exposing
    ( AOrB(..), aOfAOrB
    , Config, config
    , LogEntry
    , State, initialState
    , Parent, childOfParent
    , IndecisiveParent, childOfIndecisiveParent
    , numberOfNumbers, lotsOfNumbers
    )

{-|

@docs AOrB, aOfAOrB
@docs Config, config
@docs LogEntry
@docs State, initialState
@docs Parent, childOfParent
@docs IndecisiveParent, childOfIndecisiveParent
@docs numberOfNumbers, lotsOfNumbers

-}


type alias State =
    { count : Int }


type alias Parent =
    { child : { n : Int } }


childOfParent : { get : Parent -> { n : Int }, set : { n : Int } -> Parent -> Parent }
childOfParent =
    { get = .child
    , set = \c p -> { p | child = c }
    }


type alias IndecisiveParent =
    { maybeChild : Maybe { n : Int } }


childOfIndecisiveParent : { getOption : IndecisiveParent -> Maybe { n : Int }, set : { n : Int } -> IndecisiveParent -> IndecisiveParent }
childOfIndecisiveParent =
    { getOption = .maybeChild
    , set = \c p -> { p | maybeChild = Just c }
    }


type alias Config =
    { maxWidgets : Int }


type alias LogEntry =
    String


type AOrB
    = A Int
    | B


aOfAOrB : { getOption : AOrB -> Maybe Int, reverseGet : Int -> AOrB }
aOfAOrB =
    { getOption =
        \t ->
            case t of
                A n ->
                    Just n

                B ->
                    Nothing
    , reverseGet = A
    }


config : Config
config =
    { maxWidgets = 10 }


initialState : State
initialState =
    { count = 0 }


numberOfNumbers : number
numberOfNumbers =
    30000


lotsOfNumbers : List Int
lotsOfNumbers =
    List.range 1 numberOfNumbers
