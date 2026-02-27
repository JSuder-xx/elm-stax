module Internal.Optics exposing (..)


type alias Lens a b =
    { get : a -> b, set : b -> a -> a }


type alias Prism a b =
    { getOption : a -> Maybe b, reverseGet : b -> a }


type alias Optional a b =
    { getOption : a -> Maybe b, set : b -> a -> a }


optionalToLens : b -> Optional a b -> Lens a b
optionalToLens default { getOption, set } =
    Lens (getOption >> Maybe.withDefault default) set


andSet : { r | set : b -> a -> a } -> a -> b -> a
andSet { set } a b =
    set b a
