module Simplify.Match exposing
    ( Match(..)
    , map, traverse
    )

{-|

@docs Match
@docs map, traverse

-}


type Match a
    = Determined a
    | Undetermined


map : (a -> b) -> Match a -> Match b
map mapper match =
    case match of
        Determined a ->
            Determined (mapper a)

        Undetermined ->
            Undetermined


{-| If all mapped elements are Determined, returns a List of the Determined values.
If any match is Undetermined, returns Undetermined
-}
traverse : (a -> Match b) -> List a -> Match (List b)
traverse f list =
    traverseHelp f list []


traverseHelp : (a -> Match b) -> List a -> List b -> Match (List b)
traverseHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Determined a ->
                    traverseHelp f tail (a :: acc)

                Undetermined ->
                    Undetermined

        [] ->
            Determined (List.reverse acc)
