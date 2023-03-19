module Vendor.ListExtra exposing
    ( find, last
    , orderIndependentMapAppend, orderIndependentConcatMapAppend
    , anyCombination
    , foldlSwitched
    )

{-| Functions taken from elm-community/list-extra.

These were used so that we wouldn't have dependency conflicts when
elm-community/list-extra would release a new major version.

This also includes a few custom functions


# List functions

@docs find, last

@docs orderIndependentMapAppend, orderIndependentConcatMapAppend

@docs anyCombination

-}


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
find (\\num -> num > 5) [2, 4, 6, 8] == Just 6
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


{-| Same as List.foldl but the position of the list and initial value arguments are switched.
-}
foldlSwitched : (a -> b -> b) -> List a -> b -> b
foldlSwitched func list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            foldlSwitched func xs (func x acc)



-- Not originally from elm-community/list-extra


{-| Version of "List.append (List.map fn left) right" that doesn't care about order.
-}
orderIndependentMapAppend : (a -> b) -> List a -> List b -> List b
orderIndependentMapAppend fn left right =
    List.foldl (\element acc -> fn element :: acc) right left


{-| Version of "(List.concatMap fn left) ++ right" that doesn't care about order.
-}
orderIndependentConcatMapAppend : (a -> List b) -> List a -> List b -> List b
orderIndependentConcatMapAppend fn left right =
    List.foldl (\item acc -> List.append (fn item) acc) right left


{-| Similar to List.any, but on any combination pair of the list.
-}
anyCombination : (a -> a -> Bool) -> List a -> Bool
anyCombination predicate xs =
    case xs of
        [] ->
            False

        x :: xs_ ->
            if List.any (\y -> predicate x y) xs_ then
                True

            else
                anyCombination predicate xs_
