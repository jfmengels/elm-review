module List.Extra exposing (find, indexedFilterMap)

{-| Some utilities.
-}


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ] == Just 6

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


indexedFilterMap : (Int -> a -> Maybe b) -> Int -> List a -> List b -> List b
indexedFilterMap predicate index list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            case predicate index x of
                Just b ->
                    indexedFilterMap predicate (index + 1) xs (b :: acc)

                Nothing ->
                    indexedFilterMap predicate (index + 1) xs acc
