module List.Extra exposing
    ( dictToListFilterAndMap
    , dictToListMap
    , elemIndex
    , find
    , findMap
    , findMapWithIndex
    , findNeighboring
    , getNeighboring
    , indexedFilterMap
    , indexedFoldl
    , indexedFoldr
    , insertAllJusts
    , last
    , listFilterThenMapInto
    , maybeCons
    , reverseMap
    , splitAt
    , splitWhen
    , stableSortWith
    , uncons
    , unconsLast
    , uniquePairs
    , zip
    )

{-| Some utilities.
-}

import Dict exposing (Dict)
import Set exposing (Set)


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list


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


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case mapper first of
                Just value ->
                    Just value

                Nothing ->
                    findMap mapper rest


findMapWithIndex : (Int -> a -> Maybe b) -> List a -> Maybe b
findMapWithIndex mapper list =
    findMapWithIndexHelp mapper 0 list


findMapWithIndexHelp : (Int -> a -> Maybe b) -> Int -> List a -> Maybe b
findMapWithIndexHelp mapper index list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case mapper index first of
                Just value ->
                    Just value

                Nothing ->
                    findMapWithIndexHelp mapper (index + 1) rest


getNeighboring : Int -> List a -> Maybe ( a, { before : Maybe a, after : Maybe a } )
getNeighboring index list =
    getNeighboringAfter Nothing index list


getNeighboringAfter : Maybe a -> Int -> List a -> Maybe ( a, { before : Maybe a, after : Maybe a } )
getNeighboringAfter before index list =
    if index == 0 then
        case list of
            [] ->
                Nothing

            a :: after ->
                Just ( a, { before = before, after = List.head after } )

    else
        case list of
            [] ->
                Nothing

            a :: after ->
                getNeighboringAfter (Just a) (index - 1) after


findNeighboring : (Int -> a -> Bool) -> List a -> Maybe ( a, { before : Maybe a, after : Maybe a } )
findNeighboring predicate list =
    findNeighboringAfter Nothing 0 predicate list


findNeighboringAfter : Maybe a -> Int -> (Int -> a -> Bool) -> List a -> Maybe ( a, { before : Maybe a, after : Maybe a } )
findNeighboringAfter before index predicate list =
    case list of
        [] ->
            Nothing

        a :: after ->
            if predicate index a then
                Just ( a, { before = before, after = List.head after } )

            else
                findNeighboringAfter (Just a) (index + 1) predicate after


splitWhen : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


reverseMap : (a -> b) -> List a -> List b
reverseMap f xs =
    List.foldl (\x acc -> f x :: acc) [] xs


indexedFoldr : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i - 1, func i x thisAcc )
    in
    Tuple.second (List.foldr step ( List.length list - 1, acc ) list)


stableSortWith : (a -> a -> Basics.Order) -> List a -> List a
stableSortWith pred list =
    let
        listWithIndex =
            List.indexedMap (\i a -> ( a, i )) list

        predWithIndex ( a1, i1 ) ( a2, i2 ) =
            let
                result =
                    pred a1 a2
            in
            case result of
                Basics.EQ ->
                    Basics.compare i1 i2

                _ ->
                    result
    in
    List.sortWith predWithIndex listWithIndex |> List.map Tuple.first


elemIndex : a -> List a -> Maybe Int
elemIndex x =
    findIndex ((==) x)


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


uncons : List a -> Maybe ( a, List a )
uncons list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, rest )


{-| Variant of `foldl` that passes the index of the current element to the step function. `indexedFoldl` is to `List.foldl` as `List.indexedMap` is to `List.map`.
-}
indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (List.foldl step ( 0, acc ) list)


uniquePairs : List a -> List ( a, a )
uniquePairs xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            List.map (\y -> ( x, y )) xs_ ++ uniquePairs xs_


unconsLast : List a -> Maybe ( a, List a )
unconsLast list =
    case List.reverse list of
        [] ->
            Nothing

        last_ :: rest ->
            ( last_, List.reverse rest )
                |> Just


indexedFilterMap : (Int -> a -> Maybe b) -> Int -> List a -> List b -> List b
indexedFilterMap predicate index list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            indexedFilterMap predicate
                (index + 1)
                xs
                (case predicate index x of
                    Just b ->
                        b :: acc

                    Nothing ->
                        acc
                )


{-| Note: Doesn't preserve order of the list.
-}
listFilterThenMapInto : (a -> Bool) -> (a -> b) -> List a -> List b -> List b
listFilterThenMapInto predicate mapper list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            if predicate x then
                mapper x :: acc

            else
                listFilterThenMapInto predicate mapper xs acc


dictToListMap : (k -> v -> a) -> Dict k v -> List a -> List a
dictToListMap mapper dict baseAcc =
    Dict.foldr (\k v acc -> mapper k v :: acc) baseAcc dict


dictToListFilterAndMap : (k -> Bool) -> (k -> v -> a) -> Dict k v -> List a -> List a
dictToListFilterAndMap predicate mapper dict baseAcc =
    Dict.foldr
        (\k v acc ->
            if predicate k then
                mapper k v :: acc

            else
                acc
        )
        baseAcc
        dict


insertAllJusts : List ( a, Maybe comparable ) -> Set comparable -> Set comparable
insertAllJusts list set =
    case list of
        [] ->
            set

        ( _, head ) :: rest ->
            case head of
                Nothing ->
                    insertAllJusts rest set

                Just value ->
                    insertAllJusts rest (Set.insert value set)
