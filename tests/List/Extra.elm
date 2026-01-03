module List.Extra exposing
    ( dictToListFilterAndMap
    , dictToListMap
    , find
    , findMap
    , findMapWithIndex
    , findNeighboring
    , getNeighboring
    , indexedFilterMap
    , insertAllJusts
    , listFilterThenMapInto
    , maybeCons
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
