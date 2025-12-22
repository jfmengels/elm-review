module Simplify.CoreHelpers exposing
    ( isJust, isNothing, onNothing, maybeWithDefaultLazy
    , consIf, countUnique, countUniqueBy, findMap, listFind, indexedFindMap, findMapAndAllBefore, findMapNeighboring, listAll2, list2AreSameLengthAndAll, drop2EndingsWhile, listIndexedFilterMap, listLast, traverse, traverseConcat, uniqueByThenMap, listMapToStringsThenJoin
    , listFilledFromList, listFilledHead, listFilledInit, listFilledLast, listFilledLength, listFilledMap, listFilledTail, listFilledToList
    )

{-| Project-wide collection of utilities for `elm/core`,
moved to a separate module for easier testing etc.


## Maybe

@docs isJust, isNothing, onNothing, maybeWithDefaultLazy


## List

@docs consIf, countUnique, countUniqueBy, findMap, listFind, indexedFindMap, findMapAndAllBefore, findMapNeighboring, listAll2, list2AreSameLengthAndAll, drop2EndingsWhile, listIndexedFilterMap, listLast, traverse, traverseConcat, uniqueByThenMap, listMapToStringsThenJoin


## `( a, List a )`

@docs listFilledFromList, listFilledHead, listFilledInit, listFilledLast, listFilledLength, listFilledMap, listFilledTail, listFilledToList

-}

-- LIST HELPERS


consIf : Bool -> (() -> a) -> List a -> List a
consIf condition fn list =
    if condition then
        fn () :: list

    else
        list


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just (listFilledLast ( head, tail ))


listFilledFromList : List a -> Maybe ( a, List a )
listFilledFromList list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, tail )


listFilledLength : ( a, List a ) -> Int
listFilledLength ( _, tail ) =
    1 + List.length tail


listFilledToList : ( a, List a ) -> List a
listFilledToList ( head, tail ) =
    head :: tail


listFilledHead : ( a, List a ) -> a
listFilledHead ( head, _ ) =
    head


listFilledTail : ( a, List a ) -> List a
listFilledTail ( _, tail ) =
    tail


listFilledLast : ( a, List a ) -> a
listFilledLast ( head, tail ) =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast ( tailHead, tailTail )


listFilledInit : ( a, List a ) -> List a
listFilledInit ( head, tail ) =
    case tail of
        [] ->
            []

        tailHead :: tailTail ->
            head :: listFilledInit ( tailHead, tailTail )


listFilledMap : (a -> b) -> ( a, List a ) -> ( b, List b )
listFilledMap elementChange ( head, tail ) =
    ( elementChange head, List.map elementChange tail )


{-| `listAll2 f as bs` is equivalent to both

    List.all (\(a,b)-> f a b) (List.map2 Tuple.pair as bs)
    List.all identity (List.map2 f as bs)

but more performant.

-}
listAll2 : (a -> b -> Bool) -> List a -> List b -> Bool
listAll2 elementsAreRegular aList bList =
    case aList of
        [] ->
            True

        aHead :: aTail ->
            case bList of
                [] ->
                    True

                bHead :: bTail ->
                    if elementsAreRegular aHead bHead then
                        listAll2 elementsAreRegular aTail bTail

                    else
                        False


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                (Just _) as justFound ->
                    justFound

                Nothing ->
                    findMap mapper rest


listFind : (a -> Bool) -> List a -> Maybe a
listFind isFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if isFound head then
                Just head

            else
                listFind isFound tail


indexedFindMap : (Int -> a -> Maybe b) -> List a -> Maybe b
indexedFindMap elementToMaybeFound elements =
    indexedFindMapFromIndex 0 elementToMaybeFound elements


indexedFindMapFromIndex : Int -> (Int -> a -> Maybe b) -> List a -> Maybe b
indexedFindMapFromIndex index elementToMaybeFound elements =
    case elements of
        [] ->
            Nothing

        node :: rest ->
            case elementToMaybeFound index node of
                (Just _) as justFound ->
                    justFound

                Nothing ->
                    indexedFindMapFromIndex (index + 1) elementToMaybeFound rest


{-| `listIndexedFilterMap f list` is equivalent to `List.filterMap identity (List.indexedMap f)`
but more performant
-}
listIndexedFilterMap : (number -> a -> Maybe b) -> List a -> List b
listIndexedFilterMap indexedElementToMaybeNew list =
    listIndexedFilterMapFrom 0 [] indexedElementToMaybeNew list


listIndexedFilterMapFrom : number -> List a -> (number -> b -> Maybe a) -> List b -> List a
listIndexedFilterMapFrom index soFarReverse indexedElementToMaybeNew list =
    case list of
        [] ->
            List.reverse soFarReverse

        head :: tail ->
            listIndexedFilterMapFrom (index + 1)
                (case indexedElementToMaybeNew index head of
                    Nothing ->
                        soFarReverse

                    Just newElement ->
                        newElement :: soFarReverse
                )
                indexedElementToMaybeNew
                tail


findMapNeighboringAfter : Maybe a -> (a -> Maybe b) -> List a -> Maybe { before : Maybe a, found : b, after : Maybe a }
findMapNeighboringAfter before tryMap list =
    case list of
        [] ->
            Nothing

        now :: after ->
            case tryMap now of
                Just found ->
                    Just { before = before, found = found, after = after |> List.head }

                Nothing ->
                    findMapNeighboringAfter (Just now) tryMap after


findMapNeighboring : (a -> Maybe b) -> List a -> Maybe { before : Maybe a, found : b, after : Maybe a }
findMapNeighboring tryMap list =
    findMapNeighboringAfter Nothing tryMap list


findMapAndAllBefore : (a -> Maybe b) -> List a -> Maybe { before : List a, found : b }
findMapAndAllBefore tryMap list =
    foldUntilOkFrom []
        (\el beforeReversed ->
            case tryMap el of
                Nothing ->
                    Err (el :: beforeReversed)

                Just found ->
                    Ok
                        { found = found
                        , before = List.reverse beforeReversed
                        }
        )
        list
        |> Result.toMaybe


{-| A fold that can stop early (→ `Ok`) instead of traversing the whole list.

    [ 4, 8, -1, 2 ]
        -- take from the right while not negative
        |> foldUntilOkFrom []
            (\n beforeReversed ->
                if n < 0 then
                    Ok (List.reverse beforeReversed) -- stop the fold
                else
                    Err (n :: beforeReversed)
            )
        |> Result.map
    --> [ 4, 8 ]

-}
foldUntilOkFrom : folded -> (a -> folded -> Result folded b) -> List a -> Result folded b
foldUntilOkFrom initialFolded mapOrFoldFurther list =
    case list of
        [] ->
            Err initialFolded

        head :: tail ->
            case mapOrFoldFurther head initialFolded of
                (Ok _) as okFound ->
                    okFound

                Err newFolded ->
                    foldUntilOkFrom newFolded mapOrFoldFurther tail


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f list =
    traverseHelp f list []


traverseHelp : (a -> Maybe b) -> List a -> List b -> Maybe (List b)
traverseHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Just a ->
                    traverseHelp f tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)


{-| `traverseConcat f list` is equivalent to `Maybe.map List.concat (traverse f list)`
but more performant
-}
traverseConcat : (a -> Maybe (List b)) -> List a -> Maybe (List b)
traverseConcat f list =
    traverseConcatHelp f list []


traverseConcatHelp : (a -> Maybe (List b)) -> List a -> List b -> Maybe (List b)
traverseConcatHelp f list soFar =
    case list of
        head :: tail ->
            case f head of
                Just resultValues ->
                    traverseConcatHelp f tail (listAppendReverse resultValues soFar)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse soFar)


listAppendReverse : List a -> List a -> List a
listAppendReverse leftReverse right =
    case leftReverse of
        [] ->
            right

        leftLast :: leftBeforeLastReverse ->
            listAppendReverse leftBeforeLastReverse (leftLast :: right)


uniqueByThenMap : (a -> aspect) -> (a -> mapped) -> List a -> List mapped
uniqueByThenMap toAspectThatShouldBeUnique elementChange list =
    uniqueByThenMapHelp toAspectThatShouldBeUnique elementChange [] list []


uniqueByThenMapHelp : (a -> aspect) -> (a -> mapped) -> List aspect -> List a -> List mapped -> List mapped
uniqueByThenMapHelp toAspectThatShouldBeUnique elementChange existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                firstAspect : aspect
                firstAspect =
                    toAspectThatShouldBeUnique first
            in
            if List.member firstAspect existing then
                uniqueByThenMapHelp toAspectThatShouldBeUnique elementChange existing rest accumulator

            else
                uniqueByThenMapHelp toAspectThatShouldBeUnique
                    elementChange
                    (firstAspect :: existing)
                    rest
                    (elementChange first :: accumulator)


countUnique : List a -> Int
countUnique list =
    countUniqueBy Basics.identity list


countUniqueBy : (a -> b) -> List a -> Int
countUniqueBy toAspectThatShouldBeUnique list =
    countUniqueByHelp toAspectThatShouldBeUnique [] list 0


countUniqueByHelp : (a -> b) -> List b -> List a -> Int -> Int
countUniqueByHelp toAspectThatShouldBeUnique existing remaining soFar =
    case remaining of
        [] ->
            soFar

        first :: rest ->
            let
                firstAspect : b
                firstAspect =
                    toAspectThatShouldBeUnique first
            in
            if List.member firstAspect existing then
                countUniqueByHelp toAspectThatShouldBeUnique existing rest soFar

            else
                countUniqueByHelp toAspectThatShouldBeUnique (firstAspect :: existing) rest (soFar + 1)


list2AreSameLengthAndAll : (a -> b -> Bool) -> List a -> List b -> Bool
list2AreSameLengthAndAll areRegular aList bList =
    case aList of
        [] ->
            List.isEmpty bList

        aHead :: aTail ->
            case bList of
                [] ->
                    False

                bHead :: bTail ->
                    if areRegular aHead bHead then
                        list2AreSameLengthAndAll areRegular aTail bTail

                    else
                        False


{-| Remove elements at the end of both given lists, then repeat for the previous elements until a given test returns False
-}
drop2EndingsWhile : (a -> b -> Bool) -> List a -> List b -> ( List a, List b )
drop2EndingsWhile shouldDrop aList bList =
    let
        ( reducedArgumentsReverse, reducedPatternsReverse ) =
            drop2BeginningsWhile
                shouldDrop
                (List.reverse aList)
                (List.reverse bList)
    in
    ( List.reverse reducedArgumentsReverse, List.reverse reducedPatternsReverse )


drop2BeginningsWhile : (a -> b -> Bool) -> List a -> List b -> ( List a, List b )
drop2BeginningsWhile shouldDrop aList bList =
    case aList of
        [] ->
            ( [], bList )

        aHead :: aTail ->
            case bList of
                [] ->
                    ( aList, [] )

                bHead :: bTail ->
                    if shouldDrop aHead bHead then
                        drop2BeginningsWhile shouldDrop aTail bTail

                    else
                        ( aList, bList )


listMapToStringsThenJoin : (a -> String) -> String -> List a -> String
listMapToStringsThenJoin elementToString separator list =
    case list of
        [] ->
            ""

        head :: tail ->
            listMapToStringsThenJoinAfter (elementToString head) elementToString separator tail


listMapToStringsThenJoinAfter : String -> (a -> String) -> String -> List a -> String
listMapToStringsThenJoinAfter soFar elementToString separator list =
    case list of
        [] ->
            soFar

        head :: tail ->
            listMapToStringsThenJoinAfter
                (soFar ++ separator ++ elementToString head ++ "")
                elementToString
                separator
                tail



-- MAYBE HELPERS


{-| `Maybe.withDefault` evaluates its value eagerly, which results in wasted computation
-}
maybeWithDefaultLazy : (() -> a) -> Maybe a -> a
maybeWithDefaultLazy fallbackOnNothing maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            fallbackOnNothing ()


{-| Like `Maybe.andThen` but for the `Nothing` case.
Exceptionally useful for trying multiple things in order
-}
onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing nextTry maybe =
    case maybe of
        (Just _) as just ->
            just

        Nothing ->
            nextTry ()


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        Just _ ->
            False
