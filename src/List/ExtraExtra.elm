module List.ExtraExtra exposing
    ( fastConcatMap
    , fastConcatMapWithInitial
    , findLastMap
    , findMap
    )

{-| -}


{-| Same result as `List.concatMap` (order is preserved).

<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap.elm>
<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap-Results-Chrome.png>

-}
fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    fastConcatMapWithInitial fn list []


fastConcatMapWithInitial : (a -> List b) -> List a -> List b -> List b
fastConcatMapWithInitial fn list initial =
    List.foldr (\item acc -> fn item ++ acc) initial list


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


findLastMap : (a -> Maybe b) -> List a -> Maybe b
findLastMap f list =
    List.foldr
        (\item acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    f item
        )
        Nothing
        list
