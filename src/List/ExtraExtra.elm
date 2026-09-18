module List.ExtraExtra exposing (fastConcatMap)

{-| -}


{-| This particular variant doesn't preserve the order like List.concatMap would, but it's marginally faster. We've adjusted the code using it.

<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap.elm>
<https://github.com/jfmengels/elm-benchmarks/blob/main/src/ListOrderingExploration/ListConcatMap-Results-Chrome.png>

-}
fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap fn list =
    List.foldr (\item acc -> fn item ++ acc) [] list
