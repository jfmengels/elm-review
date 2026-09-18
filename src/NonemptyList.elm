module NonemptyList exposing
    ( NonemptyList
    , fromList
    , singleton
    , toList
    )


type alias NonemptyList a =
    ( a, List a )


{-|

    [] -> Nothing
    [a] -> Just (a, [])
    [a, b] -> Just (a, [b])

-}
fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


{-|

    a -> (a, [])

-}
singleton : a -> NonemptyList a
singleton a =
    ( a, [] )


{-|

    (a, []) -> [a]
    (a, [b]) -> [a, b]

-}
toList : NonemptyList a -> List a
toList ( x, xs ) =
    x :: xs
