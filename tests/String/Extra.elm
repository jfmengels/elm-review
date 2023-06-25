module String.Extra exposing (isCapitalized)

{-| Some utilities.
-}


{-| Check if the first character of a string is upper case, unicode aware.

Note that `Char.isUpper` returns the correct result only for ASCII characters, even though it can be called with any Unicode character.
See <https://github.com/elm/core/pull/1138>

-}
isCapitalized : String -> Bool
isCapitalized string =
    case String.uncons string of
        Just ( char, _ ) ->
            -- 1. The character is its own upper variant.
            (char == Char.toUpper char)
                -- 2. The character is not its own lower variant.
                && (char /= Char.toLower char)

        Nothing ->
            False
