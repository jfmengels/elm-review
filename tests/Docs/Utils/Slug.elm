module Docs.Utils.Slug exposing (toSlug)

import Regex exposing (Regex)


toSlug : String -> String
toSlug string =
    string
        |> String.toLower
        |> Regex.replace specialsToStrip (\_ -> "")
        |> Regex.replace specialsToReplaceByDash (\_ -> "-")
        |> Regex.replace specialsToReplaceByUnderscore (\_ -> "_")
        |> Regex.replace multipleDashes (\_ -> "-")


specialsToReplaceByDash : Regex
specialsToReplaceByDash =
    "([.() \\[\\]`?]|\\\\)"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


specialsToReplaceByUnderscore : Regex
specialsToReplaceByUnderscore =
    "\\*"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


specialsToStrip : Regex
specialsToStrip =
    "[~$]|\\[`|`\\]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


multipleDashes : Regex
multipleDashes =
    "-+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
