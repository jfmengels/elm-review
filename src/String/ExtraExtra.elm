module String.ExtraExtra exposing (firstCharIsUpper, indent, multilineInput)

{-| -}


{-|

    indent 3 "a" -> "   a"
    indent 3 "a\nb" -> "   a\n   b"

-}
indent : Int -> String -> String
indent n string =
    let
        spaces : String
        spaces =
            String.repeat n " "
    in
    string
        |> String.lines
        |> List.map (\line -> spaces ++ line)
        |> String.join "\n"


{-| Allows us to have nicely formatted multi-line strings in parser tests etc.
-}
multilineInput : String -> String
multilineInput string =
    string
        |> unindent
        |> removeNewlinesAtEnds


{-| Basically String.trim but only handles newlines, not spaces.
-}
removeNewlinesAtEnds : String -> String
removeNewlinesAtEnds string =
    if String.startsWith "\n" string then
        removeNewlinesAtEnds (String.dropLeft 1 string)

    else if String.endsWith "\n" string then
        removeNewlinesAtEnds (String.dropRight 1 string)

    else
        string


firstCharIsUpper : String -> Bool
firstCharIsUpper str =
    let
        firstCodeUnit : String
        firstCodeUnit =
            String.left 1 str
    in
    String.any Char.isUpper firstCodeUnit
        || (String.any charIsUtf8Surrogate firstCodeUnit
                && String.any Char.isUpper (String.left 2 str)
           )


{-| Some code points like 🔧 are represented as 2 consecutive UTF-16 codes
within js strings.

So when we use `String.slice`, the resulting String might only contain
one of these halves which are called surrogates.

To check for that, the only way to tell whether you've encountered
a surrogate (that I can imagine at least) is by (ab)using that Char.toCode
accesses it's first _2_ indexes if the code at the first index indicates there must be a second half,
leading to NaN being returned.

-}
charIsUtf8Surrogate : Char -> Bool
charIsUtf8Surrogate char =
    Basics.isNaN (Basics.toFloat (Char.toCode char))


{-| Remove the shortest sequence of leading spaces or tabs on each line
of the string, so that at least one of the lines will not have any
leading spaces nor tabs and the rest of the lines will have the same
amount of indentation removed.

    unindent "  Hello\n    World" --> "Hello\n  World"

    unindent "\t\tHello\n\t\t\t\tWorld" --> "Hello\n\t\tWorld"

-}
unindent : String -> String
unindent multilineSting =
    let
        lines =
            String.lines multilineSting

        countLeadingWhitespace count line =
            case String.uncons line of
                Nothing ->
                    count

                Just ( char, rest ) ->
                    case char of
                        ' ' ->
                            countLeadingWhitespace (count + 1) rest

                        '\t' ->
                            countLeadingWhitespace (count + 1) rest

                        _ ->
                            count

        isNotWhitespace char =
            char /= ' ' && char /= '\t'

        minLead =
            lines
                |> List.filterMap
                    (\s ->
                        if String.any isNotWhitespace s then
                            Just (countLeadingWhitespace 0 s)

                        else
                            Nothing
                    )
                |> List.minimum
                |> Maybe.withDefault 0
    in
    if minLead == 0 then
        multilineSting

    else
        lines
            |> List.map (String.dropLeft minLead)
            |> String.join "\n"
