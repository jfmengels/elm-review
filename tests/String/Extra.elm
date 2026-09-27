module String.Extra exposing
    ( toSentenceCase, toTitleCase, decapitalize, isCapitalized
    , camelize, classify, underscored, dasherize, humanize
    , replaceSlice, insertAt, nonEmpty, nonBlank
    , break, softBreak
    , wrap, wrapWith, softWrap, softWrapWith, quote, surround
    , isBlank, countOccurrences
    , clean, unquote, unsurround, unindent, ellipsis, softEllipsis, ellipsisWith, stripTags, pluralize
    , toSentence, toSentenceOxford
    , rightOf, leftOf, rightOfBack, leftOfBack
    , toCodePoints, fromCodePoints
    )

{-| Additional functions for working with Strings


## Change words casing

@docs toSentenceCase, toTitleCase, decapitalize, isCapitalized


## Inflector functions

Functions borrowed from the Rails Inflector class

@docs camelize, classify, underscored, dasherize, humanize


## Replace and Splice

@docs replaceSlice, insertAt, nonEmpty, nonBlank


## Splitting

@docs break, softBreak


## Wrapping

@docs wrap, wrapWith, softWrap, softWrapWith, quote, surround


## Checks

@docs isBlank, countOccurrences


## Formatting

@docs clean, unquote, unsurround, unindent, ellipsis, softEllipsis, ellipsisWith, stripTags, pluralize


## Converting Lists

@docs toSentence, toSentenceOxford


## Finding

@docs rightOf, leftOf, rightOfBack, leftOfBack


## Converting UTF-32

@docs toCodePoints, fromCodePoints

-}

import Array
import Char exposing (toLower, toUpper)
import List
import Maybe exposing (Maybe(..))
import Regex exposing (Regex)
import String exposing (cons, uncons)


{-| Change the case of the first letter of a string to either uppercase or
lowercase, depending of the value of `wantedCase`. This is an internal
function for use in `toSentenceCase` and `decapitalize`.
-}
changeCase : (Char -> Char) -> String -> String
changeCase mutator word =
    uncons word
        |> Maybe.map (\( head, tail ) -> cons (mutator head) tail)
        |> Maybe.withDefault ""


{-| Capitalize the first letter of a string.

    toSentenceCase "this is a phrase" --> "This is a phrase"

    toSentenceCase "hello, world" --> "Hello, world"

-}
toSentenceCase : String -> String
toSentenceCase word =
    changeCase toUpper word


{-| Decapitalize the first letter of a string.

    decapitalize "This is a phrase" --> "this is a phrase"

    decapitalize "Hello, World" --> "hello, World"

-}
decapitalize : String -> String
decapitalize word =
    changeCase toLower word


{-| Capitalize the first character of each word in a string.

    toTitleCase "this is a phrase" --> "This Is A Phrase"

    toTitleCase "hello, world" --> "Hello, World"

-}
toTitleCase : String -> String
toTitleCase ws =
    Regex.replace titleCaseRegex (\{ match } -> String.toUpper match) ws


titleCaseRegex : Regex
titleCaseRegex =
    regexFromString "^([^\\s])|\\s[^\\s]"


{-| Replace text within a portion of a string given a substitution
string, a start index and an end index. The substitution includes the character
at the start index but not the one at the end index.

    replaceSlice "Sue" 4 7 "Hi, Bob" --> "Hi, Sue"

    replaceSlice "elephants" 0 6 "snakes on a plane!" --> "elephants on a plane!"

    replaceSlice "under" 7 9 "snakes on a plane!" --> "snakes under a plane!"

-}
replaceSlice : String -> Int -> Int -> String -> String
replaceSlice substitution start end string =
    String.slice 0 start string ++ substitution ++ String.slice end (String.length string) string


{-| Insert a substring at the specified index.

    insertAt "world" 6 "Hello " --> "Hello world"

-}
insertAt : String -> Int -> String -> String
insertAt insert pos string =
    replaceSlice insert pos pos string


{-| Break a string into a list of strings of a specified maximum length.

    break 10 "The quick brown fox" --> [ "The quick ", "brown fox" ]

    break 2 "" --> [ "" ]

-}
break : Int -> String -> List String
break width string =
    if width == 0 || string == "" then
        [ string ]

    else
        breaker width string []


breaker : Int -> String -> List String -> List String
breaker width string acc =
    case string of
        "" ->
            List.reverse acc

        _ ->
            breaker width
                (String.dropLeft width string)
                (String.slice 0 width string :: acc)


{-| Break a string into a list of strings of a specified maximum length,
without truncating words.

    softBreak 6 "The quick brown fox" --> [ "The ", "quick ", "brown ", "fox" ]

-}
softBreak : Int -> String -> List String
softBreak width string =
    if width <= 0 then
        []

    else
        string
            |> Regex.find (softBreakRegex width)
            |> List.map .match


softBreakRegex : Int -> Regex
softBreakRegex width =
    regexFromString <| ".{0," ++ String.fromInt (width - 1) ++ "}(\\s|.$)|\\S+?(\\s|$)"


{-| Trim the whitespace of both sides of the string and compress
repeated whitespace internally to a single whitespace char.

    clean " The   quick brown   fox    " --> "The quick brown fox"

-}
clean : String -> String
clean string =
    string
        |> Regex.replace cleanRegex (always " ")
        |> String.trim


cleanRegex : Regex
cleanRegex =
    regexFromString "\\s+"


{-| Test if a string is empty or only contains whitespace.

    isBlank "" --> True

    isBlank "\n" --> True

    isBlank "  " --> True

    isBlank " a" --> False

-}
isBlank : String -> Bool
isBlank string =
    String.trim string == ""


{-| Convert an underscored or dasherized string to a camelized one.

    camelize "-moz-transform" --> "MozTransform"

-}
camelize : String -> String
camelize string =
    Regex.replace
        camelizeRegex
        (\{ submatches } ->
            case submatches of
                (Just match) :: _ ->
                    String.toUpper match

                _ ->
                    ""
        )
        (String.trim string)


camelizeRegex : Regex
camelizeRegex =
    regexFromString "[-_\\s]+(.)?"


{-| Convert a string to a camelized string starting with an uppercase letter.
All non-alphanumeric characters will be stripped out of the original string.

    classify "some_class_name" --> "SomeClassName"

    classify "myLittleCamel.class.name" --> "MyLittleCamelClassName"

    classify "München" --> "MNchen"

-}
classify : String -> String
classify string =
    string
        |> Regex.replace classifyRegex (always " ")
        |> camelize
        |> String.replace " " ""
        |> toSentenceCase


classifyRegex : Regex
classifyRegex =
    regexFromString "[\\W_]"


{-| Surround a string with another string.

    surround "bar" "foo" --> "barfoobar"

-}
surround : String -> String -> String
surround wrapper string =
    wrapper ++ string ++ wrapper


{-| Remove surrounding strings from another string.

    unsurround "foo" "foobarfoo" --> "bar"

-}
unsurround : String -> String -> String
unsurround wrapper string =
    if String.startsWith wrapper string && String.endsWith wrapper string then
        if String.isEmpty wrapper then
            string

        else
            let
                length =
                    String.length wrapper
            in
            String.slice length -length string

    else
        string


{-| Add quotes to a string.

    quote "foo" --> "\"foo\""

-}
quote : String -> String
quote string =
    surround "\"" string


{-| Remove quotes that surround a string.

    unquote "\"foo\"" --> "foo"

    unquote "\"foo\"bar\"" --> "foo\"bar"

-}
unquote : String -> String
unquote string =
    unsurround "\"" string


{-| Return a string joined by underscores after separating it by its uppercase characters.
Any sequence of spaces or dashes will also be converted to a single underscore.
The final string will be lowercased.

    underscored "SomeClassName" --> "some_class_name"

    underscored "some-class-name" --> "some_class_name"

    underscored "SomeClass name" --> "some_class_name"

-}
underscored : String -> String
underscored string =
    string
        |> String.trim
        |> Regex.replace underscoredAlphaRegex
            (\{ submatches } ->
                submatches
                    |> List.filterMap identity
                    |> String.join "_"
            )
        |> Regex.replace underscoredDelimiterRegex (always "_")
        |> String.toLower


underscoredAlphaRegex : Regex
underscoredAlphaRegex =
    regexFromString "([a-z\\d])([A-Z]+)"


underscoredDelimiterRegex : Regex
underscoredDelimiterRegex =
    regexFromString "[_\\-\\s]+"


{-| Return a string joined by dashes after separating it by its uppercase characters.
Any sequence of spaces or underscores will also be converted to a single dash.
The final string will be lowercased.

    dasherize "SomeClassName" --> "some-class-name"

    dasherize "some_class_name" --> "some-class-name"

    dasherize "someClass name" --> "some-class-name"

-}
dasherize : String -> String
dasherize string =
    string
        |> underscored
        |> String.replace "_" "-"


{-| Separate a string into parts of a given width, using a given separator.

Look at `wrap` if you just want to wrap using newlines.

    wrapWith 7 "\n" "My very long text" --> "My very\n long t\next"

    wrapWith 100 "\n" "Too short" --> "Too short"

-}
wrapWith : Int -> String -> String -> String
wrapWith width separator string =
    string
        |> break width
        |> String.join separator


{-| Chop a given string into parts of a given width, separating them with a
new line.

    wrap 7 "My very long text" --> "My very\n long t\next"

    wrap 100 "Too short" --> "Too short"

-}
wrap : Int -> String -> String
wrap width string =
    wrapWith width "\n" string


{-| Chop a given string into parts of a given width without breaking words apart,
and then separate them using a new line.

    softWrap 9 "My very long text" --> "My very\nlong text"

    softWrap 3 "Hello World" --> "Hello\nWorld"

    softWrap 100 "Too short" --> "Too short"

-}
softWrap : Int -> String -> String
softWrap width string =
    softWrapWith width "\n" string


{-| Chop a given string into parts of a given width without breaking words apart,
and then separate them using the given separator.

    softWrapWith 9 "..." "My very long text" --> "My very...long text"

    softWrapWith 3 "\n" "Hello World" --> "Hello\nWorld"

    softWrapWith 100 "\t" "Too short" --> "Too short"

-}
softWrapWith : Int -> String -> String -> String
softWrapWith width separator string =
    string
        |> softBreak width
        |> List.map trimEndSpace
        |> String.join separator


trimEndSpace : String -> String
trimEndSpace =
    Regex.replace trimEndSpaceRegex (always "")


trimEndSpaceRegex : Regex
trimEndSpaceRegex =
    regexFromString " ?$"


{-| Convert an underscored, camelized, or dasherized string into one that can be
read by humans. Also remove beginning and ending whitespace, and removes the
postfix '\_id'. The first character will be capitalized.

    humanize "this_is_great" --> "This is great"

    humanize "ThisIsGreat" --> "This is great"

    humanize "this-is-great" --> "This is great"

    humanize "author_id" --> "Author"

-}
humanize : String -> String
humanize string =
    string
        |> Regex.replace humanizeAlphaRegex (\{ match } -> "-" ++ match)
        |> Regex.replace humanizeIgnoreRegex (always " ")
        |> String.trim
        |> String.toLower
        |> toSentenceCase


humanizeAlphaRegex : Regex
humanizeAlphaRegex =
    regexFromString "[A-Z]+"


humanizeIgnoreRegex : Regex
humanizeIgnoreRegex =
    regexFromString "_id$|[-_\\s]+"


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


{-| Return the number of occurrences of a substring in another string.

    countOccurrences "Hello" "Hello World" --> 1

    countOccurrences "o" "Hello World" --> 2

-}
countOccurrences : String -> String -> Int
countOccurrences needle haystack =
    if String.length needle == 0 || String.length haystack == 0 then
        0

    else
        haystack
            |> String.indexes needle
            |> List.length


{-| Truncate the second string at the specified length if the string is
longer than the specified length, and replace the end of the truncated
string with the first string, such that the resulting string is of the
specified length.

The resulting string will have at most the specified length.

    ellipsisWith 5 " .." "Hello World" --> "He .."

    ellipsisWith 10 " .." "Hello World" --> "Hello W .."

    ellipsisWith 10 " .." "Hello" --> "Hello"

    ellipsisWith 8 " .." "Hello World" --> "Hello .."

-}
ellipsisWith : Int -> String -> String -> String
ellipsisWith howLong append string =
    if String.length string <= howLong then
        string

    else
        String.left (howLong - String.length append) string ++ append


{-| Truncate the string at the specified length if the string is
longer than the specified length, and replace the end of the truncated
string with `"..."`, such that the resulting string is of the
specified length.

The resulting string will have at most the specified length.

    ellipsis 5 "Hello World" --> "He..."

    ellipsis 10 "Hello World" --> "Hello W..."

    ellipsis 10 "Hello" --> "Hello"

    ellipsis 8 "Hello World" --> "Hello..."

-}
ellipsis : Int -> String -> String
ellipsis howLong string =
    ellipsisWith howLong "..." string


{-| Truncate the string at the last complete word less than or equal to
the specified length and append `"..."`. When the specified length is
less than the length of the first word, the ellipsis is appended to the
first word. When the specified length is greater than or equal to the
length of the string, an identical string is returned.

In contrast to `ellipsis`, this function will not produce incomplete
words, and the resulting string can exceed the specified length. In
addition, it removes trailing whitespace and punctuation characters at
the end of the truncated string.

    softEllipsis 1 "Hello, World" --> "Hello..."

    softEllipsis 5 "Hello, World" --> "Hello..."

    softEllipsis 6 "Hello, World" --> "Hello..."

    softEllipsis 15 "Hello, cruel world" --> "Hello, cruel..."

    softEllipsis 10 "Hello" --> "Hello"

-}
softEllipsis : Int -> String -> String
softEllipsis howLong string =
    if String.length string <= howLong then
        string

    else
        string
            |> Regex.findAtMost 1 (softBreakRegex howLong)
            |> List.map .match
            |> String.concat
            |> Regex.replace softEllipsisRegex (always "")
            |> (\a -> String.append a "...")


softEllipsisRegex : Regex
softEllipsisRegex =
    regexFromString "([\\.,;:\\s])+$"


{-| Convert a list of strings into a human-readable list.

    toSentence [] --> ""

    toSentence [ "lions" ] --> "lions"

    toSentence [ "lions", "tigers" ] --> "lions and tigers"

    toSentence [ "lions", "tigers", "bears" ] --> "lions, tigers and bears"

-}
toSentence : List String -> String
toSentence list =
    case list of
        x :: y :: z :: more ->
            toSentenceHelper " and " (x ++ ", " ++ y) (z :: more)

        _ ->
            toSentenceBaseCase list


{-| Convert a list of strings into a human-readable list using an oxford comma.

    toSentenceOxford [] --> ""

    toSentenceOxford [ "lions" ] --> "lions"

    toSentenceOxford [ "lions", "tigers" ] --> "lions and tigers"

    toSentenceOxford [ "lions", "tigers", "bears" ] --> "lions, tigers, and bears"

-}
toSentenceOxford : List String -> String
toSentenceOxford list =
    case list of
        x :: y :: z :: more ->
            toSentenceHelper ", and " (x ++ ", " ++ y) (z :: more)

        _ ->
            toSentenceBaseCase list


toSentenceBaseCase : List String -> String
toSentenceBaseCase list =
    case list of
        x :: [] ->
            x

        x :: y :: [] ->
            x ++ " and " ++ y

        _ ->
            ""


toSentenceHelper : String -> String -> List String -> String
toSentenceHelper lastPart sentence list =
    case list of
        [] ->
            sentence

        x :: [] ->
            sentence ++ lastPart ++ x

        x :: xs ->
            toSentenceHelper lastPart (sentence ++ ", " ++ x) xs


{-| Remove all HTML tags from the string, preserving the text inside them.

    stripTags "a <a href=\"#\">link</a>" --> "a link"

    stripTags "<script>alert('hello world!')</script>" --> "alert('hello world!')"

-}
stripTags : String -> String
stripTags string =
    string
        |> Regex.replace stripTagsRegex (always "")


stripTagsRegex : Regex
stripTagsRegex =
    regexFromString "<\\/?[^>]+>"


{-| Given a number, a singular string, and a plural string, return the number
followed by a space, followed by either the singular string if the number was 1,
or the plural string otherwise.

    pluralize "elf" "elves" 2 --> "2 elves"

    pluralize "elf" "elves" 1 --> "1 elf"

    pluralize "elf" "elves" 0 --> "0 elves"

**Note:** This will only work in English and if you anticipate needing to translate
your application into multiple languages, you would be better served by adopting
a [package better prepared to handle various languages](https://package.elm-lang.org/packages/GlobalWebIndex/elm-plural-rules/latest/PluralRules).

-}
pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count == 1 then
        "1 " ++ singular

    else
        String.fromInt count ++ " " ++ plural


{-| Search a string from left to right for a pattern and return a substring
consisting of the characters in the string that are to the right of the pattern.

    rightOf "_" "This_is_a_test_string" --> "is_a_test_string"

-}
rightOf : String -> String -> String
rightOf pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: _ ->
            String.slice (String.length pattern + firstIndex) (String.length string) string


{-| Search a string from left to right for a pattern and return a substring
consisting of the characters in the string that are to the left of the pattern.

    leftOf "_" "This_is_a_test_string" --> "This"

-}
leftOf : String -> String -> String
leftOf pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: _ ->
            String.slice 0 firstIndex string


{-| Search a string from right to left for a pattern and return a substring
consisting of the characters in the string that are to the right of the pattern.

    rightOfBack "_" "This_is_a_test_string" --> "string"

-}
rightOfBack : String -> String -> String
rightOfBack pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: rest ->
            case last rest of
                Just lastIndex ->
                    String.slice (String.length pattern + lastIndex) (String.length string) string

                Nothing ->
                    String.slice (String.length pattern + firstIndex) (String.length string) string


{-| Search a string from right to left for a pattern and return a substring
consisting of the characters in the string that are to the left of the pattern.

    leftOfBack "_" "This_is_a_test_string" --> "This_is_a_test"

-}
leftOfBack : String -> String -> String
leftOfBack pattern string =
    case String.indexes pattern string of
        [] ->
            ""

        firstIndex :: rest ->
            case last rest of
                Just lastIndex ->
                    String.slice 0 lastIndex string

                Nothing ->
                    String.slice 0 firstIndex string


last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


{-| Convert a string into a list of UTF-32 code points.

    toCodePoints "abc" --> [ 97, 98, 99 ]

    toCodePoints "©§π" --> [ 169, 167, 960 ]

    toCodePoints "💩!" --> [ 128169, 33 ]

Note that code points do not necessarily correspond to logical/visual
characters, since it is possible for things like accented characters to be
represented as two separate UTF-32 code points (a base character and a
combining accent).

`toCodePoints string` is equivalent to:

    List.map Char.toCode (String.toList string)

-}
toCodePoints : String -> List Int
toCodePoints string =
    List.map Char.toCode (String.toList string)


{-| Convert a list of UTF-32 code points into a string. Inverse of
`toCodePoints`.

    fromCodePoints [ 97, 98, 99 ] --> "abc"

    fromCodePoints [ 169, 167, 960 ] --> "©§π"

    fromCodePoints [ 128169, 33 ] --> "💩!"

`fromCodePoints codePoints` is equivalent to:

    String.fromList (List.map Char.fromCode codePoints)

-}
fromCodePoints : List Int -> String
fromCodePoints codePoints =
    String.fromList (List.map Char.fromCode codePoints)


{-| Convert a string to a Nothing when empty.

    nonEmpty "" --> Nothing

    nonEmpty "Hello world" --> Just "Hello world"

-}
nonEmpty : String -> Maybe String
nonEmpty string =
    if String.isEmpty string then
        Nothing

    else
        Just string


{-| Convert a string to a Nothing when blank.

    nonBlank "" --> Nothing

    nonBlank " " --> Nothing

    nonBlank "Hello world" --> Just "Hello world"

-}
nonBlank : String -> Maybe String
nonBlank string =
    if isBlank string then
        Nothing

    else
        Just string


regexFromString : String -> Regex
regexFromString str =
    Regex.fromString str
        |> Maybe.withDefault Regex.never


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
