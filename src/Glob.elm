module Glob exposing
    ( Glob, fromString, never
    , match
    )

{-| A library for working with [glob].

[glob]: https://en.wikipedia.org/wiki/Glob_%28programming%29

@docs Glob, fromString, never
@docs match

-}

import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import Set


{-| A Glob expression.
-}
type Glob
    = Glob (List Component)


type Component
    = TwoAsterisks
    | Fragments Regex


type Fragment
    = Literal String
    | Alternatives (List String)
    | Class { negative : Bool, inner : String }
    | QuestionMark
    | Asterisk


{-| Match a file path against a glob.
-}
match : Glob -> String -> Bool
match (Glob parsed) input =
    matchComponents parsed (String.split "/" input)


matchComponents : List Component -> List String -> Bool
matchComponents components segments =
    case ( components, segments ) of
        ( [], [] ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( [], _ :: _ ) ->
            False

        ( TwoAsterisks :: ctail, _ :: stail ) ->
            if matchComponents components stail then
                True

            else
                matchComponents ctail segments

        ( (Fragments chead) :: ctail, shead :: stail ) ->
            if Regex.contains chead shead then
                matchComponents ctail stail

            else
                False


{-| A `glob` that never matches.
-}
never : Glob
never =
    Glob []


{-| Parse a string into a `glob`.
-}
fromString : String -> Result (List Parser.DeadEnd) Glob
fromString input =
    input
        |> Parser.run parser
        |> Result.map Glob


parser : Parser (List Component)
parser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "/"
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        , item = componentParser
        }
        |. Parser.end


componentParser : Parser Component
componentParser =
    Parser.oneOf
        [ Parser.succeed TwoAsterisks
            |. Parser.symbol "**"
        , Parser.succeed fragmentsToRegex
            |= Parser.getOffset
            |= Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , trailing = Parser.Optional
                , spaces = Parser.succeed ()
                , item = fragmentParser
                }
            |= Parser.getOffset
            |= Parser.getSource
            |> Parser.andThen identity
            |> Parser.map Fragments
        ]


fragmentsToRegex : Int -> List Fragment -> Int -> String -> Parser Regex
fragmentsToRegex before fragments after source =
    let
        regexString : String
        regexString =
            List.foldl
                (\fragment acc -> acc ++ fragmentToRegexString fragment)
                "^"
                fragments
    in
    case Regex.fromStringWith { caseInsensitive = False, multiline = True } (regexString ++ "$") of
        Nothing ->
            Parser.problem <|
                "Could not parse \""
                    ++ regexString
                    ++ "\" as a regex, obtained from "
                    ++ String.slice before after source

        Just regex ->
            Parser.succeed regex


fragmentToRegexString : Fragment -> String
fragmentToRegexString fragment =
    case fragment of
        Literal literal ->
            regexEscape literal

        Alternatives alternatives ->
            "(" ++ String.join "|" (List.map regexEscape alternatives) ++ ")"

        Class { negative, inner } ->
            let
                cut : String
                cut =
                    inner
                        |> String.replace "^" "\\^"
                        |> String.replace "\\" "\\\\"
            in
            if negative then
                "[^" ++ cut ++ "]"

            else
                "[" ++ cut ++ "]"

        QuestionMark ->
            "."

        Asterisk ->
            ".*"


regexEscape : String -> String
regexEscape input =
    input
        |> String.foldr
            (\c acc ->
                if Char.isAlphaNum c then
                    c :: acc

                else
                    case c of
                        '\\' ->
                            '\\' :: '\\' :: acc

                        ']' ->
                            '\\' :: c :: acc

                        _ ->
                            '[' :: c :: ']' :: acc
            )
            []
        |> String.fromList


fragmentParser : Parser Fragment
fragmentParser =
    Parser.oneOf
        [ Parser.succeed Literal
            |. Parser.symbol "\\"
            |= Parser.getChompedString (Parser.chompIf (\_ -> True))
        , Parser.succeed QuestionMark
            |. Parser.symbol "?"
        , Parser.succeed Asterisk
            |. Parser.symbol "*"
        , Parser.succeed (Alternatives << Set.toList << Set.fromList)
            |= Parser.sequence
                { start = "{"
                , end = "}"
                , separator = ","
                , trailing = Parser.Forbidden
                , spaces = Parser.succeed ()
                , item = nonemptyChomper <| \c -> notSpecial c && c /= ','
                }
        , Parser.succeed
            (\negative inner closed source ->
                if closed then
                    Class { negative = negative, inner = inner }

                else
                    Literal source
            )
            |. Parser.symbol "["
            |= Parser.oneOf
                [ Parser.succeed True
                    |. Parser.symbol "!"
                , Parser.succeed False
                ]
            |= Parser.getChompedString
                (Parser.chompWhile (\c -> c /= ']'))
            |= Parser.oneOf
                [ Parser.symbol "]"
                    |> Parser.map (\() -> True)
                , Parser.end
                    |> Parser.map (\() -> False)
                ]
            |= Parser.getSource
        , Parser.succeed Literal
            |= nonemptyChomper notSpecial
        , Parser.problem "fragmentParser"
        ]


nonemptyChomper : (Char -> Bool) -> Parser String
nonemptyChomper f =
    Parser.getChompedString
        (Parser.chompIf f
            |. Parser.chompWhile f
        )


notSpecial : Char -> Bool
notSpecial c =
    not (isSpecialChar c)


isSpecialChar : Char -> Bool
isSpecialChar c =
    (c == '/')
        || (c == '*')
        || (c == '{')
        || (c == '}')
        || (c == '[')
        || (c == ']')
        || (c == '?')
