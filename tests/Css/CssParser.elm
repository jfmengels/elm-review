module Css.CssParser exposing (parse)

import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)


parse : String -> Result (List Parser.DeadEnd) (Set String)
parse source =
    Parser.run cssParser source


cssParser : Parser (Set String)
cssParser =
    Parser.loop Set.empty cssRule


cssRule : Set String -> Parser (Parser.Step (Set String) (Set String))
cssRule acc =
    Parser.oneOf
        [ Parser.succeed (\selector -> Parser.Loop (Set.insert selector acc))
            |. Parser.token "."
            |= (Parser.chompWhile (\c -> Char.isAlphaNum c || c == '-' || c == '_')
                    |> Parser.getChompedString
               )
        , Parser.end
            |> Parser.map (\_ -> Parser.Done acc)
        , Parser.succeed (\() -> Parser.Loop acc)
            |. Parser.token "{"
            |. Parser.chompUntil "}"
            |. Parser.token "}"
            |= Parser.spaces
        , Parser.succeed (\() -> Parser.Loop acc)
            |= Parser.chompIf (always True)
        ]
