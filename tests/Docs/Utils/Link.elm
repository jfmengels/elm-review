module Docs.Utils.Link exposing
    ( FileTarget(..)
    , Link
    , SubTarget(..)
    , findLinks
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)


addOffset : Int -> Range -> Range
addOffset lineNumber { start, end } =
    { start = addLineNumber lineNumber start
    , end = addLineNumber lineNumber end
    }


addLineNumber : Int -> Location -> Location
addLineNumber lineNumber { row, column } =
    { row = lineNumber + row
    , column = column + 1
    }


type alias Link =
    { file : FileTarget
    , startsWithDotSlash : Bool
    , slug : Maybe String
    }


type FileTarget
    = ModuleTarget ModuleName
    | ReadmeTarget
    | PackagesTarget { name : String, version : String, subTarget : SubTarget }
    | External String


type SubTarget
    = ModuleSubTarget ModuleName
    | ReadmeSubTarget


idParser : Char -> Parser String
idParser endChar =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= endChar && c /= ' ')
        |> Parser.getChompedString


onlyModuleNameParser : Parser ModuleName
onlyModuleNameParser =
    Parser.succeed identity
        |= moduleNameParser
        |. Parser.end


moduleNameParser : Parser ModuleName
moduleNameParser =
    manySeparated
        { by = "-"
        , item = moduleNameSegmentParser
        }


moduleNameSegmentParser : Parser String
moduleNameSegmentParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> Char.isUpper c)
        |. Parser.chompWhile (\c -> Char.isAlphaNum c)
        |> Parser.getChompedString


findLinks : Int -> ModuleName -> String -> List (Node Link)
findLinks row moduleName string =
    string
        |> String.lines
        |> List.indexedMap
            (\lineNumber lineContent ->
                lineContent
                    |> Parser.run (findParser (linkParser (lineNumber + row) moduleName))
                    |> Result.withDefault []
                    |> List.filterMap identity
                    |> List.indexedMap
                        (\index (Node { start, end } link) ->
                            Node
                                { start = { row = start.row, column = start.column - (index * 2) }
                                , end = { row = end.row, column = end.column - (index * 2) }
                                }
                                link
                        )
            )
        |> List.concat


linkParser : Int -> ModuleName -> Parser (Maybe (Node Link))
linkParser row moduleName =
    Parser.succeed identity
        |= Parser.getCol
        |. bracketsParser
        |> Parser.andThen
            (\col ->
                if col == 1 then
                    Parser.oneOf
                        [ inlineLinkParser
                            |> Parser.map Just
                        , referenceLinkParser
                            |> Parser.map Just
                        , Parser.succeed Nothing
                        ]

                else
                    Parser.oneOf
                        [ Parser.map Just inlineLinkParser
                        , Parser.succeed Nothing
                        ]
            )
        |> Parser.map
            (Maybe.map
                (\(Node range link) ->
                    Node (addOffset row range) (normalizeModuleName moduleName link)
                )
            )


normalizeModuleName : ModuleName -> Link -> Link
normalizeModuleName currentModuleName link =
    case link.file of
        ModuleTarget [] ->
            let
                file : FileTarget
                file =
                    if List.isEmpty currentModuleName then
                        ReadmeTarget

                    else
                        ModuleTarget currentModuleName
            in
            { link | file = file }

        ModuleTarget _ ->
            link

        ReadmeTarget ->
            link

        PackagesTarget _ ->
            link

        External _ ->
            link


{-| Parses things like:

    This is a [link](#Link).

-}
inlineLinkParser : Parser (Node Link)
inlineLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow, column = startCol - 2 }
                , end = { row = endRow, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol "("
        |= Parser.getPosition
        |= pathParser ')'
        |= Parser.getPosition
        |. Parser.chompUntil ")"
        |. Parser.symbol ")"


{-| Parses things like:

    [link]: #Link

-}
referenceLinkParser : Parser (Node Link)
referenceLinkParser =
    Parser.succeed
        (\( startRow, startCol ) link ( endRow, endCol ) ->
            Node
                { start = { row = startRow, column = startCol - 2 }
                , end = { row = endRow, column = endCol - 2 }
                }
                link
        )
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.getPosition
        |= pathParser '\n'
        |= Parser.getPosition


pathParser : Char -> Parser Link
pathParser endChar =
    Parser.oneOf
        [ Parser.succeed
            (\section ->
                { file = ModuleTarget [], startsWithDotSlash = False, slug = Just section }
            )
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed (\startsWithDotSlash file slug -> { file = file, startsWithDotSlash = startsWithDotSlash, slug = slug })
            |= ignoreDotSlash
            |= parseModuleName
            |= optionalSectionParser endChar
        ]


optionalSectionParser : Char -> Parser (Maybe String)
optionalSectionParser endChar =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.symbol "#"
            |= idParser endChar
        , Parser.succeed Nothing
        ]


parseModuleName : Parser FileTarget
parseModuleName =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c /= '#' && c /= ')' && c /= ' ')
        |> Parser.getChompedString
        |> Parser.map
            (\linkTarget ->
                if linkTarget == "" then
                    ReadmeTarget

                else
                    case Parser.run onlyModuleNameParser linkTarget of
                        Ok moduleName ->
                            ModuleTarget moduleName

                        Err _ ->
                            case Regex.find linkRegex linkTarget |> List.head |> Maybe.andThen parseSubTarget of
                                Just fileTarget ->
                                    fileTarget

                                Nothing ->
                                    External linkTarget
            )


parseSubTarget : Regex.Match -> Maybe FileTarget
parseSubTarget match =
    case match.submatches of
        (Just authorAndPackage) :: (Just linkVersion) :: _ :: rest :: [] ->
            let
                subTarget : SubTarget
                subTarget =
                    case rest of
                        Just nonemptyModuleName ->
                            nonemptyModuleName
                                |> String.replace "/" ""
                                |> String.split "-"
                                |> ModuleSubTarget

                        Nothing ->
                            ReadmeSubTarget
            in
            Just (PackagesTarget { name = authorAndPackage, version = linkVersion, subTarget = subTarget })

        _ ->
            Nothing


linkRegex : Regex
linkRegex =
    Regex.fromString "https://package\\.elm-lang\\.org/packages/([\\w-]+/[\\w-]+)/(latest|\\w+\\.\\w+\\.\\w+)(/(.*))?"
        |> Maybe.withDefault Regex.never


ignoreDotSlash : Parser Bool
ignoreDotSlash =
    Parser.oneOf
        [ Parser.symbol "."
            |. Parser.symbol "/"
            |> Parser.map (\_ -> True)
        , Parser.succeed False
        ]


bracketsParser : Parser ()
bracketsParser =
    Parser.succeed identity
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.chompUntil "]"
        |. Parser.spaces
        |. Parser.symbol "]"


findParser : Parser a -> Parser (List a)
findParser parser =
    Parser.loop []
        (\parsed ->
            Parser.oneOf
                [ Parser.succeed (\p -> p :: parsed)
                    |= parser
                    |> Parser.map Parser.Loop
                , Parser.succeed parsed
                    |. Parser.chompIf (\_ -> True)
                    |> Parser.map Parser.Loop
                , Parser.end
                    |> Parser.map (\() -> Parser.Done (List.reverse parsed))
                ]
        )


{-| 0 or more things directly separated by a string like "go-gi-ga".
-}
manySeparated :
    { by : String
    , item : Parser between
    }
    -> Parser (List between)
manySeparated { by, item } =
    Parser.sequence
        { start = ""
        , separator = by
        , end = ""
        , spaces = Parser.symbol ""
        , item = item
        , trailing = Parser.Forbidden
        }
