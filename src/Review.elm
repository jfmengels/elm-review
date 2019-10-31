module Review exposing
    ( parseFile
    , reviewFiles, reviewRawFiles
    )

{-| Module to configure your review configuration and run it on a source file.


# Parsing files

@docs parseFile


# Reviewing

@docs reviewFiles, reviewRawFiles

-}

import Elm.Parser as Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Review.File exposing (ParsedFile, RawFile)
import Review.Fix exposing (Fix)
import Review.Project exposing (Project)
import Review.Rule as Rule exposing (Error, Rule)



-- REVIEWING


{-| Review a list of files and gives back the errors raised by the given rules.

    import Review
    import Review.File exposing (ParsedFile)

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new

    type alias Model =
        { parsedFiles : ParsedFile
        , parsingErrors : List Review.Error
        }

    model : Model
    model =
        [ { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
        , { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }
        ]
            |> List.foldl
                (\file model_ ->
                    case Review.parseFile file of
                        Ok parsedFile ->
                            { model_ | parsedFiles = parsedFile :: model_.parsedFiles }

                        Err parsingError ->
                            { model_ | parsingErrors = parsingError :: model_.parsingErrors }
                )
                { parsedFiles = [], parsingErrors = [] }

    errors : List Error
    errors =
        List.concat
            [ Review.reviewFiles rules project model.parsedFiles
            , model.parsingErrors
            ]

-}
reviewFiles : List Rule -> Project -> List ParsedFile -> List Error
reviewFiles rules project files =
    rules
        |> List.concatMap
            (\rule ->
                case Rule.analyzer rule of
                    Rule.Single fn ->
                        List.concatMap (fn project) files

                    Rule.Multi fn ->
                        fn project files
            )


{-| Review a list of raw files and gives back the errors raised by the given rules.

    import Review
    import Review.File exposing (RawFile)

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new

    files : List RawFile
    files =
        [ { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
        , { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }
        ]

    errors : List Error
    errors =
        Review.reviewRawFiles rules project files

Note: This function does the same thing a [`reviewFiles`](#reviewFiles), except
that it also parses the files using `elm-syntax`. If you work with already
parsed files, or if you are likely to review files multiple times, you should
use [`reviewFiles`](#reviewFiles).

-}
reviewRawFiles : List Rule -> Project -> List RawFile -> List Error
reviewRawFiles rules project files =
    let
        ( parsedFiles, parsingErrors ) =
            files
                |> List.map parseFile
                |> List.foldl
                    (\result ( parsed, errors ) ->
                        case result of
                            Ok parsedFile ->
                                ( parsedFile :: parsed, errors )

                            Err parsingError ->
                                ( parsed, parsingError :: errors )
                    )
                    ( [], [] )
    in
    List.concat
        [ reviewFiles rules project parsedFiles
        , parsingErrors
        ]


parseFile : RawFile -> Result Error ParsedFile
parseFile rawFile =
    case parseSource rawFile.source of
        Ok ast ->
            Ok
                { path = rawFile.path
                , source = rawFile.source
                , ast = ast
                }

        Err _ ->
            Err (Rule.parsingError rawFile)


{-| Parse source code into a AST
-}
parseSource : String -> Result String File
parseSource source =
    source
        |> Parser.parse
        |> Result.mapError (\error -> "Parsing error")
        |> Result.map (process init)
