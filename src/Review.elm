module Review exposing
    ( parseFile, parseFiles
    , review, reviewFiles
    )

{-| Module to configure your review configuration and run it on a source file.


# Parsing files

@docs parseFile, parseFiles


# Reviewing

@docs review, reviewFiles

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


{-| Review a file and gives back the errors raised by the given rules.

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new

    errors : List Error
    errors =
        review config project sourceCode

-}
review : List Rule -> Project -> RawFile -> List Error
review config project rawFile =
    case parseSource rawFile.source of
        Ok file ->
            config
                |> List.concatMap (reviewWithRule project rawFile.path file)
                |> List.sortWith compareErrorPositions

        Err _ ->
            [ Rule.parsingError rawFile
            ]


reviewWithRule : Project -> String -> File -> Rule -> List Error
reviewWithRule project path file rule =
    -- Rule.analyzer rule project file
    -- |> List.map (ruleErrorToReviewError (moduleName file) rule)
    Debug.todo "Not done"


type alias RawFile =
    { path : String
    , source : String
    }


{-| TODO documentation
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


parseFiles : List RawFile -> ( List ParsedFile, List Error )
parseFiles files =
    files
        |> List.map parseFile
        |> List.foldl
            (\result ( files_, errors_ ) ->
                case result of
                    Ok file ->
                        ( file :: files_, errors_ )

                    Err error ->
                        ( files_, error :: errors_ )
            )
            ( [], [] )


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


compareErrorPositions : Error -> Error -> Order
compareErrorPositions a b =
    compareRange (Rule.errorRange a) (Rule.errorRange b)


compareRange : Range -> Range -> Order
compareRange a b =
    if a.start.row < b.start.row then
        LT

    else if a.start.row > b.start.row then
        GT

    else
    -- Start row is the same from here on
    if
        a.start.column < b.start.column
    then
        LT

    else if a.start.column > b.start.column then
        GT

    else
    -- Start row and column are the same from here on
    if
        a.end.row < b.end.row
    then
        LT

    else if a.end.row > b.end.row then
        GT

    else
    -- Start row and column, and end row are the same from here on
    if
        a.end.column < b.end.column
    then
        LT

    else if a.end.column > b.end.column then
        GT

    else
        EQ


{-| Parse source code into a AST
-}
parseSource : String -> Result String File
parseSource source =
    source
        |> Parser.parse
        |> Result.mapError (\error -> "Parsing error")
        |> Result.map (process init)
