module Review exposing
    ( review
    , Error, errorModuleName, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes
    )

{-| Module to configure your review configuration and run it on a source file.


# Reviewing

@docs review


# Errors

@docs Error, errorModuleName, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes

-}

import Elm.Parser as Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix exposing (Fix)
import Review.Project exposing (Project)
import Review.Rule as Rule exposing (Rule)
import Review.Util as Util


{-| Represents an error in a file found by a rule.

Note: This should not be confused with `Error` from the `Review.Rule` module.
`Review.Error` is created from `Review.Rule.Error` but contains additional information
like the name of the rule that emitted it and the file name.

-}
type Error
    = Error
        { moduleName : Maybe String
        , ruleName : String
        , message : String
        , details : List String
        , range : Range
        , fixes : Maybe (List Fix)
        }



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
review : List Rule -> Project -> { path : String, source : String } -> List Error
review config project { path, source } =
    case parseSource source of
        Ok file ->
            config
                |> List.concatMap (reviewWithRule project path file)
                |> List.sortWith compareErrorPositions

        Err _ ->
            [ Error
                { moduleName = Nothing
                , ruleName = "ParsingError"
                , message = path ++ " is not a correct Elm file"
                , details =
                    [ "I could not understand the content of this file, and this prevents me from analyzing it. It is highly likely that the content of the file is not correct Elm code."
                    , "Hint: Try running `elm make`. The compiler should give you better hints on how to resolve the problem."
                    ]
                , range = { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                , fixes = Nothing
                }
            ]


reviewWithRule : Project -> String -> File -> Rule -> List Error
reviewWithRule project path file rule =
    Rule.analyzer rule project file
        |> List.map (ruleErrorToReviewError (moduleName file) rule)


moduleName : File -> String
moduleName file =
    let
        moduleNameNode : Node (List String)
        moduleNameNode =
            case Node.value file.moduleDefinition of
                NormalModule data ->
                    data.moduleName

                PortModule data ->
                    data.moduleName

                EffectModule data ->
                    data.moduleName
    in
    Util.moduleName moduleNameNode


compareErrorPositions : Error -> Error -> Order
compareErrorPositions (Error a) (Error b) =
    compareRange a.range b.range


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


ruleErrorToReviewError : String -> Rule -> Rule.Error -> Error
ruleErrorToReviewError moduleName_ rule error =
    Error
        { moduleName = Just moduleName_
        , ruleName = Rule.name rule
        , message = Rule.errorMessage error
        , details = Rule.errorDetails error
        , range = Rule.errorRange error
        , fixes = Rule.errorFixes error
        }


{-| Parse source code into a AST
-}
parseSource : String -> Result String File
parseSource source =
    source
        |> Parser.parse
        |> Result.mapError (\error -> "Parsing error")
        |> Result.map (process init)



-- ERRORS


{-| Get the name of the module for which the error occurred.
-}
errorModuleName : Error -> Maybe String
errorModuleName (Error error) =
    error.moduleName


{-| Get the name of the rule of an error.
-}
errorRuleName : Error -> String
errorRuleName (Error error) =
    error.ruleName


{-| Get the message of an error.
-}
errorMessage : Error -> String
errorMessage (Error error) =
    error.message


{-| Get the details of an error.
-}
errorDetails : Error -> List String
errorDetails (Error error) =
    error.details


{-| Get the range of an error.
-}
errorRange : Error -> Range
errorRange (Error error) =
    error.range


{-| Get the fixes for an error.
-}
errorFixes : Error -> Maybe (List Fix)
errorFixes (Error error) =
    error.fixes
