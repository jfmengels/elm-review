module Css.NoUnusedClasses exposing (cssFiles, dontReport, rule, withCssUsingFunctions)

import Css.ClassFunction as ClassFunction exposing (CssArgument)
import Css.CssParser
import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Regex exposing (Regex)
import Review.FilePattern as FilePattern exposing (FilePattern)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Configuration -> Rule
rule (Configuration configuration) =
    Rule.newProjectRuleSchema "Css.NoUnusedClasses" initialProjectContext
        |> Rule.withExtraFilesProjectVisitor cssFilesVisitor
            [ FilePattern.include "**/*.css" ]
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type Configuration
    = Configuration
        { classesNotToReport : Set String
        , cssFiles : List FilePattern
        , cssFunctions : CssFunctions
        }


type alias CssFunctions =
    Dict String (ClassFunction.Arguments -> List CssArgument)


cssFiles : List FilePattern -> Configuration
cssFiles globs =
    Configuration
        { classesNotToReport = Set.empty
        , cssFiles = globs
        , cssFunctions = Dict.fromList ClassFunction.baseCssFunctions
        }


withCssUsingFunctions :
    List ( String, ClassFunction.Arguments -> List CssArgument )
    -> Configuration
    -> Configuration
withCssUsingFunctions newFunctions (Configuration configuration) =
    Configuration { configuration | cssFunctions = List.foldl (\( key, fn ) acc -> Dict.insert key fn acc) configuration.cssFunctions newFunctions }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


dontReport : List String -> Configuration -> Configuration
dontReport classesNotToReport (Configuration configuration) =
    Configuration { configuration | classesNotToReport = List.foldl Set.insert configuration.classesNotToReport classesNotToReport }


type alias ProjectContext =
    { cssFiles :
        Dict
            String
            { fileKey : Rule.ExtraFileKey
            , classes : Set String
            }
    , usedCssClasses : Set String
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , usedCssClasses : Set String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { cssFiles = Dict.empty
    , usedCssClasses = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable _ ->
            { lookupTable = lookupTable
            , usedCssClasses = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\{ usedCssClasses } ->
            { cssFiles = Dict.empty
            , usedCssClasses = usedCssClasses
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { cssFiles = previousContext.cssFiles
    , usedCssClasses = Set.union newContext.usedCssClasses previousContext.usedCssClasses
    }


cssFilesVisitor : Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error scope), ProjectContext )
cssFilesVisitor files context =
    let
        ( errors, cssFiles_ ) =
            Dict.foldl parseCssFile ( [], Dict.empty ) files
    in
    ( errors, { cssFiles = cssFiles_, usedCssClasses = context.usedCssClasses } )


parseCssFile :
    String
    -> { fileKey : Rule.ExtraFileKey, content : String }
    ->
        ( List (Rule.Error scope)
        , Dict
            String
            { fileKey : Rule.ExtraFileKey
            , classes : Set String
            }
        )
    ->
        ( List (Rule.Error scope)
        , Dict
            String
            { fileKey : Rule.ExtraFileKey
            , classes : Set String
            }
        )
parseCssFile filePath file ( errors, dict ) =
    case Css.CssParser.parse file.content of
        Ok cssClasses ->
            ( errors, Dict.insert filePath { fileKey = file.fileKey, classes = cssClasses } dict )

        Err _ ->
            ( Rule.errorForExtraFile file.fileKey
                { message = "Unable to parse CSS file `" ++ filePath ++ "`"
                , details = [ "Please check that this file is syntactically correct. It is possible that I'm mistaken as my CSS parser is still very naive. Contributions are welcome to solve the issue." ]
                }
                { start = { row = 1, column = 1 }, end = { row = 1, column = 100000 } }
                :: errors
            , dict
            )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.Application [ function, firstArg ] ->
            case Node.value function of
                Expression.FunctionOrValue [ "Html", "Attributes" ] "class" ->
                    case Node.value firstArg of
                        Expression.Literal stringLiteral ->
                            let
                                usedCssClasses : List String
                                usedCssClasses =
                                    String.split " " stringLiteral
                            in
                            ( []
                            , { context | usedCssClasses = List.foldl Set.insert context.usedCssClasses usedCssClasses }
                            )

                        _ ->
                            ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation context =
    context.cssFiles
        |> Dict.toList
        |> List.filterMap (\( filePath, file ) -> reportUnusedClasses context.usedCssClasses filePath file)


reportUnusedClasses : Set String -> String -> { a | fileKey : Rule.ExtraFileKey, classes : Set String } -> Maybe (Rule.Error { useErrorForModule : () })
reportUnusedClasses usedCssClasses filePath { fileKey, classes } =
    let
        unusedClasses : Set String
        unusedClasses =
            Set.diff classes usedCssClasses
    in
    if Set.isEmpty unusedClasses then
        Nothing

    else
        Just
            (Rule.errorForExtraFile fileKey
                { message = "Found unused CSS classes"
                , details =
                    [ "This file declared the usage of some CSS classes for which I could not any usage in the Elm codebase. Please check that no typo was made in the name of the classes, and remove them if they still seem unused."
                    , "Here are the classes that seem unused: " ++ String.join " " (Set.toList unusedClasses)
                    ]
                }
                { start = { row = 1, column = 1 }, end = { row = 1, column = 100000 } }
            )
