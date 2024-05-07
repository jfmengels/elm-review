module Css.NoUnusedClasses exposing (cssFiles, dontReport, rule, withCssUsingFunctions)

import Css.ClassFunction as ClassFunction exposing (CssArgument)
import Css.CssParser
import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import RangeDict exposing (RangeDict)
import Regex exposing (Regex)
import Review.FilePattern as FilePattern exposing (FilePattern)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Configuration -> Rule
rule (Configuration configuration) =
    Rule.newProjectRuleSchema "Css.NoUnusedClasses" initialProjectContext
        |> Rule.withExtraFilesProjectVisitor cssFilesVisitor
            [ FilePattern.include "**/*.css" ]
        |> Rule.withModuleVisitor (moduleVisitor configuration.cssFunctions)
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


moduleVisitor : CssFunctions -> Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor cssFunctions schema =
    schema
        |> Rule.withExpressionEnterVisitor (expressionVisitor cssFunctions)


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
    , functionOrValuesToIgnore : RangeDict ()
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
            , functionOrValuesToIgnore = RangeDict.empty
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


expressionVisitor : CssFunctions -> Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor cssFunctions node context =
    case Node.value node of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ name)) :: firstArg :: restOfArguments) ->
            registerClasses cssFunctions context fnRange name firstArg restOfArguments

        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ name)) ->
            registerClasses cssFunctions context fnRange name firstArg []

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ name)) firstArg ->
            registerClasses cssFunctions context fnRange name firstArg []

        Expression.FunctionOrValue _ name ->
            ( reportStrayCssFunction cssFunctions context (Node.range node) name
            , context
            )

        _ ->
            ( [], context )


reportStrayCssFunction : CssFunctions -> ModuleContext -> Range -> String -> List (Rule.Error {})
reportStrayCssFunction cssFunctions context range name =
    if RangeDict.member range context.functionOrValuesToIgnore then
        []

    else
        case
            ModuleNameLookupTable.moduleNameAt context.lookupTable range
                |> Maybe.andThen (\moduleName -> getCssFunction cssFunctions name moduleName)
        of
            Just _ ->
                [ Rule.error
                    { message = "Class using function is used without arguments"
                    , details = [ "Having the function used without arguments confuses me and will prevent me from figuring out whether the classes passed to this function will be known or unknown. Please pass in all the arguments at the location." ]
                    }
                    range
                ]

            Nothing ->
                []


getCssFunction : Dict String v -> String -> List String -> Maybe v
getCssFunction cssFunctions name moduleName =
    Dict.get (String.join "." moduleName ++ "." ++ name) cssFunctions


registerClasses : CssFunctions -> ModuleContext -> Range -> String -> Node Expression -> List (Node Expression) -> ( List (Rule.Error {}), ModuleContext )
registerClasses cssFunctions context fnRange name firstArgument restOfArguments =
    case
        ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
            |> Maybe.andThen (\moduleName -> getCssFunction cssFunctions name moduleName)
    of
        Just cssFunction ->
            ( []
            , { context
                | usedCssClasses = errorsForCssFunction context.usedCssClasses cssFunction fnRange { lookupTable = context.lookupTable, firstArgument = firstArgument, restOfArguments = restOfArguments }
                , functionOrValuesToIgnore = RangeDict.insert fnRange () context.functionOrValuesToIgnore
              }
            )

        Nothing ->
            ( [], context )


errorsForCssFunction :
    Set String
    -> (ClassFunction.Arguments -> List CssArgument)
    -> Range
    -> ClassFunction.Arguments
    -> Set String
errorsForCssFunction usedCssClasses cssFunction fnRange target =
    List.foldl
        (\arg acc ->
            case arg of
                ClassFunction.Literal class ->
                    Set.insert class acc

                ClassFunction.Variable range ->
                    Debug.todo "Variable"

                ClassFunction.UngraspableExpression range ->
                    Debug.todo "UngraspableExpression"

                ClassFunction.MissingArgument index ->
                    Debug.todo "Missing argument"
        )
        usedCssClasses
        (cssFunction target)


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
