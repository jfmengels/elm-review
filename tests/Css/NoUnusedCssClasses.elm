module Css.NoUnusedCssClasses exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Regex exposing (Regex)
import Review.FilePattern as FilePattern
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnusedCssClasses" initialProjectContext
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


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionVisitor


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
    { usedCssClasses : Set String
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { cssFiles = Dict.empty
    , usedCssClasses = Set.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator (\_ -> { usedCssClasses = Set.empty })


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


cssClassRegex : Regex
cssClassRegex =
    Regex.fromString "\\.([\\w-_]+)"
        |> Maybe.withDefault Regex.never


cssFilesVisitor : Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
cssFilesVisitor files context =
    ( []
    , { context
        | cssFiles =
            Dict.map
                (\_ { fileKey, content } ->
                    { fileKey = fileKey
                    , classes =
                        Regex.find cssClassRegex content
                            |> List.map (\m -> String.dropLeft 1 m.match)
                            |> Set.fromList
                    }
                )
                files
      }
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
                { message = "Found unused CSS classes in " ++ filePath
                , details =
                    [ "This file declared the usage of some CSS classes for which I could not any usage in the Elm codebase. Please check that no typo was made in the name of the classes, and remove them if they still seem unused."
                    , "Here are the classes that seem unused: " ++ String.join " " (Set.toList unusedClasses)
                    ]
                }
                { start = { row = 1, column = 1 }, end = { row = 2, column = 1 } }
            )
