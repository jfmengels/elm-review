module NoUnusedDependencies exposing (rule)

{-| Forbid the use of modules that are never used in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Package
import Elm.Project exposing (Project)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of modules that are never used in your project.

A module is considered unused if it does not contain a `main` function
(be it exposed or not), does not import `Test` module, and is never imported in
other modules. For packages, modules listed in the `elm.json`'s
`exposed-modules` are considered used. The `ReviewConfig` is also always
considered as used.

A module will be considered as used if it gets imported, even if none of its
functions or types are used. Other rules from this package will help detect and
remove code so that the import statement is removed.

    config =
        [ NoUnused.Dependencies.rule
        ]


# When (not) to use this rule

You may not want to enable this rule if you are not concerned about having
unused modules in your application or package.

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Dependencies" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        moduleNameToDependency : Dict String String
        moduleNameToDependency =
            dependencies
                |> Dict.toList
                |> List.concatMap
                    (\( packageName, dependency ) ->
                        List.map (\{ name } -> ( name, packageName )) (Dependency.modules dependency)
                    )
                |> Dict.fromList
    in
    ( [], { projectContext | moduleNameToDependency = moduleNameToDependency } )



-- CONTEXT


type alias ProjectContext =
    { moduleNameToDependency : Dict String String
    , directProjectDependencies : Set String
    , importedModuleNames : Set String
    , elmJsonKey : Maybe Rule.ElmJsonKey
    }


type alias ModuleContext =
    Set String


initialProjectContext : ProjectContext
initialProjectContext =
    { moduleNameToDependency = Dict.empty
    , directProjectDependencies = Set.empty
    , importedModuleNames = Set.empty
    , elmJsonKey = Nothing
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    projectContext.importedModuleNames


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName importedModuleNames =
    { moduleNameToDependency = Dict.empty
    , directProjectDependencies = Set.empty
    , importedModuleNames = importedModuleNames
    , elmJsonKey = Nothing
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { moduleNameToDependency = previousContext.moduleNameToDependency
    , directProjectDependencies = previousContext.directProjectDependencies
    , importedModuleNames = Set.union previousContext.importedModuleNames newContext.importedModuleNames
    , elmJsonKey = previousContext.elmJsonKey
    }



-- PROJECT VISITORS


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeProject projectContext =
    case maybeProject of
        Just { elmJsonKey, project } ->
            let
                directProjectDependencies : Set String
                directProjectDependencies =
                    case project of
                        Elm.Project.Package { deps } ->
                            deps
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList

                        Elm.Project.Application { depsDirect } ->
                            depsDirect
                                |> List.map (Tuple.first >> Elm.Package.toString)
                                |> Set.fromList
            in
            ( []
            , { projectContext
                | elmJsonKey = Just elmJsonKey
                , directProjectDependencies =
                    Set.filter ((/=) "elm/core") directProjectDependencies
              }
            )

        Nothing ->
            ( [], projectContext )



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node importedModuleNames =
    ( []
    , Set.insert (moduleNameForImport node) importedModuleNames
    )


moduleNameForImport : Node Import -> String
moduleNameForImport node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value
        |> String.join "."



-- FINAL EVALUATION


finalEvaluationForProject : ProjectContext -> List (Error {})
finalEvaluationForProject projectContext =
    case projectContext.elmJsonKey of
        Just elmJsonKey ->
            projectContext.importedModuleNames
                |> Set.toList
                |> List.filterMap (\importedModuleName -> Dict.get importedModuleName projectContext.moduleNameToDependency)
                |> Set.fromList
                |> Set.diff projectContext.directProjectDependencies
                |> Set.toList
                |> List.map (error elmJsonKey)

        Nothing ->
            []


error : Rule.ElmJsonKey -> String -> Error {}
error elmJsonKey packageName =
    Rule.errorForElmJson elmJsonKey
        (\elmJson ->
            { message = "Unused dependency `" ++ packageName ++ "`"
            , details = [ "You can simplify your project a tiny bit by removing this dependency." ]
            , range = findPackageNameInElmJson packageName elmJson
            }
        )


findPackageNameInElmJson : String -> String -> Range
findPackageNameInElmJson packageName elmJson =
    elmJson
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( row, line ) ->
                case String.indexes ("\"" ++ packageName ++ "\"") line of
                    [] ->
                        Nothing

                    column :: _ ->
                        Just
                            { start =
                                { row = row + 1
                                , column = column + 2
                                }
                            , end =
                                { row = row + 1
                                , column = column + String.length packageName + 2
                                }
                            }
            )
        |> List.head
        |> Maybe.withDefault { start = { row = 1, column = 1 }, end = { row = 10000, column = 1 } }
