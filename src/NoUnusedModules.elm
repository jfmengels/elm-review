module NoUnusedModules exposing (rule)

{-| Forbid the use of modules that are never used in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
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
        [ NoUnused.Modules.rule
        ]


# When (not) to use this rule

You may not want to enable this rule if you are not concerned about having
unused modules in your application or package.

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Modules"
        { moduleVisitor = moduleVisitor
        , initProjectContext = initProjectContext
        , fromProjectToModule = fromProjectToModule
        , fromModuleToProject = fromModuleToProject
        , foldProjectContexts = foldProjectContexts
        }
        |> Rule.withProjectElmJsonVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- CONTEXT


type alias ProjectContext =
    { modules :
        Dict ModuleName
            { moduleKey : Rule.ModuleKey
            , moduleNameLocation : Range
            }
    , usedModules : Set ModuleName
    , isPackage : Bool
    }


type alias ModuleContext =
    { importedModules : Set ModuleName
    , containsMainFunction : Bool
    , isPackage : Bool
    }


initProjectContext : ProjectContext
initProjectContext =
    { modules = Dict.empty
    , usedModules = Set.singleton [ "ReviewConfig" ]
    , isPackage = False
    }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ _ projectContext =
    { importedModules = Set.empty
    , containsMainFunction = False
    , isPackage = projectContext.isPackage
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { modules =
        Dict.singleton
            (Node.value moduleName)
            { moduleKey = moduleKey, moduleNameLocation = Node.range moduleName }
    , usedModules =
        if Set.member [ "Test" ] moduleContext.importedModules || moduleContext.containsMainFunction then
            Set.insert (Node.value moduleName) moduleContext.importedModules

        else
            moduleContext.importedModules
    , isPackage = moduleContext.isPackage
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { modules = Dict.union previousContext.modules newContext.modules
    , usedModules = Set.union previousContext.usedModules newContext.usedModules
    , isPackage = previousContext.isPackage
    }



-- PROJECT VISITORS


elmJsonVisitor : Maybe { a | project : Project } -> ProjectContext -> ProjectContext
elmJsonVisitor maybeProject projectContext =
    let
        ( exposedModules, isPackage ) =
            case maybeProject |> Maybe.map .project of
                Just (Elm.Project.Package { exposed }) ->
                    case exposed of
                        Elm.Project.ExposedList names ->
                            ( names, True )

                        Elm.Project.ExposedDict fakeDict ->
                            ( List.concatMap Tuple.second fakeDict, True )

                _ ->
                    ( [], False )
    in
    { projectContext
        | usedModules =
            exposedModules
                |> List.map (Elm.Module.toString >> String.split ".")
                |> Set.fromList
                |> Set.union projectContext.usedModules
        , isPackage = isPackage
    }


finalEvaluationForProject : ProjectContext -> List Error
finalEvaluationForProject { modules, usedModules } =
    modules
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName usedModules)
        |> Dict.toList
        |> List.map error


error : ( ModuleName, { moduleKey : Rule.ModuleKey, moduleNameLocation : Range } ) -> Error
error ( moduleName, { moduleKey, moduleNameLocation } ) =
    Rule.errorForFile moduleKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List nothing, ModuleContext )
importVisitor node context =
    ( []
    , { context | importedModules = Set.insert (moduleNameForImport node) context.importedModules }
    )


moduleNameForImport : Node Import -> ModuleName
moduleNameForImport node =
    node
        |> Node.value
        |> .moduleName
        |> Node.value



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor list context =
    if context.isPackage then
        ( [], context )

    else
        let
            containsMainFunction : Bool
            containsMainFunction =
                List.any
                    (\decl ->
                        case Node.value decl of
                            Declaration.FunctionDeclaration function ->
                                (function.declaration |> Node.value |> .name |> Node.value) == "main"

                            _ ->
                                False
                    )
                    list
        in
        ( []
        , { context | containsMainFunction = containsMainFunction }
        )
