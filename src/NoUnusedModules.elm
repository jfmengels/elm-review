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
import Elm.Syntax.Module as Module exposing (Module)
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
    Rule.newMultiSchema "NoUnused.Modules"
        { moduleVisitorSchema = moduleVisitorSchema
        , initGlobalContext = initGlobalContext
        , initModuleContext = initModuleContext
        , fromModuleToGlobal = fromModuleToGlobal
        , fold = fold
        }
        |> Rule.withMultiElmJsonVisitor elmJsonVisitor
        |> Rule.withMultiFinalEvaluation finalEvaluationForProject
        |> Rule.fromMultiSchema



-- CONTEXT


type alias GlobalContext =
    { modules :
        Dict ModuleName
            { fileKey : Rule.FileKey
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


initGlobalContext : GlobalContext
initGlobalContext =
    { modules = Dict.empty
    , usedModules = Set.singleton [ "ReviewConfig" ]
    , isPackage = False
    }


initModuleContext : Rule.FileKey -> Node ModuleName -> GlobalContext -> ModuleContext
initModuleContext _ _ globalContext =
    { importedModules = Set.empty
    , containsMainFunction = False
    , isPackage = globalContext.isPackage
    }


fromModuleToGlobal : Rule.FileKey -> Node ModuleName -> ModuleContext -> GlobalContext
fromModuleToGlobal fileKey moduleName moduleContext =
    { modules =
        Dict.singleton
            (Node.value moduleName)
            { fileKey = fileKey, moduleNameLocation = Node.range moduleName }
    , usedModules =
        if Set.member [ "Test" ] moduleContext.importedModules || moduleContext.containsMainFunction then
            Set.insert (Node.value moduleName) moduleContext.importedModules

        else
            moduleContext.importedModules
    , isPackage = moduleContext.isPackage
    }


fold : GlobalContext -> GlobalContext -> GlobalContext
fold contextA contextB =
    { modules = Dict.union contextA.modules contextB.modules
    , usedModules = Set.union contextA.usedModules contextB.usedModules
    , isPackage = contextA.isPackage
    }



-- GLOBAL VISITORS


elmJsonVisitor : Maybe Project -> GlobalContext -> GlobalContext
elmJsonVisitor maybeProject context =
    let
        ( exposedModules, isPackage ) =
            case maybeProject of
                Just (Elm.Project.Package { exposed }) ->
                    case exposed of
                        Elm.Project.ExposedList names ->
                            ( names, True )

                        Elm.Project.ExposedDict fakeDict ->
                            ( List.concatMap Tuple.second fakeDict, True )

                _ ->
                    ( [], False )
    in
    { context
        | usedModules =
            exposedModules
                |> List.map (Elm.Module.toString >> String.split ".")
                |> Set.fromList
                |> Set.union context.usedModules
        , isPackage = isPackage
    }


finalEvaluationForProject : GlobalContext -> List Error
finalEvaluationForProject { modules, usedModules } =
    modules
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName usedModules)
        |> Dict.toList
        |> List.map error



-- MODULE VISITORS


moduleVisitorSchema :
    Rule.Schema Rule.ForLookingAtSeveralFiles { hasNoVisitor : () } ModuleContext
    -> Rule.Schema Rule.ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitorSchema schema =
    schema
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor


error : ( ModuleName, { fileKey : Rule.FileKey, moduleNameLocation : Range } ) -> Error
error ( moduleName, { fileKey, moduleNameLocation } ) =
    Rule.errorForFile fileKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation



-- IMPORT VISITOR


importVisitor : Node Import -> ModuleContext -> ( List Error, ModuleContext )
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


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List Error, ModuleContext )
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
