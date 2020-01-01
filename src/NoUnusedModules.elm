module NoUnusedModules exposing (rule)

{-| Forbid the use of modules that are never used in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of modules that are never used in your project.

For an application, a module is considered unused if it does not contain a
`main` function (be it exposed or not), does not import `Test` module, and is
never imported in other modules.

For a package, a module is considered unused if it is not exposed in the
`elm.json`'s `exposed-modules`, does not import `Test` module, and is never
imported in other modules.

A module will be considered as used if it gets imported, even if none of its
functions or types are used. Other rules from this package will help detect and
remove code so that the import statement is removed.

    config =
        [ NoUnused.Modules.rule
        ]


# When (not) to use this rule

You may not want to enable this rule if you are not concerned about having
unused modules in your application of package.

-}
rule : Rule
rule =
    Rule.newMultiSchema "NoUnusedModules"
        { elmJsonVisitors = [ elmJsonVisitor ]
        , dependenciesVisitors = []
        , moduleVisitorSchema = moduleVisitorSchema
        , context =
            { initGlobalContext = initGlobalContext
            , initModuleContext = initModuleContext
            , toGlobalContext = toGlobalContext
            , fold = fold
            }
        , finalEvaluation = finalEvaluationForProject
        }
        |> Rule.fromMultiSchema



-- CONTEXT


type alias GlobalContext =
    { modules :
        Dict ModuleName
            { fileKey : Rule.FileKey
            , moduleNameLocation : Range
            }
    , usedModules : Set ModuleName
    }


type alias ModuleContext =
    { modules :
        Dict ModuleName
            { fileKey : Rule.FileKey
            , moduleNameLocation : Range
            }
    , usedModules : Set ModuleName
    , fileKey : Rule.FileKey
    }


initGlobalContext : GlobalContext
initGlobalContext =
    { modules = Dict.empty
    , usedModules = Set.empty
    }


initModuleContext : Rule.FileKey -> Node ModuleName -> GlobalContext -> ModuleContext
initModuleContext fileKey moduleName globalContext =
    { modules = Dict.empty
    , usedModules = Set.empty
    , fileKey = fileKey
    }


toGlobalContext : Rule.FileKey -> Node ModuleName -> ModuleContext -> GlobalContext
toGlobalContext fileKey moduleName moduleContext =
    { modules = moduleContext.modules
    , usedModules = moduleContext.usedModules
    }


fold : GlobalContext -> GlobalContext -> GlobalContext
fold contextA contextB =
    { modules = Dict.union contextA.modules contextB.modules
    , usedModules = Set.union contextA.usedModules contextB.usedModules
    }



-- VISITORS


moduleVisitorSchema :
    Rule.Schema Rule.ForLookingAtSeveralFiles { hasNoVisitor : () } ModuleContext
    -> Rule.Schema Rule.ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitorSchema schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor


error : ( ModuleName, { fileKey : Rule.FileKey, moduleNameLocation : Range } ) -> Error
error ( moduleName, { fileKey, moduleNameLocation } ) =
    Rule.errorForFile fileKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation


elmJsonVisitor : Maybe Project -> GlobalContext -> GlobalContext
elmJsonVisitor maybeProject context =
    let
        exposedModules : List Elm.Module.Name
        exposedModules =
            case maybeProject of
                Just (Elm.Project.Package { exposed }) ->
                    case exposed of
                        Elm.Project.ExposedList names ->
                            names

                        Elm.Project.ExposedDict fakeDict ->
                            List.concatMap Tuple.second fakeDict

                _ ->
                    []
    in
    { context
        | usedModules =
            exposedModules
                |> List.map (Elm.Module.toString >> String.split ".")
                |> Set.fromList
    }


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List Error, ModuleContext )
moduleDefinitionVisitor node context =
    let
        (Node.Node range moduleName) =
            case Node.value node of
                Module.NormalModule data ->
                    data.moduleName

                Module.PortModule data ->
                    data.moduleName

                Module.EffectModule data ->
                    data.moduleName
    in
    ( []
    , { context
        | modules =
            Dict.insert
                moduleName
                { fileKey = context.fileKey
                , moduleNameLocation = range
                }
                context.modules
      }
    )


importVisitor : Node Import -> ModuleContext -> ( List Error, ModuleContext )
importVisitor node context =
    let
        moduleName : ModuleName
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    ( []
    , { context | usedModules = Set.insert moduleName context.usedModules }
    )


finalEvaluationForProject : GlobalContext -> List Error
finalEvaluationForProject { modules, usedModules } =
    modules
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName usedModules)
        |> Dict.toList
        |> List.map error
