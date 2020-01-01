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
        { initialContext =
            { modules = Dict.empty
            , usedModules = Set.empty
            , fileKey = Nothing
            }
        , elmJsonVisitors = [ elmJsonVisitor ]
        , dependenciesVisitors = []
        , fileVisitor = fileVisitor
        , mergeContexts =
            \contextA contextB ->
                { modules = Dict.union contextA.modules contextB.modules
                , usedModules = Set.union contextA.usedModules contextB.usedModules
                , fileKey = Nothing
                }
        , finalEvaluation = finalEvaluationForProject
        }
        |> Rule.fromMultiSchema


fileVisitor : Context -> Rule.Schema Rule.ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } Context
fileVisitor context =
    Rule.newFileVisitorSchema context
        |> Rule.withFileKeyVisitor fileKeyVisitor
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor


fileKeyVisitor : Rule.FileKey -> Context -> Context
fileKeyVisitor fileKey context =
    { context | fileKey = Just fileKey }


error : ( List String, { fileKey : Rule.FileKey, moduleNameLocation : Range } ) -> Error
error ( moduleName, { fileKey, moduleNameLocation } ) =
    Rule.errorForFile fileKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation


type alias Context =
    { modules :
        Dict (List String)
            { fileKey : Rule.FileKey
            , moduleNameLocation : Range
            }
    , usedModules : Set (List String)
    , fileKey : Maybe Rule.FileKey
    }


elmJsonVisitor : Maybe Project -> Context -> Context
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


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
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
    case context.fileKey of
        Just fileKey ->
            ( []
            , { context
                | modules =
                    Dict.insert
                        moduleName
                        { fileKey = fileKey
                        , moduleNameLocation = range
                        }
                        context.modules
              }
            )

        Nothing ->
            ( [], context )


importVisitor : Node Import -> Context -> ( List Error, Context )
importVisitor node context =
    let
        moduleName : List String
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    ( []
    , { context | usedModules = Set.insert moduleName context.usedModules }
    )


finalEvaluationForProject : Context -> List Error
finalEvaluationForProject { modules, usedModules } =
    modules
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName usedModules)
        |> Dict.toList
        |> List.map error
