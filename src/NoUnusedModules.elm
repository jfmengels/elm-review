module NoUnusedModules exposing (rule)

{-| Forbid the use of unused dependencies in your project.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Forbid the use of the [`Debug`](https://package.elm-lang.org/packages/elm/core/latest/Debug) module before it goes into production or fails in the CI.

    config =
        [ NoDebug.rule
        ]


## Fail

    if Debug.log "condition" condition then
        a

    else
        b

    if condition then
        Debug.todo "Nooo!"

    else
        value


## Success

    if condition then
        a

    else
        b


# When (not) to use this rule

You may not want to enable this rule if you are developing an application and do
not care about having extraneous dependencies.

-}
rule : Rule
rule =
    Rule.newSchema "NoUnusedModules"
        { initialContext =
            { modules = Dict.empty
            , usedModules = Set.empty
            }
        , fileVisitor = fileVisitor
        , mergeContexts =
            \contextA contextB ->
                { modules = Dict.union contextA.modules contextB.modules
                , usedModules = Set.union contextA.usedModules contextB.usedModules
                }
        , finalEvaluation = finalEvaluationForProject
        }
        |> Rule.withElmJsonVisitor elmJsonVisitor
        |> Rule.fromSchema


fileVisitor context =
    Rule.schemaForFile
        |> Rule.withInitialContext context
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor


error : { file : File, moduleNameLocation : Range } -> List String -> Error
error { file, range } moduleName =
    Rule.errorForFile file
        { message = "`" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused dependencies in your project." ]
        }
        range


type alias Context =
    { modules : Dict (List String) { file : File, moduleNameLocation : Range }
    , usedModules : Set (List String)
    }


elmJsonVisitor : Maybe Project -> Context -> Context
elmJsonVisitor maybeProject context =
    let
        exposedModules : List String
        exposedModules =
            case maybeProject of
                Just (Elm.Project.Package { exposed }) ->
                    case exposed of
                        Elm.Project.ExposedList names ->
                            names
                                |> List.map Elm.Module.toString

                        Elm.Project.ExposedDict fakeDict ->
                            fakeDict
                                |> List.concatMap Tuple.second
                                |> List.map Elm.Module.toString

                _ ->
                    []
    in
    { context | usedModules = Set.fromList exposedModules }


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
moduleDefinitionVisitor node context =
    let
        (Node range moduleName) =
            node |> Node.value |> .moduleName
    in
    ( []
    , { context | modules = Dict.insert moduleName range context.modules }
    )


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
    , { context | usedModules = Set.insert (Node.value moduleName) context.usedModules }
    )


finalEvaluationForProject : Context -> List Error
finalEvaluationForProject { modules, usedModules } =
    modules
        |> Dict.filter (\moduleName _ -> not <| Set.member moduleName usedModules)
        |> Dict.toList
        |> List.map (\moduleName range -> [])
