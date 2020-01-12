module NoUnusedExports exposing (rule)

{-| Forbid the use of modules that are never used in your project.


# Rule

@docs rule

-}

-- TODO Don't report type or type aliases (still `A(..)` though) if they are
-- used in exposed function arguments/return values.

import Dict exposing (Dict)
import Elm.Module
import Elm.Project exposing (Project)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
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
    Rule.newMultiSchema "NoUnused.Exports"
        { moduleVisitorSchema =
            \schema ->
                schema
                    |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        , initGlobalContext = initGlobalContext
        , fromGlobalToModule = fromGlobalToModule
        , fromModuleToGlobal = fromModuleToGlobal
        , foldGlobalContexts = foldGlobalContexts
        }
        |> Rule.traversingImportedModulesFirst
        |> Rule.withMultiFinalEvaluation finalEvaluationForProject
        |> Rule.fromMultiSchema



-- CONTEXT


type alias GlobalContext =
    { modules :
        Dict ModuleName
            { fileKey : Rule.FileKey
            , exposed : Dict String { range : Range, exposedElement : ExposedElement }
            }
    }


type ExposedElement
    = Function
    | TypeOrTypeAlias
    | ExposedType


type alias ModuleContext =
    { exposesEverything : Bool
    , exposed : Dict String { range : Range, exposedElement : ExposedElement }
    }


initGlobalContext : GlobalContext
initGlobalContext =
    { modules = Dict.empty
    }


fromGlobalToModule : Rule.FileKey -> Node ModuleName -> GlobalContext -> ModuleContext
fromGlobalToModule fileKey moduleNameNode globalContext =
    { exposesEverything = False
    , exposed = Dict.empty
    }


fromModuleToGlobal : Rule.FileKey -> Node ModuleName -> ModuleContext -> GlobalContext
fromModuleToGlobal fileKey moduleName moduleContext =
    { modules =
        Dict.singleton
            (Node.value moduleName)
            { fileKey = fileKey
            , exposed = moduleContext.exposed
            }
    }


foldGlobalContexts : GlobalContext -> GlobalContext -> GlobalContext
foldGlobalContexts contextA contextB =
    { modules = Dict.union contextA.modules contextB.modules
    }


error : ( ModuleName, { fileKey : Rule.FileKey, moduleNameLocation : Range } ) -> Error
error ( moduleName, { fileKey, moduleNameLocation } ) =
    Rule.errorForFile fileKey
        { message = "Module `" ++ String.join "." moduleName ++ "` is never used."
        , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
        }
        moduleNameLocation



-- GLOBAL EVALUATION


finalEvaluationForProject : GlobalContext -> List Error
finalEvaluationForProject globalContext =
    globalContext.modules
        |> Dict.values
        |> List.concatMap
            (\{ fileKey, exposed } ->
                exposed
                    |> removeExceptions
                    |> Dict.toList
                    |> List.map
                        (\( name, { range, exposedElement } ) ->
                            Rule.errorForFile fileKey
                                { message = "Exposed function or type `" ++ name ++ "` is never used outside this module."
                                , details = [ "This exposed element is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                                }
                                range
                        )
            )


removeExceptions : Dict String a -> Dict String a
removeExceptions dict =
    Dict.filter (\name _ -> name /= "main") dict



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List Error, ModuleContext )
moduleDefinitionVisitor moduleNode moduleContext =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { moduleContext | exposesEverything = True } )

        Exposing.Explicit list ->
            ( [], { moduleContext | exposed = exposedElements list } )


exposedElements : List (Node Exposing.TopLevelExpose) -> Dict String { range : Range, exposedElement : ExposedElement }
exposedElements nodes =
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    Exposing.FunctionExpose name ->
                        Just <| ( name, { range = Node.range node, exposedElement = Function } )

                    Exposing.TypeOrAliasExpose name ->
                        -- TODO
                        Nothing

                    Exposing.TypeExpose { name } ->
                        -- TODO
                        Nothing

                    Exposing.InfixExpose name ->
                        Nothing
            )
        |> Dict.fromList
