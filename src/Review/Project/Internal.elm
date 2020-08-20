module Review.Project.Internal exposing
    ( Project(..)
    , ProjectModule
    , buildModuleGraph
    , computeModuleNameLookupTables
    , getModuleName
    , moduleGraph
    , moduleNameLookupTables
    , sourceDirectories
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal
import Review.Project.Dependency as Dependency exposing (Dependency)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)


type Project
    = Project
        { modules : Dict String ProjectModule
        , modulesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        , readme : Maybe { path : String, content : String }
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph ModuleName ())
        , sourceDirectories : List String
        , moduleNameLookupTables : Maybe (Dict ModuleName ModuleNameLookupTable)
        }


{-| Represents a parsed file.
-}
type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , isInSourceDirectories : Bool
    }


{-| Get the module graph for the project in the form of a
[`elm-community/graph` Graph]. This is used by `Review.Rule` internally.

The dependency is actually copied into the project, which means that you won't
be able to use this value, even if you add `elm-community/graph` as a dependency.
This is an unfortunately visible implementation detail, it is not meant for you
to use.

[`elm-community/graph` Graph]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Graph

-}
moduleGraph : Project -> Graph (List String) ()
moduleGraph (Project project) =
    case project.moduleGraph of
        Just graph ->
            graph

        Nothing ->
            buildModuleGraph <| Dict.values project.modules


moduleNameLookupTables : Graph (List String) () -> Project -> Dict ModuleName ModuleNameLookupTable
moduleNameLookupTables graph ((Project project) as rawProject) =
    case project.moduleNameLookupTables of
        Nothing ->
            graph
                |> Graph.checkAcyclic
                |> Result.map Graph.topologicalSort
                |> Result.withDefault []
                |> computeModuleNameLookupTables rawProject

        Just moduleNameLookupTables_ ->
            moduleNameLookupTables_


sourceDirectories : Project -> List String
sourceDirectories (Project project) =
    project.sourceDirectories


buildModuleGraph : List ProjectModule -> Graph ModuleName ()
buildModuleGraph mods =
    let
        moduleIds : Dict ModuleName Int
        moduleIds =
            mods
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, module_ ) dict ->
                        Dict.insert
                            (getModuleName module_)
                            index
                            dict
                    )
                    Dict.empty

        getModuleId : ModuleName -> Int
        getModuleId moduleName =
            case Dict.get moduleName moduleIds of
                Just moduleId ->
                    moduleId

                Nothing ->
                    getModuleId moduleName

        ( nodes, edges ) =
            mods
                |> List.foldl
                    (\module_ ( resNodes, resEdges ) ->
                        let
                            ( moduleNode, modulesEdges ) =
                                nodesAndEdges
                                    (\moduleName -> Dict.get moduleName moduleIds)
                                    module_
                                    (getModuleId <| getModuleName module_)
                        in
                        ( moduleNode :: resNodes, List.concat [ modulesEdges, resEdges ] )
                    )
                    ( [], [] )
    in
    Graph.fromNodesAndEdges nodes edges


nodesAndEdges : (ModuleName -> Maybe Int) -> ProjectModule -> Int -> ( Graph.Node ModuleName, List (Graph.Edge ()) )
nodesAndEdges getModuleId module_ moduleId =
    let
        moduleName : ModuleName
        moduleName =
            getModuleName module_
    in
    ( Graph.Node moduleId moduleName
    , importedModules module_
        |> List.filterMap getModuleId
        |> List.map
            (\importedModuleId ->
                Graph.Edge importedModuleId moduleId ()
            )
    )


importedModules : ProjectModule -> List ModuleName
importedModules module_ =
    module_.ast.imports
        |> List.map (Node.value >> .moduleName >> Node.value)


getModuleName : ProjectModule -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Elm.Syntax.Module.moduleName


computeModuleNameLookupTables : Project -> List (Graph.NodeContext ModuleName ()) -> Dict ModuleName ModuleNameLookupTable
computeModuleNameLookupTables ((Project project) as rawProject) nodeContexts =
    let
        dependenciesModules : Dict String Elm.Docs.Module
        dependenciesModules =
            project.dependencies
                |> Dict.values
                |> List.concatMap Dependency.modules
                |> List.map (\dependencyModule -> ( dependencyModule.name, dependencyModule ))
                |> Dict.fromList

        modules : Dict ModuleName ProjectModule
        modules =
            List.foldl
                (\module_ dict ->
                    Dict.insert
                        (getModuleName module_)
                        module_
                        dict
                )
                Dict.empty
                (Dict.values project.modules)
    in
    nodeContexts
        |> List.filterMap
            (\nodeContext ->
                case Dict.get nodeContext.node.label modules of
                    Just module_ ->
                        Just ( nodeContext.node.label, computeModuleNameLookupTable rawProject module_.ast nodeContext )

                    Nothing ->
                        -- TODO Fail here?
                        Nothing
            )
        |> Dict.fromList


computeModuleNameLookupTable : Project -> Elm.Syntax.File.File -> Graph.NodeContext ModuleName () -> ModuleNameLookupTable
computeModuleNameLookupTable (Project project) ast { node, incoming } =
    let
        exposedForModule : ExposedForModule
        exposedForModule =
            case Node.value ast.moduleDefinition |> Elm.Syntax.Module.exposingList of
                Exposing.All _ ->
                    Everything

                Exposing.Explicit list ->
                    Specific (exposedElements list)
    in
    ModuleNameLookupTableInternal.empty


type ExposedForModule
    = Everything
    | Specific (Set String)


exposedElements : List (Node Exposing.TopLevelExpose) -> Set String
exposedElements nodes =
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    Exposing.FunctionExpose name ->
                        Just name

                    Exposing.TypeOrAliasExpose name ->
                        Just name

                    Exposing.TypeExpose { name } ->
                        Just name

                    Exposing.InfixExpose _ ->
                        Nothing
            )
        |> Set.fromList
