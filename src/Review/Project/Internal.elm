module Review.Project.Internal exposing
    ( DataCache
    , ModuleCacheKey
    , ModuleGraphErrors(..)
    , Project(..)
    , ProjectModule
    , acyclicModuleGraph
    , buildModuleGraph
    , emptyDataCache
    , getModuleByPath
    , moduleGraph
    , sourceDirectories
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Review.ImportCycle as ImportCycle
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency exposing (Dependency)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.Zipper as Zipper exposing (Zipper)


type Project
    = Project
        { modules : Dict String ProjectModule
        , modulesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        , readme : Maybe { path : String, content : String }
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph ModuleName ())
        , sourceDirectories : List String
        , dataCache : DataCache
        }


type alias DataCache =
    { dependenciesModules : Maybe { elmJsonRaw : Maybe String, deps : Dict ModuleName Elm.Docs.Module }
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTables : Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
    }


type alias ModuleCacheKey =
    { imported : Dict ModuleName Elm.Docs.Module
    , source : String
    }


emptyDataCache : DataCache
emptyDataCache =
    { dependenciesModules = Nothing
    , modules = Dict.empty
    , lookupTables = Dict.empty
    }


{-| Represents a parsed file.
-}
type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , isInSourceDirectories : Bool
    }


getModuleByPath : String -> Project -> Maybe ProjectModule
getModuleByPath path (Project project) =
    Dict.get path project.modules


{-| Get the module graph for the project in the form of a
[`elm-community/graph` Graph]. This is used by `Review.Rule` internally.

The dependency is actually copied into the project, which means that you won't
be able to use this value, even if you add `elm-community/graph` as a dependency.
This is an unfortunately visible implementation detail, it is not meant for you
to use.

[`elm-community/graph` Graph]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Graph

-}
moduleGraph : Project -> Graph ModuleName ()
moduleGraph (Project project) =
    case project.moduleGraph of
        Just graph ->
            graph

        Nothing ->
            buildModuleGraph (Dict.values project.modules)


type ModuleGraphErrors
    = SomeModulesFailedToParse (List String)
    | DuplicateModuleNames { moduleName : ModuleName, paths : List String }
    | ImportCycleError (List ModuleName)
    | NoModulesError


acyclicModuleGraph : Project -> Result ModuleGraphErrors ( Project, Zipper (Graph.NodeContext ModuleName ()) )
acyclicModuleGraph ((Project p) as project) =
    if not (List.isEmpty p.modulesThatFailedToParse) then
        Err (SomeModulesFailedToParse (List.map .path p.modulesThatFailedToParse))

    else
        let
            projectModules : List ProjectModule
            projectModules =
                Dict.values p.modules
        in
        case duplicateModuleNames Dict.empty projectModules of
            Just duplicate ->
                Err (DuplicateModuleNames duplicate)

            Nothing ->
                let
                    graph : Graph ModuleName ()
                    graph =
                        buildModuleGraph projectModules
                in
                case Graph.checkAcyclic graph of
                    Err edge ->
                        ImportCycle.findCycle graph edge
                            |> List.reverse
                            |> ImportCycleError
                            |> Err

                    Ok acyclicGraph ->
                        case Zipper.fromList (Graph.topologicalSort acyclicGraph) of
                            Nothing ->
                                Err NoModulesError

                            Just zipper ->
                                Ok ( project, zipper )


duplicateModuleNames : Dict ModuleName String -> List ProjectModule -> Maybe { moduleName : ModuleName, paths : List String }
duplicateModuleNames visitedModules projectModules =
    case projectModules of
        [] ->
            Nothing

        projectModule :: restOfModules ->
            let
                moduleName : ModuleName
                moduleName =
                    getModuleName projectModule
            in
            case Dict.get moduleName visitedModules of
                Nothing ->
                    duplicateModuleNames
                        (Dict.insert moduleName projectModule.path visitedModules)
                        restOfModules

                Just path ->
                    Just
                        { moduleName = moduleName
                        , paths =
                            path
                                :: projectModule.path
                                :: (restOfModules
                                        |> List.filter (\p -> getModuleName p == moduleName)
                                        |> List.map .path
                                   )
                        }


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
