module Review.Project.Valid exposing
    ( ValidProject
    , parse
    , toRegularProject
    )

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Review.ImportCycle as ImportCycle
import Review.Project.Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..))
import Review.Project.ProjectCache exposing (DataCache)
import Review.Project.ProjectModule exposing (ProjectModule)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.Zipper as Zipper exposing (Zipper)


type ValidProject
    = ValidProject
        { modules : Dict String ProjectModule
        , modulesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        , readme : Maybe { path : String, content : String }
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph ModuleName ())
        , sourceDirectories : List String
        , dataCache : DataCache
        , acyclicGraph : Graph.AcyclicGraph ModuleName ()
        }


toRegularProject : ValidProject -> Project
toRegularProject (ValidProject validProject) =
    Project
        { modules = validProject.modules
        , modulesThatFailedToParse = validProject.modulesThatFailedToParse
        , elmJson = validProject.elmJson
        , readme = validProject.readme
        , dependencies = validProject.dependencies
        , moduleGraph = validProject.moduleGraph
        , sourceDirectories = validProject.sourceDirectories
        , dataCache = validProject.dataCache
        }


type ModuleGraphErrors
    = SomeModulesFailedToParse (List String)
    | DuplicateModuleNames { moduleName : ModuleName, paths : List String }
    | ImportCycleError (List ModuleName)
    | NoModulesError


parse : Project -> Result ModuleGraphErrors ( ValidProject, Zipper (Graph.NodeContext ModuleName ()) )
parse ((Project p) as project) =
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
                                Ok ( fromProjectAndGraph acyclicGraph project, zipper )


fromProjectAndGraph : Graph.AcyclicGraph ModuleName () -> Project -> ValidProject
fromProjectAndGraph acyclicGraph (Project project) =
    ValidProject
        { modules = project.modules
        , modulesThatFailedToParse = project.modulesThatFailedToParse
        , elmJson = project.elmJson
        , readme = project.readme
        , dependencies = project.dependencies
        , moduleGraph = project.moduleGraph
        , sourceDirectories = project.sourceDirectories
        , dataCache = project.dataCache
        , acyclicGraph = acyclicGraph
        }


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
