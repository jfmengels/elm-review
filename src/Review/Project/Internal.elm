module Review.Project.Internal exposing
    ( Project(..)
    , ProjectInternals
    , addModuleToGraph
    , removeModuleFromGraph
    , sourceDirectories
    , sourceDirectoriesForProject
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Path
import Review.Cache.ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.Project.Dependency exposing (Dependency)
import Review.Project.ModuleIds as ModuleIds exposing (ModuleId, ModuleIds)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.WorkList exposing (WorkList)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)


type Project
    = Project ProjectInternals


type alias ProjectInternals =
    { modulesByPath : Dict String OpaqueProjectModule
    , modulesThatFailedToParse : Dict {- path -} String {- content -} String
    , moduleIds : ModuleIds
    , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
    , readme : Maybe ( { path : String, content : String }, ContentHash )
    , extraFiles : Dict {- path -} String {- content -} String
    , extraFilesContentHashes : Dict {- path -} String ContentHash
    , dependencies : Dict String Dependency
    , moduleGraph : Graph FilePath
    , sourceDirectories : List String
    , cache : ProjectCache
    , sortedModules : Maybe (List (Graph.NodeContext FilePath))
    , workList : WorkList
    }


sourceDirectories : Project -> List String
sourceDirectories (Project project) =
    project.sourceDirectories


sourceDirectoriesForProject : Elm.Project.Project -> List String
sourceDirectoriesForProject elmJson_ =
    case elmJson_ of
        Elm.Project.Application { dirs } ->
            List.map (removeDotSlashAtBeginning >> Path.makeOSAgnostic >> endWithSlash) dirs

        Elm.Project.Package _ ->
            [ "src/" ]


removeDotSlashAtBeginning : String -> String
removeDotSlashAtBeginning dir =
    if String.startsWith "./" dir then
        String.dropLeft 2 dir

    else
        dir


endWithSlash : String -> String
endWithSlash dir =
    if String.endsWith "/" dir then
        dir

    else
        dir ++ "/"


addModuleToGraph :
    OpaqueProjectModule
    -> Maybe OpaqueProjectModule
    -> Set ModuleName
    -> ModuleIds
    -> Graph FilePath
    ->
        { moduleGraph : Graph FilePath
        , moduleIds : ModuleIds
        , needToRecomputeSortedModules : Bool
        }
addModuleToGraph module_ maybeExistingModule dependencyModules moduleIds baseModuleGraph =
    case maybeExistingModule of
        Just existingModule ->
            let
                previousFileImports : Set ModuleName
                previousFileImports =
                    importedModulesSet (ProjectModule.ast existingModule) dependencyModules

                newFileImports : Set ModuleName
                newFileImports =
                    importedModulesSet (ProjectModule.ast module_) dependencyModules

                addedImports : Set ModuleName
                addedImports =
                    Set.diff newFileImports previousFileImports

                removedImports : Set ModuleName
                removedImports =
                    Set.diff previousFileImports newFileImports
            in
            if Set.isEmpty addedImports && Set.isEmpty removedImports then
                -- Imports haven't changed, we don't need to recompute the graph
                { moduleGraph = baseModuleGraph
                , moduleIds = moduleIds
                , needToRecomputeSortedModules = False
                }

            else
                let
                    moduleId : ModuleId
                    moduleId =
                        ProjectModule.moduleId existingModule

                    graphAfterRemovingImports : Graph FilePath
                    graphAfterRemovingImports =
                        Set.foldl
                            (\moduleName subGraph ->
                                case ModuleIds.get moduleName moduleIds of
                                    Just importedModuleId ->
                                        Graph.removeEdge (Graph.Edge importedModuleId moduleId) subGraph

                                    Nothing ->
                                        subGraph
                            )
                            baseModuleGraph
                            removedImports

                    ( moduleGraph, newModuleIds ) =
                        Set.foldl
                            (\moduleName ( subGraph, ids ) ->
                                if Set.member moduleName dependencyModules then
                                    ( subGraph, ids )

                                else
                                    let
                                        ( importedModuleId, newIds ) =
                                            ModuleIds.addAndGet moduleName ids
                                    in
                                    ( Graph.addEdge (Graph.Edge importedModuleId moduleId) subGraph
                                    , newIds
                                    )
                            )
                            ( graphAfterRemovingImports, moduleIds )
                            addedImports
                in
                { moduleGraph = moduleGraph
                , moduleIds = newModuleIds
                , needToRecomputeSortedModules = True
                }

        Nothing ->
            let
                moduleId : ModuleId
                moduleId =
                    ProjectModule.moduleId module_

                ( moduleGraph, newModuleIds ) =
                    List.foldl
                        (\(Node _ import_) ( subGraph, ids ) ->
                            let
                                moduleName : ModuleName
                                moduleName =
                                    Node.value import_.moduleName
                            in
                            if Set.member moduleName dependencyModules then
                                ( subGraph, ids )

                            else
                                let
                                    ( importedModuleId, newIds ) =
                                        ModuleIds.addAndGet moduleName ids
                                in
                                ( Graph.addEdge (Graph.Edge importedModuleId moduleId) subGraph
                                , newIds
                                )
                        )
                        ( Graph.addNode (Graph.Node moduleId (ProjectModule.path module_)) baseModuleGraph, moduleIds )
                        (ProjectModule.ast module_).imports
            in
            { moduleGraph = moduleGraph
            , moduleIds = newModuleIds
            , needToRecomputeSortedModules = True
            }


removeModuleFromGraph : OpaqueProjectModule -> ModuleIds -> Graph FilePath -> Graph FilePath
removeModuleFromGraph module_ moduleIds baseModuleGraph =
    let
        moduleId : ModuleId
        moduleId =
            ProjectModule.moduleId module_
    in
    List.foldl
        (\(Node _ { moduleName }) subGraph ->
            case ModuleIds.get (Node.value moduleName) moduleIds of
                Just importedModuleId ->
                    Graph.removeEdge (Graph.Edge importedModuleId moduleId) subGraph

                Nothing ->
                    subGraph
        )
        (Graph.removeNode moduleId baseModuleGraph)
        (ProjectModule.ast module_).imports


importedModulesSet : Elm.Syntax.File.File -> Set ModuleName -> Set ModuleName
importedModulesSet ast dependencyModules =
    List.foldl
        (\(Node _ { moduleName }) set ->
            let
                name : ModuleName
                name =
                    Node.value moduleName
            in
            if Set.member name dependencyModules then
                set

            else
                Set.insert name set
        )
        Set.empty
        ast.imports
