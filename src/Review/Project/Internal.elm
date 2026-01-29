module Review.Project.Internal exposing
    ( Project(..)
    , ProjectInternals
    , sourceDirectories
    , sourceDirectoriesForProject
    , updateGraph
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
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)


type Project
    = Project ProjectInternals


type alias ProjectInternals =
    { modules : Dict String OpaqueProjectModule
    , modulesThatFailedToParse : List { path : String, source : String }
    , moduleIds : ModuleIds
    , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
    , readme : Maybe ( { path : String, content : String }, ContentHash )
    , extraFiles : Dict {- path -} String {- content -} String
    , extraFilesContentHashes : Dict {- path -} String ContentHash
    , dependencies : Dict String Dependency
    , moduleGraph : Graph FilePath
    , sourceDirectories : List String
    , cache : ProjectCache
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


updateGraph :
    OpaqueProjectModule
    -> Maybe OpaqueProjectModule
    -> Set ModuleName
    -> ModuleIds
    -> Graph FilePath
    ->
        { moduleGraph : Graph FilePath
        , needToRecomputeSortedModules : Bool
        , moduleIds : ModuleIds
        }
updateGraph module_ maybeExistingModule dependencyModules baseModuleIds baseModuleGraph =
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
                , needToRecomputeSortedModules = False
                , moduleIds = baseModuleIds
                }

            else
                let
                    moduleId : ModuleId
                    moduleId =
                        ProjectModule.moduleId existingModule

                    moduleGraph : Graph FilePath
                    moduleGraph =
                        Set.foldl
                            (\moduleName subGraph ->
                                case ModuleIds.get moduleName baseModuleIds of
                                    Just importedModuleId ->
                                        Graph.addEdge (Graph.Edge importedModuleId moduleId) subGraph

                                    Nothing ->
                                        subGraph
                            )
                            (Set.foldl
                                (\moduleName subGraph ->
                                    case ModuleIds.get moduleName baseModuleIds of
                                        Just importedModuleId ->
                                            Graph.removeEdge (Graph.Edge importedModuleId moduleId) subGraph

                                        Nothing ->
                                            subGraph
                                )
                                baseModuleGraph
                                removedImports
                            )
                            addedImports
                in
                { moduleGraph = moduleGraph
                , needToRecomputeSortedModules = True
                , moduleIds = baseModuleIds
                }

        Nothing ->
            let
                ( moduleId, moduleIds ) =
                    ModuleIds.addAndGet (ProjectModule.moduleName module_) baseModuleIds

                moduleGraph : Graph FilePath
                moduleGraph =
                    List.foldl
                        (\(Node _ import_) subGraph ->
                            let
                                moduleName : ModuleName
                                moduleName =
                                    Node.value import_.moduleName
                            in
                            if Set.member moduleName dependencyModules then
                                case ModuleIds.get moduleName baseModuleIds of
                                    Just importedModuleId ->
                                        Graph.addEdge (Graph.Edge importedModuleId moduleId) subGraph

                                    Nothing ->
                                        subGraph

                            else
                                subGraph
                        )
                        baseModuleGraph
                        (ProjectModule.ast module_).imports
            in
            { moduleGraph = moduleGraph
            , needToRecomputeSortedModules = True
            , moduleIds = moduleIds
            }


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
