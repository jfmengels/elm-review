module Review.Project.Internal exposing
    ( EdgeChange
    , Project(..)
    , ProjectInternals
    , SortedModules(..)
    , addModuleToGraph
    , removeModuleFromGraph
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
import Vendor.IntSet as IntSet


type Project
    = Project ProjectInternals


type alias ProjectInternals =
    { modulesByPath : Dict String OpaqueProjectModule
    , modulesThatFailedToParse : Dict {- path -} String {- content -} String
    , moduleIds : ModuleIds
    , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
    , readme : Maybe ( { path : String, content : String }, ContentHash )
    , extraFiles : Dict {- path -} String {- content -} String
    , extraFilesContentHash : Maybe ContentHash
    , extraFilesContentHashes : Dict {- path -} String ContentHash
    , dependencies : Dict String Dependency
    , directDependencies : Dict String Dependency
    , moduleGraph : Graph FilePath
    , sourceDirectories : List String
    , cache : ProjectCache
    , sortedModules : SortedModules
    , workList : WorkList
    }


type SortedModules
    = ComputedSortedModules (List (Graph.NodeContext FilePath))
    | NeedsToAddNewEdgesToGraph (Dict ModuleId (List EdgeChange))


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


type alias EdgeChange =
    { edge : Graph.Edge
    , insert : Bool
    }


addModuleToGraph :
    OpaqueProjectModule
    -> Maybe OpaqueProjectModule
    -> ModuleIds
    -> Dict ModuleId (List EdgeChange)
    ->
        { moduleIds : ModuleIds
        , edgeChanges : Dict ModuleId (List EdgeChange)
        }
addModuleToGraph module_ maybeExistingModule moduleIds edgeChanges =
    let
        moduleId : ModuleId
        moduleId =
            ProjectModule.moduleId module_
    in
    case maybeExistingModule of
        Just existingModule ->
            let
                previousFileImports : Set ModuleName
                previousFileImports =
                    importedModulesSet (ProjectModule.ast existingModule)

                newFileImports : Set ModuleName
                newFileImports =
                    importedModulesSet (ProjectModule.ast module_)

                addedImports : Set ModuleName
                addedImports =
                    Set.diff newFileImports previousFileImports

                removedImports : Set ModuleName
                removedImports =
                    Set.diff previousFileImports newFileImports
            in
            if Set.isEmpty addedImports && Set.isEmpty removedImports then
                -- Imports haven't changed, we don't need to recompute the graph
                { moduleIds = moduleIds
                , edgeChanges = Dict.remove moduleId edgeChanges
                }

            else
                let
                    edgesAfterRemovingImports : List EdgeChange
                    edgesAfterRemovingImports =
                        Set.foldl
                            (\moduleName edges ->
                                case ModuleIds.get moduleName moduleIds of
                                    Just importedModuleId ->
                                        { edge = Graph.Edge importedModuleId moduleId, insert = False } :: edges

                                    Nothing ->
                                        edges
                            )
                            []
                            removedImports

                    ( edgeChangesForFile, newModuleIds ) =
                        Set.foldl
                            (\moduleName ( edges, ids ) ->
                                let
                                    ( importedModuleId, newIds ) =
                                        ModuleIds.addAndGet moduleName ids
                                in
                                ( { edge = Graph.Edge importedModuleId moduleId, insert = True } :: edges
                                , newIds
                                )
                            )
                            ( edgesAfterRemovingImports, moduleIds )
                            addedImports
                in
                { moduleIds = newModuleIds
                , edgeChanges =
                    if List.isEmpty edgeChangesForFile then
                        Dict.remove moduleId edgeChanges

                    else
                        Dict.insert moduleId edgeChangesForFile edgeChanges
                }

        Nothing ->
            let
                ( edgeChangesForFile, newModuleIds ) =
                    List.foldl
                        (\(Node _ import_) ( edges, ids ) ->
                            let
                                ( importedModuleId, newIds ) =
                                    ModuleIds.addAndGet (Node.value import_.moduleName) ids
                            in
                            ( { edge = Graph.Edge importedModuleId moduleId, insert = True } :: edges
                            , newIds
                            )
                        )
                        ( [], moduleIds )
                        (ProjectModule.ast module_).imports
            in
            { moduleIds = newModuleIds
            , edgeChanges =
                if List.isEmpty edgeChangesForFile then
                    Dict.remove moduleId edgeChanges

                else
                    Dict.insert moduleId edgeChangesForFile edgeChanges
            }


removeModuleFromGraph : ModuleId -> Graph FilePath -> Dict ModuleId (List EdgeChange) -> Dict ModuleId (List EdgeChange)
removeModuleFromGraph moduleId moduleGraph edgeChanges =
    case Graph.get moduleId moduleGraph of
        Just { outgoing } ->
            let
                newEdgeChanges : List EdgeChange
                newEdgeChanges =
                    IntSet.foldl
                        (\importedModuleId edges ->
                            { edge = Graph.Edge importedModuleId moduleId, insert = False } :: edges
                        )
                        []
                        outgoing
            in
            if List.isEmpty newEdgeChanges then
                Dict.remove moduleId edgeChanges

            else
                Dict.insert moduleId newEdgeChanges edgeChanges

        Nothing ->
            edgeChanges


importedModulesSet : Elm.Syntax.File.File -> Set ModuleName
importedModulesSet ast =
    List.foldl
        (\(Node _ { moduleName }) set ->
            Set.insert (Node.value moduleName) set
        )
        Set.empty
        ast.imports
