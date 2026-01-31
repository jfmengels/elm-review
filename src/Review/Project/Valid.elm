module Review.Project.Valid exposing
    ( ExtraFileData
    , ValidProject
    , addElmJson
    , addExtraFile
    , addParsedModule
    , addReadme
    , checkGraph
    , dependencies
    , dependenciesHash
    , directDependencies
    , doesModuleExist
    , elmJson
    , elmJsonHash
    , extraFiles
    , extraFilesHash
    , extraFilesWithoutKeys
    , getGraphNode
    , getModuleByPath
    , parse
    , projectCache
    , readme
    , readmeHash
    , removeExtraFile
    , removeModule
    , toRegularProject
    , updateProjectCache
    , updateWorkList
    , workList
    )

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.Fix.FixProblem as FixProblem exposing (FixProblem)
import Review.ImportCycle as ImportCycle
import Review.Project.Dependency exposing (Dependency)
import Review.Project.Internal as Internal exposing (Project(..))
import Review.Project.InvalidProjectError as InvalidProjectError exposing (InvalidProjectError)
import Review.Project.ModuleIds as ModuleIds exposing (ModuleId, ModuleIds)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.WorkList as WorkList exposing (WorkList)
import Vendor.Graph as Graph exposing (Graph)


type ValidProject
    = ValidProject ValidProjectData


type alias ValidProjectData =
    { modulesByPath : Dict String OpaqueProjectModule
    , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
    , readme : Maybe ( { path : String, content : String }, ContentHash )
    , extraFiles : Dict {- path -} String {- content -} String
    , extraFilesContentHash : ContentHash
    , extraFilesContentHashes : Dict {- path -} String ContentHash
    , dependencies : Dict String Dependency
    , directDependencies : Dict String Dependency
    , sourceDirectories : List String
    , projectCache : ProjectCache
    , moduleGraph : Graph FilePath
    , sortedModules : List (Graph.NodeContext FilePath)
    , edgeChanges : Dict ModuleId (List Internal.EdgeChange)
    , moduleIds : ModuleIds
    , workList : WorkList
    }


toRegularProject : ValidProject -> Project
toRegularProject (ValidProject validProject) =
    Project
        { modulesByPath = validProject.modulesByPath
        , modulesThatFailedToParse = Dict.empty
        , moduleIds = validProject.moduleIds
        , elmJson = validProject.elmJson
        , readme = validProject.readme
        , extraFiles = validProject.extraFiles
        , extraFilesContentHashes = validProject.extraFilesContentHashes
        , dependencies = validProject.dependencies
        , directDependencies = validProject.directDependencies
        , moduleGraph = validProject.moduleGraph
        , sourceDirectories = validProject.sourceDirectories
        , cache = validProject.projectCache
        , sortedModules = Internal.ComputedSortedModules validProject.sortedModules
        , workList = validProject.workList
        }


parse : Project -> Result InvalidProjectError ValidProject
parse ((Project p) as project) =
    if not (Dict.isEmpty p.modulesThatFailedToParse) then
        Err (InvalidProjectError.SomeModulesFailedToParse (Dict.keys p.modulesThatFailedToParse))

    else if Dict.isEmpty p.modulesByPath then
        Err InvalidProjectError.NoModulesError

    else
        let
            projectModules : List OpaqueProjectModule
            projectModules =
                Dict.values p.modulesByPath
        in
        case duplicateModuleNames Dict.empty projectModules of
            Just duplicate ->
                Err (InvalidProjectError.DuplicateModuleNames duplicate)

            Nothing ->
                let
                    sortedModulesResult : Result ( Graph FilePath, Graph.Edge ) ( Graph FilePath, List (Graph.NodeContext FilePath) )
                    sortedModulesResult =
                        case p.sortedModules of
                            Internal.ComputedSortedModules sorted ->
                                Ok ( p.moduleGraph, sorted )

                            Internal.NeedsToAddNewEdgesToGraph edgeChanges ->
                                let
                                    moduleGraph : Graph FilePath
                                    moduleGraph =
                                        addEdgesToModuleGraph edgeChanges p.moduleGraph
                                in
                                Graph.checkAcyclic moduleGraph
                                    |> Result.map (Tuple.pair moduleGraph)
                                    |> Result.mapError (Tuple.pair moduleGraph)
                in
                case sortedModulesResult of
                    Err ( moduleGraph, edge ) ->
                        ImportCycle.findCycle p.modulesByPath moduleGraph edge
                            |> InvalidProjectError.ImportCycleError
                            |> Err

                    Ok ( moduleGraph, sortedModules ) ->
                        Ok (fromProjectAndGraph moduleGraph sortedModules project)


fromProjectAndGraph : Graph FilePath -> List (Graph.NodeContext FilePath) -> Project -> ValidProject
fromProjectAndGraph moduleGraph sortedModules (Project project) =
    ValidProject
        { modulesByPath = project.modulesByPath
        , elmJson = project.elmJson
        , readme = project.readme
        , extraFiles = project.extraFiles
        , extraFilesContentHash = ContentHash.combine project.extraFilesContentHashes
        , extraFilesContentHashes = project.extraFilesContentHashes
        , dependencies = project.dependencies
        , directDependencies = project.directDependencies
        , sourceDirectories = project.sourceDirectories
        , projectCache = project.cache
        , moduleGraph = moduleGraph
        , sortedModules = sortedModules
        , edgeChanges = Dict.empty
        , moduleIds = project.moduleIds
        , workList = WorkList.recomputeModules moduleGraph sortedModules project.workList
        }


duplicateModuleNames : Dict ModuleName String -> List OpaqueProjectModule -> Maybe { moduleName : ModuleName, paths : List String }
duplicateModuleNames visitedModules projectModules =
    case projectModules of
        [] ->
            Nothing

        projectModule :: restOfModules ->
            let
                moduleName : ModuleName
                moduleName =
                    ProjectModule.moduleName projectModule

                projectModulePath : String
                projectModulePath =
                    ProjectModule.path projectModule
            in
            case Dict.get moduleName visitedModules of
                Nothing ->
                    duplicateModuleNames
                        (Dict.insert moduleName projectModulePath visitedModules)
                        restOfModules

                Just path ->
                    Just
                        { moduleName = moduleName
                        , paths =
                            path
                                :: projectModulePath
                                :: List.foldl
                                    (\p acc ->
                                        if ProjectModule.moduleName p == moduleName then
                                            ProjectModule.path p :: acc

                                        else
                                            acc
                                    )
                                    []
                                    restOfModules
                        }



-- ACCESSORS


elmJson : ValidProject -> Maybe { path : String, raw : String, project : Elm.Project.Project }
elmJson (ValidProject project) =
    Maybe.map Tuple.first project.elmJson


elmJsonHash : ValidProject -> Maybe ContentHash
elmJsonHash (ValidProject project) =
    Maybe.map Tuple.second project.elmJson


readme : ValidProject -> Maybe { path : String, content : String }
readme (ValidProject project) =
    Maybe.map Tuple.first project.readme


readmeHash : ValidProject -> Maybe ContentHash
readmeHash (ValidProject project) =
    Maybe.map Tuple.second project.readme


type alias ExtraFileData fileKey =
    { withFileKeys : Dict String { fileKey : fileKey, content : String }
    , withoutFileKeys : Dict String String
    }


extraFiles : (String -> fileKey) -> ValidProject -> ExtraFileData fileKey
extraFiles toFileKey (ValidProject project) =
    { withFileKeys = Dict.map (\path content -> { fileKey = toFileKey path, content = content }) project.extraFiles
    , withoutFileKeys = project.extraFiles
    }


extraFilesWithoutKeys : ValidProject -> Dict String String
extraFilesWithoutKeys (ValidProject project) =
    project.extraFiles


extraFilesHash : ValidProject -> ContentHash
extraFilesHash (ValidProject project) =
    project.extraFilesContentHash


dependencies : ValidProject -> Dict String Dependency
dependencies (ValidProject project) =
    project.dependencies


dependenciesHash : ValidProject -> Maybe ContentHash
dependenciesHash =
    -- Re-using the elm.json hash because these 2 should always be in sync
    elmJsonHash


{-| Get the direct [dependencies](./Review-Project-Dependency#Dependency) of the project.
-}
directDependencies : ValidProject -> Dict String Dependency
directDependencies (ValidProject project) =
    project.directDependencies


getGraphNode : ModuleId -> ValidProject -> Maybe (Graph.NodeContext FilePath)
getGraphNode moduleId (ValidProject project) =
    Graph.get moduleId project.moduleGraph


getModuleByPath : String -> ValidProject -> Maybe OpaqueProjectModule
getModuleByPath path (ValidProject project) =
    Dict.get path project.modulesByPath


doesModuleExist : String -> ValidProject -> Bool
doesModuleExist path (ValidProject project) =
    Dict.member path project.modulesByPath


projectCache : ValidProject -> ProjectCache
projectCache (ValidProject project) =
    project.projectCache


workList : ValidProject -> WorkList
workList (ValidProject project) =
    project.workList


updateWorkList : (WorkList -> WorkList) -> ValidProject -> ValidProject
updateWorkList fn (ValidProject project) =
    ValidProject { project | workList = fn project.workList }


updateProjectCache : ProjectCache -> ValidProject -> ValidProject
updateProjectCache projectCache_ (ValidProject project) =
    ValidProject { project | projectCache = projectCache_ }


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
addParsedModule :
    { path : FilePath, source : String, ast : Elm.Syntax.File.File }
    -> ValidProject
    -> Result FixProblem ValidProject
addParsedModule { path, source, ast } (ValidProject project) =
    case Dict.get path project.modulesByPath of
        Just existingModule ->
            let
                moduleId : ModuleIds.ModuleId
                moduleId =
                    ProjectModule.moduleId existingModule

                module_ : OpaqueProjectModule
                module_ =
                    ProjectModule.create
                        { path = path
                        , source = source
                        , ast = ast
                        , moduleId = moduleId
                        , isInSourceDirectories = ProjectModule.isInSourceDirectories existingModule
                        }

                modulesByPath : Dict FilePath OpaqueProjectModule
                modulesByPath =
                    Dict.insert path module_ project.modulesByPath

                { moduleIds, edgeChanges } =
                    Internal.addModuleToGraph
                        module_
                        (Just existingModule)
                        project.moduleIds
                        project.edgeChanges
            in
            Ok
                (ValidProject
                    { project
                        | moduleGraph = Graph.addNode (Graph.Node moduleId (ProjectModule.path module_)) project.moduleGraph
                        , edgeChanges = edgeChanges
                        , moduleIds = moduleIds
                        , modulesByPath = modulesByPath
                        , workList = WorkList.touchedModule path project.workList
                    }
                )

        Nothing ->
            -- We don't support adding new files at the moment.
            -- TODO Support creating a new file (only in known source-directories?)
            Err (FixProblem.Unchanged { filePath = path, edits = [] })


checkGraph : ValidProject -> Result FixProblem ValidProject
checkGraph (ValidProject project) =
    if Dict.isEmpty project.edgeChanges then
        Ok (ValidProject { project | workList = WorkList.recomputeModules project.moduleGraph project.sortedModules project.workList })

    else
        let
            moduleGraph : Graph FilePath
            moduleGraph =
                addEdgesToModuleGraph project.edgeChanges project.moduleGraph
        in
        case Graph.checkAcyclic moduleGraph of
            Err edge ->
                ImportCycle.findCycle project.modulesByPath moduleGraph edge
                    |> FixProblem.CreatesImportCycle
                    |> Err

            Ok sortedModules ->
                ValidProject
                    { project
                        | moduleGraph = moduleGraph
                        , sortedModules = sortedModules
                        , edgeChanges = Dict.empty
                        , workList = WorkList.recomputeModules project.moduleGraph sortedModules project.workList
                    }
                    |> Ok


addEdgesToModuleGraph : Dict k (List Internal.EdgeChange) -> Graph FilePath -> Graph FilePath
addEdgesToModuleGraph edgeChanges baseModuleGraph =
    Dict.foldl
        (\_ edges graph ->
            List.foldl
                (\{ edge, insert } g ->
                    if insert then
                        Graph.addEdge edge g

                    else
                        Graph.removeEdge edge g
                )
                graph
                edges
        )
        baseModuleGraph
        edgeChanges


{-| Remove a module from the project.
-}
removeModule :
    FilePath
    -> ValidProject
    -> Result FixProblem ValidProject
removeModule path (ValidProject project) =
    case Dict.get path project.modulesByPath of
        Just module_ ->
            let
                modulesByPath : Dict FilePath OpaqueProjectModule
                modulesByPath =
                    Dict.remove path project.modulesByPath

                moduleId : ModuleId
                moduleId =
                    ProjectModule.moduleId module_

                edgeChanges : Dict ModuleId (List Internal.EdgeChange)
                edgeChanges =
                    Internal.removeModuleFromGraph moduleId project.moduleGraph project.edgeChanges
            in
            ValidProject
                { project
                    | modulesByPath = modulesByPath
                    , moduleGraph = Graph.removeNode moduleId project.moduleGraph
                    , edgeChanges = edgeChanges
                }
                |> Ok

        Nothing ->
            -- File should always exist.
            FixProblem.RemovesUnknownFile path
                |> Err


{-| Add the content of the `README.md` file to the project, making it
available for rules to access using
[`Review.Rule.withReadmeModuleVisitor`](./Review-Rule#withReadmeModuleVisitor) and
[`Review.Rule.withReadmeProjectVisitor`](./Review-Rule#withReadmeProjectVisitor).
-}
addReadme : { path : String, content : String } -> ValidProject -> ValidProject
addReadme readme_ (ValidProject project) =
    ValidProject
        { project
            | readme = Just ( readme_, ContentHash.hash readme_.content )
            , workList = WorkList.touchedReadme project.workList
        }


{-| Add an extra file to the project.
-}
addExtraFile : { path : String, content : String } -> ValidProject -> ValidProject
addExtraFile file (ValidProject project) =
    let
        extraFilesContentHashes : Dict String ContentHash
        extraFilesContentHashes =
            Dict.insert file.path (ContentHash.hash file.content) project.extraFilesContentHashes
    in
    ValidProject
        { project
            | extraFiles = Dict.insert file.path file.content project.extraFiles
            , extraFilesContentHashes = extraFilesContentHashes
            , extraFilesContentHash = ContentHash.combine extraFilesContentHashes
            , workList = WorkList.touchedExtraFiles project.workList
        }


{-| Remove an extra file from the project.
-}
removeExtraFile : String -> ValidProject -> ValidProject
removeExtraFile path (ValidProject project) =
    let
        extraFilesContentHashes : Dict String ContentHash
        extraFilesContentHashes =
            Dict.remove path project.extraFilesContentHashes
    in
    ValidProject
        { project
            | extraFiles = Dict.remove path project.extraFiles
            , extraFilesContentHashes = extraFilesContentHashes
            , extraFilesContentHash = ContentHash.combine extraFilesContentHashes
            , workList = WorkList.touchedExtraFiles project.workList
        }


addElmJson : { path : String, raw : String, project : Elm.Project.Project } -> ValidProject -> ValidProject
addElmJson elmJson_ (ValidProject project) =
    ValidProject
        { project
            | elmJson = Just ( elmJson_, ContentHash.hash elmJson_.raw )
            , workList = WorkList.touchedElmJson project.workList
        }
