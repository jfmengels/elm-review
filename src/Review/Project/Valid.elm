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
    , getModuleByPath
    , moduleGraph
    , moduleZipper
    , parse
    , projectCache
    , readme
    , readmeHash
    , removeExtraFile
    , removeModule
    , toRegularProject
    , updateProjectCache
    , updateWorkList
    )

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Path
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.Fix.FixProblem as FixProblem exposing (FixProblem)
import Review.ImportCycle as ImportCycle
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..))
import Review.Project.InvalidProjectError as InvalidProjectError exposing (InvalidProjectError)
import Review.Project.ModuleIds as ModuleIds exposing (ModuleIds)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.WorkList as WorkList exposing (WorkList)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict exposing (IntDict)
import Vendor.Zipper as Zipper exposing (Zipper)


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
    , dependencyModules : Set ModuleName
    , sourceDirectories : List String
    , projectCache : ProjectCache
    , moduleGraph : Graph FilePath
    , sortedModules : List (Graph.NodeContext FilePath)
    , moduleIds : ModuleIds
    , workList : WorkList
    }


toRegularProject : ValidProject -> Project
toRegularProject (ValidProject validProject) =
    Project
        { modules = validProject.modulesByPath
        , modulesThatFailedToParse = []
        , moduleIds = validProject.moduleIds
        , elmJson = validProject.elmJson
        , readme = validProject.readme
        , extraFiles = validProject.extraFiles
        , extraFilesContentHashes = validProject.extraFilesContentHashes
        , dependencies = validProject.dependencies
        , moduleGraph = validProject.moduleGraph
        , sourceDirectories = validProject.sourceDirectories
        , cache = validProject.projectCache
        }


parse : Project -> Result InvalidProjectError ValidProject
parse ((Project p) as project) =
    if not (List.isEmpty p.modulesThatFailedToParse) then
        Err (InvalidProjectError.SomeModulesFailedToParse (List.map .path p.modulesThatFailedToParse))

    else if Dict.isEmpty p.modules then
        Err InvalidProjectError.NoModulesError

    else
        let
            projectModules : List OpaqueProjectModule
            projectModules =
                Dict.values p.modules
        in
        case duplicateModuleNames Dict.empty projectModules of
            Just duplicate ->
                Err (InvalidProjectError.DuplicateModuleNames duplicate)

            Nothing ->
                let
                    ( graph, moduleIds ) =
                        buildModuleGraph p.modules p.moduleIds
                in
                case Graph.checkAcyclic graph of
                    Err edge ->
                        ImportCycle.findCycle p.modules graph edge
                            |> InvalidProjectError.ImportCycleError
                            |> Err

                    Ok acyclicGraph ->
                        Ok (fromProjectAndGraph graph acyclicGraph moduleIds project)


{-| This is unsafe because we assume that there are some modules. We do check for this earlier in the exposed functions.
-}
unsafeCreateZipper : List a -> Zipper a
unsafeCreateZipper sortedModules =
    case Zipper.fromList sortedModules of
        Just zipper ->
            zipper

        Nothing ->
            unsafeCreateZipper sortedModules


fromProjectAndGraph : Graph FilePath -> Graph.AcyclicGraph FilePath -> ModuleIds -> Project -> ValidProject
fromProjectAndGraph moduleGraph_ acyclicGraph moduleIds (Project project) =
    let
        directDependencies_ : Dict String Dependency
        directDependencies_ =
            computeDirectDependencies project

        sortedModules : List (Graph.NodeContext FilePath)
        sortedModules =
            Graph.topologicalSort acyclicGraph
    in
    ValidProject
        { modulesByPath = project.modules
        , elmJson = project.elmJson
        , readme = project.readme
        , extraFiles = project.extraFiles
        , extraFilesContentHash = ContentHash.combine project.extraFilesContentHashes
        , extraFilesContentHashes = project.extraFilesContentHashes
        , dependencies = project.dependencies
        , directDependencies = directDependencies_
        , dependencyModules = computeDependencyModules directDependencies_
        , sourceDirectories = project.sourceDirectories
        , projectCache = project.cache
        , moduleGraph = moduleGraph_
        , sortedModules = sortedModules
        , moduleIds = moduleIds
        , workList = WorkList.fromSortedModules (List.map (\m -> m.node.label) sortedModules)
        }


computeDirectDependencies : { a | elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash ), dependencies : Dict String Dependency } -> Dict String Dependency
computeDirectDependencies project =
    case Maybe.map (\( elmJson_, _ ) -> elmJson_.project) project.elmJson of
        Just (Elm.Project.Application { depsDirect, testDepsDirect }) ->
            let
                allDeps : List String
                allDeps =
                    List.map (\( name, _ ) -> Elm.Package.toString name) (depsDirect ++ testDepsDirect)
            in
            Dict.filter (\depName _ -> List.member depName allDeps) project.dependencies

        Just (Elm.Project.Package { deps, testDeps }) ->
            let
                allDeps : List String
                allDeps =
                    List.map (\( name, _ ) -> Elm.Package.toString name) (deps ++ testDeps)
            in
            Dict.filter (\depName _ -> List.member depName allDeps) project.dependencies

        Nothing ->
            project.dependencies


computeDependencyModules : Dict a Dependency -> Set ModuleName
computeDependencyModules directDependencies_ =
    Dict.foldl
        (\_ v acc ->
            List.foldl (\mod subAcc -> Set.insert (String.split "." mod.name) subAcc) acc (Dependency.modules v)
        )
        Set.empty
        directDependencies_


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


buildModuleGraph : Dict a OpaqueProjectModule -> ModuleIds -> ( Graph FilePath, ModuleIds )
buildModuleGraph mods baseModuleIds =
    let
        { nodes, edges, moduleIds } =
            Dict.foldl
                (\_ module_ acc ->
                    let
                        ( moduleId, moduleIdsWithCurrent ) =
                            ModuleIds.addAndGet
                                (ProjectModule.moduleName module_)
                                acc.moduleIds

                        newNodes : IntDict (Graph.NodeContext FilePath)
                        newNodes =
                            Graph.addNode (Graph.Node moduleId (ProjectModule.path module_)) acc.nodes

                        result : { edges : List Graph.Edge, moduleIds : ModuleIds }
                        result =
                            addEdges
                                module_
                                moduleId
                                moduleIdsWithCurrent
                                acc.edges
                    in
                    { nodes = newNodes
                    , edges = result.edges
                    , moduleIds = result.moduleIds
                    }
                )
                { nodes = IntDict.empty, edges = [], moduleIds = baseModuleIds }
                mods
    in
    ( Graph.fromNodesAndEdges nodes edges
    , moduleIds
    )


addEdges : OpaqueProjectModule -> Int -> ModuleIds -> List Graph.Edge -> { edges : List Graph.Edge, moduleIds : ModuleIds }
addEdges module_ moduleId initialModuleIds initialEdges =
    List.foldl
        (\(Node _ { moduleName }) acc ->
            let
                ( importedModuleId, moduleIds ) =
                    ModuleIds.addAndGet (Node.value moduleName) acc.moduleIds
            in
            { edges = Graph.Edge importedModuleId moduleId :: acc.edges
            , moduleIds = moduleIds
            }
        )
        { edges = initialEdges, moduleIds = initialModuleIds }
        (ProjectModule.ast module_).imports



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


moduleGraph : ValidProject -> Graph FilePath
moduleGraph (ValidProject project) =
    project.moduleGraph


getModuleByPath : String -> ValidProject -> Maybe OpaqueProjectModule
getModuleByPath path (ValidProject project) =
    Dict.get path project.modulesByPath


doesModuleExist : String -> ValidProject -> Bool
doesModuleExist path (ValidProject project) =
    Dict.member path project.modulesByPath


projectCache : ValidProject -> ProjectCache
projectCache (ValidProject project) =
    project.projectCache


moduleZipper : ValidProject -> Zipper FilePath
moduleZipper (ValidProject project) =
    unsafeCreateZipper (List.map (\m -> m.node.label) project.sortedModules)


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
    -> Maybe (Zipper FilePath)
    -> ValidProject
    -> Result FixProblem ( ValidProject, Zipper FilePath )
addParsedModule { path, source, ast } maybeModuleZipper (ValidProject project) =
    case
        Dict.get path project.modulesByPath
            |> Maybe.andThen
                (\mod ->
                    ModuleIds.get (ProjectModule.moduleName mod) project.moduleIds
                        |> Maybe.map (Tuple.pair mod)
                )
    of
        Just ( existingModule, moduleId ) ->
            let
                osAgnosticPath : String
                osAgnosticPath =
                    Path.makeOSAgnostic path

                module_ : OpaqueProjectModule
                module_ =
                    ProjectModule.create
                        { path = path
                        , source = source
                        , ast = ast
                        , moduleId = ProjectModule.moduleId existingModule
                        , isInSourceDirectories = List.any (\dir -> String.startsWith (Path.makeOSAgnostic dir) osAgnosticPath) project.sourceDirectories
                        }

                modulesByPath : Dict FilePath OpaqueProjectModule
                modulesByPath =
                    Dict.insert path module_ project.modulesByPath

                previousFileImports : Set ModuleName
                previousFileImports =
                    importedModulesSet (ProjectModule.ast existingModule) project.dependencyModules

                newFileImports : Set ModuleName
                newFileImports =
                    importedModulesSet ast project.dependencyModules

                addedImports : Set ModuleName
                addedImports =
                    Set.diff newFileImports previousFileImports

                removedImports : Set ModuleName
                removedImports =
                    Set.diff previousFileImports newFileImports
            in
            if Set.isEmpty addedImports && Set.isEmpty removedImports then
                let
                    -- Imports haven't changed, we don't need to recompute the zipper or the graph
                    newModuleZipper : Zipper FilePath
                    newModuleZipper =
                        case maybeModuleZipper of
                            Just moduleZipper_ ->
                                moduleZipper_

                            Nothing ->
                                let
                                    moduleZipper_ : Zipper FilePath
                                    moduleZipper_ =
                                        unsafeCreateZipper (List.map (\m -> m.node.label) project.sortedModules)
                                in
                                Zipper.focusr (\filePath -> filePath == path) moduleZipper_
                                    -- Should not happen :/
                                    |> Maybe.withDefault moduleZipper_
                in
                Ok ( ValidProject { project | modulesByPath = modulesByPath }, newModuleZipper )

            else
                let
                    graph : Graph FilePath
                    graph =
                        Set.foldl
                            (\moduleName subGraph ->
                                case ModuleIds.get moduleName project.moduleIds of
                                    Just importedModuleId ->
                                        Graph.addEdge (Graph.Edge importedModuleId moduleId) subGraph

                                    Nothing ->
                                        subGraph
                            )
                            (Set.foldl
                                (\moduleName subGraph ->
                                    case ModuleIds.get moduleName project.moduleIds of
                                        Just importedModuleId ->
                                            Graph.removeEdge (Graph.Edge importedModuleId moduleId) subGraph

                                        Nothing ->
                                            subGraph
                                )
                                project.moduleGraph
                                removedImports
                            )
                            addedImports
                in
                case Graph.checkAcyclic graph of
                    Err edge ->
                        ImportCycle.findCycle modulesByPath graph edge
                            |> FixProblem.CreatesImportCycle
                            |> Err

                    Ok acyclicGraph ->
                        let
                            sortedModules : List (Graph.NodeContext FilePath)
                            sortedModules =
                                Graph.topologicalSort acyclicGraph

                            moduleZipper_ : Zipper FilePath
                            moduleZipper_ =
                                unsafeCreateZipper (List.map (\m -> m.node.label) sortedModules)

                            newModuleZipper : Zipper FilePath
                            newModuleZipper =
                                case maybeModuleZipper of
                                    Just prevModuleZipper ->
                                        -- We were evaluating modules. Take the new zipper but move it to the first
                                        -- of either the touched module or the first module that is different for the 2 zippers
                                        advanceZipper path (Zipper.start prevModuleZipper) moduleZipper_

                                    Nothing ->
                                        -- We were not evaluating modules. Create a zipper and move to the touched module name
                                        Zipper.focusr (\filePath -> filePath == path) moduleZipper_
                                            -- Should not happen :/
                                            |> Maybe.withDefault moduleZipper_
                        in
                        Ok
                            ( ValidProject
                                { project
                                    | moduleGraph = graph
                                    , sortedModules = sortedModules
                                    , modulesByPath = modulesByPath
                                }
                            , newModuleZipper
                            )

        Nothing ->
            -- We don't support adding new files at the moment.
            -- TODO Support creating a new file (only in known source-directories?)
            Err (FixProblem.Unchanged { filePath = path, edits = [] })


checkGraph : ValidProject -> Result FixProblem ValidProject
checkGraph (ValidProject project) =
    case Graph.checkAcyclic project.moduleGraph |> Result.map Graph.topologicalSort of
        Err edge ->
            ImportCycle.findCycle project.modulesByPath project.moduleGraph edge
                |> FixProblem.CreatesImportCycle
                |> Err

        Ok sortedModules ->
            ValidProject
                { project
                    | sortedModules = sortedModules
                    , workList = WorkList.recomputeModules project.moduleGraph sortedModules project.workList
                }
                |> Ok


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
removeModule :
    FilePath
    -> ValidProject
    -> Result FixProblem ValidProject
removeModule path (ValidProject project) =
    if Dict.member path project.modulesByPath then
        let
            modulesByPath : Dict FilePath OpaqueProjectModule
            modulesByPath =
                Dict.remove path project.modulesByPath

            ( graph, moduleIds ) =
                buildModuleGraph modulesByPath project.moduleIds
        in
        case Graph.checkAcyclic graph |> Result.map Graph.topologicalSort of
            Err edge ->
                -- Removing a module should never be able to introduce an import cycle
                ImportCycle.findCycle project.modulesByPath graph edge
                    |> FixProblem.CreatesImportCycle
                    |> Err

            Ok sortedModules ->
                ValidProject
                    { project
                        | modulesByPath = modulesByPath
                        , moduleGraph = graph
                        , sortedModules = sortedModules
                        , moduleIds = moduleIds
                    }
                    |> Ok

    else
        -- File should always exist.
        FixProblem.RemovesUnknownFile path
            |> Err


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


advanceZipper : FilePath -> Zipper FilePath -> Zipper FilePath -> Zipper FilePath
advanceZipper path oldZipper newZipper =
    let
        current : FilePath
        current =
            Zipper.current newZipper
    in
    if current == path || current /= Zipper.current oldZipper then
        newZipper

    else
        case Maybe.map2 Tuple.pair (Zipper.next oldZipper) (Zipper.next newZipper) of
            Just ( old, new ) ->
                advanceZipper path old new

            Nothing ->
                -- Should not happen
                newZipper


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
