module Review.Project.Valid exposing
    ( ValidProject
    , addElmJson
    , addParsedModule
    , addReadme
    , dependencies
    , dependenciesHash
    , directDependencies
    , doesModuleExist
    , elmJson
    , elmJsonHash
    , getModuleByPath
    , moduleGraph
    , moduleZipper
    , modulesByModuleName
    , parse
    , projectCache
    , readme
    , readmeHash
    , toRegularProject
    , updateProjectCache
    )

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Path
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.ImportCycle as ImportCycle
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..))
import Review.Project.InvalidProjectError as InvalidProjectError exposing (InvalidProjectError)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.Zipper as Zipper exposing (Zipper)


type ValidProject
    = ValidProject ValidProjectData


type alias ValidProjectData =
    { modulesByPath : Dict String OpaqueProjectModule
    , modulesByModuleName : Dict ModuleName OpaqueProjectModule
    , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
    , readme : Maybe ( { path : String, content : String }, ContentHash )
    , dependencies : Dict String Dependency
    , directDependencies : Dict String Dependency
    , dependencyModules : Set ModuleName
    , sourceDirectories : List String
    , projectCache : ProjectCache
    , moduleGraph : Graph FilePath ()
    , sortedModules : List (Graph.NodeContext FilePath ())
    }


toRegularProject : ValidProject -> Project
toRegularProject (ValidProject validProject) =
    Project
        { modules = validProject.modulesByPath
        , modulesThatFailedToParse = []
        , elmJson = validProject.elmJson
        , readme = validProject.readme
        , dependencies = validProject.dependencies
        , moduleGraph = Just validProject.moduleGraph
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
                    graph : Graph FilePath ()
                    graph =
                        buildModuleGraph p.modules
                in
                case Graph.checkAcyclic graph of
                    Err edge ->
                        ImportCycle.findCycle graph edge
                            |> List.filterMap (\path -> Dict.get path p.modules |> Maybe.map ProjectModule.moduleName)
                            |> InvalidProjectError.ImportCycleError
                            |> Err

                    Ok acyclicGraph ->
                        Ok (fromProjectAndGraph graph acyclicGraph project)


{-| This is unsafe because we assume that there are some modules. We do check for this earlier in the exposed functions.
-}
unsafeCreateZipper : List a -> Zipper a
unsafeCreateZipper sortedModules =
    case Zipper.fromList sortedModules of
        Just zipper ->
            zipper

        Nothing ->
            unsafeCreateZipper sortedModules


fromProjectAndGraph : Graph FilePath () -> Graph.AcyclicGraph FilePath () -> Project -> ValidProject
fromProjectAndGraph moduleGraph_ acyclicGraph (Project project) =
    let
        directDependencies_ : Dict String Dependency
        directDependencies_ =
            computeDirectDependencies project
    in
    ValidProject
        { modulesByPath = project.modules
        , modulesByModuleName = computeModulesByModuleName project.modules
        , elmJson = project.elmJson
        , readme = project.readme
        , dependencies = project.dependencies
        , directDependencies = directDependencies_
        , dependencyModules = computeDependencyModules directDependencies_
        , sourceDirectories = project.sourceDirectories
        , projectCache = project.cache
        , moduleGraph = moduleGraph_
        , sortedModules = Graph.topologicalSort acyclicGraph
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


computeModulesByModuleName : Dict a OpaqueProjectModule -> Dict ModuleName OpaqueProjectModule
computeModulesByModuleName modules =
    Dict.foldl
        (\_ module_ acc ->
            Dict.insert (ProjectModule.moduleName module_) module_ acc
        )
        Dict.empty
        modules


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
                                :: (restOfModules
                                        |> List.filter (\p -> ProjectModule.moduleName p == moduleName)
                                        |> List.map ProjectModule.path
                                   )
                        }


buildModuleGraph : Dict a OpaqueProjectModule -> Graph FilePath ()
buildModuleGraph mods =
    let
        moduleIds : Dict ModuleName Int
        moduleIds =
            Dict.foldl
                (\_ module_ ( index, dict ) ->
                    ( index + 1
                    , Dict.insert
                        (ProjectModule.moduleName module_)
                        index
                        dict
                    )
                )
                ( 0, Dict.empty )
                mods
                |> Tuple.second

        getModuleId : ModuleName -> Int
        getModuleId moduleName =
            case Dict.get moduleName moduleIds of
                Just moduleId ->
                    moduleId

                Nothing ->
                    getModuleId moduleName

        ( nodes, edges ) =
            Dict.foldl
                (\_ module_ ( resNodes, resEdges ) ->
                    let
                        ( moduleNode, modulesEdges ) =
                            nodesAndEdges
                                (\moduleName -> Dict.get moduleName moduleIds)
                                module_
                                (getModuleId <| ProjectModule.moduleName module_)
                    in
                    ( moduleNode :: resNodes, modulesEdges ++ resEdges )
                )
                ( [], [] )
                mods
    in
    Graph.fromNodesAndEdges nodes edges


nodesAndEdges : (ModuleName -> Maybe Int) -> OpaqueProjectModule -> Int -> ( Graph.Node FilePath, List (Graph.Edge ()) )
nodesAndEdges getModuleId module_ moduleId =
    ( Graph.Node moduleId (ProjectModule.path module_)
    , importedModules module_
        |> List.filterMap getModuleId
        |> List.map
            (\importedModuleId ->
                Graph.Edge importedModuleId moduleId ()
            )
    )


importedModules : OpaqueProjectModule -> List ModuleName
importedModules module_ =
    (ProjectModule.ast module_).imports
        |> List.map (Node.value >> .moduleName >> Node.value)



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


moduleGraph : ValidProject -> Graph FilePath ()
moduleGraph (ValidProject project) =
    project.moduleGraph


modulesByModuleName : ValidProject -> Dict ModuleName OpaqueProjectModule
modulesByModuleName (ValidProject project) =
    project.modulesByModuleName


getModuleByPath : String -> ValidProject -> Maybe OpaqueProjectModule
getModuleByPath path (ValidProject project) =
    Dict.get path project.modulesByPath


doesModuleExist : String -> ValidProject -> Bool
doesModuleExist path (ValidProject project) =
    Dict.member path project.modulesByPath


projectCache : ValidProject -> ProjectCache
projectCache (ValidProject project) =
    project.projectCache


moduleZipper : ValidProject -> Zipper (Graph.NodeContext FilePath ())
moduleZipper (ValidProject project) =
    unsafeCreateZipper project.sortedModules


updateProjectCache : ProjectCache -> ValidProject -> ValidProject
updateProjectCache projectCache_ (ValidProject project) =
    ValidProject { project | projectCache = projectCache_ }


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
addParsedModule :
    { path : FilePath, source : String, ast : Elm.Syntax.File.File }
    -> Maybe (Zipper (Graph.NodeContext FilePath ()))
    -> ValidProject
    -> Maybe ( ValidProject, Zipper (Graph.NodeContext FilePath ()) )
addParsedModule { path, source, ast } maybeModuleZipper (ValidProject project) =
    case Dict.get path project.modulesByPath of
        Just existingModule ->
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
                        , isInSourceDirectories = List.any (\dir -> String.startsWith (Path.makeOSAgnostic dir) osAgnosticPath) project.sourceDirectories
                        }

                newProject : ValidProjectData
                newProject =
                    { project | modulesByPath = Dict.insert path module_ project.modulesByPath }
            in
            if importedModulesSet (ProjectModule.ast existingModule) project.dependencyModules == importedModulesSet ast project.dependencyModules then
                let
                    -- Imports haven't changed, we don't need to recompute the zipper or the graph
                    newModuleZipper : Zipper (Graph.NodeContext FilePath ())
                    newModuleZipper =
                        case maybeModuleZipper of
                            Just moduleZipper_ ->
                                moduleZipper_

                            Nothing ->
                                let
                                    moduleZipper_ : Zipper (Graph.NodeContext FilePath ())
                                    moduleZipper_ =
                                        unsafeCreateZipper newProject.sortedModules
                                in
                                Zipper.focusr (\mod -> mod.node.label == path) moduleZipper_
                                    -- Should not happen :/
                                    |> Maybe.withDefault moduleZipper_
                in
                Just ( ValidProject newProject, newModuleZipper )

            else
                let
                    graph : Graph FilePath ()
                    graph =
                        buildModuleGraph newProject.modulesByPath
                in
                case Graph.checkAcyclic graph of
                    Err _ ->
                        -- TODO Breaking change: Add a new kind of FixProblem about introducing import cycles
                        Nothing

                    Ok acyclicGraph ->
                        let
                            sortedModules : List (Graph.NodeContext FilePath ())
                            sortedModules =
                                Graph.topologicalSort acyclicGraph

                            moduleZipper_ : Zipper (Graph.NodeContext FilePath ())
                            moduleZipper_ =
                                unsafeCreateZipper sortedModules

                            newModuleZipper : Zipper (Graph.NodeContext FilePath ())
                            newModuleZipper =
                                case maybeModuleZipper of
                                    Just prevModuleZipper ->
                                        -- We were evaluating modules. Take the new zipper but move it to the first
                                        -- of either the touched module or the first module that is different for the 2 zippers
                                        advanceZipper path (Zipper.start prevModuleZipper) moduleZipper_

                                    Nothing ->
                                        -- We were not evaluating modules. Create a zipper and move to the touched module name
                                        Zipper.focusr (\mod -> mod.node.label == path) moduleZipper_
                                            -- Should not happen :/
                                            |> Maybe.withDefault moduleZipper_
                        in
                        Just ( ValidProject { newProject | moduleGraph = graph, sortedModules = sortedModules }, newModuleZipper )

        Nothing ->
            -- We don't support adding new files at the moment.
            -- TODO Support creating a new file (only in known source-directories?)
            Nothing


importedModulesSet : Elm.Syntax.File.File -> Set ModuleName -> Set ModuleName
importedModulesSet ast dependencyModules =
    Set.diff
        (List.foldl
            (\import_ set ->
                Set.insert (Node.value (Node.value import_).moduleName) set
            )
            Set.empty
            ast.imports
        )
        dependencyModules


advanceZipper : FilePath -> Zipper (Graph.NodeContext FilePath ()) -> Zipper (Graph.NodeContext FilePath ()) -> Zipper (Graph.NodeContext FilePath ())
advanceZipper path oldZipper newZipper =
    let
        current : FilePath
        current =
            (Zipper.current newZipper).node.label
    in
    if current == path || current /= (Zipper.current oldZipper).node.label then
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
    ValidProject { project | readme = Just ( readme_, ContentHash.hash readme_.content ) }


addElmJson : { path : String, raw : String, project : Elm.Project.Project } -> ValidProject -> ValidProject
addElmJson elmJson_ (ValidProject project) =
    ValidProject { project | elmJson = Just ( elmJson_, ContentHash.hash elmJson_.raw ) }
