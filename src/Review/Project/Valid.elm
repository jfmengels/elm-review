module Review.Project.Valid exposing
    ( ValidProject
    , addElmJson
    , addParsedModule
    , addReadme
    , dependencies
    , directDependencies
    , elmJson
    , getModuleByModuleName
    , getModuleByPath
    , moduleGraph
    , moduleZipper
    , modules
    , parse
    , projectCache
    , readme
    , toRegularProject
    , updateProjectCache
    )

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Path
import Review.ImportCycle as ImportCycle
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..))
import Review.Project.InvalidProjectError as InvalidProjectError exposing (InvalidProjectError)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule exposing (ProjectModule)
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.Zipper as Zipper exposing (Zipper)


type ValidProject
    = ValidProject ValidProjectData


type alias ValidProjectData =
    { modulesByPath : Dict String ProjectModule
    , modulesByModuleName : Dict ModuleName ProjectModule
    , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
    , readme : Maybe { path : String, content : String }
    , dependencies : Dict String Dependency
    , directDependencies : Dict String Dependency
    , dependencyModules : Set ModuleName
    , sourceDirectories : List String
    , projectCache : ProjectCache
    , moduleGraph : Graph ModuleName ()
    , sortedModules : List (Graph.NodeContext ModuleName ())
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


parse : Project -> Result InvalidProjectError ( ValidProject, Zipper (Graph.NodeContext ModuleName ()) )
parse ((Project p) as project) =
    if not (List.isEmpty p.modulesThatFailedToParse) then
        Err (InvalidProjectError.SomeModulesFailedToParse (List.map .path p.modulesThatFailedToParse))

    else
        let
            projectModules : List ProjectModule
            projectModules =
                Dict.values p.modules
        in
        if List.isEmpty projectModules then
            Err InvalidProjectError.NoModulesError

        else
            case duplicateModuleNames Dict.empty projectModules of
                Just duplicate ->
                    Err (InvalidProjectError.DuplicateModuleNames duplicate)

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
                                |> InvalidProjectError.ImportCycleError
                                |> Err

                        Ok acyclicGraph ->
                            case Zipper.fromList (Graph.topologicalSort acyclicGraph) of
                                Nothing ->
                                    Err InvalidProjectError.NoModulesError

                                Just zipper ->
                                    Ok ( fromProjectAndGraph graph acyclicGraph project, zipper )


{-| This is unsafe because we assume that there are some modules. We do check for this earlier in the exposed functions.
-}
unsafeCreateZipper : List a -> Zipper a
unsafeCreateZipper sortedModules =
    case Zipper.fromList sortedModules of
        Just zipper ->
            zipper

        Nothing ->
            unsafeCreateZipper sortedModules


fromProjectAndGraph : Graph ModuleName () -> Graph.AcyclicGraph ModuleName () -> Project -> ValidProject
fromProjectAndGraph moduleGraph_ acyclicGraph (Project project) =
    let
        directDependencies_ : Dict String Dependency
        directDependencies_ =
            computeDirectDependencies project
    in
    ValidProject
        { modulesByPath = project.modules
        , modulesByModuleName =
            Dict.foldl
                (\_ module_ dict ->
                    Dict.insert
                        (getModuleName module_)
                        module_
                        dict
                )
                Dict.empty
                project.modules
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


computeDirectDependencies : { a | elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }, dependencies : Dict String Dependency } -> Dict String Dependency
computeDirectDependencies project =
    case Maybe.map .project project.elmJson of
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



-- ACCESSORS


elmJson : ValidProject -> Maybe { path : String, raw : String, project : Elm.Project.Project }
elmJson (ValidProject project) =
    project.elmJson


readme : ValidProject -> Maybe { path : String, content : String }
readme (ValidProject project) =
    project.readme


dependencies : ValidProject -> Dict String Dependency
dependencies (ValidProject project) =
    project.dependencies


{-| Get the direct [dependencies](./Review-Project-Dependency#Dependency) of the project.
-}
directDependencies : ValidProject -> Dict String Dependency
directDependencies (ValidProject project) =
    project.directDependencies


moduleGraph : ValidProject -> Graph ModuleName ()
moduleGraph (ValidProject project) =
    project.moduleGraph


{-| Get the list of modules in the project.
-}
modules : ValidProject -> List ProjectModule
modules (ValidProject project) =
    Dict.values project.modulesByPath


getModuleByPath : String -> ValidProject -> Maybe ProjectModule
getModuleByPath path (ValidProject project) =
    Dict.get path project.modulesByPath


getModuleByModuleName : ModuleName -> ValidProject -> Maybe ProjectModule
getModuleByModuleName moduleName (ValidProject project) =
    Dict.get moduleName project.modulesByModuleName


projectCache : ValidProject -> ProjectCache
projectCache (ValidProject project) =
    project.projectCache


moduleZipper : ValidProject -> Zipper (Graph.NodeContext ModuleName ())
moduleZipper (ValidProject project) =
    unsafeCreateZipper project.sortedModules


updateProjectCache : ProjectCache -> ValidProject -> ValidProject
updateProjectCache projectCache_ (ValidProject project) =
    ValidProject { project | projectCache = projectCache_ }


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
addParsedModule :
    { path : String, source : String, ast : Elm.Syntax.File.File }
    -> Maybe (Zipper (Graph.NodeContext ModuleName ()))
    -> ValidProject
    -> Maybe ( ValidProject, Zipper (Graph.NodeContext ModuleName ()) )
addParsedModule { path, source, ast } maybeModuleZipper (ValidProject project) =
    let
        moduleName : ModuleName
        moduleName =
            Elm.Syntax.Module.moduleName (Node.value ast.moduleDefinition)
    in
    case Dict.get moduleName project.modulesByModuleName of
        Just existingModule ->
            let
                osAgnosticPath : String
                osAgnosticPath =
                    Path.makeOSAgnostic path

                module_ : ProjectModule
                module_ =
                    { path = path
                    , source = source
                    , ast = Review.Project.Internal.sanitizeModule ast
                    , isInSourceDirectories = List.any (\dir -> String.startsWith (Path.makeOSAgnostic dir) osAgnosticPath) project.sourceDirectories
                    }

                newProject : ValidProjectData
                newProject =
                    { project
                        | modulesByPath = Dict.insert path module_ project.modulesByPath
                        , modulesByModuleName = Dict.insert moduleName module_ project.modulesByModuleName
                    }
            in
            if importedModulesSet existingModule.ast project.dependencyModules == importedModulesSet ast project.dependencyModules then
                let
                    -- Imports haven't changed, we don't need to recompute the zipper or the graph
                    newModuleZipper : Zipper (Graph.NodeContext ModuleName ())
                    newModuleZipper =
                        case maybeModuleZipper of
                            Just moduleZipper_ ->
                                moduleZipper_

                            Nothing ->
                                let
                                    moduleZipper_ : Zipper (Graph.NodeContext ModuleName ())
                                    moduleZipper_ =
                                        unsafeCreateZipper newProject.sortedModules
                                in
                                Zipper.focusr (\mod -> mod.node.label == moduleName) moduleZipper_
                                    -- Should not happen :/
                                    |> Maybe.withDefault moduleZipper_
                in
                Just ( ValidProject newProject, newModuleZipper )

            else
                let
                    graph : Graph ModuleName ()
                    graph =
                        buildModuleGraph (Dict.values newProject.modulesByPath)
                in
                case Graph.checkAcyclic graph of
                    Err _ ->
                        Nothing

                    Ok acyclicGraph ->
                        let
                            sortedModules : List (Graph.NodeContext ModuleName ())
                            sortedModules =
                                Graph.topologicalSort acyclicGraph

                            moduleZipper_ : Zipper (Graph.NodeContext ModuleName ())
                            moduleZipper_ =
                                unsafeCreateZipper sortedModules

                            newModuleZipper : Zipper (Graph.NodeContext ModuleName ())
                            newModuleZipper =
                                case maybeModuleZipper of
                                    Just prevModuleZipper ->
                                        -- We were evaluating modules. Take the new zipper but move it to the first
                                        -- of either the touched module or the first module that is different for the 2 zippers
                                        advanceZipper moduleName (Zipper.start prevModuleZipper) moduleZipper_

                                    Nothing ->
                                        -- We were not evaluating modules. Create a zipper and move to the touched module name
                                        Zipper.focusr (\mod -> mod.node.label == moduleName) moduleZipper_
                                            -- Should not happen :/
                                            |> Maybe.withDefault moduleZipper_
                        in
                        Just ( ValidProject { newProject | moduleGraph = graph, sortedModules = sortedModules }, newModuleZipper )

        Nothing ->
            -- We don't support adding new files at the moment.
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


advanceZipper : ModuleName -> Zipper (Graph.NodeContext ModuleName ()) -> Zipper (Graph.NodeContext ModuleName ()) -> Zipper (Graph.NodeContext ModuleName ())
advanceZipper moduleName oldZipper newZipper =
    let
        current : ModuleName
        current =
            (Zipper.current newZipper).node.label
    in
    if current == moduleName || current /= (Zipper.current oldZipper).node.label then
        newZipper

    else
        case Maybe.map2 Tuple.pair (Zipper.next oldZipper) (Zipper.next newZipper) of
            Just ( old, new ) ->
                advanceZipper moduleName old new

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
    ValidProject { project | readme = Just readme_ }


addElmJson : { path : String, raw : String, project : Elm.Project.Project } -> ValidProject -> Maybe ValidProject
addElmJson elmJson_ (ValidProject project) =
    case project.elmJson of
        Nothing ->
            -- Can't add an elm.json on the fly
            Nothing

        Just previousElmJson ->
            let
                sourceDirectories : List String
                sourceDirectories =
                    Review.Project.Internal.sourceDirectoriesForProject elmJson_.project
            in
            if sourceDirectories /= Review.Project.Internal.sourceDirectoriesForProject previousElmJson.project then
                Nothing

            else
                computeUpdatedDependencies previousElmJson.project elmJson_ project
                    |> Maybe.map
                        (\updatedDependencies ->
                            ValidProject
                                { project
                                    | elmJson = Just elmJson_
                                    , sourceDirectories = sourceDirectories
                                    , dependencies = updatedDependencies.dependencies
                                    , directDependencies = updatedDependencies.directDependencies
                                    , dependencyModules = updatedDependencies.dependencyModules
                                }
                        )


computeUpdatedDependencies :
    Elm.Project.Project
    -> { path : String, raw : String, project : Elm.Project.Project }
    -> ValidProjectData
    -> Maybe { dependencies : Dict String Dependency, directDependencies : Dict String Dependency, dependencyModules : Set ModuleName }
computeUpdatedDependencies previousElmJsonProject newElmJson project =
    if areDependenciesUnchanged { before = previousElmJsonProject, after = newElmJson.project } then
        Just
            { dependencies = project.dependencies
            , directDependencies = project.directDependencies
            , dependencyModules = project.dependencyModules
            }

    else
        let
            newDependencies : Dict String Dependency
            newDependencies =
                -- TODO Remove from `dependencies` if some have been removed or their version has changed
                -- and return `Nothing` if some have been added
                project.dependencies

            directDependencies_ : Dict String Dependency
            directDependencies_ =
                computeDirectDependencies { elmJson = Just newElmJson, dependencies = newDependencies }
        in
        Just
            { dependencies = newDependencies
            , directDependencies = directDependencies_
            , dependencyModules = computeDependencyModules directDependencies_
            }


areDependenciesUnchanged : { before : Elm.Project.Project, after : Elm.Project.Project } -> Bool
areDependenciesUnchanged { before, after } =
    case ( before, after ) of
        ( Elm.Project.Application beforeApp, Elm.Project.Application afterApp ) ->
            (beforeApp.depsDirect == afterApp.depsDirect)
                && (beforeApp.depsIndirect == afterApp.depsIndirect)
                && (beforeApp.testDepsDirect == afterApp.testDepsDirect)
                && (beforeApp.testDepsIndirect == afterApp.testDepsIndirect)

        ( Elm.Project.Package beforePkg, Elm.Project.Package afterPkg ) ->
            (beforePkg.deps == afterPkg.deps)
                && (beforePkg.testDeps == afterPkg.testDeps)

        _ ->
            False
