module Review.Project exposing
    ( Project, new
    , ProjectModule, addModule, addParsedModule, removeModule, modules, filesThatFailedToParse, moduleGraph, precomputeModuleGraph
    , withElmJson, elmJson
    , Dependency, withDependency, removeDependencies, dependencies
    )

{-| Represents the contents of the project to be analyzed. This information will
then be fed to the review rules.

Looking at this module is useful if you try to make `elm-review` run in a new environment,
but you can safely ignore it if you just want to write a review rule or run it
in existing environments like the CLI tool.


# Project

@docs Project, new


# Project files


## Adding files

@docs ProjectModule, addModule, addParsedModule, removeModule, modules, filesThatFailedToParse, moduleGraph, precomputeModuleGraph


# `elm.json`

@docs withElmJson, elmJson


# Project dependencies

@docs Dependency, withDependency, removeDependencies, dependencies

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Parser as Parser
import Elm.Processing
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node
import Elm.Version
import Graph exposing (Graph)
import Review.Dependencies



-- PROJECT


{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}
type Project
    = Project
        { modules : List ProjectModule
        , filesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph ModuleName ())
        }


{-| Create a new Project.
-}
new : Project
new =
    Project
        { modules = []
        , filesThatFailedToParse = []
        , elmJson = Nothing
        , dependencies = Dict.empty
        , moduleGraph = Nothing
        }



-- PROJECT FILES


{-| Represents a parsed file.
-}
type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    }


{-| Add an Elm file to the project. If a file with the same path already exists,
then it will replace it.

If the file is syntactically valid Elm code, it will then be analyzed by the
review rules. Otherwise, the file will be added to the list of files that failed
to parse, which you can get using [`filesThatFailedToParse`](#filesThatFailedToParse),
and for which a parsing error will be reported when running [`Review.review`](./Review#review).

-}
addModule : { path : String, source : String } -> Project -> Project
addModule { path, source } project =
    recomputeModuleGraphIfNeeded <|
        case parseSource source of
            Ok ast ->
                project
                    |> removeFileFromProject path
                    |> addModuleToProject
                        { path = path
                        , source = source
                        , ast = ast
                        }
                    |> recomputeModuleGraphIfNeeded

            Err _ ->
                project
                    |> removeFileFromProject path
                    |> addFileThatFailedToParse
                        { path = path
                        , source = source
                        }
                    |> recomputeModuleGraphIfNeeded


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long, which the compiler does not support for Elm 0.19.1.
    row * 1000000 + column


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
addParsedModule : { path : String, source : String, ast : Elm.Syntax.File.File } -> Project -> Project
addParsedModule module_ project =
    project
        |> removeFileFromProject module_.path
        |> addModuleToProject module_
        |> recomputeModuleGraphIfNeeded


addModuleToProject : ProjectModule -> Project -> Project
addModuleToProject module_ (Project project) =
    Project { project | modules = sanitizeModule module_ :: project.modules }


sanitizeModule : ProjectModule -> ProjectModule
sanitizeModule module_ =
    { module_ | ast = reorderComments module_.ast }


reorderComments : Elm.Syntax.File.File -> Elm.Syntax.File.File
reorderComments ast =
    { ast | comments = List.sortBy (Node.range >> .start >> positionAsInt >> negate) ast.comments }


addFileThatFailedToParse : { path : String, source : String } -> Project -> Project
addFileThatFailedToParse { path, source } (Project project) =
    Project
        { project
            | filesThatFailedToParse = { path = path, source = source } :: project.filesThatFailedToParse
        }


{-| Remove a module from the project by its path.
-}
removeModule : String -> Project -> Project
removeModule path project =
    project
        |> removeFileFromProject path
        |> recomputeModuleGraphIfNeeded


removeFileFromProject : String -> Project -> Project
removeFileFromProject path (Project project) =
    Project
        { project
            | modules = List.filter (\module_ -> module_.path /= path) project.modules
            , filesThatFailedToParse = List.filter (\file -> file.path /= path) project.filesThatFailedToParse
        }


{-| Parse source code into a AST
-}
parseSource : String -> Result () File
parseSource source =
    source
        |> Parser.parse
        |> Result.mapError (always ())
        |> Result.map (Elm.Processing.process elmProcessContext)


elmProcessContext : Elm.Processing.ProcessContext
elmProcessContext =
    Elm.Processing.init
        |> Elm.Processing.addDependency Review.Dependencies.elmCore
        |> Elm.Processing.addDependency Review.Dependencies.elmUrl
        |> Elm.Processing.addDependency Review.Dependencies.elmParser


{-| Get the list of modules in the project.
-}
modules : Project -> List ProjectModule
modules (Project project) =
    project.modules


{-| Get the list of file paths that failed to parse, because they were syntactically invalid Elm code.
-}
filesThatFailedToParse : Project -> List { path : String, source : String }
filesThatFailedToParse (Project project) =
    project.filesThatFailedToParse


{-| Get the module graph for the project in the form of a
[`elm-community/graph` Graph].

The value contained in the [`Node`]s correspond to the module name, where the
name is split by the `.` symbol. So the module `Some.Module` would correspond to
`[ "Some", "Module" ]`.

[`Edge`]s in this graph mean that a module is imported by another module: If there
is an edge going from `[ "Some", "Module" ]` to `[ "Other", "Module" ]`, then
module `Other.Module` is importing module `Some.Module`.

Note that the graph will be computed every time this function is called, and that
every rule may call this function once per review. To avoid this computation at
every call, you can use [`precomputeModuleGraph`].

[`elm-community/graph` Graph]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Graph
[`Node`]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Node
[`Edge`]: https://package.elm-lang.org/packages/elm-community/graph/6.0.0/Graph#Edge
[`precomputeModuleGraph`]: #precomputeModuleGraph

-}
moduleGraph : Project -> Graph (List String) ()
moduleGraph (Project project) =
    case project.moduleGraph of
        Just graph ->
            graph

        Nothing ->
            buildModuleGraph project.modules


{-| Precomputes the module graph that you get using [`moduleGraph`](#moduleGraph).
This is to avoid a potentially long computation for every rule run. Once the graph
is precomputed, it will be recomputed every time a module is changed, meaning
you won't need to reuse this call `precomputeModuleGraph` again.

You should use this function if and when you know you loaded all the files in
the project.

-}
precomputeModuleGraph : Project -> Project
precomputeModuleGraph ((Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            project

        Nothing ->
            Project { p | moduleGraph = Just <| buildModuleGraph p.modules }



-- `elm.json`


{-| Add the content of the `elm.json` file to the project details, making it
available for rules to access using
[`Review.Rule.withElmJsonModuleVisitor`](./Review-Rule#withElmJsonModuleVisitor) and
[`Review.Rule.withProjectElmJsonVisitor`](./Review-Rule#withProjectElmJsonVisitor).

The `raw` value should be the raw JSON as a string, and `contents` corresponds to
[`elm/project-metadata-utils`'s Project project structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).

-}
withElmJson : { path : String, raw : String, project : Elm.Project.Project } -> Project -> Project
withElmJson elmJson_ (Project project) =
    Project { project | elmJson = Just elmJson_ }


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
elmJson : Project -> Maybe { path : String, raw : String, project : Elm.Project.Project }
elmJson (Project project) =
    project.elmJson



-- PROJECT DEPENDENCIES


{-| TODO Documentation
-}
type alias Dependency =
    { name : String
    , version : Elm.Version.Version
    , elmJson : Elm.Project.Project
    , modules : List Elm.Docs.Module
    }


{-| Add a dependency to the project. These will be available for rules to make
better assumptions on what is happening in the code.

Knowing the dependencies of the project will also help better parse the source
files, since the dependencies will allow us to know the precedence and
associativity of operators, which has an impact on the resulting AST when
parsing a file.

-}
withDependency : Dependency -> Project -> Project
withDependency dependency (Project project) =
    Project
        { project
            | dependencies =
                Dict.insert
                    dependency.name
                    dependency
                    project.dependencies
        }
        |> recomputeModuleGraphIfNeeded


{-| Remove all dependencies of a project. Use this to flush the dependencies of
a project when they are changed, before re-adding them.
-}
removeDependencies : Project -> Project
removeDependencies (Project project) =
    Project { project | dependencies = Dict.empty }
        |> recomputeModuleGraphIfNeeded


{-| TODO Rewrite documentation
Get the modules for every dependency in the project.

This will give you a `Elm.Docs.Module` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Docs)
package, so you will need to install and use it to gain access to the dependency
information.

-}
dependencies : Project -> Dict String Dependency
dependencies (Project project) =
    project.dependencies



-- GRAPH CREATION


recomputeModuleGraphIfNeeded : Project -> Project
recomputeModuleGraphIfNeeded ((Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            precomputeModuleGraph project

        Nothing ->
            project


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
