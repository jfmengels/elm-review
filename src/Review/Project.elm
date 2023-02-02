module Review.Project exposing
    ( Project, new
    , ProjectModule, addModule, addParsedModule, removeModule, modules, modulesThatFailedToParse, precomputeModuleGraph
    , addElmJson, elmJson
    , addReadme, readme
    , addDependency, removeDependency, removeDependencies, directDependencies, dependencies
    )

{-| Represents the contents of the project to be analyzed. This information will
then be fed to the review rules.

You may need to use use this module if you want

  - to create test cases where the project is in a certain configuration
  - to make `elm-review` run in a new environment

You can safely ignore this module if you just want to write a review rule that
does not look at project information (like the `elm.json`, dependencies, ...).

@docs Project, new


## Elm modules

@docs ProjectModule, addModule, addParsedModule, removeModule, modules, modulesThatFailedToParse, precomputeModuleGraph


# `elm.json`

@docs addElmJson, elmJson


# `README.md`

@docs addReadme, readme


# Project dependencies

@docs addDependency, removeDependency, removeDependencies, directDependencies, dependencies

-}

import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Syntax.File
import Path
import Review.Cache.ContentHash as ContentHash
import Review.FileParser as FileParser
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Project.Internal as Internal exposing (Project)
import Review.Project.ProjectCache as ProjectCache
import Review.Project.ProjectModule as ProjectModule



-- PROJECT


{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}
type alias Project =
    Internal.Project


{-| Create a new empty Project.

**For tests**, you can also start of with a project that contains the `elm/core` dependency using
[`Review.Test.Dependencies.projectWithElmCore`](./Review-Test-Dependencies#projectWithElmCore). Some more prepared
dependencies can be found in that same module.

-}
new : Project
new =
    Internal.Project
        { modules = Dict.empty
        , modulesThatFailedToParse = []
        , elmJson = Nothing
        , readme = Nothing
        , dependencies = Dict.empty
        , moduleGraph = Nothing
        , sourceDirectories = [ "src/" ]
        , cache = ProjectCache.empty
        }



-- PROJECT FILES


{-| Represents a parsed file.
-}
type alias ProjectModule =
    ProjectModule.ProjectModule


{-| Add an Elm file to the project. If a file with the same path already exists,
then it will replace it.

If the file is syntactically valid Elm code, it will then be analyzed by the
review rules. Otherwise, the file will be added to the list of files that failed
to parse, which you can get using [`modulesThatFailedToParse`](#modulesThatFailedToParse),
and for which a parsing error will be reported when running [`the review function`](./Review-Rule#reviewV2).

-}
addModule : { path : String, source : String } -> Project -> Project
addModule { path, source } project =
    case FileParser.parse source of
        Ok ast ->
            let
                osAgnosticPath : String
                osAgnosticPath =
                    Path.makeOSAgnostic path
            in
            project
                |> addModuleToProject
                    (ProjectModule.create
                        { path = path
                        , source = source
                        , ast = ast
                        , isInSourceDirectories = List.any (\dir -> String.startsWith (Path.makeOSAgnostic dir) osAgnosticPath) (Internal.sourceDirectories project)
                        }
                    )
                |> removeFileFromFilesThatFailedToParse path
                |> forceModuleGraphRecomputation

        Err _ ->
            project
                |> removeFileFromProject path
                |> addFileThatFailedToParse
                    { path = path
                    , source = source
                    }
                |> forceModuleGraphRecomputation


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
addParsedModule : { path : String, source : String, ast : Elm.Syntax.File.File } -> Project -> Project
addParsedModule { path, source, ast } project =
    let
        osAgnosticPath : String
        osAgnosticPath =
            Path.makeOSAgnostic path
    in
    project
        |> removeFileFromFilesThatFailedToParse path
        |> addModuleToProject
            (ProjectModule.create
                { path = path
                , source = source
                , ast = ast
                , isInSourceDirectories = List.any (\dir -> String.startsWith (Path.makeOSAgnostic dir) osAgnosticPath) (Internal.sourceDirectories project)
                }
            )
        |> forceModuleGraphRecomputation


addModuleToProject : ProjectModule.OpaqueProjectModule -> Project -> Project
addModuleToProject module_ (Internal.Project project) =
    Internal.Project { project | modules = Dict.insert (ProjectModule.path module_) module_ project.modules }


addFileThatFailedToParse : { path : String, source : String } -> Project -> Project
addFileThatFailedToParse { path, source } (Internal.Project project) =
    Internal.Project
        { project
            | modulesThatFailedToParse = { path = path, source = source } :: project.modulesThatFailedToParse
        }


{-| Remove a module from the project by its path.
-}
removeModule : String -> Project -> Project
removeModule path project =
    project
        |> removeFileFromProject path
        |> forceModuleGraphRecomputation


removeFileFromProject : String -> Project -> Project
removeFileFromProject path (Internal.Project project) =
    Internal.Project { project | modules = Dict.remove path project.modules }
        |> removeFileFromFilesThatFailedToParse path


removeFileFromFilesThatFailedToParse : String -> Project -> Project
removeFileFromFilesThatFailedToParse path (Internal.Project project) =
    Internal.Project
        { project
            | modulesThatFailedToParse = List.filter (\file -> file.path /= path) project.modulesThatFailedToParse
        }


{-| Get the list of modules in the project.
-}
modules : Project -> List ProjectModule
modules (Internal.Project project) =
    Dict.values project.modules
        |> List.map ProjectModule.toRecord


{-| Get the list of file paths that failed to parse, because they were syntactically invalid Elm code.
-}
modulesThatFailedToParse : Project -> List { path : String, source : String }
modulesThatFailedToParse (Internal.Project project) =
    project.modulesThatFailedToParse


{-| Precomputes the module graph.

**@deprecated** This is not useful anymore.

-}
precomputeModuleGraph : Project -> Project
precomputeModuleGraph project =
    project



-- `elm.json`


{-| Add the content of the `elm.json` file to the project, making it
available for rules to access using
[`Review.Rule.withElmJsonModuleVisitor`](./Review-Rule#withElmJsonModuleVisitor) and
[`Review.Rule.withElmJsonProjectVisitor`](./Review-Rule#withElmJsonProjectVisitor).

The `raw` value should be the raw JSON as a string, and `project` corresponds to
[`elm/project-metadata-utils`'s `Elm.Project.Project` type](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project#Project).

-}
addElmJson : { path : String, raw : String, project : Elm.Project.Project } -> Project -> Project
addElmJson elmJson_ (Internal.Project project) =
    let
        sourceDirectories : List String
        sourceDirectories =
            Internal.sourceDirectoriesForProject elmJson_.project

        modules_ : Dict String ProjectModule.OpaqueProjectModule
        modules_ =
            if project.sourceDirectories == sourceDirectories then
                project.modules

            else
                Dict.map
                    (\path module_ ->
                        let
                            osAgnosticPath : String
                            osAgnosticPath =
                                Path.makeOSAgnostic path
                        in
                        ProjectModule.setIsInSourceDirectories
                            (List.any (\dir -> String.startsWith dir osAgnosticPath) sourceDirectories)
                            module_
                    )
                    project.modules
    in
    Internal.Project
        { project
            | elmJson = Just ( elmJson_, ContentHash.hash elmJson_.raw )
            , sourceDirectories = sourceDirectories
            , modules = modules_
        }


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Elm.Project.Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information from the `elm.json` file.

-}
elmJson : Project -> Maybe { path : String, raw : String, project : Elm.Project.Project }
elmJson (Internal.Project project) =
    Maybe.map Tuple.first project.elmJson



-- `README.md`


{-| Add the content of the `README.md` file to the project, making it
available for rules to access using
[`Review.Rule.withReadmeModuleVisitor`](./Review-Rule#withReadmeModuleVisitor) and
[`Review.Rule.withReadmeProjectVisitor`](./Review-Rule#withReadmeProjectVisitor).
-}
addReadme : { path : String, content : String } -> Project -> Project
addReadme readme_ (Internal.Project project) =
    Internal.Project { project | readme = Just ( readme_, ContentHash.hash readme_.content ) }


{-| Get the contents of the `README.md` file, if available.
-}
readme : Project -> Maybe { path : String, content : String }
readme (Internal.Project project) =
    Maybe.map Tuple.first project.readme


{-| Add a dependency to the project. These will be available for rules to make
better assumptions on what is happening in the code.

Knowing the dependencies of the project will also help better parse the source
files, since the dependencies will allow us to know the precedence and
associativity of operators, which has an impact on the resulting AST when
parsing a file.

**For tests**, `elm-review` comes with a few dependencies that you can find in
[`Review.Test.Dependencies`](./Review-Test-Dependencies).

-}
addDependency : Dependency -> Project -> Project
addDependency dependency (Internal.Project project) =
    Internal.Project
        { project
            | dependencies =
                Dict.insert
                    (Dependency.name dependency)
                    dependency
                    project.dependencies
        }


{-| Remove a dependency from a project by name.
-}
removeDependency : String -> Project -> Project
removeDependency dependencyName (Internal.Project project) =
    Internal.Project { project | dependencies = Dict.remove dependencyName project.dependencies }


{-| Remove all dependencies of a project. Use this to flush the dependencies of
a project when they are changed, before re-adding them.
-}
removeDependencies : Project -> Project
removeDependencies (Internal.Project project) =
    Internal.Project { project | dependencies = Dict.empty }


{-| Get the [dependencies](./Review-Project-Dependency#Dependency) of the project.
-}
dependencies : Project -> Dict String Dependency
dependencies (Internal.Project project) =
    project.dependencies


{-| Get the direct [dependencies](./Review-Project-Dependency#Dependency) of the project.
-}
directDependencies : Project -> Dict String Dependency
directDependencies (Internal.Project project) =
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



-- GRAPH CREATION


forceModuleGraphRecomputation : Project -> Project
forceModuleGraphRecomputation (Internal.Project project) =
    Internal.Project { project | moduleGraph = Nothing }
