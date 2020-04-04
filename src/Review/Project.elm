module Review.Project exposing
    ( Project, new
    , ProjectModule, addModule, addParsedModule, removeModule, modules, modulesThatFailedToParse, precomputeModuleGraph
    , addElmJson, elmJson
    , addReadme, readme
    , addDependency, removeDependencies, dependencies
    )

{-| Represents the contents of the project to be analyzed. This information will
then be fed to the review rules.

You may need to use use this module if you want

  - to create test cases where the project is in a certain configuration
  - to make `elm-review` run in a new environment

You can safely ignore this module if you just want to write a review rule that
does not look at project information (like the `elm.json`, dependencies, ...).


# Project

@docs Project, new


# Project files


## Elm modules

@docs ProjectModule, addModule, addParsedModule, removeModule, modules, modulesThatFailedToParse, precomputeModuleGraph


# `elm.json`

@docs addElmJson, elmJson


# `README.md`

@docs addReadme, readme


# Project dependencies

@docs addDependency, removeDependencies, dependencies

-}

import Dict exposing (Dict)
import Elm.Parser as Parser
import Elm.Processing
import Elm.Project
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node
import Review.Dependencies
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Project.Internal as Internal exposing (Project)



-- PROJECT


{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}
type alias Project =
    Internal.Project


{-| Create a new Project.
-}
new : Project
new =
    Internal.Project
        { modules = []
        , modulesThatFailedToParse = []
        , elmJson = Nothing
        , readme = Nothing
        , dependencies = Dict.empty
        , moduleGraph = Nothing
        }



-- PROJECT FILES


{-| Represents a parsed file.
-}
type alias ProjectModule =
    Internal.ProjectModule


{-| Add an Elm file to the project. If a file with the same path already exists,
then it will replace it.

If the file is syntactically valid Elm code, it will then be analyzed by the
review rules. Otherwise, the file will be added to the list of files that failed
to parse, which you can get using [`modulesThatFailedToParse`](#modulesThatFailedToParse),
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
addModuleToProject module_ (Internal.Project project) =
    Internal.Project { project | modules = sanitizeModule module_ :: project.modules }


sanitizeModule : ProjectModule -> ProjectModule
sanitizeModule module_ =
    { module_ | ast = reorderComments module_.ast }


reorderComments : Elm.Syntax.File.File -> Elm.Syntax.File.File
reorderComments ast =
    { ast | comments = List.sortBy (Node.range >> .start >> positionAsInt >> negate) ast.comments }


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
        |> recomputeModuleGraphIfNeeded


removeFileFromProject : String -> Project -> Project
removeFileFromProject path (Internal.Project project) =
    Internal.Project
        { project
            | modules = List.filter (\module_ -> module_.path /= path) project.modules
            , modulesThatFailedToParse = List.filter (\file -> file.path /= path) project.modulesThatFailedToParse
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
modules (Internal.Project project) =
    project.modules


{-| Get the list of file paths that failed to parse, because they were syntactically invalid Elm code.
-}
modulesThatFailedToParse : Project -> List { path : String, source : String }
modulesThatFailedToParse (Internal.Project project) =
    project.modulesThatFailedToParse


{-| Precomputes the module graph that you get using [`moduleGraph`](#moduleGraph).

This is to avoid a potentially long computation for every rule run. Once the graph
is precomputed, it will be recomputed every time a module is changed, meaning
you won't need to reuse this call `precomputeModuleGraph` again.

You should use this function if and when you know you loaded all the files in
the project.

-}
precomputeModuleGraph : Project -> Project
precomputeModuleGraph ((Internal.Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            project

        Nothing ->
            Internal.Project { p | moduleGraph = Just <| Internal.buildModuleGraph p.modules }



-- `elm.json`


{-| Add the content of the `elm.json` file to the project, making it
available for rules to access using
[`Review.Rule.withElmJsonModuleVisitor`](./Review-Rule#withElmJsonModuleVisitor) and
[`Review.Rule.withElmJsonProjectVisitor`](./Review-Rule#withElmJsonProjectVisitor).

The `raw` value should be the raw JSON as a string, and `contents` corresponds to
[`elm/project-metadata-utils`'s Project project structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).

-}
addElmJson : { path : String, raw : String, project : Elm.Project.Project } -> Project -> Project
addElmJson elmJson_ (Internal.Project project) =
    Internal.Project { project | elmJson = Just elmJson_ }


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
elmJson : Project -> Maybe { path : String, raw : String, project : Elm.Project.Project }
elmJson (Internal.Project project) =
    project.elmJson



-- `README.md`


{-| Add the content of the `README.md` file to the project, making it
available for rules to access using
[`Review.Rule.withReadmeModuleVisitor`](./Review-Rule#withReadmeModuleVisitor) and
[`Review.Rule.withReadmeProjectVisitor`](./Review-Rule#withReadmeProjectVisitor).
-}
addReadme : { path : String, content : String } -> Project -> Project
addReadme readme_ (Internal.Project project) =
    Internal.Project { project | readme = Just readme_ }


{-| Get the contents of the `README.md` file, if available.
-}
readme : Project -> Maybe { path : String, content : String }
readme (Internal.Project project) =
    project.readme


{-| Add a dependency to the project. These will be available for rules to make
better assumptions on what is happening in the code.

Knowing the dependencies of the project will also help better parse the source
files, since the dependencies will allow us to know the precedence and
associativity of operators, which has an impact on the resulting AST when
parsing a file.

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
        |> recomputeModuleGraphIfNeeded


{-| Remove all dependencies of a project. Use this to flush the dependencies of
a project when they are changed, before re-adding them.
-}
removeDependencies : Project -> Project
removeDependencies (Internal.Project project) =
    Internal.Project { project | dependencies = Dict.empty }
        |> recomputeModuleGraphIfNeeded


{-| Get the [dependencies](./Review-Project-Dependency#Dependency) of the project.
-}
dependencies : Project -> Dict String Dependency
dependencies (Internal.Project project) =
    project.dependencies



-- GRAPH CREATION


recomputeModuleGraphIfNeeded : Project -> Project
recomputeModuleGraphIfNeeded ((Internal.Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            precomputeModuleGraph project

        Nothing ->
            project
