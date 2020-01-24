module Review.Project exposing
    ( Project, ProjectModule, ElmJson
    , modules, filesThatFailedToParse, moduleGraph, elmJson, dependencyModules
    , new, withModule, withParsedModule, removeModule, withElmJson, withDependency, removeDependencies, precomputeModuleGraph
    )

{-| Represents project-related data, that a rule can access to get more information.

These will be accessible in rules with functions like [`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
This module is made to build all of the project-related data that we want
rules to have access to, to later pass it to the [`Review.review`](./Review#review) function.

This module is useful if you try to run `elm-review` by yourself. You can safely
ignore it if you just want to write a review rule.


# Definition

@docs Project, ProjectModule, ElmJson


# Access

@docs modules, filesThatFailedToParse, moduleGraph, elmJson, dependencyModules


# Build

@docs new, withModule, withParsedModule, removeModule, withElmJson, withDependency, removeDependencies, precomputeModuleGraph

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
import Graph exposing (Graph)



-- DEFINITION


{-| Represents all kinds of details about the project, such as the contents of
the `elm.json` file.
-}
type Project
    = Project
        { modules : List ProjectModule
        , filesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe ElmJson
        , dependencyModules : Dict String Elm.Docs.Module
        , moduleToDependency : Dict String String
        , moduleGraph : Maybe (Graph ModuleName ())
        }


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    }


{-| Contents of the `elm.json` file. Alias to
[`elm/project-metadata-utils`'s Project project structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).
-}
type alias ElmJson =
    Elm.Project.Project



-- ACCESS


{-| Get the list of modules in the project.
-}
modules : Project -> List ProjectModule
modules (Project project) =
    project.modules


moduleGraph : Project -> Graph ModuleName ()
moduleGraph (Project project) =
    case project.moduleGraph of
        Just graph ->
            graph

        Nothing ->
            buildModuleGraph project.modules


{-| Get the list of file paths that failed to parse, because they were syntactically invalid Elm code.
-}
filesThatFailedToParse : Project -> List { path : String, source : String }
filesThatFailedToParse (Project project) =
    project.filesThatFailedToParse


{-| Get the contents of the `elm.json` file, if available.

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

-}
elmJson : Project -> Maybe ElmJson
elmJson (Project project) =
    project.elmJson


{-| Get the modules for every dependency in the project.

This will give you a `Elm.Docs.Module` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Docs)
package, so you will need to install and use it to gain access to the dependency
information.

-}
dependencyModules : Project -> Dict String Elm.Docs.Module
dependencyModules (Project project) =
    project.dependencyModules



-- BUILD


{-| Create a new Project.
-}
new : Project
new =
    Project
        { modules = []
        , filesThatFailedToParse = []
        , elmJson = Nothing
        , dependencyModules = Dict.empty
        , moduleToDependency = Dict.empty
        , moduleGraph = Nothing
        }


{-| Add a module to the project. This module will then be analyzed by the rules.
-}
withModule : { path : String, source : String } -> Project -> Project
withModule { path, source } project =
    recomputeModuleGraphIfNeeded <|
        case parseSource source of
            Ok ast ->
                project
                    |> removeFileFromProject path
                    |> addModule
                        { path = path
                        , source = source
                        , ast = reorderComments ast
                        }

            Err _ ->
                project
                    |> removeFileFromProject path
                    |> addFileThatFailedToParse
                        { path = path
                        , source = source
                        }


reorderComments : Elm.Syntax.File.File -> Elm.Syntax.File.File
reorderComments ast =
    { ast | comments = List.sortBy (Node.range >> .start >> positionAsInt >> negate) ast.comments }


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long, which the compiler does not support for Elm 0.19.1.
    row * 1000000 + column


{-| Add an already parsed module to the project. This module will then be analyzed by the rules.
-}
withParsedModule : { path : String, source : String, ast : Elm.Syntax.File.File } -> Project -> Project
withParsedModule module_ project =
    project
        |> removeFileFromProject module_.path
        |> addModule module_
        |> recomputeModuleGraphIfNeeded


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


addModule : ProjectModule -> Project -> Project
addModule module_ (Project project) =
    Project { project | modules = module_ :: project.modules }


addFileThatFailedToParse : { path : String, source : String } -> Project -> Project
addFileThatFailedToParse { path, source } (Project project) =
    Project
        { project
            | filesThatFailedToParse = { path = path, source = source } :: project.filesThatFailedToParse
        }


{-| Parse source code into a AST
-}
parseSource : String -> Result () File
parseSource source =
    source
        |> Parser.parse
        |> Result.mapError (always ())
        |> Result.map (Elm.Processing.process Elm.Processing.init)


{-| Add the content of the `elm.json` file to the project details, making it
available for rules to access using
[`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
-}
withElmJson : ElmJson -> Project -> Project
withElmJson elmJson_ (Project project) =
    Project { project | elmJson = Just elmJson_ }


{-| Add a dependency to the project. These will be available for rules to make
better assumptions on what is happening in the code.

Knowing the dependencies of the project will also help better parse the source
files, since the dependencies will allow us to know the precedence and
associativity of operators, which has an impact on the resulting AST when
parsing a file.

-}
withDependency : { r | packageName : String, modules : List Elm.Docs.Module } -> Project -> Project
withDependency dependency (Project project) =
    Project
        { project
            | dependencyModules =
                Dict.union
                    project.dependencyModules
                    (dependency.modules
                        |> List.map (\module_ -> ( module_.name, module_ ))
                        |> Dict.fromList
                    )
            , moduleToDependency =
                Dict.union
                    project.moduleToDependency
                    (dependency.modules
                        |> List.map (\module_ -> ( module_.name, dependency.packageName ))
                        |> Dict.fromList
                    )
        }
        |> recomputeModuleGraphIfNeeded


{-| Remove all dependencies of a project. Use this to flush the dependencies of
a project when they are changed, before re-adding them.
-}
removeDependencies : Project -> Project
removeDependencies (Project project) =
    Project
        { project
            | dependencyModules = Dict.empty
            , moduleToDependency = Dict.empty
        }
        |> recomputeModuleGraphIfNeeded



-- GRAPH CREATION


recomputeModuleGraphIfNeeded : Project -> Project
recomputeModuleGraphIfNeeded ((Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            precomputeModuleGraph project

        Nothing ->
            project


precomputeModuleGraph : Project -> Project
precomputeModuleGraph ((Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            project

        Nothing ->
            Project { p | moduleGraph = Just <| buildModuleGraph p.modules }


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
