module Review.Project exposing
    ( Project, ElmJson
    , modules, filesThatFailedToParse, moduleGraph, elmJson, dependencyModules
    , new, withModule, withElmJson, withDependency, precomputeModuleGraph
    )

{-| Represents project-related data, that a rule can access to get more information.

These will be accessible in rules with functions like [`Review.Rule.withElmJsonVisitor`](./Review-Rule#withElmJsonVisitor).
This module is made to build all of the project-related data that we want
rules to have access to, to later pass it to the [`Review.review`](./Review#review) function.

This module is useful if you try to run `elm-review` by yourself. You can safely
ignore it if you just want to write a review rule.


# Definition

@docs Project, ElmJson


# Access

@docs modules, filesThatFailedToParse, moduleGraph, elmJson, dependencyModules


# Build

@docs new, withModule, withElmJson, withDependency, precomputeModuleGraph

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
import IntDict exposing (IntDict)
import Review.File exposing (ParsedFile)
import Set exposing (Set)



-- DEFINITION


{-| Represents all kinds of details about the project, such as the contents of
the `elm.json` file.
-}
type Project
    = Project
        { modules : List ParsedFile
        , filesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe ElmJson
        , dependencyModules : Dict String Elm.Docs.Module
        , moduleToDependency : Dict String String
        , moduleGraph : Maybe (Graph ModuleName ())
        }


{-| Contents of the `elm.json` file. Alias to
[`elm/project-metadata-utils`'s Project project structure](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project).
-}
type alias ElmJson =
    Elm.Project.Project



-- ACCESS


{-| Get the list of modules in the project.
-}
modules : Project -> List ParsedFile
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

This will give you a `Project` type from the
[`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.0/Elm-Project)
package, so you will need to install and use it to gain access to the
information inside the `elm.json` file.

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
    case parseSource source of
        Ok ast ->
            project
                |> removeFileFromProject path
                |> addModule
                    { path = path
                    , source = source
                    , ast = ast
                    }

        Err _ ->
            project
                |> removeFileFromProject path
                |> addFileThatFailedToParse
                    { path = path
                    , source = source
                    }


removeFileFromProject : String -> Project -> Project
removeFileFromProject path (Project project) =
    Project
        { project
            | modules = List.filter (\module_ -> module_.path /= path) project.modules
            , filesThatFailedToParse = List.filter (\file -> file.path /= path) project.filesThatFailedToParse
        }


addModule : ParsedFile -> Project -> Project
addModule module_ (Project project) =
    -- TODO Recompute module graph if it was already computed
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
        -- TODO Add the dependencies to fix how files will be parsed
        -- Note: If the dependencies change, we'll need to reprocess everything, just in case.
        -- Also, invalidate the cache for all the files.
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
    -- TODO Recompute module graph if it was already computed
    Project
        { project
            | dependencyModules =
                dependency.modules
                    |> List.map (\module_ -> ( module_.name, module_ ))
                    |> Dict.fromList
                    |> Dict.union project.dependencyModules
            , moduleToDependency =
                dependency.modules
                    |> List.map (\module_ -> ( module_.name, dependency.packageName ))
                    |> Dict.fromList
                    |> Dict.union project.moduleToDependency
        }



-- GRAPH CREATION


precomputeModuleGraph : Project -> Project
precomputeModuleGraph ((Project p) as project) =
    case p.moduleGraph of
        Just _ ->
            project

        Nothing ->
            Project { p | moduleGraph = Just <| buildModuleGraph p.modules }


buildModuleGraph : List ParsedFile -> Graph ModuleName ()
buildModuleGraph mods =
    let
        fileIds : Dict ModuleName Int
        fileIds =
            mods
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, file ) dict ->
                        Dict.insert
                            (getModuleName file)
                            index
                            dict
                    )
                    Dict.empty

        getFileId : ModuleName -> Int
        getFileId moduleName =
            case Dict.get moduleName fileIds of
                Just fileId ->
                    fileId

                Nothing ->
                    getFileId moduleName

        ( nodes, edges ) =
            mods
                |> List.foldl
                    (\file ( resNodes, resEdges ) ->
                        let
                            ( moduleNode, modulesEdges ) =
                                nodesAndEdges (\moduleName -> Dict.get moduleName fileIds) file (getFileId <| getModuleName file)
                        in
                        ( moduleNode :: resNodes, List.concat [ modulesEdges, resEdges ] )
                    )
                    ( [], [] )
    in
    Graph.fromNodesAndEdges nodes edges


nodesAndEdges : (ModuleName -> Maybe Int) -> ParsedFile -> Int -> ( Graph.Node ModuleName, List (Graph.Edge ()) )
nodesAndEdges getFileId module_ fileId =
    let
        moduleName =
            getModuleName module_
    in
    ( Graph.Node fileId moduleName
    , importedModules module_
        |> List.filterMap getFileId
        |> List.map
            (\importFileId ->
                Graph.Edge fileId importFileId ()
            )
    )


importedModules : ParsedFile -> List ModuleName
importedModules module_ =
    module_.ast.imports
        |> List.map (Node.value >> .moduleName >> Node.value)


getModuleName : ParsedFile -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Elm.Syntax.Module.moduleName
