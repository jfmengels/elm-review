module Review.Project.Internal exposing
    ( Project(..)
    , sanitizeModule
    , sourceDirectories
    , sourceDirectoriesForProject
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Node exposing (Node(..))
import Path
import Review.Cache.ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.Project.Dependency exposing (Dependency)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule exposing (ProjectModule)
import Vendor.Graph exposing (Graph)


type Project
    = Project
        { modules : Dict String ProjectModule
        , modulesThatFailedToParse : List { path : String, source : String }
        , elmJson : Maybe ( { path : String, raw : String, project : Elm.Project.Project }, ContentHash )
        , readme : Maybe ( { path : String, content : String }, ContentHash )
        , dependencies : Dict String Dependency
        , moduleGraph : Maybe (Graph FilePath ())
        , sourceDirectories : List String
        , cache : ProjectCache
        }


sourceDirectories : Project -> List String
sourceDirectories (Project project) =
    project.sourceDirectories


sanitizeModule : Elm.Syntax.File.File -> Elm.Syntax.File.File
sanitizeModule ast =
    { ast | comments = List.sortBy (\(Node range _) -> positionAsInt range.start) ast.comments }


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long, which the compiler does not support for Elm 0.19.1.
    row * 1000000 + column


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
