module Review.Project.Internal exposing
    ( Project(..)
    , sourceDirectories
    , sourceDirectoriesForProject
    )

{-| Holds all the information related to the project such as the contents of
the `elm.json` file, the project modules and the project dependencies.
-}

import Dict exposing (Dict)
import Elm.Project
import Path
import Review.Cache.ContentHash exposing (ContentHash)
import Review.FilePath exposing (FilePath)
import Review.Project.Dependency exposing (Dependency)
import Review.Project.ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule exposing (OpaqueProjectModule)
import Vendor.Graph exposing (Graph)


type Project
    = Project
        { modules : Dict String OpaqueProjectModule
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
