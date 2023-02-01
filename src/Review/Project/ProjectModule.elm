module Review.Project.ProjectModule exposing (ProjectModule, create)

{-| Represents a parsed file.
-}

import Elm.Syntax.File
import Elm.Syntax.Node exposing (Node(..))
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , contentHash : ContentHash
    , isInSourceDirectories : Bool
    }


create :
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , isInSourceDirectories : Bool
    }
    -> ProjectModule
create params =
    { path = params.path
    , source = params.source
    , ast = sanitizeModule params.ast
    , contentHash = ContentHash.hash params.source
    , isInSourceDirectories = params.isInSourceDirectories
    }


sanitizeModule : Elm.Syntax.File.File -> Elm.Syntax.File.File
sanitizeModule ast =
    { ast | comments = List.sortBy (\(Node range _) -> positionAsInt range.start) ast.comments }


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long, which the compiler does not support for Elm 0.19.1.
    row * 1000000 + column
