module Review.Project.ProjectModule exposing (ProjectModule)

{-| Represents a parsed file.
-}

import Elm.Syntax.File
import Review.Project.ContentHash exposing (ContentHash)


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , contentHash : ContentHash
    , isInSourceDirectories : Bool
    }
