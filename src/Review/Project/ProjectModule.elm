module Review.Project.ProjectModule exposing (..)

{-| Represents a parsed file.
-}

import Elm.Syntax.File


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , isInSourceDirectories : Bool
    }
