module Review.Project.ProjectModule exposing (ProjectModule)

{-| Represents a parsed file.
-}

import Elm.Syntax.File
import Review.Project.FileHash exposing (FileHash)


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , fileHash : FileHash
    , isInSourceDirectories : Bool
    }
