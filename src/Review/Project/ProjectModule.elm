module Review.Project.ProjectModule exposing (ProjectModule)

{-| Represents a parsed file.
-}

import Elm.Syntax.File
import Review.Project.CacheHash exposing (CacheHash)


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , cacheHash : CacheHash
    , isInSourceDirectories : Bool
    }
