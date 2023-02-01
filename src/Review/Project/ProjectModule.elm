module Review.Project.ProjectModule exposing (ProjectModule, create)

{-| Represents a parsed file.
-}

import Elm.Syntax.File
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
    , ast = params.ast
    , contentHash = ContentHash.hash params.source
    , isInSourceDirectories = params.isInSourceDirectories
    }
