module Review.Project.ProjectModule exposing
    ( OpaqueProjectModule, create
    , path, source, ast, contentHash, isInSourceDirectories
    , setIsInSourceDirectories
    , ProjectModule, toRecord
    )

{-| Represents a parsed file.

@docs OpaqueProjectModule, create

@docs path, source, ast, contentHash, isInSourceDirectories
@docs setIsInSourceDirectories

@docs ProjectModule, toRecord

-}

import Elm.Syntax.File
import Elm.Syntax.Node exposing (Node(..))
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)


type OpaqueProjectModule
    = OpaqueProjectModule
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
    -> OpaqueProjectModule
create params =
    OpaqueProjectModule
        { path = params.path
        , source = params.source
        , ast = sanitizeModule params.ast
        , contentHash = ContentHash.hash params.source
        , isInSourceDirectories = params.isInSourceDirectories
        }


sanitizeModule : Elm.Syntax.File.File -> Elm.Syntax.File.File
sanitizeModule ast_ =
    { ast_ | comments = List.sortBy (\(Node range _) -> positionAsInt range.start) ast_.comments }


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long, which the compiler does not support for Elm 0.19.1.
    row * 1000000 + column


path : OpaqueProjectModule -> String
path (OpaqueProjectModule module_) =
    module_.path


source : OpaqueProjectModule -> String
source (OpaqueProjectModule module_) =
    module_.source


ast : OpaqueProjectModule -> Elm.Syntax.File.File
ast (OpaqueProjectModule module_) =
    module_.ast


contentHash : OpaqueProjectModule -> ContentHash
contentHash (OpaqueProjectModule module_) =
    module_.contentHash


isInSourceDirectories : OpaqueProjectModule -> Bool
isInSourceDirectories (OpaqueProjectModule module_) =
    module_.isInSourceDirectories


setIsInSourceDirectories : Bool -> OpaqueProjectModule -> OpaqueProjectModule
setIsInSourceDirectories isInSourceDirectories_ (OpaqueProjectModule module_) =
    OpaqueProjectModule { module_ | isInSourceDirectories = isInSourceDirectories_ }


type alias ProjectModule =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    , contentHash : ContentHash
    , isInSourceDirectories : Bool
    }


toRecord : OpaqueProjectModule -> ProjectModule
toRecord (OpaqueProjectModule module_) =
    module_
