module Review.Project.ProjectModule exposing
    ( ProjectModule, create
    , path, source, ast, contentHash, isInSourceDirectories
    , setIsInSourceDirectories
    )

{-| Represents a parsed file.

@docs ProjectModule, create

@docs path, source, ast, contentHash, isInSourceDirectories
@docs setIsInSourceDirectories

-}

import Elm.Syntax.File
import Elm.Syntax.Node exposing (Node(..))
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)


type ProjectModule
    = ProjectModule
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
    ProjectModule
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


path : ProjectModule -> String
path (ProjectModule module_) =
    module_.path


source : ProjectModule -> String
source (ProjectModule module_) =
    module_.source


ast : ProjectModule -> Elm.Syntax.File.File
ast (ProjectModule module_) =
    module_.ast


contentHash : ProjectModule -> ContentHash
contentHash (ProjectModule module_) =
    module_.contentHash


isInSourceDirectories : ProjectModule -> Bool
isInSourceDirectories (ProjectModule module_) =
    module_.isInSourceDirectories


setIsInSourceDirectories : Bool -> ProjectModule -> ProjectModule
setIsInSourceDirectories isInSourceDirectories_ (ProjectModule module_) =
    ProjectModule { module_ | isInSourceDirectories = isInSourceDirectories_ }
