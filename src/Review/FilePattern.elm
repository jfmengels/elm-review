module Review.FilePattern exposing (FilePattern, exclude, excludeFolder, include, match)

import Glob exposing (Glob)


type FilePattern
    = Include Glob
    | Exclude Glob
    | ExcludeFolder Glob
    | InvalidGlob String


include : String -> FilePattern
include globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Include glob

        Err _ ->
            InvalidGlob globStr


exclude : String -> FilePattern
exclude globStr =
    case Glob.fromString globStr of
        Ok glob ->
            Exclude glob

        Err _ ->
            InvalidGlob globStr


excludeFolder : String -> FilePattern
excludeFolder globStr =
    case Glob.fromString globStr of
        Ok glob ->
            ExcludeFolder glob

        Err _ ->
            InvalidGlob globStr


match : List FilePattern -> String -> Bool
match filePatterns str =
    False
