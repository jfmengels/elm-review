module Review.FilePattern exposing (FilePattern, exclude, excludeFolder, include, match)


type FilePattern
    = Include String
    | Exclude String
    | ExcludeFolder String


include : String -> FilePattern
include =
    Include


exclude : String -> FilePattern
exclude =
    Exclude


excludeFolder : String -> FilePattern
excludeFolder =
    ExcludeFolder


match : List FilePattern -> String -> Bool
match filePatterns str =
    False
