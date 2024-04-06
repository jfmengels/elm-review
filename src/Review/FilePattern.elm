module Review.FilePattern exposing (FilePattern, exclude, excludeFolder, include)


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
