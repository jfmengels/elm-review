module Review.ExtraFiles exposing (ExtraFileRequest, exclude, excludeFolder, include)


type ExtraFileRequest
    = Include String
    | Exclude String
    | ExcludeFolder String


include : String -> ExtraFileRequest
include =
    Include


exclude : String -> ExtraFileRequest
exclude =
    Exclude


excludeFolder : String -> ExtraFileRequest
excludeFolder =
    ExcludeFolder
