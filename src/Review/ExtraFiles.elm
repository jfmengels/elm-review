module Review.ExtraFiles exposing (ExtraFileRequest, exclude, excludeFolder, include)

import Review.Project.Internal as Internal


type alias ExtraFileRequest =
    Internal.ExtraFileRequest


include : String -> ExtraFileRequest
include =
    Internal.Include


exclude : String -> ExtraFileRequest
exclude =
    Internal.Exclude


excludeFolder : String -> ExtraFileRequest
excludeFolder =
    Internal.ExcludeFolder
