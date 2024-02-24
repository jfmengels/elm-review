module Review.ExtraFiles.Internal exposing (ExtraFileRequest(..))


type ExtraFileRequest
    = Include String
    | Exclude String
    | ExcludeFolder String
