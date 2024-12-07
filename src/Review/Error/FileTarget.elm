module Review.Error.FileTarget exposing
    ( FileTarget(..)
    , filePath
    , setCurrentFilePathOnTargetIfNeeded
    )


type FileTarget
    = Module String
    | ElmJson
    | Readme
    | ExtraFile String


setCurrentFilePathOnTargetIfNeeded : String -> FileTarget -> FileTarget
setCurrentFilePathOnTargetIfNeeded path target =
    case target of
        Module "" ->
            Module path

        _ ->
            target


filePath : FileTarget -> String
filePath target =
    case target of
        Module path ->
            path

        ElmJson ->
            "elm.json"

        Readme ->
            "README.md"

        ExtraFile path ->
            path
