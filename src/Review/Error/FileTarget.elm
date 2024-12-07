module Review.Error.FileTarget exposing
    ( Target(..)
    , filePath
    , setCurrentFilePathOnTargetIfNeeded
    )


type Target
    = Module String
    | ElmJson
    | Readme
    | ExtraFile String
    | Global
    | UserGlobal


setCurrentFilePathOnTargetIfNeeded : String -> Target -> Target
setCurrentFilePathOnTargetIfNeeded path target =
    case target of
        Module "" ->
            Module path

        ExtraFile _ ->
            target

        Module _ ->
            target

        ElmJson ->
            target

        Readme ->
            target

        Global ->
            target

        UserGlobal ->
            target


filePath : Target -> Maybe String
filePath target =
    case target of
        Module path ->
            Just path

        ElmJson ->
            Just "elm.json"

        Readme ->
            Just "README.md"

        ExtraFile path ->
            Just path

        Global ->
            Nothing

        UserGlobal ->
            Nothing
