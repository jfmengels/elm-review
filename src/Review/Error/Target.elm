module Review.Error.Target exposing
    ( Target(..)
    , elmJson
    , extraFile
    , filePath
    , module_
    , readme
    , setCurrentFilePathOnTargetIfNeeded
    )


type Target
    = Module String
    | ElmJson
    | Readme
    | ExtraFile String
    | Global
    | UserGlobal


module_ : String -> Target
module_ =
    Module


elmJson : Target
elmJson =
    ElmJson


readme : Target
readme =
    Readme


extraFile : String -> Target
extraFile =
    ExtraFile


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
