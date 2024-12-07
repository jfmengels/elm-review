module Review.Error.Target exposing (..)


type Target
    = Module String
    | ElmJson
    | Readme
    | ExtraFile String
    | Global
    | UserGlobal


setCurrentFilePathOnTargetIfNeeded : String -> Target -> Target
setCurrentFilePathOnTargetIfNeeded filePath target =
    case target of
        Module "" ->
            Module filePath

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
