module Review.Error.Target exposing (..)


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
