module Review.Error.Target exposing
    ( Target(..)
    , elmJson
    , extraFile
    , module_
    , readme
    , setCurrentFilePathOnTargetIfNeeded
    )

import Review.Error.FileTarget as FileTarget exposing (FileTarget)


type Target
    = FileTarget FileTarget
    | Global
    | UserGlobal


module_ : String -> Target
module_ =
    FileTarget.Module >> FileTarget


elmJson : Target
elmJson =
    FileTarget FileTarget.ElmJson


readme : Target
readme =
    FileTarget FileTarget.Readme


extraFile : String -> Target
extraFile =
    FileTarget.ExtraFile >> FileTarget


setCurrentFilePathOnTargetIfNeeded : String -> Target -> Target
setCurrentFilePathOnTargetIfNeeded path target =
    case target of
        FileTarget fileTarget ->
            FileTarget.setCurrentFilePathOnTargetIfNeeded path fileTarget
                |> FileTarget

        Global ->
            target

        UserGlobal ->
            target


filePath : Target -> Maybe String
filePath target =
    case target of
        FileTarget fileTarget ->
            Just (FileTarget.filePath fileTarget)

        Global ->
            Nothing

        UserGlobal ->
            Nothing
