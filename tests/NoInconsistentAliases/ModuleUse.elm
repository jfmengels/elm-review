module NoInconsistentAliases.ModuleUse exposing (ModuleUse, mapFunction, new, range)

import Elm.Syntax.Range exposing (Range)


type ModuleUse
    = ModuleUse String Range


new : String -> Range -> ModuleUse
new newFunction newRange =
    ModuleUse newFunction newRange


mapFunction : (String -> a) -> ModuleUse -> a
mapFunction mapper (ModuleUse name _) =
    mapper name


range : ModuleUse -> Range
range (ModuleUse _ useRange) =
    useRange
