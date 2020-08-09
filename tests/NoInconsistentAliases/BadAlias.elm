module NoInconsistentAliases.BadAlias exposing (BadAlias, mapExpectedName, mapModuleName, mapName, mapUses, new, range, withModuleUse)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type BadAlias
    = BadAlias
        { name : String
        , moduleName : ModuleName
        , expectedName : String
        , at : Range
        , uses : List ModuleUse
        }


new : { name : String, moduleName : ModuleName, expectedName : String, range : Range } -> BadAlias
new options =
    BadAlias
        { name = options.name
        , moduleName = options.moduleName
        , expectedName = options.expectedName
        , at = options.range
        , uses = []
        }


withModuleUse : ModuleUse -> BadAlias -> BadAlias
withModuleUse moduleUse (BadAlias alias) =
    BadAlias { alias | uses = moduleUse :: alias.uses }


range : BadAlias -> Range
range (BadAlias alias) =
    alias.at


mapName : (String -> a) -> BadAlias -> a
mapName mapper (BadAlias alias) =
    mapper alias.name


mapModuleName : (ModuleName -> a) -> BadAlias -> a
mapModuleName mapper (BadAlias alias) =
    mapper alias.moduleName


mapExpectedName : (String -> a) -> BadAlias -> a
mapExpectedName mapper (BadAlias alias) =
    mapper alias.expectedName


mapUses : (ModuleUse -> a) -> BadAlias -> List a
mapUses mapper (BadAlias { uses }) =
    List.map mapper uses
