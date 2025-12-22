module NoInconsistentAliases.MissingAlias exposing (MissingAlias, hasUses, mapExpectedName, mapModuleName, mapUses, new, range, withModuleUse)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type MissingAlias
    = MissingAlias
        { moduleName : ModuleName
        , expectedName : String
        , at : Range
        , uses : List ModuleUse
        }


new : ModuleName -> String -> Range -> MissingAlias
new newModuleName newExpectedName newRange =
    MissingAlias
        { moduleName = newModuleName
        , expectedName = newExpectedName
        , at = newRange
        , uses = []
        }


withModuleUse : ModuleUse -> MissingAlias -> MissingAlias
withModuleUse moduleUse (MissingAlias alias) =
    MissingAlias { alias | uses = moduleUse :: alias.uses }


hasUses : MissingAlias -> Bool
hasUses (MissingAlias { uses }) =
    not (List.isEmpty uses)


mapModuleName : (ModuleName -> a) -> MissingAlias -> a
mapModuleName mapper (MissingAlias { moduleName }) =
    mapper moduleName


mapExpectedName : (String -> a) -> MissingAlias -> a
mapExpectedName mapper (MissingAlias { expectedName }) =
    mapper expectedName


mapUses : (ModuleUse -> a) -> MissingAlias -> List a
mapUses mapper (MissingAlias { uses }) =
    List.map mapper uses


range : MissingAlias -> Range
range (MissingAlias { at }) =
    at
