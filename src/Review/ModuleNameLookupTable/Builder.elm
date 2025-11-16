module Review.ModuleNameLookupTable.Builder exposing
    ( ModuleNameLookupTableBuilder(..)
    , add
    , empty
    , finalize
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Internal as Internal


type ModuleNameLookupTableBuilder
    = ModuleNameLookupTableBuilder (List ( Range, ModuleName ))


empty : ModuleNameLookupTableBuilder
empty =
    ModuleNameLookupTableBuilder []


add : Range -> ModuleName -> ModuleNameLookupTableBuilder -> ModuleNameLookupTableBuilder
add range moduleName (ModuleNameLookupTableBuilder builder) =
    ModuleNameLookupTableBuilder (( range, moduleName ) :: builder)


finalize : ModuleName -> ModuleNameLookupTableBuilder -> ModuleNameLookupTable
finalize fileModuleName (ModuleNameLookupTableBuilder builder) =
    Internal.fromList fileModuleName builder
