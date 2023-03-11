module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), add, empty, fromList, toRangeLike)

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable ModuleName (Dict RangeLike ModuleName)


type alias RangeLike =
    Int


empty : ModuleName -> ModuleNameLookupTable
empty currentModuleName =
    ModuleNameLookupTable currentModuleName Dict.empty


fromList : ModuleName -> List ( Range, ModuleName ) -> ModuleNameLookupTable
fromList fileModuleName list =
    List.foldl
        (\( range, moduleName ) acc -> add range moduleName acc)
        (empty fileModuleName)
        list


add : Range -> ModuleName -> ModuleNameLookupTable -> ModuleNameLookupTable
add range moduleName (ModuleNameLookupTable currentModuleName moduleNameLookupTable) =
    ModuleNameLookupTable currentModuleName (Dict.insert (toRangeLike range) moduleName moduleNameLookupTable)


toRangeLike : Range -> RangeLike
toRangeLike { start } =
    -- We are able to only take a look the start because the lookup table because it is not possible for 2 nodes
    -- that can have a module name to have the same start position. This does have the tradeoff that when a lookup
    -- is done on a node that can't have a module name (like `A.a + B.b`) that a module name will be returned, but
    -- that would be a misuse of the API.
    Bitwise.shiftLeftBy 16 start.row + start.column
