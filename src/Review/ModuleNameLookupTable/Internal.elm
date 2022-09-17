module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), add, empty, fromList, toRangeLike)

import Bitwise
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable ModuleName (Dict RangeLike ModuleName)


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


type alias RangeLike =
    ( Int, Int )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )
