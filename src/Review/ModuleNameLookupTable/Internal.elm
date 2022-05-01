module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), add, addMultiple, empty, toRangeLike)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable ModuleName (Dict RangeLike ModuleName)


empty : ModuleName -> ModuleNameLookupTable
empty currentModuleName =
    ModuleNameLookupTable currentModuleName Dict.empty


add : Range -> ModuleName -> ModuleNameLookupTable -> ModuleNameLookupTable
add range moduleName (ModuleNameLookupTable currentModuleName moduleNameLookupTable) =
    ModuleNameLookupTable currentModuleName (Dict.insert (toRangeLike range) moduleName moduleNameLookupTable)


addMultiple : List ( Range, ModuleName ) -> ModuleNameLookupTable -> ModuleNameLookupTable
addMultiple elements moduleNameLookupTable =
    List.foldl
        (\( range, moduleName ) lookupTable -> add range moduleName lookupTable)
        moduleNameLookupTable
        elements


type alias RangeLike =
    ( ( Int, Int ), ( Int, Int ) )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    ( ( start.row, start.column )
    , ( end.row, end.column )
    )
