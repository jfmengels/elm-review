module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), add, empty, toRangeLike)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable (Dict RangeLike ModuleName)


empty : ModuleNameLookupTable
empty =
    ModuleNameLookupTable Dict.empty


add : Range -> ModuleName -> ModuleNameLookupTable -> ModuleNameLookupTable
add range moduleName (ModuleNameLookupTable moduleNameLookupTable) =
    ModuleNameLookupTable (Dict.insert (toRangeLike range) moduleName moduleNameLookupTable)


type alias RangeLike =
    ( ( Int, Int ), ( Int, Int ) )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    ( ( start.row, start.column )
    , ( end.row, end.column )
    )
