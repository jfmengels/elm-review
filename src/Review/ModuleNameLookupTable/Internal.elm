module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), empty, toRangeLike)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable (Dict RangeLike (List String))


empty : ModuleNameLookupTable
empty =
    ModuleNameLookupTable Dict.empty


type alias RangeLike =
    ( ( Int, Int ), ( Int, Int ) )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    ( ( start.row, start.column )
    , ( end.row, end.column )
    )
