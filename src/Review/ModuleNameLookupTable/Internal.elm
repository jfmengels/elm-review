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
        (\( range, moduleName ) dict ->
            Dict.insert (toRangeLike range) moduleName dict
        )
        Dict.empty
        list
        |> ModuleNameLookupTable fileModuleName


add : Range -> ModuleName -> ModuleNameLookupTable -> ModuleNameLookupTable
add range moduleName (ModuleNameLookupTable currentModuleName moduleNameLookupTable) =
    ModuleNameLookupTable currentModuleName (Dict.insert (toRangeLike range) moduleName moduleNameLookupTable)


type alias RangeLike =
    ( Int, Int )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    -- TODO Optimization with elm-syntax v8
    -- They We could only look the at the start because it is not possible for 2 nodes to have the same position.
    --
    --   type alias RangeLike =
    --       Int
    --
    --   toRangeLike { start } =
    --       Bitwise.shiftLeftBy 16 start.row + start.colum
    --
    -- Unfortunately with v7 this is not possible, because we do not have the position of the
    -- operator with `Expression.OperatorApplication`, creating a collision when looking for the
    -- module name for `+` in `a + b`, as that conflicts with the position for `a`.
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )
