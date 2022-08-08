module Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable(..), add, addMultiple, empty, fromList, toRangeLike)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)


type ModuleNameLookupTable
    = ModuleNameLookupTable ModuleName (Dict RangeLike ( ModuleName, String ))


empty : ModuleName -> ModuleNameLookupTable
empty currentModuleName =
    ModuleNameLookupTable currentModuleName Dict.empty


fromList : ModuleName -> List ( Range, ( ModuleName, String ) ) -> ModuleNameLookupTable
fromList fileModuleName list =
    List.foldl
        (\( range, ( moduleName, name ) ) acc -> add range moduleName name acc)
        (empty fileModuleName)
        list


add : Range -> ModuleName -> String -> ModuleNameLookupTable -> ModuleNameLookupTable
add range moduleName name (ModuleNameLookupTable currentModuleName moduleNameLookupTable) =
    ModuleNameLookupTable currentModuleName (Dict.insert (toRangeLike range) ( moduleName, name ) moduleNameLookupTable)


addMultiple : List ( Range, ModuleName, String ) -> ModuleNameLookupTable -> ModuleNameLookupTable
addMultiple elements moduleNameLookupTable =
    List.foldl
        (\( range, moduleName, name ) lookupTable -> add range moduleName name lookupTable)
        moduleNameLookupTable
        elements


type alias RangeLike =
    ( ( Int, Int ), ( Int, Int ) )


toRangeLike : Range -> RangeLike
toRangeLike { start, end } =
    ( ( start.row, start.column )
    , ( end.row, end.column )
    )
