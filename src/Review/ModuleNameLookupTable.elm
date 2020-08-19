module Review.ModuleNameLookupTable exposing (..)

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable.Internal as Internal


type alias ModuleNameLookupTable =
    Internal.ModuleNameLookupTable


moduleName : ModuleNameLookupTable -> Range -> ModuleName
moduleName (Internal.ModuleNameLookupTable dict) range =
    Dict.get (Internal.toRangeLike range) dict
        |> Maybe.withDefault [ "UNKNOWN" ]
