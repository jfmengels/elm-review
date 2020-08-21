module Review.ModuleNameLookupTable exposing
    ( ModuleNameLookupTable
    , moduleNameAt
    , moduleNameFor
    )

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable.Internal as Internal


type alias ModuleNameLookupTable =
    Internal.ModuleNameLookupTable


moduleNameFor : ModuleNameLookupTable -> Node a -> Maybe ModuleName
moduleNameFor (Internal.ModuleNameLookupTable dict) (Node range _) =
    Dict.get (Internal.toRangeLike range) dict


moduleNameAt : ModuleNameLookupTable -> Range -> Maybe ModuleName
moduleNameAt (Internal.ModuleNameLookupTable dict) range =
    Dict.get (Internal.toRangeLike range) dict
