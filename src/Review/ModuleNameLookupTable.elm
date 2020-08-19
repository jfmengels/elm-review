module Review.ModuleNameLookupTable exposing
    ( ModuleNameLookupTable
    , moduleNameFor
    )

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Review.ModuleNameLookupTable.Internal as Internal


type alias ModuleNameLookupTable =
    Internal.ModuleNameLookupTable


moduleNameFor : ModuleNameLookupTable -> Node a -> Maybe ModuleName
moduleNameFor (Internal.ModuleNameLookupTable dict) (Node range _) =
    Dict.get (Internal.toRangeLike range) dict
