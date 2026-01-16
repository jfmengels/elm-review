module Review.ModuleNameLookupTable.ComputeContext exposing
    ( Context
    , Scope
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import NonEmpty exposing (NonEmpty)
import Review.ModuleNameLookupTable.Builder exposing (ModuleNameLookupTableBuilder)
import Set exposing (Set)


type alias Context =
    { scopes : NonEmpty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Set String
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , lookupTable : ModuleNameLookupTableBuilder
    , branches : NonEmpty ( Range, Scope )
    , caseToExit : NonEmpty Range
    }


type alias Scope =
    Set String
