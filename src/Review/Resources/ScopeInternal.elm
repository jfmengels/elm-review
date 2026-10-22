module Review.Resources.ScopeInternal exposing
    ( Scope(..)
    , ScopeInfo
    , ScopeNames
    , ScopeProvider(..)
    , fromScopes
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Set exposing (Set)


type ScopeProvider
    = ScopeProvider
        { topLevel : ScopeInfo
        , scopes : Dict Int ScopeInfo
        }


type Scope
    = Scope ScopeInfo


type alias ScopeInfo =
    { id : Int
    , range : Range
    , names : ScopeNames
    }


type alias ScopeNames =
    Set String


fromScopes : ModuleName -> ScopeInfo -> Dict Int ScopeInfo -> ScopeProvider
fromScopes fileModuleName topLevel scopes =
    ScopeProvider
        { topLevel = topLevel
        , scopes = scopes
        }
