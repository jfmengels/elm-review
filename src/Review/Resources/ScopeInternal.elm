module Review.Resources.ScopeInternal exposing
    ( Scope(..)
    , ScopeInfo
    , ScopeNames
    , ScopeProvider(..)
    , fromScopes
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Set exposing (Set)


type ScopeProvider
    = ScopeProvider
        { topLevel : ScopeInfo
        , scopes : List ScopeInfo
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


fromScopes : ModuleName -> ScopeInfo -> List ScopeInfo -> ScopeProvider
fromScopes fileModuleName topLevel scopes =
    let
        sortedScopes : List ScopeInfo
        sortedScopes =
            List.sortBy .id (topLevel :: scopes)
    in
    ScopeProvider
        { topLevel = topLevel
        , scopes = sortedScopes
        }
