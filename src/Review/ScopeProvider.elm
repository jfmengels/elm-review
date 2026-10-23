module Review.ScopeProvider exposing
    ( ScopeProvider, topLevel
    , children
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict
import Review.Resources.ScopeInternal as Internal
import Review.Scope exposing (Scope)


type alias ScopeProvider =
    Internal.ScopeProvider


topLevel : ScopeProvider -> Scope
topLevel (Internal.ScopeProvider scopeProvider) =
    Internal.Scope scopeProvider.topLevel


children : ScopeProvider -> Scope -> List Scope
children (Internal.ScopeProvider scopeProvider) (Internal.Scope scope) =
    List.filterMap
        (\id -> Dict.get id scopeProvider.scopes |> Maybe.map Internal.Scope)
        scope.children
