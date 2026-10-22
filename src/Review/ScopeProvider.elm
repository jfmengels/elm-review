module Review.ScopeProvider exposing (ScopeProvider, topLevel)

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Review.Resources.ScopeInternal as Internal
import Review.Scope exposing (Scope)


type alias ScopeProvider =
    Internal.ScopeProvider


topLevel : ScopeProvider -> Scope
topLevel (Internal.ScopeProvider scopeProvider) =
    Internal.Scope scopeProvider.topLevel
