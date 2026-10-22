module Review.Scope exposing
    ( Scope
    , names
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Review.Resources.ScopeInternal as Internal
import Set exposing (Set)


type alias Scope =
    Internal.Scope


names : Scope -> Set String
names (Internal.Scope scope) =
    scope.bindings
