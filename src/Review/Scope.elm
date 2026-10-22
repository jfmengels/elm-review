module Review.Scope exposing
    ( Scope
    , names
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict
import Review.Resources.ScopeInternal as Internal
import Set exposing (Set)


type alias Scope =
    Internal.Scope


names : Scope -> Set String
names (Internal.Scope scope) =
    Dict.foldl (\name _ set -> Set.insert name set) Set.empty scope.bindings
