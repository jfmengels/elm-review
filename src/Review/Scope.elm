module Review.Scope exposing
    ( Scope
    , bindings
    , names
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict exposing (Dict)
import Review.Resources.ScopeInternal as Internal
import Set exposing (Set)


type alias Scope =
    Internal.Scope


type alias Binding =
    Internal.Binding


names : Scope -> Set String
names (Internal.Scope scope) =
    Dict.foldl (\name _ set -> Set.insert name set) Set.empty scope.bindings


bindings : Scope -> Dict String Binding
bindings (Internal.Scope scope) =
    scope.bindings
