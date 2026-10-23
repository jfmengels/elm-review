module Review.Scope exposing
    ( Scope
    , bindings
    , children
    , names
    , parentId
    , range
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
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


range : Internal.Scope -> Range
range (Internal.Scope scope) =
    scope.range


parentId : Internal.Scope -> Maybe Internal.ScopeId
parentId (Internal.Scope scope) =
    scope.parentId


children : Internal.Scope -> List Scope
children (Internal.Scope scope) =
    scope.children
