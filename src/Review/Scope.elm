module Review.Scope exposing
    ( Scope
    , bindings
    , canIntroduceName
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


range : Scope -> Range
range (Internal.Scope scope) =
    scope.range


parentId : Scope -> Maybe Internal.ScopeId
parentId (Internal.Scope scope) =
    scope.parentId


children : Scope -> List Scope
children (Internal.Scope scope) =
    scope.children


canIntroduceName : Scope -> String -> Bool
canIntroduceName (Internal.Scope scope) name =
    List.any (Dict.member name) (scope.bindings :: scope.parentBindings)
        |> not
