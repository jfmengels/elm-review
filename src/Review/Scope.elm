module Review.Scope exposing
    ( Scope
    , bindings
    , canIntroduceName
    , children
    , getBinding
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


getBinding : Scope -> String -> Maybe Binding
getBinding (Internal.Scope scope) name =
    getBindingHelp (scope.bindings :: scope.parentBindings) name


getBindingHelp : List (Dict String Binding) -> String -> Maybe Binding
getBindingHelp bindingsStack name =
    case bindingsStack of
        [] ->
            Nothing

        bindings_ :: rest ->
            case Dict.get name bindings_ of
                Nothing ->
                    getBindingHelp rest name

                just ->
                    just


canIntroduceName : Scope -> String -> Bool
canIntroduceName (Internal.Scope scope) name =
    List.any (Dict.member name) (scope.bindings :: scope.parentBindings)
        |> not
