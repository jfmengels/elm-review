module Review.ScopeProvider exposing
    ( ScopeProvider, topLevel
    , children, scopeAt
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Resources.ScopeInternal as Internal exposing (ScopeInfo)
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


scopeAt : ScopeProvider -> Range -> Scope
scopeAt (Internal.ScopeProvider scopeProvider) range =
    scopeAtHelp
        scopeProvider.scopes
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


scopeAtHelp : Dict Int ScopeInfo -> Range -> ScopeInfo -> List Int -> Scope
scopeAtHelp scopeDict range bestMatch childrenIds =
    case childrenIds of
        [] ->
            Internal.Scope bestMatch

        id :: rest ->
            case Dict.get id scopeDict of
                Just scope ->
                    if isRangeIncludedIn range scope.range then
                        scopeAtHelp scopeDict range scope scope.children

                    else
                        scopeAtHelp scopeDict range bestMatch rest

                Nothing ->
                    scopeAtHelp scopeDict range bestMatch rest


isRangeIncludedIn : Range -> Range -> Bool
isRangeIncludedIn inner outer =
    case compareLocation inner.start outer.start of
        LT ->
            False

        _ ->
            case compareLocation inner.end outer.end of
                GT ->
                    False

                _ ->
                    True


compareLocation : Location -> Location -> Order
compareLocation a b =
    case Basics.compare a.row b.row of
        EQ ->
            Basics.compare a.column b.column

        ltOrGt ->
            ltOrGt
