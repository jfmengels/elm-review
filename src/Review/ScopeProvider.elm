module Review.ScopeProvider exposing
    ( ScopeProvider, topLevel
    , at, for, parent
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict exposing (Dict)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Review.Resources.ScopeInternal as Internal exposing (ScopeInfo)
import Review.Scope exposing (Scope)


type alias ScopeProvider =
    Internal.ScopeProvider


topLevel : ScopeProvider -> Scope
topLevel (Internal.ScopeProvider scopeProvider) =
    Internal.Scope scopeProvider.topLevel


at : ScopeProvider -> Range -> Scope
at (Internal.ScopeProvider scopeProvider) range =
    atHelp
        scopeProvider.scopes
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


atHelp : Dict Int ScopeInfo -> Range -> ScopeInfo -> List Scope -> Scope
atHelp scopeDict range bestMatch childrenIds =
    case childrenIds of
        [] ->
            Internal.Scope bestMatch

        (Internal.Scope scope) :: rest ->
            if isRangeIncludedIn range scope.range then
                atHelp scopeDict range scope scope.children

            else
                atHelp scopeDict range bestMatch rest


for : ScopeProvider -> Node a -> Scope
for (Internal.ScopeProvider scopeProvider) (Node range _) =
    atHelp
        scopeProvider.scopes
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


parent : ScopeProvider -> Scope -> Scope
parent (Internal.ScopeProvider scopeProvider) ((Internal.Scope scope) as untouched) =
    case
        Maybe.andThen
            (\parentId -> Dict.get parentId scopeProvider.scopes)
            scope.parentId
    of
        Just parentScope ->
            Internal.Scope parentScope

        Nothing ->
            untouched


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
