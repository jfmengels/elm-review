module Review.Resources.ScopeInternal exposing
    ( Bindings
    , Scope(..)
    , ScopeInfo
    , ScopeProvider(..)
    , empty
    , fromScopes
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range as Range exposing (Range)
import Set exposing (Set)


type ScopeProvider
    = ScopeProvider
        { topLevel : ScopeInfo
        , scopes : Dict Int ScopeInfo
        }


type Scope
    = Scope ScopeInfo


type alias ScopeInfo =
    { id : Int
    , parentId : Maybe Int
    , range : Range
    , bindings : Bindings
    , children : List Int
    }


type alias Bindings =
    Set String


empty : ModuleName -> ScopeProvider
empty moduleName =
    ScopeProvider
        { topLevel =
            { id = 0
            , parentId = Nothing
            , range = Range.empty
            , bindings = Set.empty
            , children = []
            }
        , scopes = Dict.empty
        }


fromScopes : ModuleName -> ScopeInfo -> Dict Int ScopeInfo -> ScopeProvider
fromScopes moduleName topLevel scopes =
    ScopeProvider
        { topLevel = topLevel
        , scopes = scopes
        }
