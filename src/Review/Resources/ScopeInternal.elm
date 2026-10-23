module Review.Resources.ScopeInternal exposing
    ( Binding
    , Bindings
    , Reference
    , Scope(..)
    , ScopeId
    , ScopeInfo
    , ScopeProvider(..)
    , empty
    , fromScopes
    )

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)


type ScopeProvider
    = ScopeProvider
        { topLevel : ScopeInfo
        , scopes : Dict Int ScopeInfo
        }


type Scope
    = Scope ScopeInfo


type alias ScopeId =
    Int


type alias ReferenceId =
    Int


type alias ScopeInfo =
    { id : ScopeId
    , parentId : Maybe ScopeId
    , range : Range
    , bindings : Bindings
    , children : List ScopeId
    }


type alias Bindings =
    Dict String Binding


type alias Binding =
    { name : String
    , range : Range
    , references : List Range
    }


type alias Reference =
    Range


empty : ModuleName -> ScopeProvider
empty moduleName =
    ScopeProvider
        { topLevel =
            { id = 0
            , parentId = Nothing
            , range = Range.empty
            , bindings = Dict.empty
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
