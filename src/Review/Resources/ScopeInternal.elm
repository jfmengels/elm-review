module Review.Resources.ScopeInternal exposing
    ( Binding
    , BindingId(..)
    , BindingIdCounter
    , Scope(..)
    , ScopeId
    , ScopeInfo
    , ScopeProvider(..)
    , bindingId
    , empty
    , fromScopes
    , initialBindingIdCounter
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
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


type BindingId
    = BindingId Int


type BindingIdCounter
    = BindingIdCounter Int


initialBindingIdCounter : BindingIdCounter
initialBindingIdCounter =
    BindingIdCounter 0


bindingId : BindingIdCounter -> ( BindingId, BindingIdCounter )
bindingId (BindingIdCounter n) =
    ( BindingId n, BindingIdCounter (n + 1) )


type alias ScopeInfo =
    { id : ScopeId
    , parentId : Maybe ScopeId
    , range : Range
    , bindings : Dict String Binding
    , children : List Scope
    }


type alias Binding =
    { id : BindingId
    , name : String
    , range : Range
    , references : List Range
    }


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
