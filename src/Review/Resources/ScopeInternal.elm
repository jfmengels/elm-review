module Review.Resources.ScopeInternal exposing
    ( Kind(..)
    , Reference(..)
    , Scope(..)
    , ScopeId
    , ScopeInfo
    , ScopeProvider(..)
    , SharedScopeInfo
    , create
    , empty
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import NonEmpty exposing (NonEmpty)
import Review.Resources.Internal.Bindings exposing (Binding, ImportBinding)
import Review.Resources.ScopeOrigin as ScopeOrigin exposing (ScopeOrigin)
import Review.Resources.Target exposing (Target)


type ScopeProvider
    = ScopeProvider
        { topLevel : ScopeInfo
        , scopes : Dict Int ScopeInfo
        , valueReferences : Dict Int (List Reference)
        , typeReferences : Dict Int (List Reference)
        , qualifiedReferences : Dict ModuleName (List Reference)
        , moduleReferences : Dict ModuleName (List Reference)
        }


type Scope
    = Scope ScopeInfo


type alias ScopeId =
    Int


type alias ScopeInfo =
    { id : ScopeId
    , moduleName : ModuleName
    , parentId : Maybe ScopeId
    , range : Range
    , origin : ScopeOrigin
    , valueBindings : Dict String Binding
    , parentBindings : List (Dict String Binding)
    , children : List Scope
    , shared : SharedScopeInfo
    }


type alias SharedScopeInfo =
    { typeBindings : Dict String Binding
    , importValueBindings : Dict String ImportBinding
    , importTypeBindings : Dict String ImportBinding
    , aliasToModuleName : Dict String (NonEmpty ModuleName)
    , moduleQualifiers : Dict ModuleName ModuleName
    , modules : Dict ModuleName Elm.Docs.Module
    }


type Reference
    = Reference
        { target : Target
        , qualifier : ModuleName
        , range : Range
        , kind : Kind
        }


type Kind
    = Expression (Node Expression)
    | InfixReference (Node Infix)
    | Pattern (Node Pattern)
    | Type (Node TypeAnnotation)


empty : ModuleName -> ScopeProvider
empty moduleName =
    let
        shared : SharedScopeInfo
        shared =
            { typeBindings = Dict.empty
            , importValueBindings = Dict.empty
            , importTypeBindings = Dict.empty
            , aliasToModuleName = Dict.empty
            , moduleQualifiers = Dict.empty
            , modules = Dict.empty
            }

        topLevel : ScopeInfo
        topLevel =
            { id = 0
            , moduleName = moduleName
            , parentId = Nothing
            , range = Range.empty
            , origin = ScopeOrigin.TopLevel
            , valueBindings = Dict.empty
            , parentBindings = []
            , children = []
            , shared = shared
            }
    in
    ScopeProvider
        { topLevel = topLevel
        , scopes = Dict.empty
        , valueReferences = Dict.empty
        , typeReferences = Dict.empty
        , qualifiedReferences = Dict.empty
        , moduleReferences = Dict.empty
        }


create :
    ScopeInfo
    -> Dict Int ScopeInfo
    -> Dict Int (List Reference)
    -> Dict Int (List Reference)
    -> Dict ModuleName (List Reference)
    -> Dict ModuleName (List Reference)
    -> ScopeProvider
create topLevel scopes valueReferences_ typeReferences_ qualifiedReferences moduleReferences =
    ScopeProvider
        { topLevel = topLevel
        , scopes = scopes
        , valueReferences = valueReferences_
        , typeReferences = typeReferences_
        , qualifiedReferences = qualifiedReferences
        , moduleReferences = moduleReferences
        }
