module Review.Resources.ScopeInternal exposing
    ( Binding(..)
    , ImportBinding(..)
    , Kind(..)
    , Realm
    , Reference(..)
    , Scope(..)
    , ScopeId
    , ScopeInfo
    , ScopeProvider(..)
    , Target(..)
    , create
    , empty
    , typeRealm
    , valueRealm
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
import Review.Resources.BindingId as BindingId
import Review.Resources.ScopeOrigin as ScopeOrigin exposing (ScopeOrigin)


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
    , typeBindings : Dict String Binding
    , parentBindings : List (Dict String Binding)
    , importValueBindings : Dict String ImportBinding
    , importTypeBindings : Dict String ImportBinding
    , importAliases : Dict String (NonEmpty ModuleName)
    , modules : Dict ModuleName Elm.Docs.Module
    , children : List Scope
    }


type Binding
    = Binding
        { id : BindingId.Id
        , name : String
        , range : Range
        , realm : Realm
        }


type alias Realm =
    -- valueRealm for values
    -- | typeRealm for types
    -- can be both
    Int


valueRealm : Realm
valueRealm =
    1


typeRealm : Realm
typeRealm =
    2


type ImportBinding
    = ImportBinding
        { id : BindingId.Id
        , name : String
        , moduleName : ModuleName
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


type Target
    = LocalBinding Binding
    | ImportBinding_ ImportBinding
    | QualifiedReference { moduleName : ModuleName, name : String }


empty : ModuleName -> ScopeProvider
empty moduleName =
    let
        topLevel : ScopeInfo
        topLevel =
            { id = 0
            , moduleName = moduleName
            , parentId = Nothing
            , range = Range.empty
            , origin = ScopeOrigin.TopLevel
            , valueBindings = Dict.empty
            , typeBindings = Dict.empty
            , importValueBindings = Dict.empty
            , importTypeBindings = Dict.empty
            , parentBindings = []
            , importAliases = Dict.empty
            , modules = Dict.empty
            , children = []
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
