module Review.Internal.Alias exposing
    ( Alias
    , args
    , create
    , documentation
    , fromElmDocs
    , name
    , relateToModule
    , tipe
    , toElmDocs
    )

import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Type
import Review.Type as Type exposing (Type)


type Alias
    = Alias
        { name : String
        , documentation : Maybe String
        , args : List String
        , tipe : Type
        }


create :
    { name : String
    , documentation : Maybe String
    , args : List String
    , tipe : Type
    }
    -> Alias
create =
    Alias


fromElmDocs : Elm.Docs.Alias -> Alias
fromElmDocs alias =
    Alias
        { name = alias.name
        , documentation = Just alias.comment
        , args = alias.args
        , tipe = Type.fromElmDocs alias.tipe
        }


toElmDocs : Alias -> Elm.Docs.Alias
toElmDocs (Alias alias) =
    { name = alias.name
    , comment = Maybe.withDefault "" alias.documentation
    , args = alias.args
    , tipe =
        Type.toElmDocs alias.tipe
            |> Maybe.withDefault (Elm.Type.Var "unknown")
    }


relateToModule : ModuleName -> Alias -> Alias
relateToModule moduleName (Alias alias) =
    Alias { alias | tipe = Type.relateToModule moduleName alias.tipe }


name : Alias -> String
name (Alias a) =
    a.name


documentation : Alias -> Maybe String
documentation (Alias a) =
    a.documentation


args : Alias -> List String
args (Alias a) =
    a.args


tipe : Alias -> Type
tipe (Alias a) =
    a.tipe
