module Review.Internal.Binop exposing
    ( Binop
    , associatedFunction
    , associativity
    , create
    , documentation
    , fromElmDocs
    , name
    , precedence
    , setDocumentationAndType
    , tipe
    )

import Elm.Docs
import Review.Api.Type as Type


type Binop
    = Binop
        { name : String
        , associatedFunction : Maybe String
        , documentation : Maybe String
        , tipe : Maybe Type.Type
        , associativity : Elm.Docs.Associativity
        , precedence : Int
        }


create :
    { name : String
    , associatedFunction : Maybe String
    , documentation : Maybe String
    , tipe : Maybe Type.Type
    , associativity : Elm.Docs.Associativity
    , precedence : Int
    }
    -> Binop
create =
    Binop


setDocumentationAndType : { documentation : Maybe String, tipe : Maybe Type.Type } -> Binop -> Binop
setDocumentationAndType params (Binop binop) =
    Binop { binop | documentation = params.documentation, tipe = params.tipe }


fromElmDocs : Elm.Docs.Binop -> Binop
fromElmDocs binop =
    Binop
        { name = binop.name
        , associatedFunction = Nothing
        , documentation = Just binop.comment
        , tipe = Just (Type.fromElmDocs binop.tipe)
        , associativity = binop.associativity
        , precedence = binop.precedence
        }


name : Binop -> String
name (Binop binop) =
    binop.name


associatedFunction : Binop -> Maybe String
associatedFunction (Binop binop) =
    binop.associatedFunction


documentation : Binop -> Maybe String
documentation (Binop binop) =
    binop.documentation


tipe : Binop -> Maybe Type.Type
tipe (Binop binop) =
    binop.tipe


associativity : Binop -> Elm.Docs.Associativity
associativity (Binop binop) =
    binop.associativity


precedence : Binop -> Int
precedence (Binop binop) =
    binop.precedence
