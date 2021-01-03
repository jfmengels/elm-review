module Review.Type.Binop exposing
    ( Binop
    , associativity
    , comment
    , fromElmDocs
    , name
    , precedence
    , tipe
    , toElmDocs
    )

import Elm.Docs
import Elm.Type
import Review.Type as Type


type Binop
    = Binop
        { name : String
        , documentation : String
        , tipe : Type.Type
        , associativity : Elm.Docs.Associativity
        , precedence : Int
        }


fromElmDocs : Elm.Docs.Binop -> Binop
fromElmDocs binop =
    Binop
        { name = binop.name
        , documentation = binop.comment
        , tipe = Type.fromElmDocs binop.tipe
        , associativity = binop.associativity
        , precedence = binop.precedence
        }


toElmDocs : Binop -> Elm.Docs.Binop
toElmDocs (Binop binop) =
    { name = binop.name
    , comment = binop.documentation
    , tipe = Type.toElmDocs binop.tipe |> Maybe.withDefault (Elm.Type.Var "unknown")
    , associativity = binop.associativity
    , precedence = binop.precedence
    }


name : Binop -> String
name (Binop binop) =
    binop.name


comment : Binop -> String
comment (Binop binop) =
    binop.documentation


tipe : Binop -> Type.Type
tipe (Binop binop) =
    binop.tipe


associativity : Binop -> Elm.Docs.Associativity
associativity (Binop binop) =
    binop.associativity


precedence : Binop -> Int
precedence (Binop binop) =
    binop.precedence
