module Review.Type.Binop exposing
    ( Binop
    , associativity
    , comment
    , fromMetadata
    , name
    , precedence
    , tipe
    , toMetadata
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


fromMetadata : Elm.Docs.Binop -> Binop
fromMetadata binop =
    Binop
        { name = binop.name
        , documentation = binop.comment
        , tipe = Type.fromMetadataType binop.tipe
        , associativity = binop.associativity
        , precedence = binop.precedence
        }


toMetadata : Binop -> Elm.Docs.Binop
toMetadata (Binop binop) =
    { name = binop.name
    , comment = binop.documentation
    , tipe = Type.toMetadataType binop.tipe |> Maybe.withDefault (Elm.Type.Var "unknown")
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
