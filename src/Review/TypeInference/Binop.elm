module Review.TypeInference.Binop exposing
    ( Binop
    , associativity
    , comment
    , fromMetadata
    , name
    , precedence
    , tipe
    )

import Elm.Docs
import Review.TypeInference.Type as Type


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
