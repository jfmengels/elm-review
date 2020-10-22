module TypeInference.Binop exposing
    ( Associativity(..)
    , Binop
    , associativity
    , comment
    , fromMetadata
    , name
    , precedence
    , tipe
    )

import Elm.Docs
import TypeInference.Type as Type


type Binop
    = Binop
        { name : String
        , documentation : String
        , tipe : Type.Type
        , associativity : Associativity
        , precedence : Int
        }


fromMetadata : Elm.Docs.Binop -> Binop
fromMetadata binop =
    Binop
        { name = binop.name
        , documentation = binop.comment
        , tipe = Type.fromMetadataType binop.tipe
        , associativity = fromAssociativity binop.associativity
        , precedence = binop.precedence
        }


type Associativity
    = Left
    | None
    | Right


fromAssociativity : Elm.Docs.Associativity -> Associativity
fromAssociativity assoc =
    case assoc of
        Elm.Docs.Left ->
            Left

        Elm.Docs.None ->
            None

        Elm.Docs.Right ->
            Right


name : Binop -> String
name (Binop binop) =
    binop.name


comment : Binop -> String
comment (Binop binop) =
    binop.documentation


tipe : Binop -> Type.Type
tipe (Binop binop) =
    binop.tipe


associativity : Binop -> Associativity
associativity (Binop binop) =
    binop.associativity


precedence : Binop -> Int
precedence (Binop binop) =
    binop.precedence
