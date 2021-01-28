module Review.Type.Binop exposing
    ( Binop
    , name, tipe, documentation, associativity, precedence, associatedFunction
    , create
    )

{-| Represents binary operations defined in modules.

@docs Binop


# Access

@docs name, tipe, documentation, associativity, precedence, associatedFunction


# Creation

@docs create

-}

import Elm.Docs
import Review.Internal.Binop
import Review.Type as Type



-- TODO Expose module, but hide implementation and type inside an "Internal" module


type alias Binop =
    Review.Internal.Binop.Binop


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
    Review.Internal.Binop.create


fromElmDocs : Elm.Docs.Binop -> Binop
fromElmDocs =
    Review.Internal.Binop.fromElmDocs


toElmDocs : Binop -> Maybe Elm.Docs.Binop
toElmDocs =
    Review.Internal.Binop.toElmDocs


name : Binop -> String
name =
    Review.Internal.Binop.name


associatedFunction : Binop -> Maybe String
associatedFunction =
    Review.Internal.Binop.associatedFunction


documentation : Binop -> Maybe String
documentation =
    Review.Internal.Binop.documentation


tipe : Binop -> Maybe Type.Type
tipe =
    Review.Internal.Binop.tipe


associativity : Binop -> Elm.Docs.Associativity
associativity =
    Review.Internal.Binop.associativity


precedence : Binop -> Int
precedence =
    Review.Internal.Binop.precedence
