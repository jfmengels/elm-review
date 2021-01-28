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


{-| Representation of a binary operator, such as `+`, `-` and `|>`.

Definitions of these can only by found in projects under the `elm` organization.

-}
type alias Binop =
    Review.Internal.Binop.Binop


{-| Create a new type binary operation.
-}
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


{-| Get the name of a binary operation.


    infix left  6 (+) = add

    --> "+"

-}
name : Binop -> String
name =
    Review.Internal.Binop.name


{-| Get the associatedFunction of a binary operation.


    infix left  6 (+) = add

    --> "add"

-}
associatedFunction : Binop -> Maybe String
associatedFunction =
    Review.Internal.Binop.associatedFunction


{-| Get the documentation of a binary operation.

    {-| Add things.
    -}

    add : number -> number -> number
    add =
        Elm.Kernel.Basics.add

    infix left  6 (+) = add

    --> " Add things.\n"

The leading `{-|` and trailing `-}` are stripped off, but the rest of the string remains as is. In the example above, the documentation would be `documentation\n`.

The documentation will be `Nothing` if it is was missing.

-}
documentation : Binop -> Maybe String
documentation =
    Review.Internal.Binop.documentation


{-| Get the type of a binary operator.

    import Review.Type as Type

    add : number -> number -> number
    add =
        Elm.Kernel.Basics.add

    infix left  6 (+) = add

    --> Type.Function
    -->     (Type.Generic number)
    -->     (Type.Function (Type.Generic number) (Type.Generic number))

The odd name choice comes from `type` being a reserved word, and was inspired by the [`tipe` field in `Elm.Docs.Alias`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Alias).

-}
tipe : Binop -> Maybe Type.Type
tipe =
    Review.Internal.Binop.tipe


{-| Get the associativity of a binary operator.


    infix left  6 (+) = add

    --> Elm.Docs.Left

Read more about it at [`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#associativity).

-}
associativity : Binop -> Elm.Docs.Associativity
associativity =
    Review.Internal.Binop.associativity


{-| Get the precedence of a binary operator.


    infix left  6 (+) = add

    --> 6

Read more about it at [`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#precedence).

-}
precedence : Binop -> Int
precedence =
    Review.Internal.Binop.precedence
