module Review.Type.Union exposing
    ( Union
    , name, constructors, args, documentation
    , create
    )

{-| Represents custom type definitions found in modules.

@docs Union


# Access

@docs name, constructors, args, documentation


# Creation

@docs create

-}

import Dict exposing (Dict)
import Review.Internal.Union
import Review.Type exposing (Type)


{-| Representation of a custom type.

    type Maybe a
        = Just a
        | Nothing

    type Value
        = Value String

It is called "Union" to be consistent with [`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Union) naming convention.

-}
type alias Union =
    Review.Internal.Union.Union


{-| Create a new custom type.
-}
create :
    { name : String
    , documentation : Maybe String
    , args : List String
    , constructors : Dict String (List Type)
    }
    -> Union
create =
    Review.Internal.Union.create


{-| Get the name of a custom type.
-}
name : Union -> String
name =
    Review.Internal.Union.name


constructors : Union -> Dict String (List Type)
constructors =
    Review.Internal.Union.constructors


{-| Get the list of type variables for a custom type.

    type Union a = ...
    --> [ a ]

-}
args : Union -> List String
args =
    Review.Internal.Union.args


{-| Get the documentation of a custom type.

    {-| documentation
    -}
    type A
        = B

The leading `{-|` and trailing `-}` are stripped off, but the rest of the string remains as is. In the example above, the documentation would be `documentation\n`.

The documentation will be `Nothing` if it is was missing.

-}
documentation : Union -> Maybe String
documentation =
    Review.Internal.Union.documentation
