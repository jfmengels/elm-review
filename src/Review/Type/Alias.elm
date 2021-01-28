module Review.Type.Alias exposing
    ( Alias
    , name, tipe, args, documentation
    , create
    )

{-| Represents type aliases found in modules.

@docs Alias


# Access

@docs name, tipe, args, documentation


# Creation

@docs create

-}

import Review.Internal.Alias
import Review.Type exposing (Type(..))


{-| Representation of a type alias.

    type alias Age =
        Int

    type alias User =
        { name : String
        , age : Age
        }

-}
type alias Alias =
    Review.Internal.Alias.Alias



-- CREATION


{-| Create a new type alias.
-}
create : { name : String, documentation : Maybe String, args : List String, tipe : Type } -> Review.Internal.Alias.Alias
create =
    Review.Internal.Alias.create



-- ACCESS


{-| Get the name of a type alias.
-}
name : Alias -> String
name =
    Review.Internal.Alias.name


{-| Get the type of a value, as declared by its type annotation

If the value is a function or constant and the type annotation is missing, the type will be `Unknown`. In the future, `elm-review` may attempt to infer the type of the value.

The odd name choice comes from `type` being a reserved word, and was inspired by the [`tipe` field in `Elm.Docs.Alias`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Alias).

-}
tipe : Alias -> Type
tipe =
    Review.Internal.Alias.tipe


{-| Get the type of a value, as declared by its type annotation

If the value is a function or constant and the type annotation is missing, the type will be `Unknown`. In the future, `elm-review` may attempt to infer the type of the value.

The odd name choice comes from `type` being a reserved word, and was inspired by the [`tipe` field in `Elm.Docs.Alias`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Alias).

-}
args : Alias -> List String
args =
    Review.Internal.Alias.args


{-| Get the documentation of a value.

    {-| documentation
    -}
    value =
        1

The leading `{-|` and trailing `-}` are stripped off, but the rest of the string remains as is. In the example above, the documentation would be `documentation\n`.

The documentation will be `Nothing` if it is was missing, or if the value corresponds to a custom type constructor.

-}
documentation : Alias -> Maybe String
documentation =
    Review.Internal.Alias.documentation
