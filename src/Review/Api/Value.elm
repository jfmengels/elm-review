module Review.Api.Value exposing
    ( Value
    , name, tipe, documentation
    )

{-| Represents values found in modules.

@docs Value
@docs name, tipe, documentation

-}

import Review.Api.Type exposing (Type(..))
import Review.Internal.Value


{-| Representation of a top-level function or constant.

    -- constant is a value
    constant =
        1

    -- func is a value
    func n =
        n + 1

    type CustomType
        = A -- A is a value, as it is also a constant
        | B Int -- B is a value, as it is also a function (of `Int -> CustomType`)

    type alias Alias =
        -- Alias is a value, as it is also a function (of `Int -> Alias`)
        { field : Int }

-}
type alias Value =
    Review.Internal.Value.Value



-- ACCESS


{-| Get the name of a value.

If the name starts with an uppercase character, then it comes from a type alias or a custom type constructor. Otherwise it comes from a function or constant.

-}
name : Value -> String
name =
    Review.Internal.Value.name



-- TODO Decide whether to rename all `tipe` function to `declaredType`, which could be a `Maybe Type`.
-- (though having `Just Unknown` would be weird)
-- Later we could then have a `inferredType` which would be a `Type`.


{-| Get the type of a value, as declared by its type annotation

If the value is a function or constant and the type annotation is missing, the type will be `Unknown`. In the future, `elm-review` may attempt to infer the type of the value.

The odd name choice comes from `type` being a reserved word, and was inspired by the [`tipe` field in `Elm.Docs.Value`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Value).

-}
tipe : Value -> Type
tipe =
    Review.Internal.Value.tipe


{-| Get the documentation of a value.

    {-| documentation
    -}
    value =
        1

The leading `{-|` and trailing `-}` are stripped off, but the rest of the string remains as is. In the example above, the documentation would be `documentation\n`.

The documentation will be `Nothing` if it is was missing, or if the value corresponds to a custom type constructor.

-}
documentation : Value -> Maybe String
documentation =
    Review.Internal.Value.documentation
