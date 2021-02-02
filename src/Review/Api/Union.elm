module Review.Api.Union exposing
    ( Union
    , name, constructors, args, documentation
    )

{-| Represents union type definitions found in modules.

@docs Union
@docs name, constructors, args, documentation

-}

import Dict exposing (Dict)
import Review.Api.Type exposing (Type)
import Review.Internal.Union


{-| Representation of a union type, or a "custom type" as what it is most often referred to in Elm.

    type Maybe a
        = Just a
        | Nothing

    type Value
        = Value String

It is called "Union" to be consistent with [`elm/project-metadata-utils`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Union) naming convention.

-}
type alias Union =
    Review.Internal.Union.Union


{-| Get the name of a union type.
-}
name : Union -> String
name =
    Review.Internal.Union.name


{-| Get the constructors for a union type.

    import Review.Api.Type as Type

    type
        Maybe a
        --> Dict.fromList
        -->   [ ( "Just", [ Type.Generic "a" ] )
        -->   , ( "Nothing", [] )
        -->   ]
        = Just a
        | Nothing

    type
        Value
        --> Dict.fromList
        -->   [ ( "Value", [ Type.Type [ "Basics" ] "String" [] ] )
        -->   ]
        = Value String

-}
constructors : Union -> Dict String (List Type)
constructors =
    Review.Internal.Union.constructors


{-| Get the list of type variables for a union type.

    type Union a = ...
    --> [ a ]

-}
args : Union -> List String
args =
    Review.Internal.Union.args


{-| Get the documentation of a union type.

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
