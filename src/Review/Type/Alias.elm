module Review.Type.Alias exposing
    ( Alias
    , name, tipe, args, documentation
    )

{-| Represents type aliases found in modules.

@docs Alias
@docs name, tipe, args, documentation

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



-- ACCESS


{-| Get the name of a type alias.
-}
name : Alias -> String
name =
    Review.Internal.Alias.name


{-| Get the type of a type alias.

    import Review.Type as Type

    type alias Age =
        --> Type.Type [ "Basics" ] "Int" []
        Int

    fields =
        [ ( "name", Type.Type [ "Basics" ] "String" [] )
        , ( "age", Type.Type [] "Age" [] )
        , ( "value", Type.Generic "a" )
        ]

    type alias User a =
        --> Type.Record
        -->   { fields :
        -->       [ ( "name", Type.Type [ "Basics" ] "String" [] )
        -->       , ( "age", Type.Type [] "Age" [] )
        -->       , ( "value", Type.Generic "a" )
        -->       ]
        -->   , generic : Just "a"
        -->   , mayHaveMoreFields : False
        -->   }
        { name : String
        , age : Age
        , value : a
        }

The odd name choice comes from `type` being a reserved word, and was inspired by the [`tipe` field in `Elm.Docs.Alias`](https://package.elm-lang.org/packages/elm/project-metadata-utils/1.0.1/Elm-Docs#Alias).

-}
tipe : Alias -> Type
tipe =
    Review.Internal.Alias.tipe


{-| Get the list of type variables for a type alias.

    type alias Thing a b c = ...
    --> [ a, b, c ]

-}
args : Alias -> List String
args =
    Review.Internal.Alias.args


{-| Get the documentation of a type alias.

    {-| documentation
    -}
    type alias Age =
        Int

The leading `{-|` and trailing `-}` are stripped off, but the rest of the string remains as is. In the example above, the documentation would be `documentation\n`.

The documentation will be `Nothing` if it is was missing.

-}
documentation : Alias -> Maybe String
documentation =
    Review.Internal.Alias.documentation
