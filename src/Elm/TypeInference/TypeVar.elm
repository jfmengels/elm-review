module Elm.TypeInference.TypeVar exposing
    ( SuperType(..)
    , TypeVar
    , TypeVarStyle(..)
    , parse
    , superTypeToString
    , toString
    )

{-| -}

import List.Extra


{-|

    x : a (in source code) == NormalVar "a"
    x : a (given by compiler) == NormalId 1
    x : number (in source code) == SuperVar Number ""
    x : number1 (in source code) == SuperVar Number "1"
    x : number (given by compiler) == SuperId Number 1

-}
type alias TypeVar =
    ( TypeVarStyle, SuperType )


type TypeVarStyle
    = Generated Int
    | Named String


type SuperType
    = Normal
    | {- Int | Float -} Number
    | {- Int | Float | Char | String | List comparable | tuples of comparables -} Comparable
    | {- String | List a -} Appendable
    | {- String | List comparable -} CompAppend


toString : TypeVar -> String
toString ( style, super ) =
    let
        prefix : String
        prefix =
            if super == Normal then
                ""

            else
                superTypeToString super
    in
    case ( super, style ) of
        ( Normal, Generated theId ) ->
            "#" ++ String.fromInt theId

        ( Normal, Named name ) ->
            name

        ( _, Generated theId ) ->
            prefix ++ "#" ++ String.fromInt theId

        ( _, Named name ) ->
            prefix ++ name


parse : String -> TypeVar
parse name =
    let
        prefixes : List ( String, SuperType )
        prefixes =
            [ ( "compappend", CompAppend )
            , ( "comparable", Comparable )
            , ( "appendable", Appendable )
            , ( "number", Number )
            ]
    in
    prefixes
        |> List.Extra.findMap
            (\( prefix, super ) ->
                if String.startsWith prefix name then
                    Just
                        ( Named (String.dropLeft (String.length prefix) name)
                        , super
                        )

                else
                    Nothing
            )
        |> Maybe.withDefault ( Named name, Normal )


superTypeToString : SuperType -> String
superTypeToString super =
    case super of
        Normal ->
            "any type"

        Number ->
            "number"

        Comparable ->
            "comparable"

        Appendable ->
            "appendable"

        CompAppend ->
            "compappend"
