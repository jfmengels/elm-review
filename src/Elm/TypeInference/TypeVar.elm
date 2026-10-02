module Elm.TypeInference.TypeVar exposing
    ( GenKey
    , NamedKey
    , SuperType(..)
    , TypeVar
    , TypeVarStyle(..)
    , genKeyFrom
    , deduplicate
    , namedKeyFrom
    , parse
    , superTypeTag
    , toString
    )

{-| -}

import List.Extra
import Set exposing (Set)


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
            case super of
                Normal ->
                    ""

                _ ->
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
        maybeConstrained : Maybe ( TypeVarStyle, SuperType )
        maybeConstrained =
            typeVariableConstraintPrefixes
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
    in
    case maybeConstrained of
        Just constrained ->
            constrained

        Nothing ->
            ( Named name, Normal )


typeVariableConstraintPrefixes : List ( String, SuperType )
typeVariableConstraintPrefixes =
    [ ( "compappend", CompAppend )
    , ( "comparable", Comparable )
    , ( "appendable", Appendable )
    , ( "number", Number )
    ]


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


{-| `comparable` encoding of a generated `TypeVar`: `id * 5 + superTypeTag`.
-}
type alias GenKey =
    Int


{-| `comparable` encoding of a named `TypeVar`: `(superTypeTag, name)`.
-}
type alias NamedKey =
    ( Int, String )


{-|

    genKeyFrom 5 Number --> 5 * 5 + 1 == 26

-}
genKeyFrom : Int -> SuperType -> GenKey
genKeyFrom theId superType =
    theId * 5 + superTypeTag superType


{-|

    namedKeyFrom "hello" Comparable --> (2, "hello")

-}
namedKeyFrom : String -> SuperType -> NamedKey
namedKeyFrom name superType =
    ( superTypeTag superType, name )


superTypeTag : SuperType -> Int
superTypeTag superType =
    case superType of
        Normal ->
            0

        Number ->
            1

        Comparable ->
            2

        Appendable ->
            3

        CompAppend ->
            4


{-| Order-preserving dedupe.
-}
deduplicate : List TypeVar -> List TypeVar
deduplicate vars =
    deduplicateHelp Set.empty Set.empty vars []


deduplicateHelp : Set GenKey -> Set NamedKey -> List TypeVar -> List TypeVar -> List TypeVar
deduplicateHelp seenGen seenNamed remaining acc =
    case remaining of
        [] ->
            List.reverse acc

        (( style, super ) as var) :: rest ->
            case style of
                Generated theId ->
                    let
                        k : GenKey
                        k =
                            genKeyFrom theId super
                    in
                    if Set.member k seenGen then
                        deduplicateHelp seenGen seenNamed rest acc

                    else
                        deduplicateHelp (Set.insert k seenGen) seenNamed rest (var :: acc)

                Named name ->
                    let
                        k : NamedKey
                        k =
                            namedKeyFrom name super
                    in
                    if Set.member k seenNamed then
                        deduplicateHelp seenGen seenNamed rest acc

                    else
                        deduplicateHelp seenGen (Set.insert k seenNamed) rest (var :: acc)
