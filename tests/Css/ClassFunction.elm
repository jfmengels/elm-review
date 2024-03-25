module Css.ClassFunction exposing
    ( CssArgument(..)
    , fromLiteral
    , baseCssFunctions
    , firstArgumentIsClass, htmlAttributesAttribute, htmlAttributesClassList
    )

{-|

@docs CssArgument
@docs fromLiteral
@docs baseCssFunctions
@docs firstArgumentIsClass, htmlAttributesAttribute, htmlAttributesClassList

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)


type CssArgument
    = Literal String
    | Variable Range
    | MissingArgument Int


fromLiteral : Node Expression -> CssArgument
fromLiteral node =
    case Node.value node of
        Expression.Literal str ->
            Literal str

        _ ->
            Variable (Node.range node)


baseCssFunctions : List ( String, { firstArgument : Node Expression, restOfArguments : List (Node Expression) } -> List CssArgument )
baseCssFunctions =
    [ ( "Html.Attributes.class", \{ firstArgument } -> [ fromLiteral firstArgument ] )
    , ( "Svg.Attributes.class", \{ firstArgument } -> [ fromLiteral firstArgument ] )
    , ( "Html.Attributes.classList", htmlAttributesClassList )
    , ( "Html.Attributes.attribute", htmlAttributesAttribute )
    ]


firstArgumentIsClass : { firstArgument : Node Expression, restOfArguments : List (Node Expression) } -> List CssArgument
firstArgumentIsClass { firstArgument } =
    [ fromLiteral firstArgument ]


htmlAttributesAttribute : { firstArgument : Node Expression, restOfArguments : List (Node Expression) } -> List CssArgument
htmlAttributesAttribute { firstArgument, restOfArguments } =
    case Node.value firstArgument of
        Expression.Literal "class" ->
            case restOfArguments of
                [] ->
                    [ MissingArgument 2 ]

                classArgument :: _ ->
                    [ fromLiteral classArgument ]

        _ ->
            []


htmlAttributesClassList : { firstArgument : Node Expression, restOfArguments : List (Node Expression) } -> List CssArgument
htmlAttributesClassList { firstArgument } =
    case Node.value firstArgument of
        Expression.ListExpr list ->
            List.map
                (\element ->
                    case Node.value element of
                        Expression.TupledExpression [ first, _ ] ->
                            fromLiteral first

                        _ ->
                            Variable (Node.range element)
                )
                list

        _ ->
            [ fromLiteral firstArgument ]
