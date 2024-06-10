module Css.ClassFunction exposing
    ( CssArgument(..)
    , fromLiteral
    , baseCssFunctions
    , Arguments, firstArgumentIsClass, htmlAttributesAttribute, htmlAttributesClassList
    , smartFirstArgumentIsClass
    )

{-|

@docs CssArgument
@docs fromLiteral
@docs baseCssFunctions
@docs Arguments, firstArgumentIsClass, htmlAttributesAttribute, htmlAttributesClassList
@docs smartFirstArgumentIsClass

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


type CssArgument
    = Literal String
    | Variable Range
    | UngraspableExpression Range
    | MissingArgument Int


type alias Arguments =
    { firstArgument : Node Expression
    , restOfArguments : List (Node Expression)
    , lookupTable : ModuleNameLookupTable
    }


fromLiteral : Node Expression -> CssArgument
fromLiteral node =
    case Node.value node of
        Expression.Literal str ->
            Literal str

        _ ->
            UngraspableExpression (Node.range node)


fromExpression : ModuleNameLookupTable -> Node Expression -> List CssArgument
fromExpression lookupTable node =
    fromExpressionHelp lookupTable [ node ] []


fromExpressionHelp : ModuleNameLookupTable -> List (Node Expression) -> List CssArgument -> List CssArgument
fromExpressionHelp lookupTable nodes acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            case Node.value node of
                Expression.Literal str ->
                    fromExpressionHelp lookupTable rest (Literal str :: acc)

                Expression.FunctionOrValue [] _ ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable node of
                        Just [] ->
                            fromExpressionHelp lookupTable rest (Variable (Node.range node) :: acc)

                        _ ->
                            fromExpressionHelp lookupTable rest (UngraspableExpression (Node.range node) :: acc)

                Expression.ParenthesizedExpression expr ->
                    fromExpressionHelp lookupTable (expr :: rest) acc

                Expression.IfBlock _ then_ else_ ->
                    fromExpressionHelp lookupTable (then_ :: else_ :: rest) acc

                Expression.CaseExpression { cases } ->
                    fromExpressionHelp lookupTable (List.foldl (\( _, expr ) nodesAcc -> expr :: nodesAcc) rest cases) acc

                _ ->
                    fromExpressionHelp lookupTable rest (UngraspableExpression (Node.range node) :: acc)


baseCssFunctions : List ( String, Arguments -> List CssArgument )
baseCssFunctions =
    [ ( "Html.Attributes.class", \{ firstArgument } -> [ fromLiteral firstArgument ] )
    , ( "Svg.Attributes.class", \{ firstArgument } -> [ fromLiteral firstArgument ] )
    , ( "Html.Attributes.classList", htmlAttributesClassList )
    , ( "Html.Attributes.attribute", htmlAttributesAttribute )
    ]


firstArgumentIsClass : Arguments -> List CssArgument
firstArgumentIsClass { firstArgument } =
    [ fromLiteral firstArgument ]


smartFirstArgumentIsClass : Arguments -> List CssArgument
smartFirstArgumentIsClass { lookupTable, firstArgument } =
    fromExpression lookupTable firstArgument


htmlAttributesAttribute : Arguments -> List CssArgument
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


htmlAttributesClassList : Arguments -> List CssArgument
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
