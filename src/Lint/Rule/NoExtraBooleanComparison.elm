module Lint.Rule.NoExtraBooleanComparison exposing (rule)

{-|

@docs rule


# Fail

    if Debug.log "condition" condition then
        a

    else
        b

    if condition then
        Debug.crash "Nooo!"

    else
        value


# Success

    if condition then
        a

    else
        b

-}

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Lint exposing (Rule, lint)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule


type alias Context =
    ()


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoExtraBooleanComparison.rule
        ]

-}
rule : Rule
rule =
    Lint.createRule
        "NoExtraBooleanComparison"
        (lint implementation)


implementation : Rule.Implementation Context
implementation =
    Rule.create ()
        |> Rule.withExpressionVisitor expressionVisitor


error : String -> Node a -> Error
error comparedValue node =
    Error.create
        ("Unnecessary comparison with `" ++ comparedValue ++ "`")
        (Node.range node)


expressionVisitor : Context -> Direction -> Node Expression -> ( List Error, Context )
expressionVisitor context direction node =
    case ( direction, Node.value node ) of
        ( Direction.Enter, Elm.Syntax.Expression.OperatorApplication operator _ left right ) ->
            if isEqualityOperator operator then
                ( List.filterMap isTrueOrFalse [ left, right ], context )

            else
                ( [], context )

        _ ->
            ( [], context )


isEqualityOperator : String -> Bool
isEqualityOperator operator =
    operator == "==" || operator == "/="


isTrueOrFalse : Node Expression -> Maybe Error
isTrueOrFalse node =
    case Node.value node of
        FunctionOrValue [] functionOrValue ->
            if functionOrValue == "True" || functionOrValue == "False" then
                Just <| error functionOrValue node

            else
                Nothing

        _ ->
            Nothing
