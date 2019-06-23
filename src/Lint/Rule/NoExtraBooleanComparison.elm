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

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule exposing (Rule)


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoExtraBooleanComparison.rule
        ]

-}
rule : Rule
rule =
    Rule.newRuleSchema "NoExtraBooleanComparison"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


error : String -> Node a -> Error
error comparedValue node =
    Error.create
        ("Unnecessary comparison with `" ++ comparedValue ++ "`")
        (Node.range node)


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        Expression.OperatorApplication operator _ left right ->
            if isEqualityOperator operator then
                List.filterMap isTrueOrFalse [ left, right ]

            else
                []

        _ ->
            []


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
