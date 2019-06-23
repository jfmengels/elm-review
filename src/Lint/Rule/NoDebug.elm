module Lint.Rule.NoDebug exposing (rule)

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
import Elm.Syntax.Node exposing (Node, range, value)
import Lint.Error as Error exposing (Error)
import Lint.Rule2 as Rule exposing (Rule)


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoDebug.rule
        ]

-}
rule : Rule
rule =
    Rule.newRuleSchema "NoDebug"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


error : Node a -> Error
error node =
    Error.create "Forbidden use of Debug" (range node)


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case value node of
        FunctionOrValue moduleName fnName ->
            if List.member "Debug" moduleName then
                [ error node ]

            else
                []

        _ ->
            []
