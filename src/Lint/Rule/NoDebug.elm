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
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Rule as Rule exposing (Error, Rule)


{-| Forbid the use of `Debug` before it goes into production.

    config =
        [ ( Critical, NoDebug.rule )
        ]

-}
rule : Rule
rule =
    Rule.newSchema "NoDebug"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


error : Node a -> Error
error node =
    Rule.error "Forbidden use of Debug" (Node.range node)


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        FunctionOrValue moduleName fnName ->
            if List.member "Debug" moduleName then
                [ error node ]

            else
                []

        _ ->
            []
