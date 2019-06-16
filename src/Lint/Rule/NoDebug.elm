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
import Lint exposing (Rule, lint)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule


type alias Context =
    ()


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoDebug.rule
        ]

-}
rule : Rule
rule =
    Lint.createRule
        "NoDebug"
        (lint implementation)


implementation : Rule.Implementation Context
implementation =
    Rule.create ()
        |> Rule.withExpressionVisitor expressionVisitor


error : Node a -> Error
error node =
    Error.create "Forbidden use of Debug" (range node)


expressionVisitor : Context -> Direction -> Node Expression -> ( List Error, Context )
expressionVisitor ctx direction node =
    case ( direction, value node ) of
        ( Direction.Enter, FunctionOrValue moduleName fnName ) ->
            if List.member "Debug" moduleName then
                ( [ error node ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )
