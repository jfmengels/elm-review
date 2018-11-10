module Lint.Rules.NoDebug exposing (rule)

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
import Lint.Error exposing (Error)
import Lint.Types exposing (Direction(..), LintRuleImplementation, createRule)


type alias Context =
    {}


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoDebug.rule
        ]

-}
rule : Rule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    createRule
        Context
        (\v -> { v | visitExpression = visitExpression })


error : Node a -> Error
error node =
    Error "NoDebug" "Forbidden use of Debug" (range node)


visitExpression : Context -> Direction -> Node Expression -> ( List Error, Context )
visitExpression ctx direction node =
    case ( direction, value node ) of
        ( Enter, FunctionOrValue moduleName fnName ) ->
            if List.member "Debug" moduleName then
                ( [ error node ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )
