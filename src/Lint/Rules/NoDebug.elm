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
import Lint exposing (lint)
import Lint.Types exposing (Direction(..), LintError, LintRule, LintRuleImplementation, emptyRule)


type alias Context =
    {}


{-| Forbid the use of `Debug` before it goes into production.

    rules =
        [ NoDebug.rule
        ]

-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    let
        impl =
            emptyRule Context
    in
    { impl | expressionFn = expressionFn }


error : Node a -> LintError
error node =
    LintError "NoDebug" "Forbidden use of Debug" (range node)


expressionFn : Context -> Direction -> Node Expression -> ( List LintError, Context )
expressionFn ctx direction node =
    case ( direction, value node ) of
        ( Enter, FunctionOrValue moduleName fnName ) ->
            if List.member "Debug" moduleName then
                ( [ error node ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )
