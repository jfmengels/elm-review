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

import Ast.Expression exposing (..)
import Lint exposing (doNothing, lint)
import Lint.Types exposing (Direction(..), LintError, LintRule, LintRuleImplementation)


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
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = \ctx -> ( [], ctx )
    , initialContext = Context
    }


error : LintError
error =
    LintError "NoDebug" "Forbidden use of Debug"


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (Variable vars) ->
            if List.member "Debug" vars then
                ( [ error ], ctx )

            else
                ( [], ctx )

        _ ->
            ( [], ctx )
