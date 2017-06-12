module Lint.Rules.NoUselessIf exposing (rule)

{-|
@docs rule

# Fail

    if condition then
        value
    else
        value

# Success

    if condition then
        value1
    else
        value2
-}

import Ast.Expression exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))


type alias Context =
    {}


{-| Reports when both paths of an If expression result will lead to the same value.

    rules =
        [ NoUselessIf.rule
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
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


error : LintError
error =
    LintError "NoUselessIf" "Useless if expression: It will always evaluate to the same value"


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (If cond then_ else_) ->
            if then_ == else_ then
                ( [ error ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
