module Lint.Rules.NoConstantCondition exposing (rule)

{-|
@docs rule

# Fail

    if True then a else b
    if False then a else b
    if foo == foo then a else b

# Success

    if foo == bar then a else b
-}

import Ast.Expression exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))
import Set exposing (Set)


type alias Context =
    {}


{-| Forbid the use of expressions in an If condition whose value are always the same.

    rules =
        [ NoConstantCondition.rule
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
    LintError "NoConstantCondition" "Useless condition: It will always evaluate to the same value"


isStaticVariable : List String -> Bool
isStaticVariable names =
    case names of
        [ "True" ] ->
            True

        [ "False" ] ->
            True

        _ ->
            False


comparisonOperators : Set (List String)
comparisonOperators =
    Set.fromList [ [ "==" ], [ "/=" ], [ "<" ], [ "<=" ], [ ">" ], [ ">=" ] ]


isStatic : Expression -> Bool
isStatic expr =
    case expr of
        Variable value ->
            if isStaticVariable value then
                True
            else
                False

        Integer value ->
            True

        Float value ->
            True

        String value ->
            True

        BinOp (Variable op) left right ->
            (Set.member op comparisonOperators)
                && (left == right || (isStatic left && isStatic right))

        _ ->
            False


expressionFn : Context -> Direction Expression -> ( List LintError, Context )
expressionFn ctx node =
    case node of
        Enter (If cond _ _) ->
            if isStatic cond then
                ( [ error ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
