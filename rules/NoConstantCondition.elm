module NoConstantCondition exposing (rule)

import Lint exposing (lint, doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Expression exposing (..)
import Set exposing (Set)


type alias Context =
    {}


rule : String -> List Error
rule input =
    lint input implementation


implementation : LintRule Context
implementation =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


error : Error
error =
    Error "NoConstantCondition" "Useless condition: It will always evaluate to the same value"


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


expressionFn : Context -> Direction Expression -> ( List Error, Context )
expressionFn ctx node =
    case node of
        Enter (If cond _ _) ->
            if isStatic cond then
                ( [ error ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
