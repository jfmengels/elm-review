module Lint.Rules.NoUselessIf exposing (rule)

import Ast.Expression exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, Error, Direction(..))


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
    Error "NoUselessIf" "Useless if expression: It will always evaluate to the same value"


expressionFn : Context -> Direction Expression -> ( List Error, Context )
expressionFn ctx node =
    case node of
        Enter (If cond then_ else_) ->
            if then_ == else_ then
                ( [ error ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
