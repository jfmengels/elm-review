module Lint.Rules.NoDebug exposing (rule)

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
    Error "NoDebug" "Forbidden use of Debug"


expressionFn : Context -> Direction Expression -> ( List Error, Context )
expressionFn ctx node =
    case node of
        Enter (Variable vars) ->
            if List.member "Debug" vars then
                ( [ error ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
