module NoDebug exposing (rule)

import Lint exposing (LintRule, Error, doNothing)
import Node exposing (..)
import Ast.Expression exposing (..)


type alias Context =
    {}


rule : LintRule Context
rule =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn
    , context = Context
    }


expressionFn : Context -> Direction Expression -> ( List Error, Context )
expressionFn ctx node =
    case node of
        Enter (Variable vars) ->
            if List.member "Debug" vars then
                ( [ "Forbidden use of Debug" ], ctx )
            else
                ( [], ctx )

        _ ->
            ( [], ctx )
