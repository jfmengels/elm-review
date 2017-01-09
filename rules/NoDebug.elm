module NoDebug exposing (rule)

import Lint exposing (doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Expression exposing (..)


type alias Context =
    {}


rule : LintRule Context
rule =
    { statementFn = doNothing
    , typeFn = doNothing
    , expressionFn = expressionFn
    , moduleEndFn = (\ctx -> ( [], ctx ))
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
