module NoExposingEverything exposing (rule)

import Lint exposing (doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Statement exposing (..)


type alias Context =
    {}


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , context = Context
    }


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (ModuleDeclaration names AllExport) ->
            case names of
                [ name ] ->
                    ( [ "Do not expose everything from module " ++ name ++ " using (..)" ], ctx )

                _ ->
                    ( [], ctx )

        _ ->
            ( [], ctx )
