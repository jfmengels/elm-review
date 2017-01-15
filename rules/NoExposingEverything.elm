module NoExposingEverything exposing (rule)

import Lint exposing (lint, doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Statement exposing (..)


type alias Context =
    {}


rule : String -> List Error
rule input =
    lint input implementation


implementation : LintRule Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context
    }


createError : String -> Error
createError name =
    Error "NoExposingEverything" ("Do not expose everything from module " ++ name ++ " using (..)")


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (ModuleDeclaration names AllExport) ->
            case names of
                [ name ] ->
                    ( [ createError name ], ctx )

                _ ->
                    ( [], ctx )

        _ ->
            ( [], ctx )
