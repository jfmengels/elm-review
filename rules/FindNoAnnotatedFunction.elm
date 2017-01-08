module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (LintRule, Error, doNothing)
import Node exposing (..)
import Ast.Statement exposing (..)


type alias Context =
    { annotatedFunctions : List String
    }


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , context = Context []
    }


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (FunctionTypeDeclaration name application) ->
            ( [], { ctx | annotatedFunctions = name :: ctx.annotatedFunctions } )

        Enter (FunctionDeclaration name params body) ->
            if List.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ "`" ++ name ++ "` does not have a type declaration" ], ctx )

        _ ->
            ( [], ctx )
