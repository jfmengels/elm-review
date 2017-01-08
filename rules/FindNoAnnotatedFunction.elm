module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (LintRule, Error, doNothing)
import Ast.Statement exposing (..)


type alias Context =
    { annotatedFunctions : List String
    }


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , context = Context []
    }


statementFn : Context -> Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        FunctionTypeDeclaration name application ->
            ( [], { ctx | annotatedFunctions = name :: ctx.annotatedFunctions } )

        FunctionDeclaration name params body ->
            if List.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ "`" ++ name ++ "` does not have a type declaration" ], ctx )

        _ ->
            ( [], ctx )
