module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (lint, doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Statement exposing (..)
import Set exposing (Set)


type alias Context =
    { annotatedFunctions : Set String
    }


rule : String -> List Error
rule input =
    lint input implementation


implementation : LintRule Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context Set.empty
    }


createError : String -> Error
createError name =
    Error "FindNoAnnotatedFunction" ("`" ++ name ++ "` does not have a type declaration")


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (FunctionTypeDeclaration name application) ->
            ( [], { ctx | annotatedFunctions = Set.insert name ctx.annotatedFunctions } )

        Enter (FunctionDeclaration name params body) ->
            if Set.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ createError name ], ctx )

        _ ->
            ( [], ctx )
