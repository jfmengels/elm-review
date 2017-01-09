module FindNoAnnotatedFunction exposing (rule)

import Lint exposing (doNothing)
import Types exposing (LintRule, Error, Direction(..))
import Ast.Statement exposing (..)
import Set exposing (Set)


type alias Context =
    { annotatedFunctions : Set String
    }


rule : LintRule Context
rule =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , context = Context Set.empty
    }


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (FunctionTypeDeclaration name application) ->
            ( [], { ctx | annotatedFunctions = Set.insert name ctx.annotatedFunctions } )

        Enter (FunctionDeclaration name params body) ->
            if Set.member name ctx.annotatedFunctions then
                ( [], ctx )
            else
                ( [ "`" ++ name ++ "` does not have a type declaration" ], ctx )

        _ ->
            ( [], ctx )
