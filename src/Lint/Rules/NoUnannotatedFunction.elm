module Lint.Rules.NoUnannotatedFunction exposing (rule)

{-|
@docs rule

# Fail

    a n =
        n + 1

# Success

    a : Int -> Int
    a n =
        n + 1
-}

import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))
import Set exposing (Set)


type alias Context =
    { annotatedFunctions : Set String
    }


{-| Ensure every top-level function declaration has a type annotation.

    rules =
        [ NoUnannotatedFunction.rule
        ]
-}
rule : LintRule
rule input =
    lint input implementation


implementation : LintRuleImplementation Context
implementation =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context Set.empty
    }


createError : String -> LintError
createError name =
    LintError "NoUnannotatedFunction" ("`" ++ name ++ "` does not have a type declaration")


statementFn : Context -> Direction Statement -> ( List LintError, Context )
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
