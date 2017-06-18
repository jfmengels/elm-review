module Lint.Rules.NoExposingEverything exposing (rule)

{-|
@docs rule

# Fail

    module Main exposing (..)

# Success

    module Main exposing (a, b, C)
-}

import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))


type alias Context =
    {}


{-| Forbid exporting everything in your modules `module Main exposing (..)`, to make your module explicit in what it exposes.

    rules =
        [ NoExposingEverything.rule
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
    , initialContext = Context
    }


createError : String -> LintError
createError name =
    LintError "NoExposingEverything" ("Do not expose everything from module " ++ name ++ " using (..)")


reportModule : List String -> LintError
reportModule name =
    name
        |> String.join "."
        |> createError


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (ModuleDeclaration name AllExport) ->
            ( [ reportModule name ], ctx )

        Enter (PortModuleDeclaration name AllExport) ->
            ( [ reportModule name ], ctx )

        _ ->
            ( [], ctx )
