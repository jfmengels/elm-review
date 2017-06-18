module Lint.Rules.NoImportingEverything exposing (rule, Configuration)

{-|
@docs rule, Configuration

# Fail

    import Html exposing (..)

# Success

    import Html exposing (div, p, textarea)
-}

import Set exposing (Set)
import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, LintError, Direction(..))


type alias Context =
    { exceptions : Set String }


{-| Configuration for the rule.
-}
type alias Configuration =
    { exceptions : List String }


{-| Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed
functions and types are unknown to them.

    rules =
        [ NoImportingEverything.rule { exceptions = ["Html"]}
        ]
-}
rule : Configuration -> LintRule
rule exceptions input =
    lint input (implementation exceptions)


implementation : Configuration -> LintRuleImplementation Context
implementation config =
    { statementFn = statementFn
    , typeFn = doNothing
    , expressionFn = doNothing
    , moduleEndFn = (\ctx -> ( [], ctx ))
    , initialContext = Context (Set.fromList config.exceptions)
    }


error : String -> LintError
error name =
    LintError "NoImportingEverything" ("Do not expose everything from " ++ name)


statementFn : Context -> Direction Statement -> ( List LintError, Context )
statementFn ctx node =
    case node of
        Enter (ImportStatement names alias (Just AllExport)) ->
            let
                name =
                    String.join "." names
            in
                if Set.member name ctx.exceptions then
                    ( [], ctx )
                else
                    ( [ error name ], ctx )

        _ ->
            ( [], ctx )
