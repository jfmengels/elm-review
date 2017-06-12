module Lint.Rules.NoImportingEverything exposing (rule)

{-|
@docs rule

# Fail

    import Html exposing (..)

# Success

    import Html exposing (div, p, textarea)
-}

import Ast.Statement exposing (..)
import Lint exposing (lint, doNothing)
import Lint.Types exposing (LintRule, LintRuleImplementation, Error, Direction(..))


type alias Context =
    {}


{-| Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed
functions and types are unknown to them.

    rules =
        [ NoImportingEverything.rule
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


error : String -> Error
error name =
    Error "NoImportingEverything" ("Do not expose everything from " ++ name)


statementFn : Context -> Direction Statement -> ( List Error, Context )
statementFn ctx node =
    case node of
        Enter (ImportStatement names alias (Just AllExport)) ->
            ( [ error <| String.join "." names ], ctx )

        _ ->
            ( [], ctx )
