module Lint exposing
    ( Severity(..)
    , lintSource
    )

{-| Module to configure your linting configuration and run it on a source file.


# Configuration

@docs Severity


# Linting

@docs lintSource

-}

import Elm.Parser as Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.File exposing (File)
import Lint.Rule as Rule exposing (Rule)
import Lint.RuleError as RuleError exposing (RuleError)


{-| When configuring `elm-lint` and adding the rules you want to enforce, you need to associate a `Severity` level to each rule.

  - `Critical` - The rule is enforced and any patterns that the rule finds will be reported. The (yet non-existent) `elm-lint` CLI will fail with an error code if anything gets reported.
  - `Warning` - The rule is enforced and any patterns that the rule finds will be reported. But the (yet non-existent) `elm-lint` CLI will not fail with an error code even if something gets reported.
  - `Disabled` - The associated rule is not enforced and nothing will get reported for this rule.

-}
type Severity
    = Disabled
    | Warning
    | Critical


{-| Lints a file and gives back the errors raised by the given rules.

    config : List ( Severity, Rule )
    config =
        [ ( Warning, Lint.Rule.NoDebug.rule )
        , ( Critical, Lint.Rule.NoUnusedVariables.rule )
        ]

    result =
        lintSource config sourceCode

-}
lintSource : List ( Severity, Rule ) -> String -> Result (List String) (List ( Severity, RuleError ))
lintSource config source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                config
                    |> List.concatMap
                        (lintSourceWithRule statements)
            )


lintSourceWithRule : File -> ( Severity, Rule ) -> List ( Severity, RuleError )
lintSourceWithRule file ( severity, rule ) =
    Rule.analyzer rule file
        |> List.map (\error -> ( severity, RuleError.fromError (Rule.name rule) error ))


{-| Parse source code into a AST
-}
parseSource : String -> Result (List String) File
parseSource source =
    source
        |> Parser.parse
        -- TODO Improve parsing error handling
        |> Result.mapError (\error -> [ "Parsing error" ])
        -- TODO Add all files to have more context https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Processing
        |> Result.map (process init)
