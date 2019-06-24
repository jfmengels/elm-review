module Lint exposing
    ( Severity(..)
    , lintSource
    )

{-| A linter for Elm.

See Lint.Rules for available rules.
To define the rules you wish to use:

    rules =
        [ Lint.Rules.NoDebug.rule
        , Lint.Rules.NoUnusedVariables
        ]

To run the rules on a source code and get a list of errors:

    lint : String -> List String
    lint source =
        let
            errors =
                List.concatMap (\rule -> rule source) rules
        in
        if List.isEmpty errors then
            [ "No errors." ]

        else
            List.map (\err -> err.rule ++ ": " ++ err.message) errors


# Configuration

@docs Severity


# Linting

@docs lintSource

-}

import Elm.Parser as Parser
import Elm.Processing exposing (addFile, init, process)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import Lint.Direction exposing (Direction)
import Lint.Error as Error exposing (Error)
import Lint.Rule as Rule exposing (Rule)
import Lint.RuleError as RuleError exposing (RuleError)


{-| Severity associated to a rule.

  - Critical: Transgressions reported by the rule will make the linting process fail.
  - Warning: Transgressions reported by the rule will not make the linting process fail.
  - Disabled: Rule will not be enforced.

-}
type Severity
    = Disabled
    | Warning
    | Critical


{-| Lints a file and gives back the errors raised by the given rules.

    errors =
        lintSource rules source

-}
lintSource : List ( Severity, Rule ) -> String -> Result (List String) (List ( Severity, RuleError ))
lintSource rules source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                rules
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
