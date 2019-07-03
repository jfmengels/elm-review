module Lint exposing
    ( Severity(..)
    , LintError, lintSource
    )

{-| Module to configure your linting configuration and run it on a source file.


# Configuration

@docs Severity


# Linting

@docs LintError, lintSource

-}

import Elm.Parser as Parser
import Elm.Processing exposing (init, process)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Range exposing (Range)
import Lint.Rule as Rule exposing (Rule)


{-| When configuring `elm-lint` and adding the rules you want to enforce, you need to associate a `Severity` level to each rule.

  - `Critical` - The rule is enforced and any patterns that the rule finds will be reported. The (yet non-existent) `elm-lint` CLI will fail with an error code if anything gets reported.
  - `Warning` - The rule is enforced and any patterns that the rule finds will be reported. But the (yet non-existent) `elm-lint` CLI will not fail with an error code even if something gets reported.
  - `Disabled` - The associated rule is not enforced and nothing will get reported for this rule.

-}
type Severity
    = Disabled
    | Warning
    | Critical


{-| Represents an error in a file found by a rule.

Note: This should not be confused with `Error` from the `Lint.Rule` module.
`Lint.LintError` is created from `Lint.Rule.Error` but contains additional information
like the name of the rule that emitted it and the file name.

-}
type alias LintError =
    { file : Maybe String
    , ruleName : String
    , message : String
    , range : Range
    }


{-| Lints a file and gives back the errors raised by the given rules.

    config : List ( Severity, Rule )
    config =
        [ ( Warning, Lint.Rule.NoDebug.rule )
        , ( Critical, Lint.Rule.NoUnusedVariables.rule )
        ]

    result =
        lintSource config sourceCode

-}
lintSource : List ( Severity, Rule ) -> String -> Result (List String) (List ( Severity, LintError ))
lintSource config source =
    source
        |> parseSource
        |> Result.map
            (\statements ->
                config
                    |> List.concatMap (lintSourceWithRule statements)
                    |> List.sortWith compareErrorPositions
            )


lintSourceWithRule : File -> ( Severity, Rule ) -> List ( Severity, LintError )
lintSourceWithRule file ( severity, rule ) =
    Rule.analyzer rule file
        |> List.map (\error -> ( severity, errorToRuleError Nothing rule error ))


compareErrorPositions : ( Severity, LintError ) -> ( Severity, LintError ) -> Order
compareErrorPositions ( _, a ) ( _, b ) =
    compareRange a.range b.range


compareRange : Range -> Range -> Order
compareRange a b =
    if a.start.row < b.start.row then
        LT

    else if a.start.row > b.start.row then
        GT

    else
    -- Start row is the same from here on
    if
        a.start.column < b.start.column
    then
        LT

    else if a.start.column > b.start.column then
        GT

    else
    -- Start row and column are the same from here on
    if
        a.end.row < b.end.row
    then
        LT

    else if a.end.row > b.end.row then
        GT

    else
    -- Start row and column, and end row are the same from here on
    if
        a.end.column < b.end.column
    then
        LT

    else if a.end.column > b.end.column then
        GT

    else
        EQ


errorToRuleError : Maybe String -> Rule -> Rule.Error -> LintError
errorToRuleError file rule error =
    { file = file
    , ruleName = Rule.name rule
    , message = Rule.errorMessage error
    , range = Rule.errorRange error
    }


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
