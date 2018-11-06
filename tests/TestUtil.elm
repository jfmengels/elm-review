module TestUtil exposing (expectErrors, ruleTester)

import Expect
import Lint exposing (parseSource)
import Lint.Types exposing (LintError, LintResult, LintRule)


ruleTester : LintRule -> String -> LintResult
ruleTester rule str =
    parseSource str
        |> Result.map rule


expectErrors : List LintError -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err errors ->
            Expect.fail <| String.join "\n" errors

        Ok errors ->
            Expect.equal expectedErrors errors
