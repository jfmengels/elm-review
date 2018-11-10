module TestUtil exposing (expectErrors, ruleTester)

import Expect
import Lint exposing (Rule, parseSource)
import Lint.Error exposing (Error)
import Lint.Types exposing (LintResult)


ruleTester : Rule -> String -> LintResult
ruleTester rule str =
    parseSource str
        |> Result.map rule


expectErrors : List Error -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err errors ->
            Expect.fail <| String.join "\n" errors

        Ok errors ->
            Expect.equal expectedErrors errors
