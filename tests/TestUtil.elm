module TestUtil exposing (ruleTester, expectErrors)

import Expect
import Regex
import Lint exposing (parseSource)
import Lint.Types exposing (LintRule, LintResult, LintError)


spacesRegex : Regex.Regex
spacesRegex =
    Regex.regex "\n              "


ruleTester : LintRule -> String -> LintResult
ruleTester rule str =
    str
        |> Regex.replace Regex.All spacesRegex (\_ -> "\n")
        |> parseSource
        |> Result.map rule


expectErrors : List LintError -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err _ ->
            Expect.fail "Parsing failure"

        Ok errors ->
            Expect.equal expectedErrors errors
