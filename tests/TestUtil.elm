module TestUtil exposing (ruleTester, expectErrors)

import Expect
import Regex
import Lint.Types exposing (LintRule, LintResult, LintError)


spacesRegex : Regex.Regex
spacesRegex =
    Regex.regex "\n              "


ruleTester : LintRule -> LintRule
ruleTester rule str =
    Regex.replace Regex.All spacesRegex (\_ -> "\n") str
        |> rule


expectErrors : List LintError -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err _ ->
            Expect.fail "Parsing failure"

        Ok errors ->
            Expect.equal expectedErrors errors
