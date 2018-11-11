module TestUtil exposing (expectErrors, ruleTester)

import Expect
import Lint exposing (Rule, Severity(..), lintSource)
import Lint.Error exposing (Error)
import Lint.Rule exposing (LintResult)


ruleTester : Rule -> String -> LintResult
ruleTester rule str =
    lintSource [ ( Critical, rule ) ] str
        |> Result.map (List.map (\( severity, error ) -> error))



-- |> Result.map rule


expectErrors : List Error -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err errors ->
            Expect.fail <| String.join "\n" errors

        Ok errors ->
            Expect.equal expectedErrors errors
