module TestUtil exposing (expectErrors, expectErrorsWithoutRange, location, ruleTester)

import Elm.Syntax.Range exposing (Range)
import Expect
import Lint exposing (Rule, Severity(..), lintSource)
import Lint.Error as Error exposing (Error)
import Lint.Rule exposing (LintResult)


ruleTester : Rule -> String -> Result (List String) (List Error)
ruleTester rule str =
    lintSource [ ( Critical, rule ) ] str
        |> Result.map (List.map (\( severity, { message, range } ) -> Error.create message range))


expectErrors : List Error -> LintResult -> Expect.Expectation
expectErrors expectedErrors result =
    case result of
        Err errors ->
            Expect.fail <| String.join "\n" errors

        Ok errors ->
            Expect.equal expectedErrors errors


expectErrorsWithoutRange : List Error -> LintResult -> Expect.Expectation
expectErrorsWithoutRange expectedErrors result =
    case result of
        Err errors ->
            Expect.fail <| String.join "\n" errors

        Ok errors ->
            Expect.equal
                (errorMessages expectedErrors)
                (errorMessages errors)


errorMessages : List Error -> List String
errorMessages errors =
    List.map Error.message errors


location : ( Int, Int ) -> ( Int, Int ) -> Range
location ( rowStart, columnStart ) ( rowEnd, columnEnd ) =
    { start = { row = rowStart, column = columnStart }
    , end = { row = rowEnd, column = columnEnd }
    }
