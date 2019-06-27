module Lint.Test exposing (LintResult, errorWithoutRange, expectErrors, expectErrorsWithoutRange, location, run)

{-| Module that helps you test your linting rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest).

TODO Add instructions and examples

TODO Add helpful tips

TODO Rework API, we can do something much nicer than this

-}

import Elm.Syntax.Range exposing (Range)
import Expect
import Lint exposing (Severity(..), lintSource)
import Lint.Rule as Rule exposing (Error, Rule)


{-| Alias for the result of a lint rule being applied on a string containing Elm code.
-}
type alias LintResult =
    Result (List String) (List Error)


{-| Run a `Rule` on a string and get the errors reported by it. If the string is
not valid Elm code, this will return a `Result` of type `Err`.

Note that to be valid, a code needs to start with a module definition followed by
a line break like `module A exposing (..)\n`.

    import Lint.Test exposing (LintResult)
    import Test

    testRule : String -> LintResult
    testRule string =
        "module A exposing (..)\n\n"
            ++ string
            |> Lint.Test.run rule

    tests : List Test
    tests =
        [ test "should not report normal function calls" <|
            \() ->
                testRule "a = Debug.log"
                    |> Lint.Test.expectErrors
                        [ error (Lint.Test.location ( 3, 5 ) ( 3, 14 )) ]
        ]

-}
run : Rule -> String -> Result (List String) (List Error)
run rule str =
    lintSource [ ( Critical, rule ) ] str
        |> Result.map (List.map (\( severity, { message, range } ) -> Rule.error message range))


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
    List.map Rule.errorMessage errors


errorWithoutRange : String -> Error
errorWithoutRange message =
    Rule.error message (location ( 0, 0 ) ( 0, 0 ))


location : ( Int, Int ) -> ( Int, Int ) -> Range
location ( rowStart, columnStart ) ( rowEnd, columnEnd ) =
    { start = { row = rowStart, column = columnStart }
    , end = { row = rowEnd, column = columnEnd }
    }
