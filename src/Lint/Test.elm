module Lint.Test exposing
    ( LintResult, run
    , ExpectedError, expectErrors, expectNoErrors, error, atExactly
    )

{-| Module that helps you test your linting rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest).

    import Lint.Test exposing (LintResult)
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    testRule : String -> LintResult
    testRule string =
        Lint.Test.run rule string

    -- In this example, the rule we're testing is `NoDebug`
    tests : Test
    tests =
        describe "NoDebug"
            [ test "should not report calls to normal functions" <|
                \() ->
                    testRule """module A exposing (..)
    a = foo n"""
                        |> Lint.Test.expectNoErrors
            , test "should report Debug.log use" <|
                \() ->
                    testRule """module A exposing (..)
    a = Debug.log "some" "message\""""
                        |> Lint.Test.expectErrors
                            [ Lint.Test.error
                                { message = "Forbidden use of Debug"
                                , under = "Debug.log"
                                }
                            ]
            ]


# Running tests

@docs LintResult, run


# Making assertions

@docs ExpectedError, expectErrors, expectNoErrors, error, atExactly


# Tips on testing


## What should you test?

TODO Add helpful tips

-}

import Array exposing (Array)
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Lint exposing (Severity(..), lintSource)
import Lint.Rule as Rule exposing (Error, Rule)
import Lint.Test.ErrorMessage as ErrorMessage


{-| The result of running a rule on a `String` containing source code.
-}
type LintResult
    = ParseFailure
    | SuccessfulRun CodeInspector (List Error)


type alias CodeInspector =
    { getCodeAtLocation : Range -> Maybe String
    , checkIfLocationIsAmbiguous : Error -> String -> Expectation
    }


{-| An expectation for an error. Use [`error`](#error) to create one.
-}
type ExpectedError
    = ExpectedError
        { message : String
        , under : Under
        }


type Under
    = Under String
    | UnderExactly String Range


type alias SourceCode =
    String


{-| Run a `Rule` on a `String` containing source code. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrors`](#expectErrors) to assert
the errors reported by the rule.

The source code needs to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that t be syntactically valid, you need at least a module declaration at the
top of the file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

-}
run : Rule -> String -> LintResult
run rule sourceCode =
    case lintSource [ ( Critical, rule ) ] sourceCode of
        Ok errors ->
            SuccessfulRun
                { getCodeAtLocation = getCodeAtLocationInSourceCode sourceCode
                , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode sourceCode
                }
                (List.map (\( _, error_ ) -> Rule.error error_.message error_.range) errors)

        Err _ ->
            ParseFailure


{-| Assert that the rule reported no errors. Note, this is equivalent to using [`expectErrors`](#expectErrors)
like `expectErrors []`.

    import Lint.Test exposing (LintResult)
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    testRule : String -> LintResult
    testRule string =
        Lint.Test.run rule string

    -- In this example, the rule we're testing is `NoDebug`
    tests : Test
    tests =
        describe "NoDebug"
            [ test "should not report calls to normal functions" <|
                \() ->
                    testRule """module A exposing (..)
    a = foo n"""
                        |> Lint.Test.expectNoErrors
            ]

-}
expectNoErrors : LintResult -> Expectation
expectNoErrors lintResult =
    case lintResult of
        ParseFailure ->
            Expect.fail ErrorMessage.parsingFailure

        SuccessfulRun _ errors ->
            Expect.true
                (ErrorMessage.didNotExpectErrors errors)
                (List.isEmpty errors)


{-| Assert that the rule reported some errors, by specifying which one.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

The errors should be in the order of where they appear in the source code. An error
at the start of the source code should appear earlier in the list than
an error at the end of the source code.

    import Lint.Test exposing (LintResult)
    import Test exposing (Test, describe, test)
    import The.Rule.You.Want.To.Test exposing (rule)

    testRule : String -> LintResult
    testRule string =
        Lint.Test.run rule string

    -- In this example, the rule we're testing is `NoDebug`
    tests : Test
    tests =
        describe "NoDebug"
            [ test "should report Debug.log use" <|
                \() ->
                    testRule """module A exposing (..)
    a = Debug.log "some" "message\""""
                        |> Lint.Test.expectErrors
                            [ Lint.Test.error
                                { message = "Forbidden use of Debug"
                                , under = "Debug.log"
                                }
                            ]
            ]

-}
expectErrors : List ExpectedError -> LintResult -> Expectation
expectErrors expectedErrors lintResult =
    case lintResult of
        ParseFailure ->
            Expect.fail ErrorMessage.parsingFailure

        SuccessfulRun codeInspector errors ->
            checkAllErrorsMatch codeInspector expectedErrors errors


{-| Create an expectation for an error.

`message` should be the message you're expecting to be shown to the user.

`under` is the part of the code where you are expecting the error to be shown to
the user. If it helps, imagine `under` to be the text under which the squiggly
lines will appear if the error appeared in an editor.

    tests : Test
    tests =
        describe "NoDebug"
            [ test "should report Debug.log use" <|
                \() ->
                    testRule """module A exposing (..)
    a = Debug.log "some" "message\""""
                        |> Lint.Test.expectErrors
                            [ Lint.Test.error
                                { message = "Forbidden use of Debug"
                                , under = "Debug.log"
                                }
                            ]
            ]

If there are multiple locations where the value of `under` appears, the test will
fail unless you use [`atExactly`](#atExactly) to remove any ambiguity of where the
error should be used.

-}
error : { message : String, under : String } -> ExpectedError
error input =
    ExpectedError
        { message = input.message
        , under = Under input.under
        }


{-| Precise the exact position where the error should be shown to the user. This
is only necessary when the `under` field is ambiguous.

`atExactly` takes a record with start and end positions.

    tests : Test
    tests =
        describe "NoDebug"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    testRule """
              a = Debug.log z
              b = Debug.log z
              """
                        |> Lint.Test.expectErrors
                            [ Lint.Test.error
                                { message = message
                                , under = "Debug.log"
                                }
                                |> Lint.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                            , Lint.Test.error
                                { message = message
                                , under = "Debug.log"
                                }
                                |> Lint.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                            ]
            ]

Tip: By default, do not provide this field. If the test fails because there is some
ambiguity, the test error will give you a recommendation of what to use as a parameter
of `atExactly`, so you do not have to bother writing this hard to write argument.

-}
atExactly : { start : { row : Int, column : Int }, end : { row : Int, column : Int } } -> ExpectedError -> ExpectedError
atExactly range ((ExpectedError expectedError_) as expectedError) =
    ExpectedError { expectedError_ | under = UnderExactly (getUnder expectedError) range }


getUnder : ExpectedError -> String
getUnder (ExpectedError expectedError) =
    case expectedError.under of
        Under str ->
            str

        UnderExactly str _ ->
            str


getCodeAtLocationInSourceCode : SourceCode -> Range -> Maybe String
getCodeAtLocationInSourceCode sourceCode =
    let
        lines : Array String
        lines =
            String.lines sourceCode
                |> Array.fromList
    in
    \{ start, end } ->
        if start.row == end.row then
            Array.get (start.row - 1) lines
                |> Maybe.map (String.slice (start.column - 1) (end.column - 1))

        else
            let
                firstLine : Maybe String
                firstLine =
                    Array.get (start.row - 1) lines
                        |> Maybe.map (String.dropLeft (start.column - 1))

                lastLine : Maybe String
                lastLine =
                    Array.get (end.row - 1) lines
                        |> Maybe.map (String.dropRight end.column)
            in
            [ [ firstLine ]
            , Array.slice start.row (end.row - 1) lines
                |> Array.toList
                |> List.map Just
            , [ lastLine ]
            ]
                |> List.concat
                |> List.filterMap identity
                |> String.join "\n"
                |> Just


checkIfLocationIsAmbiguousInSourceCode : SourceCode -> Error -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode error_ under =
    let
        occurrencesInSourceCode : List Int
        occurrencesInSourceCode =
            String.indexes under sourceCode
    in
    Expect.true
        (ErrorMessage.locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode)
        (List.length occurrencesInSourceCode == 1)



-- RUNNING THE CHECKS


checkAllErrorsMatch : CodeInspector -> List ExpectedError -> List Error -> Expectation
checkAllErrorsMatch codeInspector expectedErrors errors =
    checkErrorsMatch codeInspector expectedErrors errors
        |> List.reverse
        |> (\expectations -> Expect.all expectations ())


checkErrorsMatch : CodeInspector -> List ExpectedError -> List Error -> List (() -> Expectation)
checkErrorsMatch codeInspector expectedErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( expected :: restOfExpectedErrors, error_ :: restOfErrors ) ->
            checkErrorMatch codeInspector expected error_ :: checkErrorsMatch codeInspector restOfExpectedErrors restOfErrors

        ( expected :: restOfExpectedErrors, [] ) ->
            [ always <| Expect.fail <| ErrorMessage.expectedMoreErrors <| List.map extractExpectedErrorData (expected :: restOfExpectedErrors) ]

        ( [], error_ :: restOfErrors ) ->
            [ always <| Expect.fail <| ErrorMessage.tooManyErrors (error_ :: restOfErrors) ]


checkErrorMatch : CodeInspector -> ExpectedError -> Error -> (() -> Expectation)
checkErrorMatch codeInspector ((ExpectedError expectedError_) as expectedError) error_ =
    Expect.all
        [ \_ ->
            (expectedError_.message == Rule.errorMessage error_)
                |> Expect.true
                    (ErrorMessage.messageMismatch
                        (extractExpectedErrorData expectedError)
                        error_
                    )
        , checkMessageAppearsUnder codeInspector error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> Error -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ (ExpectedError expectedError) =
    case codeInspector.getCodeAtLocation (Rule.errorRange error_) of
        Just codeAtLocation ->
            case expectedError.under of
                Under under ->
                    Expect.all
                        [ always <|
                            Expect.true
                                (ErrorMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                                (codeAtLocation == under)
                        , always <| codeInspector.checkIfLocationIsAmbiguous error_ under
                        ]

                UnderExactly under range ->
                    Expect.all
                        [ always <|
                            Expect.true
                                (ErrorMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                                (codeAtLocation == under)
                        , always <|
                            Expect.true
                                (ErrorMessage.wrongLocation error_ range under)
                                (Rule.errorRange error_ == range)
                        ]

        Nothing ->
            always <| Expect.fail ErrorMessage.impossibleState


extractExpectedErrorData : ExpectedError -> ErrorMessage.ExpectedErrorData
extractExpectedErrorData ((ExpectedError expectedErrorContent) as expectedError) =
    { message = expectedErrorContent.message
    , under = getUnder expectedError
    }
