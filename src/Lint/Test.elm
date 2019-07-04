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


type SourceCode
    = SourceCode String


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
                { getCodeAtLocation = getCodeAtLocationInSourceCode (SourceCode sourceCode)
                , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode (SourceCode sourceCode)
                }
                (List.map (\( _, error_ ) -> Rule.error error_.message error_.range) errors)

        Err _ ->
            ParseFailure


{-| Assert that the rule reprted no errors. Note, this is equivalent to using [`expectErrors`](#expectErrors)
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
            Expect.fail parsingErrorMessage

        SuccessfulRun _ errors ->
            Expect.true
                ("I expected no errors but found:\n\n" ++ (List.map errorToString errors |> String.join "\n"))
                (List.isEmpty errors)


{-| Assert that the rule reprted some errors, by specifying which one.

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
            Expect.fail parsingErrorMessage

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


getUnder : ExpectedError -> String
getUnder (ExpectedError expectedError) =
    case expectedError.under of
        Under str ->
            str

        UnderExactly str _ ->
            str


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
            [ always <| Expect.fail <| notEnoughErrors expected restOfExpectedErrors ]

        ( [], error_ :: restOfErrors ) ->
            [ always <| Expect.fail <| tooManyErrors error_ restOfErrors ]


checkErrorMatch : CodeInspector -> ExpectedError -> Error -> (() -> Expectation)
checkErrorMatch codeInspector ((ExpectedError expectedError_) as expectedError) error_ =
    Expect.all
        [ always <| Expect.true (messageMismatchError expectedError error_) (expectedError_.message == Rule.errorMessage error_)
        , checkMessageAppearsUnder codeInspector error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> Error -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ (ExpectedError expectedError) =
    case codeInspector.getCodeAtLocation (Rule.errorRange error_) of
        Just codeAtLocation ->
            case expectedError.under of
                Under under ->
                    Expect.all
                        [ always <| Expect.true (underMismatchError error_ under codeAtLocation) (codeAtLocation == under)
                        , always <| codeInspector.checkIfLocationIsAmbiguous error_ under
                        ]

                UnderExactly under range ->
                    Expect.all
                        [ always <| Expect.true (underMismatchError error_ under codeAtLocation) (codeAtLocation == under)
                        , always <| Expect.true (wrongLocationError error_ range under) (Rule.errorRange error_ == range)
                        ]

        Nothing ->
            always <| Expect.fail impossibleStateError


getCodeAtLocationInSourceCode : SourceCode -> Range -> Maybe String
getCodeAtLocationInSourceCode (SourceCode sourceCode) =
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


formatSourceCode : String -> String
formatSourceCode string =
    let
        lines =
            String.lines string
    in
    if List.length lines == 1 then
        "`" ++ string ++ "`"

    else
        lines
            |> List.map (\str -> "    " ++ str)
            |> String.join "\n"
            |> (\str -> "\n\n```\n" ++ str ++ "\n```")


checkIfLocationIsAmbiguousInSourceCode : SourceCode -> Error -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode ((SourceCode sourceCodeContent) as sourceCode) error_ under =
    let
        occurrencesInSourceCode : List Int
        occurrencesInSourceCode =
            String.indexes under sourceCodeContent
    in
    Expect.true
        (locationIsAmbiguousInSourceCodeError sourceCode error_ under occurrencesInSourceCode)
        (List.length occurrencesInSourceCode == 1)



-- ERROR MESSAGES


parsingErrorMessage : String
parsingErrorMessage =
    """I could not parse the test source code, because it was not syntactically valid Elm code.

Maybe you forgot to add the module definition at the top, like:

  module A exposing (..)"""


messageMismatchError : ExpectedError -> Error -> String
messageMismatchError (ExpectedError expectedError) error_ =
    """I was looking for the error with the following message:

  `""" ++ expectedError.message ++ """`

but I found the following error message:

  `""" ++ Rule.errorMessage error_ ++ "`"


wrongLocationError : Error -> Range -> String -> String
wrongLocationError error_ range under =
    """I was looking for the error with the following message:

  `""" ++ Rule.errorMessage error_ ++ """`

under the following code:

  """ ++ formatSourceCode under ++ """

and I found it. But there are multiple occurrences for this piece of code, and the exact location you specified is not the one I found. I was expecting the error at:

  """ ++ rangeAsString range ++ """

but I found it at:

  """ ++ rangeAsString (Rule.errorRange error_)


underMismatchError : Error -> String -> String -> String
underMismatchError error_ under codeAtLocation =
    """I found an error with the right message, but at the wrong location:

Message: `""" ++ Rule.errorMessage error_ ++ """`

I saw it under: """ ++ formatSourceCode codeAtLocation ++ """

But I expected to see it under: """ ++ formatSourceCode under


listOccurrencesAsLocations : SourceCode -> String -> List Int -> String
listOccurrencesAsLocations sourceCode under occurrences =
    occurrences
        |> List.map
            (\occurrence ->
                occurrence
                    |> positionAsRange sourceCode under
                    |> rangeAsString
                    |> (++) "  - "
            )
        |> String.join "\n"


positionAsRange : SourceCode -> String -> Int -> Range
positionAsRange (SourceCode sourceCode) under position =
    let
        linesBeforeAndIncludingPosition : List String
        linesBeforeAndIncludingPosition =
            sourceCode
                |> String.slice 0 position
                |> String.lines

        startRow : Int
        startRow =
            List.length linesBeforeAndIncludingPosition
    in
    -- TODO Get these values right
    { start =
        { row = startRow
        , column = -1
        }
    , end =
        { row = startRow + (under |> String.indexes "\n" |> List.length)
        , column = under |> String.indexes "\n" |> List.length
        }
    }


errorToString : Error -> String
errorToString error_ =
    "- \"" ++ Rule.errorMessage error_ ++ "\" at " ++ rangeAsString (Rule.errorRange error_)


rangeAsString : Range -> String
rangeAsString { start, end } =
    "{ start = { row = " ++ String.fromInt start.row ++ ", column = " ++ String.fromInt start.column ++ " }, end = { row = " ++ String.fromInt end.row ++ ", column = " ++ String.fromInt end.column ++ " } }"


notEnoughErrors : ExpectedError -> List ExpectedError -> String
notEnoughErrors expected restOfExpectedErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length restOfExpectedErrors + 1
    in
    "I expected to see "
        ++ String.fromInt numberOfErrors
        ++ " more "
        ++ pluralizeErrors numberOfErrors
        ++ ":\n\n"
        ++ (List.map expectedErrorToString (expected :: restOfExpectedErrors) |> String.join "\n")


wrapInQuotes : String -> String
wrapInQuotes string =
    "\"" ++ string ++ "\""


tooManyErrors : Error -> List Error -> String
tooManyErrors error_ restOfErrors =
    let
        numberOfErrors : Int
        numberOfErrors =
            List.length restOfErrors + 1
    in
    "I found "
        ++ String.fromInt numberOfErrors
        ++ " "
        ++ pluralizeErrors numberOfErrors
        ++ " too many:\n"
        ++ (List.map errorToString (error_ :: restOfErrors) |> String.join "\n")


locationIsAmbiguousInSourceCodeError : SourceCode -> Error -> String -> List Int -> String
locationIsAmbiguousInSourceCodeError sourceCode error_ under occurrencesInSourceCode =
    """Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

`""" ++ Rule.errorMessage error_ ++ """`

and expecting to see it under:

""" ++ formatSourceCode under ++ """

I found """ ++ String.fromInt (List.length occurrencesInSourceCode) ++ """ locations where that code appeared. Please use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

Tip: I found them at:
""" ++ listOccurrencesAsLocations sourceCode under occurrencesInSourceCode


impossibleStateError : String
impossibleStateError =
    "Oh no! I'm in an impossible state. I found an error at a location that I could not find back. Please let me know and give me an SSCCE (http://sscce.org/) here: https://github.com/jfmengels/elm-lint/issues."


pluralizeErrors : Int -> String
pluralizeErrors n =
    case n of
        1 ->
            "error"

        _ ->
            "errors"


expectedErrorToString : ExpectedError -> String
expectedErrorToString (ExpectedError expectedError) =
    "- " ++ wrapInQuotes expectedError.message
