module Review.Test.FailureMessageTest exposing (all)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Encode
import Review.Error exposing (ReviewError)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Review.Test.FailureMessage as FailureMessage exposing (ExpectedErrorData)
import Test exposing (Test, describe, test)
import Test.Runner
import Vendor.Diff as Diff


all : Test
all =
    describe "Review.Test.FailureMessage"
        [ parsingFailureTest
        , didNotExpectErrorsTest
        , didNotExpectGlobalErrorsTest
        , messageMismatchTest
        , messageMismatchForGlobalErrorTest
        , underMismatchTest
        , unexpectedDetailsTest
        , unexpectedGlobalErrorDetailsTest
        , unexpectedConfigurationErrorTest
        , unexpectedConfigurationErrorMessageTest
        , unexpectedConfigurationErrorDetailsTest
        , emptyDetailsTest
        , wrongLocationTest
        , underMayNotBeEmptyTest
        , locationNotFoundTest
        , expectedMoreErrorsTest
        , expectedMoreGlobalErrorsTest
        , tooManyErrorsTest
        , tooManyGlobalErrorsTest
        , locationIsAmbiguousInSourceCodeTest
        , needToUsedExpectErrorsForModulesTest
        , missingSourcesTest
        , duplicateModuleNameTest
        , unknownModulesInExpectedErrorsTest
        , missingFixesTest
        , unexpectedFixesTest
        , fixedCodeMismatchTest
        , unchangedSourceAfterFixTest
        , invalidSourceAfterFixTest
        , hasCollisionsInFixRangesTest
        , unexpectedExtractTest
        , invalidJsonForExpectedDataExtractTest
        , extractMismatchTest
        , ignoredChangedResultsTest
        ]


testRuleWithMessage : (String -> { message : String, details : List String }) -> Rule
testRuleWithMessage createMessage =
    Rule.newModuleRuleSchema "TestRule" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor createMessage)
        |> Rule.fromModuleRuleSchema


testRuleReportsLiterals : Rule
testRuleReportsLiterals =
    Rule.newModuleRuleSchema "TestRule" ()
        |> Rule.withSimpleExpressionVisitor
            (expressionVisitor
                (\string ->
                    { message = "Some message including " ++ string
                    , details = [ "Some details including " ++ string ]
                    }
                )
            )
        |> Rule.fromModuleRuleSchema


expressionVisitor : (String -> { message : String, details : List String }) -> Node Expression -> List (Error {})
expressionVisitor createMessage node =
    case Node.value node of
        Expression.Literal string ->
            [ Rule.error (createMessage string) (Node.range node) ]

        _ ->
            []


testRuleWithError : (Range -> Error {}) -> Rule
testRuleWithError error =
    Rule.newModuleRuleSchema "TestRule" ()
        |> Rule.withSimpleExpressionVisitor (\node -> [ error (Node.range node) ])
        |> Rule.fromModuleRuleSchema


testRuleReportsGlobal : Rule
testRuleReportsGlobal =
    testRuleWithError
        (\_ ->
            Rule.globalError
                { message = "Some global error"
                , details = [ "Some details for global error" ]
                }
        )


expectMessageEqual : String -> String -> Expectation
expectMessageEqual expectedMessage =
    Expect.all
        [ Expect.equal <| String.trim expectedMessage
        , \receivedMessage ->
            Expect.all
                (String.lines receivedMessage
                    |> List.map
                        (\line () ->
                            String.length line
                                |> Expect.atMost 76
                                |> Expect.onFail ("Message has line longer than 76 characters:\n\n" ++ line)
                        )
                )
                ()
        ]


parsingFailureTest : Test
parsingFailureTest =
    describe "parsingFailure"
        [ test "when there is only one file" <|
            \() ->
                "module MyModule exposing (.."
                    |> Review.Test.run testRuleReportsLiterals
                    |> Review.Test.expectNoErrors
                    |> expectFailure """TEST SOURCE CODE PARSING ERROR

I could not parse the test source code, because it was not valid Elm code.

Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""
        , test "when there are multiple files" <|
            \() ->
                [ "module MyModule exposing (.."
                , "module MyOtherModule exposing (..)"
                ]
                    |> Review.Test.runOnModules testRuleReportsLiterals
                    |> Review.Test.expectNoErrors
                    |> expectFailure """TEST SOURCE CODE PARSING ERROR

I could not parse one of the test source codes, because it was not valid
Elm code.

The source code in question is the one at index 0 starting with:

  `module MyModule exposing (..`

Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""
        ]


didNotExpectErrorsTest : Test
didNotExpectErrorsTest =
    test "Unexpected errors" <|
        \() ->
            """module MyModule exposing (..)
a = "abc"
b = "def"
"""
                |> Review.Test.run testRuleReportsLiterals
                |> Review.Test.expectNoErrors
                |> expectFailure """DID NOT EXPECT ERRORS

I expected no errors for module `MyModule` but found:

  - `Some message including abc`
    at { start = { row = 2, column = 5 }, end = { row = 2, column = 10 } }
  - `Some message including def`
    at { start = { row = 3, column = 5 }, end = { row = 3, column = 10 } }
"""


didNotExpectGlobalErrorsTest : Test
didNotExpectGlobalErrorsTest =
    test "Unexpected global errors" <|
        \() ->
            """module MyModule exposing (..)
a = "abc"
"""
                |> Review.Test.run testRuleReportsGlobal
                |> Review.Test.expectNoErrors
                |> expectFailure """DID NOT EXPECT GLOBAL ERRORS

I expected no global errors but found:

  - `Some global error`
"""


messageMismatchTest : Test
messageMismatchTest =
    test "messageMismatch" <|
        \() ->
            """module MyModule exposing (..)
a = "abc"
"""
                |> Review.Test.run testRuleReportsLiterals
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Remove the use of `Debug` before shipping to production"
                        , details = [ "Some details" ]
                        , under = "\"abc\""
                        }
                    ]
                |> expectFailure """UNEXPECTED ERROR MESSAGE

I was looking for the error with the following message:

  `Remove the use of `Debug` before shipping to production`

but I found the following error message:

  `Some message including abc`"""


messageMismatchForGlobalErrorTest : Test
messageMismatchForGlobalErrorTest =
    test "messageMismatchForGlobalError" <|
        \() ->
            """module MyModule exposing (..)
a = "abc"
"""
                |> Review.Test.run testRuleReportsGlobal
                |> Review.Test.expectGlobalErrors
                    [ { message = "Remove the use of `Debug` before shipping to production"
                      , details = [ "Some details" ]
                      }
                    ]
                |> expectFailure """UNEXPECTED GLOBAL ERROR MESSAGE

I was looking for the global error with the following message:

  `Remove the use of `Debug` before shipping to production`

but I found the following error message:

  `Some global error`"""


underMismatchTest : Test
underMismatchTest =
    describe "underMismatch"
        [ test "with single-line extracts" <|
            \() ->
                """module MyModule exposing (..)
a = "abc"
"""
                    |> Review.Test.run testRuleReportsLiterals
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Some message including abc"
                            , details = [ "Some details" ]
                            , under = "a = \"abc\""
                            }
                        ]
                    |> expectFailure """UNEXPECTED ERROR LOCATION

I found an error with the following message:

  `Some message including abc`

and I was expecting it to be under:

  `a = "abc"`

but I found it under:

  `"abc"`

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`."""
        , test "with multi-line extracts" <|
            \() ->
                """module MyModule exposing (..)
a = \"\"\"some
  string\"\"\"
"""
                    |> Review.Test.run testRuleReportsLiterals
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Some message including some\n  string"
                            , details = [ "Some details" ]
                            , under = "a = \"\"\"some\n  string\"\"\""
                            }
                        ]
                    |> expectFailure """UNEXPECTED ERROR LOCATION

I found an error with the following message:

  `Some message including some
  string`

and I was expecting it to be under:

  ```
    a = \"\"\"some
      string\"\"\"
  ```

but I found it under:

  ```
    \"\"\"some
      string\"\"\"
  ```

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`.
"""
        ]


unexpectedDetailsTest : Test
unexpectedDetailsTest =
    describe "unexpectedDetails"
        [ test "with single-line details" <|
            \() ->
                """module MyModule exposing (..)
a = "abc"
"""
                    |> Review.Test.run testRuleReportsLiterals
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Some message including abc"
                            , details = [ "Not the same details" ]
                            , under = "\"abc\""
                            }
                        ]
                    |> expectFailure """UNEXPECTED ERROR DETAILS

I found an error for a file with the following message:

  `Some message including abc`

and I was expecting its details to be:

  `Not the same details`

but I found these details:

  `Some details including abc`
"""
        , test "with multi-line details" <|
            \() ->
                let
                    testRule : Rule
                    testRule =
                        testRuleWithMessage
                            (\_ ->
                                { message = "Some message"
                                , details = [ "Some", "details" ]
                                }
                            )
                in
                """module MyModule exposing (..)
a = "abc"
"""
                    |> Review.Test.run testRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Some message"
                            , details =
                                [ "Expected"
                                , "details"
                                ]
                            , under = "\"abc\""
                            }
                        ]
                    |> expectFailure """
\u{001B}[31m\u{001B}[1mUNEXPECTED ERROR DETAILS\u{001B}[22m\u{001B}[39m

I found an error for a file with the following message:

  `Some message`

and I was expecting its details to be:

  ```
  Expected

  details
  ```

but I found these details:

  ```
  Some

  details
  ```
"""
        ]


unexpectedGlobalErrorDetailsTest : Test
unexpectedGlobalErrorDetailsTest =
    test "unexpectedGlobalErrorDetails" <|
        \() ->
            """module MyModule exposing (..)
a = "abc"
"""
                |> Review.Test.run testRuleReportsGlobal
                |> Review.Test.expectGlobalErrors
                    [ { message = "Some global error"
                      , details = [ "Not the same details" ]
                      }
                    ]
                |> expectFailure """UNEXPECTED GLOBAL ERROR DETAILS

I found a global error with the following message:

  `Some global error`

and I was expecting its details to be:

  `Not the same details`

but I found these details:

  `Some details for global error`"""


unexpectedConfigurationErrorTest : Test
unexpectedConfigurationErrorTest =
    test "unexpectedConfigurationError" <|
        \() ->
            let
                rule : Rule
                rule =
                    Rule.configurationError "TestRule"
                        { message = "Some unexpected configuration message"
                        , details = [ "Some details" ]
                        }
            in
            """module MyModule exposing (..)
a = "abc"
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
                |> expectFailure """UNEXPECTED CONFIGURATION ERROR

I found a configuration error for this test:

  `Some unexpected configuration message`

  `Some details`

You should use `Review.Test.expectConfigurationError` to expect this
configuration error."""


unexpectedConfigurationErrorMessageTest : Test
unexpectedConfigurationErrorMessageTest =
    test "unexpectedConfigurationErrorMessage" <|
        \() ->
            let
                rule : Rule
                rule =
                    Rule.configurationError "TestRule"
                        { message = "Some unexpected configuration message"
                        , details = [ "Some details" ]
                        }
            in
            rule
                |> Review.Test.expectConfigurationError
                    { message = "Some configuration message"
                    , details = [ "Some details" ]
                    }
                |> expectFailure """UNEXPECTED CONFIGURATION ERROR MESSAGE

I was looking for the configuration error with the following message:

  `Some configuration message`

but I found the following error message:

  `Some unexpected configuration message`"""


unexpectedConfigurationErrorDetailsTest : Test
unexpectedConfigurationErrorDetailsTest =
    test "unexpectedConfigurationErrorDetails" <|
        \() ->
            let
                rule : Rule
                rule =
                    Rule.configurationError "TestRule"
                        { message = "Some configuration message"
                        , details = [ "Some other details" ]
                        }
            in
            rule
                |> Review.Test.expectConfigurationError
                    { message = "Some configuration message"
                    , details = [ "Some details" ]
                    }
                |> expectFailure """UNEXPECTED CONFIGURATION ERROR DETAILS

I found a configuration error with the following message:

  `Some configuration message`

and I was expecting its details to be:

  `Some details`

but I found these details:

  `Some other details`"""


emptyDetailsTest : Test
emptyDetailsTest =
    describe "Empty details"
        [ test "Regular error" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        testRuleWithError (\range -> Rule.error { message = "Some message", details = [] } range)
                in
                """module MyModule exposing (..)
a = "abc"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Some message"
                            , details = []
                            , under = "\"abc\""
                            }
                        ]
                    |> expectFailure """EMPTY ERROR DETAILS

I found an error with the following message:

  `Some message`

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives"""
        , test "Global error" <|
            \() ->
                let
                    rule : Rule
                    rule =
                        testRuleWithError (\_ -> Rule.globalError { message = "Some message", details = [] })
                in
                """module MyModule exposing (..)
a = "abc"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Some message"
                          , details = []
                          }
                        ]
                    |> expectFailure """EMPTY ERROR DETAILS

I found an error with the following message:

  `Some message`

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives"""
        , test "Configuration error" <|
            \() ->
                Rule.configurationError "TestRule" { message = "Some message", details = [] }
                    |> Review.Test.expectConfigurationError
                        { message = "Some message"
                        , details = []
                        }
                    |> expectFailure """EMPTY ERROR DETAILS

I found an error with the following message:

  `Some message`

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives"""
        ]


wrongLocationTest : Test
wrongLocationTest =
    describe "wrongLocation"
        [ test "with single-line extracts" <|
            \() ->
                let
                    error : ReviewError
                    error =
                        Review.Error.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                FailureMessage.wrongLocation
                    error
                    { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                    "abcd"
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNEXPECTED ERROR LOCATION\u{001B}[22m\u{001B}[39m

I was looking for the error with the following message:

  `Some error`

under the following code:

  `abcd`

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at:

  { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }

but I found it at:

  { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
"""
        , test "with multi-line extracts" <|
            \() ->
                let
                    error : ReviewError
                    error =
                        Review.Error.error
                            { message = "Some other error"
                            , details = [ "Some other details" ]
                            }
                            { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
                in
                FailureMessage.wrongLocation
                    error
                    { start = { row = 2, column = 1 }, end = { row = 3, column = 3 } }
                    "abcd =\n  1"
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNEXPECTED ERROR LOCATION\u{001B}[22m\u{001B}[39m

I was looking for the error with the following message:

  `Some other error`

under the following code:

  ```
    abcd =
      1
  ```

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at:

  { start = { row = 2, column = 1 }, end = { row = 3, column = 3 } }

but I found it at:

  { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
"""
        ]


locationNotFoundTest : Test
locationNotFoundTest =
    test "locationNotFound" <|
        \() ->
            let
                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
            in
            FailureMessage.locationNotFound error
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mCOULD NOT FIND LOCATION FOR ERROR\u{001B}[22m\u{001B}[39m

I was looking for the error with the following message:

  `Some error`

and I found it, but the code it points to does not lead to anything:

  { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }

Please try to have the error under the smallest region that makes sense.
This will be the most helpful for the person who reads the error message.
"""


underMayNotBeEmptyTest : Test
underMayNotBeEmptyTest =
    test "underMayNotBeEmpty" <|
        \() ->
            FailureMessage.underMayNotBeEmpty
                { message = "Some error"
                , codeAtLocation = "abcd = 1"
                }
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mCOULD NOT FIND LOCATION FOR ERROR\u{001B}[22m\u{001B}[39m

I was looking for the error with the following message:

  `Some error`

and I found it, but the expected error has an empty string for `under`. I
need to point somewhere, so as to best help the people who encounter this
error.

If this helps, this is where I found the error:

  `abcd = 1`
"""


expectedMoreErrorsTest : Test
expectedMoreErrorsTest =
    test "expectedMoreErrors" <|
        \() ->
            let
                missingErrors : List ExpectedErrorData
                missingErrors =
                    [ { message = "Remove the use of `Debug` before shipping to production"
                      , details = [ "Some details" ]
                      , under = "Debug.log"
                      }
                    , { message = "Remove the use of `Debug` before shipping to production"
                      , details = [ "Some details" ]
                      , under = "Debug.log"
                      }
                    ]
            in
            FailureMessage.expectedMoreErrors "MyModule" 5 missingErrors
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED LESS ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I expected to see 5 errors for module `MyModule` but only found 3.
Here are the 2 I could not find:

  - `Remove the use of `Debug` before shipping to production`
  - `Remove the use of `Debug` before shipping to production`
"""


expectedMoreGlobalErrorsTest : Test
expectedMoreGlobalErrorsTest =
    test "expectedMoreGlobalErrors" <|
        \() ->
            let
                missingErrors : List { message : String }
                missingErrors =
                    [ { message = "Remove the use of `Debug` before shipping to production" }
                    , { message = "Remove the use of `Debug` before shipping to production" }
                    ]
            in
            FailureMessage.expectedMoreGlobalErrors 5 missingErrors
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED LESS GLOBAL ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I expected to see 5 global errors but only found 3.
Here are the 2 I could not find:

  - `Remove the use of `Debug` before shipping to production`
  - `Remove the use of `Debug` before shipping to production`
"""


tooManyErrorsTest : Test
tooManyErrorsTest =
    describe "tooManyErrors"
        [ test "with one extra error" <|
            \() ->
                let
                    extraErrors : List ReviewError
                    extraErrors =
                        [ Review.Error.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        ]
                in
                FailureMessage.tooManyErrors "MyModule" extraErrors
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED MORE ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I found 1 error too many for module `MyModule`:

  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
"""
        , test "with multiple extra errors" <|
            \() ->
                let
                    extraErrors : List ReviewError
                    extraErrors =
                        [ Review.Error.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        , Review.Error.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                        ]
                in
                FailureMessage.tooManyErrors "MyOtherModule" extraErrors
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED MORE ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I found 2 errors too many for module `MyOtherModule`:

  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
"""
        ]


tooManyGlobalErrorsTest : Test
tooManyGlobalErrorsTest =
    describe "tooManyGlobalErrors"
        [ test "with one extra error" <|
            \() ->
                let
                    extraErrors : List { message : String }
                    extraErrors =
                        [ { message = "Remove the use of `Debug` before shipping to production" }
                        ]
                in
                FailureMessage.tooManyGlobalErrors extraErrors
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED MORE GLOBAL ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I found 1 global error too many:

  - `Remove the use of `Debug` before shipping to production`
"""
        , test "with multiple extra errors" <|
            \() ->
                let
                    extraErrors : List { message : String }
                    extraErrors =
                        [ { message = "Remove the use of `Debug` before shipping to production" }
                        , { message = "Remove the use of `Debug` before shipping to production" }
                        ]
                in
                FailureMessage.tooManyGlobalErrors extraErrors
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mRULE REPORTED MORE GLOBAL ERRORS THAN EXPECTED\u{001B}[22m\u{001B}[39m

I found 2 global errors too many:

  - `Remove the use of `Debug` before shipping to production`
  - `Remove the use of `Debug` before shipping to production`
"""
        ]


locationIsAmbiguousInSourceCodeTest : Test
locationIsAmbiguousInSourceCodeTest =
    describe "locationIsAmbiguousInSourceCode"
        [ test "with single-line extracts" <|
            \() ->
                let
                    sourceCode : String
                    sourceCode =
                        "module A exposing (..)\nabcd\nabcd"

                    under : String
                    under =
                        "abcd"

                    error : ReviewError
                    error =
                        Review.Error.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                FailureMessage.locationIsAmbiguousInSourceCode
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mAMBIGUOUS ERROR LOCATION\u{001B}[22m\u{001B}[39m

Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some error`

and expecting to see it under:

  `abcd`

I found 2 locations where that code appeared. Please use
`Review.Test.atExactly` to make the part you were targeting unambiguous.

Tip: I found them at:
  - { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
"""
        , test "with multi-line extracts" <|
            \() ->
                let
                    sourceCode : String
                    sourceCode =
                        "module A exposing (..)\nabcd =\n  1\nabcd =\n  1\nabcd =\n  1"

                    under : String
                    under =
                        "abcd =\n  1"

                    error : ReviewError
                    error =
                        Review.Error.error
                            { message = "Some other error"
                            , details = [ "Some other details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }
                in
                FailureMessage.locationIsAmbiguousInSourceCode
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mAMBIGUOUS ERROR LOCATION\u{001B}[22m\u{001B}[39m

Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some other error`

and expecting to see it under:

  ```
    abcd =
      1
  ```

I found 3 locations where that code appeared. Please use
`Review.Test.atExactly` to make the part you were targeting unambiguous.

Tip: I found them at:
  - { start = { row = 2, column = 1 }, end = { row = 3, column = 4 } }
  - { start = { row = 4, column = 1 }, end = { row = 5, column = 4 } }
  - { start = { row = 6, column = 1 }, end = { row = 7, column = 4 } }
"""
        ]


needToUsedExpectErrorsForModulesTest : Test
needToUsedExpectErrorsForModulesTest =
    test "needToUsedExpectErrorsForModules" <|
        \() ->
            FailureMessage.needToUsedExpectErrorsForModules
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mAMBIGUOUS MODULE FOR ERROR\u{001B}[22m\u{001B}[39m

You gave me several modules, and you expect some errors. I need to know for
which module you expect these errors to be reported.

You should use `Review.Test.expectErrorsForModules` to do this:

  test "..." <|
    \\() ->
      [ \"\"\"
module A exposing (..)
-- someCode
\"\"\", \"\"\"
module B exposing (..)
-- someCode
\"\"\" ]
      |> Review.Test.runOnModules rule
      |> Review.Test.expectErrorsForModules
          [ ( "B", [ Review.Test.error someError ] )
          ]"""


missingSourcesTest : Test
missingSourcesTest =
    test "missingSources" <|
        \() ->
            FailureMessage.missingSources
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mMISSING SOURCES\u{001B}[22m\u{001B}[39m

You used `runOnModules` or `runOnModulesWithProjectData` with an empty list
of sources files.

I need sources to reviewing, because reviewing an empty project does not
make much sense to me.
"""


duplicateModuleNameTest : Test
duplicateModuleNameTest =
    test "duplicateModuleName" <|
        \() ->
            FailureMessage.duplicateModuleName [ "My", "Module" ]
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mDUPLICATE MODULE NAMES\u{001B}[22m\u{001B}[39m

I found several modules named `My.Module` in the test source codes.

I expect all modules to be able to exist together in the same project,
but having several modules with the same name is not allowed by the Elm
compiler.

Please rename the modules so that they all have different names.
"""


unknownModulesInExpectedErrorsTest : Test
unknownModulesInExpectedErrorsTest =
    test "unknownModulesInExpectedErrors" <|
        \() ->
            FailureMessage.unknownModulesInExpectedErrors "My.Module"
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNKNOWN MODULES IN EXPECTED ERRORS\u{001B}[22m\u{001B}[39m

I expected errors for a module named `My.Module` in the list passed to
`expectErrorsForModules`, but I couldn't find a module in the test source
codes named that way.

I assume that there was a mistake during the writing of the test. Please
match the names of the modules in the test source codes to the ones in the
expected errors list.
"""


missingFixesTest : Test
missingFixesTest =
    test "missingFixes" <|
        \() ->
            let
                expectedError : ExpectedErrorData
                expectedError =
                    { message = "Some error"
                    , details = [ "Some details" ]
                    , under = "Debug.log"
                    }
            in
            FailureMessage.missingFixes expectedError
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mMISSING FIXES\u{001B}[22m\u{001B}[39m

I expected that the error with the following message

  `Some error`

would provide some fixes, but I didn't find any.

Hint: Maybe you forgot to call a function like `Rule.errorWithFix` or maybe
the list of provided fixes was empty."""


unexpectedFixesTest : Test
unexpectedFixesTest =
    test "unexpectedFixes" <|
        \() ->
            let
                range : Range
                range =
                    { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }

                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        range
                        |> Review.Error.withFixes [ Fix.removeRange range ]
            in
            FailureMessage.unexpectedFixes error
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNEXPECTED FIXES\u{001B}[22m\u{001B}[39m

I expected that the error with the following message

  `Some error`

would not have any fixes, but it provided some.

Because the error provides fixes, I require providing the expected result
of the automatic fix. Otherwise, there is no way to know whether the fix
will result in a correct and in the intended result.

To fix this, you can call `Review.Test.whenFixed` on your error:

  Review.Test.error
      { message = "<message>"
      , details = "<details>"
      , under = "<under>"
      }
      |> Review.Test.whenFixed "<source code>"
"""


fixedCodeMismatchTest : Test
fixedCodeMismatchTest =
    test "fixedCodeMismatch" <|
        \() ->
            let
                fixedSourceCode : String
                fixedSourceCode =
                    """module A exposing (b)
abcd =
  1"""

                expectedSourceCode : String
                expectedSourceCode =
                    """module A exposing (b)
abcd =
  2"""

                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            FailureMessage.fixedCodeMismatch
                fixedSourceCode
                expectedSourceCode
                error
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mFIXED CODE MISMATCH\u{001B}[22m\u{001B}[39m

I found a different fixed source code than expected for the error with the
following message:

  `Some error`

I expected the following result after the fixes have been applied:

  ```
    module A exposing (b)
    abcd =
      2
  ```

but I found:

  ```
    module A exposing (b)
    abcd =
      1
  ```"""


unchangedSourceAfterFixTest : Test
unchangedSourceAfterFixTest =
    test "unchangedSourceAfterFix" <|
        \() ->
            let
                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            FailureMessage.unchangedSourceAfterFix error
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNCHANGED SOURCE AFTER FIX\u{001B}[22m\u{001B}[39m

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Some error`

I expected the fix to make some changes to the source code, but it resulted
in the same source code as before the fixes.

This is problematic because I will tell the user that this rule provides an
automatic fix, but I will have to disappoint them when I later find out it
doesn't do anything.

Hint: Maybe you inserted an empty string into the source code."""


invalidSourceAfterFixTest : Test
invalidSourceAfterFixTest =
    test "invalidSourceAfterFix" <|
        \() ->
            let
                sourceCode : String
                sourceCode =
                    """ule A exposing (b)
abcd =
  1"""

                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            FailureMessage.invalidSourceAfterFix
                error
                sourceCode
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mINVALID SOURCE AFTER FIX\u{001B}[22m\u{001B}[39m

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Some error`

I was unable to parse the source code after applying the fixes. Here is
the result of the automatic fixing:

  ```
    ule A exposing (b)
    abcd =
      1
  ```

This is problematic because fixes are meant to help the user, and applying
this fix will give them more work to do. After the fix has been applied,
the problem should be solved and the user should not have to think about it
anymore. If a fix can not be applied fully, it should not be applied at
all."""


hasCollisionsInFixRangesTest : Test
hasCollisionsInFixRangesTest =
    test "hasCollisionsInFixRanges" <|
        \() ->
            let
                error : ReviewError
                error =
                    Review.Error.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            FailureMessage.hasCollisionsInFixRanges error
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mFOUND COLLISIONS IN FIX RANGES\u{001B}[22m\u{001B}[39m

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Some error`

I found that some fixes were targeting (partially or completely) the same
section of code. The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes."""


unexpectedExtractTest : Test
unexpectedExtractTest =
    test "unexpectedExtract" <|
        \() ->
            let
                extract : Encode.Value
                extract =
                    Encode.object
                        [ ( "foo", Encode.string "bar" )
                        , ( "other", Encode.list Encode.int [ 1, 2, 3 ] )
                        , ( "null", Encode.null )
                        ]
            in
            FailureMessage.unexpectedExtract extract
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mUNEXPECTED EXTRACT\u{001B}[22m\u{001B}[39m

This rule returned an extract but I did not expect one.

You should use `Review.Test.expectDataExtract` or
`Review.Test.expect`+`Review.Test.dataExtract` to assert that the extract
fits what you expected.

{
  "foo": "bar",
  "other": [
    1,
    2,
    3
  ],
  "null": null
}"""


invalidJsonForExpectedDataExtractTest : Test
invalidJsonForExpectedDataExtractTest =
    test "invalidJsonForExpectedDataExtract" <|
        \() ->
            case Decode.decodeString Decode.value "not JSON" of
                Ok _ ->
                    Expect.fail "Incorrect test setup, should have been incorrect JSON"

                Err parsingError ->
                    FailureMessage.invalidJsonForExpectedDataExtract parsingError
                        |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mINVALID JSON FOR EXPECTED DATA EXTRACT\u{001B}[22m\u{001B}[39m

The string you passed to `expectDataExtract` can't be parsed as valid JSON.

Problem with the given value:

"not JSON"

This is not valid JSON! Unexpected token o in JSON at position 1"""


extractMismatchTest : Test
extractMismatchTest =
    test "extractMismatch" <|
        \() ->
            let
                extractActual : Encode.Value
                extractActual =
                    Encode.object
                        [ ( "foo", Encode.string "bar" )
                        , ( "other", Encode.list Encode.int [ 1, 2, 3 ] )
                        , ( "actual", Encode.null )
                        ]

                extractExpected : Encode.Value
                extractExpected =
                    Encode.object
                        [ ( "foo", Encode.string "bar" )
                        , ( "other", Encode.list Encode.int [ 1, 20, 3 ] )
                        , ( "expected", Encode.object [] )
                        ]
            in
            FailureMessage.extractMismatch
                extractActual
                extractExpected
                (Diff.diffLines (Encode.encode 2 extractActual) (Encode.encode 2 extractExpected))
                |> expectMessageEqual """
\u{001B}[31m\u{001B}[1mDATA EXTRACT MISMATCH\u{001B}[22m\u{001B}[39m

I found a different extract than expected. I got the following:

{
  "foo": "bar",
  "other": [
    1,
    2,
    3
  ],
  "actual": null
}

when I was expecting the following:

{
  "foo": "bar",
  "other": [
    1,
    20,
    3
  ],
  "expected": {}
}

Here are the differences:

{
  "foo": "bar",
  "other": [
    1,
\u{001B}[31m    2,\u{001B}[39m
\u{001B}[32m    20,\u{001B}[39m
    3
  ],
\u{001B}[31m  "actual": null\u{001B}[39m
\u{001B}[32m  "expected": {}\u{001B}[39m
}"""


type alias IgnoredChangedResultsContext =
    Dict ModuleName (List ModuleName)


ignoredChangedResultsTest : Test
ignoredChangedResultsTest =
    test "Ignored changed results" <|
        \() ->
            let
                testRule : Rule
                testRule =
                    Rule.newProjectRuleSchema "TestRule" initialContext
                        |> Rule.withModuleVisitor (Rule.withSimpleModuleDefinitionVisitor (always []))
                        |> Rule.withModuleContextUsingContextCreator
                            { fromProjectToModule = fromProjectToModule
                            , fromModuleToProject = fromModuleToProject
                            , foldProjectContexts = foldProjectContexts
                            }
                        |> Rule.withFinalProjectEvaluation finalEvaluation
                        |> Rule.fromProjectRuleSchema

                initialContext : IgnoredChangedResultsContext
                initialContext =
                    Dict.empty

                fromProjectToModule : Rule.ContextCreator IgnoredChangedResultsContext ()
                fromProjectToModule =
                    Rule.initContextCreator (\_ -> ())

                fromModuleToProject : Rule.ContextCreator () IgnoredChangedResultsContext
                fromModuleToProject =
                    Rule.initContextCreator
                        (\ast isFileIgnored moduleName () ->
                            if isFileIgnored then
                                Dict.empty

                            else
                                ast.imports
                                    |> List.map (\import_ -> ( import_ |> Node.value |> .moduleName |> Node.value, [ moduleName ] ))
                                    |> Dict.fromList
                        )
                        |> Rule.withFullAst
                        |> Rule.withIsFileIgnored
                        |> Rule.withModuleName

                foldProjectContexts : IgnoredChangedResultsContext -> IgnoredChangedResultsContext -> IgnoredChangedResultsContext
                foldProjectContexts new prev =
                    Dict.merge
                        Dict.insert
                        (\key a b dict -> Dict.insert key (a ++ b) dict)
                        Dict.insert
                        new
                        prev
                        Dict.empty

                finalEvaluation : IgnoredChangedResultsContext -> List (Error { useErrorForModule : () })
                finalEvaluation refsDict =
                    refsDict
                        |> Dict.toList
                        |> List.map
                            (\( moduleName, refs ) ->
                                Rule.globalError
                                    { message = "Found imports for " ++ String.join "." moduleName
                                    , details = (String.join "." moduleName ++ " was imported by:") :: List.map (String.join ".") (List.sort refs)
                                    }
                            )
            in
            [ """module A exposing (..)
a = "abc"
""", """module B exposing (..)
import A
b = "abc"
""", """module C exposing (..)
import A
import B
c = "abc"
""" ]
                |> Review.Test.runOnModules testRule
                |> Review.Test.expectGlobalErrors
                    [ { message = "Found imports for A"
                      , details = [ "A was imported by:", "B", "C" ]
                      }
                    , { message = "Found imports for B"
                      , details = [ "B was imported by:", "C" ]
                      }
                    ]
                |> expectFailure """GOT DIFFERENT RESULTS WHEN IGNORING FILES

This rule is using `Review.Rule.withIsFileIgnored`, which gives it the
information of which files are ignored.

Without further information, I assume that this information is used solely
to improve performance of the rule, not to change the behavior in any way.
If this is not the case for your rule, then please indicate so in your test
using the `Review.Test.ignoredFilesImpactResults` function.

With that in mind, I tried re-running the test while ignoring some files
and I got different results than before.

When I ignore these files:
  - src/B.elm

then the following errors could not be found anymore:

    { message = "Found imports for A"
    , filePath = "GLOBAL ERROR"
    , details = ["A was imported by:", "B", "C"]
    , range = { start = { row = 0, column = 0 }
              , end = { row = 0, column = 0 }
              }
    , fixes = false
    }

and the following errors start appearing:

    { message = "Found imports for A"
    , filePath = "GLOBAL ERROR"
    , details = ["A was imported by:", "C"]
    , range = { start = { row = 0, column = 0 }
              , end = { row = 0, column = 0 }
              }
    , fixes = false
    }
"""


expectFailure : String -> Expectation -> Expectation
expectFailure expectedFailureMessage actualResult =
    expectFailureModifiedBy (String.trim expectedFailureMessage) actualResult


expectFailureModifiedBy : String -> Expectation -> Expectation
expectFailureModifiedBy expectedFailureMessage actualResult =
    case Test.Runner.getFailureReason actualResult of
        Nothing ->
            Expect.fail "Expected a failure, but got a pass"

        Just actualInfo ->
            Expect.all
                [ \receivedMessage ->
                    receivedMessage
                        |> Expect.equal (removeColors (String.trim expectedFailureMessage))
                , \receivedMessage ->
                    Expect.all
                        (String.lines receivedMessage
                            |> List.map
                                (\line () ->
                                    String.length line
                                        |> Expect.atMost 76
                                        |> Expect.onFail ("Message has line longer than 76 characters:\n\n" ++ line)
                                )
                        )
                        ()
                ]
                (removeColors actualInfo.description)


removeColors : String -> String
removeColors str =
    str
        |> String.replace "\u{001B}[0m" ""
        |> String.replace "\u{001B}[1m" ""
        |> String.replace "\u{001B}[2m" ""
        |> String.replace "\u{001B}[22m" ""
        |> String.replace "\u{001B}[31m" ""
        |> String.replace "\u{001B}[32m" ""
        |> String.replace "\u{001B}[37m" ""
        |> String.replace "\u{001B}[39m" ""
