module ErrorMessageTest exposing (all)

import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Lint.Fix as Fix
import Lint.Rule as Rule exposing (Error)
import Lint.Test.ErrorMessage as ErrorMessage exposing (ExpectedErrorData)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Test.ErrorMessage"
        [ parsingFailureTest
        , didNotExpectErrorsTest
        , messageMismatchTest
        , underMismatchTest
        , unexpectedDetailsTest
        , emptyDetailsTest
        , wrongLocationTest
        , expectedMoreErrorsTest
        , tooManyErrorsTest
        , locationIsAmbiguousInSourceCodeTest
        , missingFixesTest
        , unexpectedFixesTest
        , fixedCodeMismatchTest
        , unchangedSourceAfterFixTest
        , invalidSourceAfterFixTest
        , hasCollisionsInFixRangesTest
        ]


expectMessageEqual : String -> String -> Expectation
expectMessageEqual expectedMessage =
    Expect.all
        [ Expect.equal <| String.trim expectedMessage
        , \receivedMessage ->
            Expect.all
                (String.lines receivedMessage
                    |> List.map
                        (\line () ->
                            (String.length line <= 76)
                                |> Expect.true ("Message has line longer than 76 characters:\n\n" ++ line)
                        )
                )
                ()
        ]


parsingFailureTest : Test
parsingFailureTest =
    test "parsingFailure" <|
        \() ->
            ErrorMessage.parsingFailure
                |> expectMessageEqual """
TEST SOURCE CODE PARSING ERROR

I could not parse the test source code, because it was not valid Elm code.

Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""


didNotExpectErrorsTest : Test
didNotExpectErrorsTest =
    test "didNotExpectErrors" <|
        \() ->
            let
                errors : List Error
                errors =
                    [ Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        dummyRange
                    , Rule.error
                        { message = "Some other error"
                        , details = [ "Some other details" ]
                        }
                        dummyRange
                    ]
            in
            ErrorMessage.didNotExpectErrors errors
                |> expectMessageEqual """
DID NOT EXPECT ERRORS

I expected no errors but found:

  - `Some error`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - `Some other error`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
"""


messageMismatchTest : Test
messageMismatchTest =
    test "messageMismatch" <|
        \() ->
            let
                expectedError : ExpectedErrorData
                expectedError =
                    { message = "Remove the use of `Debug` before shipping to production"
                    , details = [ "Some details" ]
                    , under = "Debug.log"
                    }

                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        dummyRange
            in
            ErrorMessage.messageMismatch expectedError error
                |> expectMessageEqual """
UNEXPECTED ERROR MESSAGE

I was looking for the error with the following message:

  `Remove the use of `Debug` before shipping to production`

but I found the following error message:

  `Some error`"""


underMismatchTest : Test
underMismatchTest =
    describe "underMismatch"
        [ test "with single-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            dummyRange
                in
                ErrorMessage.underMismatch
                    error
                    { under = "abcd"
                    , codeAtLocation = "abcd = 1"
                    }
                    |> expectMessageEqual """
UNEXPECTED ERROR LOCATION

I found an error with the following message:

  `Some error`

which I was expecting, but I found it under:

  `abcd = 1`

when I was expecting it under:

  `abcd`

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`."""
        , test "with multi-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            { message = "Some other error"
                            , details = [ "Some other details" ]
                            }
                            dummyRange
                in
                ErrorMessage.underMismatch
                    error
                    { under = "abcd =\n  1\n  + 2"
                    , codeAtLocation = "abcd =\n  1"
                    }
                    |> expectMessageEqual """
UNEXPECTED ERROR LOCATION

I found an error with the following message:

  `Some other error`

which I was expecting, but I found it under:

  ```
    abcd =
      1
  ```

when I was expecting it under:

  ```
    abcd =
      1
      + 2
  ```

Hint: Maybe you're passing the `Range` of a wrong node when
calling `Rule.error`."""
        ]


unexpectedDetailsTest : Test
unexpectedDetailsTest =
    describe "unexpectedDetails"
        [ test "with single-line details" <|
            \() ->
                let
                    expectedDetails : List String
                    expectedDetails =
                        [ "Some details" ]

                    error : Error
                    error =
                        Rule.error
                            { message = "Some error"
                            , details = [ "Some other details" ]
                            }
                            dummyRange
                in
                ErrorMessage.unexpectedDetails
                    expectedDetails
                    error
                    |> expectMessageEqual """
UNEXPECTED ERROR DETAILS

I found an error with the following message:

  `Some error`

which I was expecting, but its details were:

  `Some other details`

when I was expecting them to be:

  `Some details`"""
        , test "with multi-line details" <|
            \() ->
                let
                    expectedDetails : List String
                    expectedDetails =
                        [ "Some"
                        , "details"
                        ]

                    error : Error
                    error =
                        Rule.error
                            { message = "Some other error"
                            , details =
                                [ "Some"
                                , "other"
                                , "details"
                                ]
                            }
                            dummyRange
                in
                ErrorMessage.unexpectedDetails
                    expectedDetails
                    error
                    |> expectMessageEqual """
UNEXPECTED ERROR DETAILS

I found an error with the following message:

  `Some other error`

which I was expecting, but its details were:

  ```
  Some

  other

  details
  ```

when I was expecting them to be:

  ```
  Some

  details
  ```
"""
        ]


emptyDetailsTest : Test
emptyDetailsTest =
    describe "emptyDetails"
        [ test "with single-line details" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            dummyRange
                in
                ErrorMessage.emptyDetails
                    error
                    |> expectMessageEqual """
EMPTY ERROR DETAILS

I found an error with the following message:

  `Some error`

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- give suggestions on how to solve the problem or alternatives"""
        ]


wrongLocationTest : Test
wrongLocationTest =
    describe "wrongLocation"
        [ test "with single-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                ErrorMessage.wrongLocation
                    error
                    { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                    "abcd"
                    |> expectMessageEqual """
UNEXPECTED ERROR LOCATION

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
                    error : Error
                    error =
                        Rule.error
                            { message = "Some other error"
                            , details = [ "Some other details" ]
                            }
                            { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
                in
                ErrorMessage.wrongLocation
                    error
                    { start = { row = 2, column = 1 }, end = { row = 3, column = 3 } }
                    "abcd =\n  1"
                    |> expectMessageEqual """
UNEXPECTED ERROR LOCATION

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
            ErrorMessage.expectedMoreErrors missingErrors
                |> expectMessageEqual """
RULE REPORTED LESS ERRORS THAN EXPECTED

I expected to see 2 more errors:

  - `Remove the use of `Debug` before shipping to production`
  - `Remove the use of `Debug` before shipping to production`
"""


tooManyErrorsTest : Test
tooManyErrorsTest =
    describe "tooManyErrors"
        [ test "with one extra error" <|
            \() ->
                let
                    extraErrors : List Rule.Error
                    extraErrors =
                        [ Rule.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        ]
                in
                ErrorMessage.tooManyErrors extraErrors
                    |> expectMessageEqual """
RULE REPORTED MORE ERRORS THAN EXPECTED

I found 1 error too many:

  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
"""
        , test "with multiple extra errors" <|
            \() ->
                let
                    extraErrors : List Rule.Error
                    extraErrors =
                        [ Rule.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        , Rule.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Some details about Debug" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                        ]
                in
                ErrorMessage.tooManyErrors extraErrors
                    |> expectMessageEqual """
RULE REPORTED MORE ERRORS THAN EXPECTED

I found 2 errors too many:

  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - `Remove the use of `Debug` before shipping to production`
    at { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
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

                    error : Error
                    error =
                        Rule.error
                            { message = "Some error"
                            , details = [ "Some details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                ErrorMessage.locationIsAmbiguousInSourceCode
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> expectMessageEqual """
AMBIGUOUS ERROR LOCATION

Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some error`

and expecting to see it under:

  `abcd`

I found 2 locations where that code appeared. Please
use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

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

                    error : Error
                    error =
                        Rule.error
                            { message = "Some other error"
                            , details = [ "Some other details" ]
                            }
                            { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }
                in
                ErrorMessage.locationIsAmbiguousInSourceCode
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> expectMessageEqual """
AMBIGUOUS ERROR LOCATION

Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some other error`

and expecting to see it under:

  ```
    abcd =
      1
  ```

I found 3 locations where that code appeared. Please
use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

Tip: I found them at:
  - { start = { row = 2, column = 1 }, end = { row = 3, column = 4 } }
  - { start = { row = 4, column = 1 }, end = { row = 5, column = 4 } }
  - { start = { row = 6, column = 1 }, end = { row = 7, column = 4 } }
"""
        ]


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
            ErrorMessage.missingFixes expectedError
                |> expectMessageEqual """
MISSING FIXES

I expected that the error with the following message

  `Some error`

would provide some fixes, but I didn't find any.

Hint: It's probable that you either forgot to call `Rule.withFixes` on the
error that you created, or that the list of provided fixes was empty."""


unexpectedFixesTest : Test
unexpectedFixesTest =
    test "unexpectedFixes" <|
        \() ->
            let
                range : Range
                range =
                    { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }

                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        range
                        |> Rule.withFixes [ Fix.removeRange range ]
            in
            ErrorMessage.unexpectedFixes error
                |> expectMessageEqual """
UNEXPECTED FIXES

I expected that the error with the following message

  `Some error`

would not have any fixes, but it provided some.

Hint: You may have forgotten to call `Lint.Test.whenFixed`
It's probable that you either forgot to call `Rule.withFixes` on the
error that you created, or that the list of provided fixes was empty."""


fixedCodeMismatchTest : Test
fixedCodeMismatchTest =
    test "fixedCodeMismatch" <|
        \() ->
            let
                sourceCode : String
                sourceCode =
                    """module A exposing (b)
abcd =
  1"""

                expectedSourceCode : String
                expectedSourceCode =
                    """module A exposing (b)
abcd =
  2"""

                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            ErrorMessage.fixedCodeMismatch
                sourceCode
                expectedSourceCode
                error
                |> expectMessageEqual """
FIXED CODE MISMATCH

I found a different fixed source code than expected for the error with the
following message:

  `Some error`

I found the following result after the fixes have been applied:

  ```
    module A exposing (b)
    abcd =
      1
  ```

but I was expecting:

  ```
    module A exposing (b)
    abcd =
      2
  ```"""


unchangedSourceAfterFixTest : Test
unchangedSourceAfterFixTest =
    test "unchangedSourceAfterFix" <|
        \() ->
            let
                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            ErrorMessage.unchangedSourceAfterFix error
                |> expectMessageEqual """
UNCHANGED SOURCE AFTER FIX

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

                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            ErrorMessage.invalidSourceAfterFix
                error
                sourceCode
                |> expectMessageEqual """
INVALID SOURCE AFTER FIX

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
                error : Error
                error =
                    Rule.error
                        { message = "Some error"
                        , details = [ "Some details" ]
                        }
                        { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
            in
            ErrorMessage.hasCollisionsInFixRanges error
                |> expectMessageEqual """
FOUND COLLISIONS IN FIX RANGES

I got something unexpected when applying the fixes provided by the error
with the following message:

  `Some error`

I found that some fixes were targeting (partially or completely) the same
section of code. The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targetted the wrong node for one
of your fixes."""


dummyRange : Range
dummyRange =
    { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
