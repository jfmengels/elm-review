module ErrorMessageTest exposing (all)

import Elm.Syntax.Range exposing (Range)
import Expect
import Lint.Internal.Test exposing (ExpectedError, LintResult)
import Lint.Rule as Rule exposing (Error)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Test.ErrorMessage"
        [ parsingErrorMessageTest
        , didNotExpectErrorsTest
        , messageMismatchErrorTest
        , underMismatchErrorTest
        , wrongLocationErrorTest
        , notEnoughErrorsTest
        , tooManyErrorsTest
        ]


parsingErrorMessageTest : Test
parsingErrorMessageTest =
    test "parsingErrorMessage" <|
        \() ->
            Lint.Internal.Test.parsingErrorMessage
                |> Expect.equal (String.trim """
I could not parse the test source code, because it was not syntactically valid Elm code.

Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`""")


didNotExpectErrorsTest : Test
didNotExpectErrorsTest =
    test "didNotExpectErrors" <|
        \() ->
            let
                errors : List Error
                errors =
                    [ Rule.error "Some error" dummyRange
                    , Rule.error "Some other error" dummyRange
                    ]
            in
            Lint.Internal.Test.didNotExpectErrors errors
                |> Expect.equal (String.trim """
I expected no errors but found:

  - "Some error" at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - "Some other error" at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
""")


messageMismatchErrorTest : Test
messageMismatchErrorTest =
    test "messageMismatchError" <|
        \() ->
            let
                expectedError : ExpectedError
                expectedError =
                    Lint.Internal.Test.error
                        { message = "Forbidden use of Debug"
                        , under = "Debug.log"
                        }

                error : Error
                error =
                    Rule.error "Forbidden use of Debu" dummyRange
            in
            Lint.Internal.Test.messageMismatchError expectedError error
                |> Expect.equal (String.trim """
I was looking for the error with the following message:

  `Forbidden use of Debug`

but I found the following error message:

  `Forbidden use of Debu`""")


underMismatchErrorTest : Test
underMismatchErrorTest =
    describe "underMismatchError"
        [ test "with single-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error "Some error" dummyRange
                in
                Lint.Internal.Test.underMismatchError
                    error
                    { under = "abcd"
                    , codeAtLocation = "abcd = 1"
                    }
                    |> Expect.equal (String.trim """
I found an error with the following message:

  `Some error`

which I was expecting, but I found it under:

  `abcd = 1`

when I was expecting it under:

  `abcd`

Hint: Maybe you're passing the `Range` of a wrong node when calling `Rule.error`""")
        , test "with multi-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error "Some other error" dummyRange
                in
                Lint.Internal.Test.underMismatchError
                    error
                    { under = "abcd =\n  1\n  + 2"
                    , codeAtLocation = "abcd =\n  1"
                    }
                    |> Expect.equal (String.trim """
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

Hint: Maybe you're passing the `Range` of a wrong node when calling `Rule.error`""")
        ]


wrongLocationErrorTest : Test
wrongLocationErrorTest =
    describe "wrongLocationError"
        [ test "with single-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            "Some error"
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                Lint.Internal.Test.wrongLocationError
                    error
                    { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                    "abcd"
                    |> Expect.equal (String.trim """
I was looking for the error with the following message:

  `Some error`

under the following code:

  `abcd`

and I found it, but the exact location you specified is not the one I found. I was expecting the error at:

  { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }

but I found it at:

  { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
""")
        , test "with multi-line extracts" <|
            \() ->
                let
                    error : Error
                    error =
                        Rule.error
                            "Some other error"
                            { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
                in
                Lint.Internal.Test.wrongLocationError
                    error
                    { start = { row = 2, column = 1 }, end = { row = 3, column = 3 } }
                    "abcd =\n  1"
                    |> Expect.equal (String.trim """
I was looking for the error with the following message:

  `Some other error`

under the following code:

  ```
    abcd =
      1
  ```

and I found it, but the exact location you specified is not the one I found. I was expecting the error at:

  { start = { row = 2, column = 1 }, end = { row = 3, column = 3 } }

but I found it at:

  { start = { row = 4, column = 1 }, end = { row = 5, column = 3 } }
""")
        ]


notEnoughErrorsTest : Test
notEnoughErrorsTest =
    test "notEnoughErrors" <|
        \() ->
            let
                missingErrors : List ExpectedError
                missingErrors =
                    [ Lint.Internal.Test.error
                        { message = "Forbidden use of Debug"
                        , under = "Debug.log"
                        }
                    , Lint.Internal.Test.error
                        { message = "Forbidden use of Debug"
                        , under = "Debug.log"
                        }
                    ]
            in
            Lint.Internal.Test.notEnoughErrors missingErrors
                |> Expect.equal (String.trim """
I expected to see 2 more errors:

- "Forbidden use of Debug"
- "Forbidden use of Debug"
""")


tooManyErrorsTest : Test
tooManyErrorsTest =
    describe "tooManyErrors"
        [ test "with one extra error" <|
            \() ->
                let
                    extraErrors : List Rule.Error
                    extraErrors =
                        [ Rule.error
                            "Forbidden use of Debug"
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        ]
                in
                Lint.Internal.Test.tooManyErrors extraErrors
                    |> Expect.equal (String.trim """
I found 1 error too many:

- "Forbidden use of Debug" at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
""")
        , test "with multiple extra errors" <|
            \() ->
                let
                    extraErrors : List Rule.Error
                    extraErrors =
                        [ Rule.error
                            "Forbidden use of Debug"
                            { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        , Rule.error
                            "Forbidden use of Debug"
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                        ]
                in
                Lint.Internal.Test.tooManyErrors extraErrors
                    |> Expect.equal (String.trim """
I found 2 errors too many:

- "Forbidden use of Debug" at { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
- "Forbidden use of Debug" at { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
""")
        ]


locationIsAmbiguousInSourceCodeErrorTest : Test
locationIsAmbiguousInSourceCodeErrorTest =
    describe "locationIsAmbiguousInSourceCodeError"
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
                            "Some error"
                            { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
                in
                Lint.Internal.Test.locationIsAmbiguousInSourceCodeError
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> Expect.equal (String.trim """
Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some error`

and expecting to see it under:

  `abcd`

I found 2 locations where that code appeared. Please use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

Tip: I found them at:
  - { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
  - { start = { row = 3, column = 1 }, end = { row = 3, column = 5 } }
""")
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
                            "Some other error"
                            { start = { row = 3, column = 1 }, end = { row = 4, column = 3 } }
                in
                Lint.Internal.Test.locationIsAmbiguousInSourceCodeError
                    sourceCode
                    error
                    under
                    (String.indexes under sourceCode)
                    |> Expect.equal (String.trim """
Your test passes, but where the message appears is ambiguous.

You are looking for the following error message:

  `Some other error`

and expecting to see it under:

  ```
    abcd =
      1
  ```

I found 3 locations where that code appeared. Please use `Lint.Rule.atExactly` to make the part you were targetting unambiguous.

Tip: I found them at:
  - { start = { row = 2, column = 1 }, end = { row = 3, column = 4 } }
  - { start = { row = 4, column = 1 }, end = { row = 5, column = 4 } }
  - { start = { row = 6, column = 1 }, end = { row = 7, column = 4 } }
""")
        ]


dummyRange : Range
dummyRange =
    { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
