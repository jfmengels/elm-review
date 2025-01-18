module ReportGlobalErrorWithFixesInAllFilesTest exposing (all)

import ReportGlobalErrorWithFixesInAllFiles exposing (rule)
import Review.Test
import Review.Test.FailureMessageHelper exposing (expectFailure)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ReportGlobalErrorWithFixesInAllFiles"
        [ test "should report a global introducing a change in all files" <|
            \_ ->
                [ """
module A exposing (..)
a = 1
""", """
module B exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Oh no"
                          , details = [ "I'll fix all modules now" ]
                          }
                        ]
                    |> expectFailure """UNEXPECTED FIXES

I expected that the global error with the following message

  `Oh no`

would provide fixes, but I found an unexpected fix for `src/A.elm`.
This is what it gets fixed to:

  ```

    module A exposing (..)
    a = 1

  ```

If this fix was expected, update the test by using `Review.Test.whenFixed`
or `Review.Test.shouldFixFiles`. If it isn't, then change the rule's
implementation to not provide a fix for this situation."""

        -- TODO MULTIFILE-FIXES This is not recommending the correct functions.
        ]
