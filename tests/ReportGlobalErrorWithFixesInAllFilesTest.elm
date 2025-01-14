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
                    |> expectFailure """TEST SOURCE CODE PARSING ERROR

I could not parse the test source code, because it was not valid Elm code.

Hint: Maybe you forgot to add the module definition at the top, like:

  `module A exposing (..)`"""
        ]
