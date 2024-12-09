module ReportGlobalErrorWithFixesInAllFilesTest exposing (all)

import ReportGlobalErrorWithFixesInAllFiles exposing (rule)
import Review.Test
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

                        -- TODO MULTIFILE-FIXES Should fail because there are no expected fixes
                        ]
        ]
