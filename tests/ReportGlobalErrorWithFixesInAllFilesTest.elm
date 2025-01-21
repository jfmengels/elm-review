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
                    |> Review.Test.expect
                        [ Review.Test.globalErrorsWithFixes
                            [ { message = "Oh no"
                              , details = [ "I'll fix all modules now" ]
                              , fixes =
                                    [ ( "A", Review.Test.edited """-- Hi
module A exposing (..)
a = 1
""" )
                                    , ( "B", Review.Test.edited """-- Hi
module B exposing (..)
b = 1
""" )
                                    ]
                              }
                            ]
                        ]
        , test "should report a global introducing a change in all files (using expectGlobalErrorsWithFixes)" <|
            \_ ->
                [ """
module A exposing (..)
a = 1
""", """
module B exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectGlobalErrorsWithFixes
                        [ { message = "Oh no"
                          , details = [ "I'll fix all modules now" ]
                          , fixes =
                                [ ( "A", Review.Test.edited """-- Hi
module A exposing (..)
a = 1
""" )
                                , ( "B", Review.Test.edited """-- Hi
module B exposing (..)
b = 1
""" )
                                ]
                          }
                        ]
        , test "test should fail if fix is not provided" <|
            \_ ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Oh no"
                          , details = [ "I'll fix all modules now" ]
                          }
                        ]
                    |> expectFailure """UNEXPECTED FIXES

I found the global error with the following message

  `Oh no`

but it provided an unexpected fix for `src/A.elm`.
This is what it gets fixed to:

  ```

    module A exposing (..)
    a = 1

  ```

If this fix was expected, update the test by using
`expectGlobalErrorsWithFixes` or `globalErrorsWithFixes`. If it isn't, then
change the rule's implementation to not provide a fix for this situation."""
        ]
