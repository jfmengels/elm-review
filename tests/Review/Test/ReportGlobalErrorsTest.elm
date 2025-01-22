module Review.Test.ReportGlobalErrorsTest exposing (all)

import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, FixesV2, ModuleKey, Rule)
import Review.Test
import Review.Test.FailureMessageHelper exposing (expectFailure)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Report global errors"
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
        , test "test should fail if message was not found" <|
            \_ ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Incorrect message"
                          , details = [ "I'll fix all modules now" ]
                          }
                        ]
                    |> expectFailure """UNEXPECTED ERROR MESSAGE

I was looking for the global error with the following message:

  `Incorrect message`

but I found the following error message:

  `Oh no`"""
        , test "test should fail if details are empty" <|
            \_ ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.run ruleWithEmptyDetails
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Oh no"
                          , details = []
                          }
                        ]
                    |> expectFailure """EMPTY ERROR DETAILS

I found an error with the following message:

  `Oh no`

but its details were empty. I require having details as I believe they will
help the user who encounters the problem.

The details could:
- explain what the problem is
- explain the reasoning behind the problem
- give suggestions on how to solve the problem or alternatives"""
        , test "test should fail if details are unexpected" <|
            \_ ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Oh no"
                          , details = [ "Unexpected details" ]
                          }
                        ]
                    |> expectFailure """UNEXPECTED GLOBAL ERROR DETAILS

I found a global error with the following message:

  `Oh no`

and I was expecting its details to be:

  `Unexpected details`

but I found these details:

  `I'll fix all modules now`"""
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

I found the global error with the following message:

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
        , test "test should fail if fix is incorrect" <|
            \_ ->
                """
module A exposing (..)
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectGlobalErrorsWithFixes
                        [ { message = "Oh no"
                          , details = [ "I'll fix all modules now" ]
                          , fixes = [ ( "A", Review.Test.edited """-- Hi!!!
module A exposing (..)
a = 1
""" ) ]
                          }
                        ]
                    |> expectFailure """FIXED CODE MISMATCH

I found a different fixed source code than expected for the error with the
following message:

  `Oh no`

I expected the following result after the fixes have been applied:

  ```
    -- Hi!!!
    module A exposing (..)
    a = 1
  ```

but I found:

  ```
    -- Hi
    module A exposing (..)
    a = 1
  ```"""
        ]


rule : Rule
rule =
    Rule.newProjectRuleSchema "ReportGlobalErrorWithFixesInAllFiles" []
        |> Rule.withModuleVisitor (Rule.withSimpleCommentsVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject =
                Rule.initContextCreator
                    (\moduleKey () ->
                        [ Rule.fixesForModule moduleKey
                            [ Fix.insertAt { row = 1, column = 1 } "-- Hi\n" ]
                        ]
                    )
                    |> Rule.withModuleKey
            , foldProjectContexts = List.append
            }
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


initialProjectContext : List FixesV2
initialProjectContext =
    []


finalProjectEvaluation : List FixesV2 -> List (Error scope)
finalProjectEvaluation fixes =
    [ Rule.globalError
        { message = "Oh no"
        , details = [ "I'll fix all modules now" ]
        }
        |> Rule.withFixesV2 fixes
    ]



---


ruleWithEmptyDetails : Rule
ruleWithEmptyDetails =
    Rule.newProjectRuleSchema "ReportGlobalErrorWithFixesInAllFiles" ()
        |> Rule.withModuleVisitor (Rule.withSimpleCommentsVisitor (always []))
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator (\_ -> ())
            , fromModuleToProject = Rule.initContextCreator (\_ -> ())
            , foldProjectContexts = always
            }
        |> Rule.withFinalProjectEvaluation
            (\() ->
                [ Rule.globalError
                    { message = "Oh no"
                    , details = []
                    }
                ]
            )
        |> Rule.fromProjectRuleSchema
