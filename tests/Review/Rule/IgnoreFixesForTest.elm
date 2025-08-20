module Review.Rule.IgnoreFixesForTest exposing (ignoreFixesForTest)

import Elm.Syntax.Node as Node
import Review.FilePattern as FilePattern
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


projectRule : Rule
projectRule =
    Rule.newProjectRuleSchema "TestRule" ()
        |> Rule.withModuleVisitor
            (\schema ->
                schema
                    |> Rule.withSimpleExpressionVisitor
                        (\node ->
                            [ Rule.errorWithFix
                                { message = "Error reported"
                                , details = [ "No details" ]
                                }
                                (Node.range node)
                                [ Fix.replaceRangeBy (Node.range node) "1" ]
                            ]
                        )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator identity
            , fromModuleToProject = Rule.initContextCreator identity
            , foldProjectContexts = \_ b -> b
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


ignoreFixesForTest : Test
ignoreFixesForTest =
    describe "Rule.ignoreFixesFor"
        [ test "should report configuration error if invalid globs are passed" <|
            \() ->
                projectRule
                    |> Rule.ignoreFixesFor [ FilePattern.include "** " ]
                    |> Review.Test.expectConfigurationError
                        { message = "Invalid globs provided when using `ignoreFixesFor` for TestRule"
                        , details =
                            [ "This rule indicated files to not have fixes for, but did so by specifying globs that I could not make sense of:"
                            , "  1. ** "
                            ]
                        }
        , test "should not ignore fix if file doesn't match file pattern" <|
            \() ->
                """module Ignored exposing (a)
a = ()
"""
                    |> Review.Test.run
                        (projectRule
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "SomethingElse.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ expectedError
                            |> Review.Test.whenFixed """module Ignored exposing (a)
a = 1
"""
                        ]
        , test "should ignore fix if file matches file pattern" <|
            \() ->
                """module Ignored exposing (a)
a = ()
"""
                    |> Review.Test.run
                        (projectRule
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "src/Ignored.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ expectedError
                        ]
        , test "should ignore fix if file matches one of several file patterns (matches first)" <|
            \() ->
                """module Ignored exposing (a)
a = ()
"""
                    |> Review.Test.run
                        (projectRule
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "src/Ignored.elm" ]
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "src/No.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ expectedError
                        ]
        , test "should ignore fix if file matches one of several file patterns (matches second)" <|
            \() ->
                """module Ignored exposing (a)
a = ()
"""
                    |> Review.Test.run
                        (projectRule
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "src/No.elm" ]
                            |> Rule.ignoreFixesFor [ FilePattern.exclude "src/Ignored.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ expectedError
                        ]
        ]


expectedError : Review.Test.ExpectedError
expectedError =
    Review.Test.error
        { message = "Error reported"
        , details = [ "No details" ]
        , under = "()"
        }
