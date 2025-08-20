module Review.Rule.AvoidFixesTest exposing (..)

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


avoidFixesTest : Test
avoidFixesTest =
    describe "Rule.avoidFixes"
        [ test "should report configuration error if invalid globs are passed" <|
            \() ->
                projectRule
                    |> Rule.avoidFixesIn [ FilePattern.include "** " ]
                    |> Review.Test.expectConfigurationError
                        { message = "Invalid globs provided when using `avoidFixesIn`"
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
                            |> Rule.avoidFixesIn [ FilePattern.exclude "SomethingElse.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Error reported"
                            , details = [ "No details" ]
                            , under = "()"
                            }
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
                            |> Rule.avoidFixesIn [ FilePattern.exclude "src/Ignored.elm" ]
                        )
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Error reported"
                            , details = [ "No details" ]
                            , under = "()"
                            }
                        ]
        ]
