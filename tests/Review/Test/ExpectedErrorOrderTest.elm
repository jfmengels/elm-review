module Review.Test.ExpectedErrorOrderTest exposing (all)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Review.Test - order of tests"
        [ differentMessageTest
        , sameMessageTest
        ]


differentMessageTest : Test
differentMessageTest =
    describe "when message is different for every error"
        [ test "should match a single error to the single expected error" <|
            \() ->
                """module MyModule exposing (b)
a = foo"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are already in the correct order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                        , Review.Test.error
                            { message = "Detected bar"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "Detected baz"
                            , details = details
                            , under = "baz"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are in reverse order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected baz"
                            , details = details
                            , under = "baz"
                            }
                        , Review.Test.error
                            { message = "Detected bar"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are in a 'random' order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected bar"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "Detected baz"
                            , details = details
                            , under = "baz"
                            }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                        ]
        , test "should match multiple errors with the same details and 'under' if they are in the correct order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    foo
    foo
"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 8 } }
                        ]
        , test "should match multiple errors with the same details and 'under' if they are in a random order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    foo
    foo
"""
                    |> Review.Test.run ruleWithDifferentMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 8 } }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 8 } }
                        , Review.Test.error
                            { message = "Detected foo"
                            , details = details
                            , under = "foo"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                        ]
        ]


sameMessageTest : Test
sameMessageTest =
    describe "when message is the same for every error"
        [ test "should match a single error to the single expected error" <|
            \() ->
                """module MyModule exposing (b)
a = foo"""
                    |> Review.Test.run ruleWithSameMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "foo"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are already in the correct order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithSameMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "foo"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "baz"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are in reverse order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithSameMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "baz"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "foo"
                            }
                        ]
        , test "should match multiple errors with the expected errors that are in a 'random' order" <|
            \() ->
                """module MyModule exposing (b)
a = foo
    bar
    baz
"""
                    |> Review.Test.run ruleWithSameMessage
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "bar"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "baz"
                            }
                        , Review.Test.error
                            { message = "message"
                            , details = details
                            , under = "foo"
                            }
                        ]
        ]



-- TODO with different locations for the same element (with atexactly)
-- TEST RULES


ruleWithDifferentMessage : Rule
ruleWithDifferentMessage =
    Rule.newModuleRuleSchema "NoValues" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor (\fnName -> "Detected " ++ fnName))
        |> Rule.fromModuleRuleSchema


ruleWithSameMessage : Rule
ruleWithSameMessage =
    Rule.newModuleRuleSchema "NoValues" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor (always "message"))
        |> Rule.fromModuleRuleSchema


expressionVisitor : (String -> String) -> Node Expression -> List (Rule.Error {})
expressionVisitor messageFn node =
    case Node.value node of
        Expression.FunctionOrValue _ fnName ->
            [ Rule.error
                { message = messageFn fnName
                , details = details
                }
                (Node.range node)
            ]

        _ ->
            []


details : List String
details =
    [ "details" ]
