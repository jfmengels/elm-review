module ElmTest.NoDuplicateTestBodiesTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.ElmTest.NoDuplicateTestBodies exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "ElmTest.NoDuplicateTestBodies"


tests : List Test
tests =
    [ test "should not report non-tests" <|
        \() ->
            testRule """
            foo = [ 1, 1 ]
            bar = [ foo 2 <| 1, foo 2 <| 1 ]
            tests =
                [ fn "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , fn "bar" <|
                    \\() -> 1 + 2
                        |> Expect.equal 3
                ]
            """
                |> expectErrors []
    , test "should not report tests that have distinct bodies" <|
        \() ->
            testRule """
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 2
                        |> Expect.equal 3
                ]
            """
                |> expectErrors []
    , test "should not report tests that have the same body when Test is not imported and used in list" <|
        \() ->
            testRule """
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors []
    , test "should report tests that have the same body when using test and exposing (test)" <|
        \() ->
            testRule """
            import Test exposing (test, foo)
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "baz" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors
                    [ error "Test `bar` has the same body as test `foo`"
                    , error "Test `baz` has the same body as test `foo`"
                    ]
    , test "should report tests that have the same body when using Test.test and exposing (test)" <|
        \() ->
            testRule """
            import Test exposing (test, foo)
            tests =
                [ Test.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Test.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should report tests that have the same body when using test and exposing (..)" <|
        \() ->
            testRule """
            import Test exposing (..)
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should report tests that have the same body when using Test.test and exposing (..)" <|
        \() ->
            testRule """
            import Test exposing (..)
            tests =
                [ Test.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Test.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should report tests that have the same body when using Foo.test, aliasing as Foo and exposing (..)" <|
        \() ->
            testRule """
            import Test as Foo exposing (..)
            tests =
                [ Foo.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Foo.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should report tests that have the same body when using Test.test without exposing anything" <|
        \() ->
            testRule """
            import Test
            tests =
                [ Test.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Test.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should report tests that have the same body when using Foo.test aliasing as Foo and without exposing anything" <|
        \() ->
            testRule """
            import Test as Foo
            tests =
                [ Foo.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Foo.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors [ error "Test `bar` has the same body as test `foo`" ]
    , test "should not report tests that have the same body when using test and not exposing (test)" <|
        \() ->
            testRule """
            import Test exposing (foo)
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors []
    , test "should not report tests that have the same body when using test without exposing anything" <|
        \() ->
            testRule """
            import Test
            tests =
                [ test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors []
    , test "should not report tests that have the same body when using test and aliasing as Foo without exposing anything" <|
        \() ->
            testRule """
            import Test as Foo
            tests =
                [ Test.test "foo" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                , Test.test "bar" <|
                    \\() -> 1 + 1
                        |> Expect.equal 2
                ]
            """
                |> expectErrors []
    ]


all : Test
all =
    describe "ElmTest.NoDuplicateTestBodies" tests
