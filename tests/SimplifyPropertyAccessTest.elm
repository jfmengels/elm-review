module SimplifyPropertyAccessTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.SimplifyPropertyAccess exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> LintError
error =
    LintError "SimplifyPropertyAccess"


tests : List Test
tests =
    [ test "should report a named function that returns the property of a record" <|
        \() ->
            testRule "a x = x.foo"
                |> expectErrors [ error "Access to property `foo` could be simplified by using `.foo`" ]
    , test "should report an anonymous function that returns the property of a record" <|
        \() ->
            testRule "a = List.map (\\x -> x.foo)"
                |> expectErrors [ error "Access to property `foo` could be simplified by using `.foo`" ]
    , test "should not report a named function that returns a nested property" <|
        \() ->
            testRule "a x = x.foo.bar"
                |> expectErrors []
    , test "should not report an anonymous function that returns a nested property" <|
        \() ->
            testRule "a = List.map (\\x -> x.foo.bar)"
                |> expectErrors []
    , test "should not report a named function that returns the property of another value" <|
        \() ->
            testRule "a x = b.foo"
                |> expectErrors []
    , test "should not report an anonymous function that returns the property of another value" <|
        \() ->
            testRule "a = List.map (\\x -> b.foo)"
                |> expectErrors []
    , test "should not report a named function that has multiple parameters" <|
        \() ->
            testRule "a x y = x.foo"
                |> expectErrors []
    , test "should not report an anonymous function that has multiple parameters" <|
        \() ->
            testRule "a = List.map (\\x y -> x.foo)"
                |> expectErrors []
    ]


all : Test
all =
    describe "SimplifyPropertyAccess" tests
