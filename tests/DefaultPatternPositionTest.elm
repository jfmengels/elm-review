module DefaultPatternPositionTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint.Error as Error exposing (Error)
import Lint.Rule exposing (Rule)
import Lint.Rule.DefaultPatternPosition exposing (PatternPosition(..), rule)
import Test exposing (Test, describe, test)
import TestUtil exposing (LintResult)


testRule : PatternPosition -> String -> LintResult
testRule patternPosition =
    TestUtil.ruleTester (rule patternPosition)


error : String -> Error
error position =
    TestUtil.errorWithoutRange
        ("Expected default pattern to appear " ++ position ++ " in the list of patterns")


tests : List Test
tests =
    [ test "should not report when default pattern is at the expected position (first)" <|
        \() ->
            """module A exposing(..)
a = case b of
  _ -> 1
  Bar -> 1
  Foo -> 1
"""
                |> testRule ShouldBeFirst
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report when default pattern is at the expected position (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule ShouldBeLast
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report when there is no default pattern (first)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeFirst
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report when there is no default pattern (last)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> TestUtil.expectErrorsWithoutRange []
    , test "should report an error when the default pattern is not at the expected position (first) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  Bar -> 1
  _ -> 1
"""
                |> testRule ShouldBeFirst
                |> TestUtil.expectErrorsWithoutRange [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (first) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule ShouldBeFirst
                |> TestUtil.expectErrorsWithoutRange [ error "first" ]
    , test "should report an error when the default pattern is not at the expected position (last) (opposite expected position)" <|
        \() ->
            """module A exposing(..)
a = case b of
  _ -> 1
  Foo -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> TestUtil.expectErrorsWithoutRange [ error "last" ]
    , test "should report an error when the default pattern is not at the expected position (last) (somewhere in the middle)" <|
        \() ->
            """module A exposing(..)
a = case b of
  Foo -> 1
  _ -> 1
  Bar -> 1
"""
                |> testRule ShouldBeLast
                |> TestUtil.expectErrorsWithoutRange [ error "last" ]
    ]


all : Test
all =
    describe "DefaultPatternPosition" tests
