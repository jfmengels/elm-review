module NoExtraBooleanComparisonTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint.Error as Error exposing (Error, LintResult)
import Lint.Rule exposing (Rule)
import Lint.Rule.NoExtraBooleanComparison exposing (rule)
import Test exposing (Test, describe, test)
import TestUtil


testRule : String -> LintResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> TestUtil.ruleTester rule


tests : List Test
tests =
    [ test "should not report condition without an operator" <|
        \() ->
            testRule "a = if n then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report condition with integer operators" <|
        \() ->
            testRule """
a = if n < 1 then 1 else 2
b = if n <= 1 then 1 else 2
c = if n > 1 then 1 else 2
d = if n >= 1 then 1 else 2
"""
                |> TestUtil.expectErrorsWithoutRange []
    , test "should not report condition using `not`" <|
        \() ->
            testRule "a = if not n then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange []
    , test "should report condition with `expr == True`" <|
        \() ->
            testRule "a = if b == True then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `True`" ]
    , test "should report condition with `True == expr`" <|
        \() ->
            testRule "a = if True == b then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `True`" ]
    , test "should report condition with `expr == False`" <|
        \() ->
            testRule "a = if b == False then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `False`" ]
    , test "should report condition with `False == expr`" <|
        \() ->
            testRule "a = if False == b then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `False`" ]
    , test "should report condition with `expr /= True`" <|
        \() ->
            testRule "a = if b /= True then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `True`" ]
    , test "should report condition with `True /= expr`" <|
        \() ->
            testRule "a = if True /= b then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `True`" ]
    , test "should report condition with `expr /= False`" <|
        \() ->
            testRule "a = if b /= False then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `False`" ]
    , test "should report condition with `False /= expr`" <|
        \() ->
            testRule "a = if False /= b then 1 else 2"
                |> TestUtil.expectErrorsWithoutRange
                    [ TestUtil.errorWithoutRange "Unnecessary comparison with `False`" ]
    ]


all : Test
all =
    describe "NoExtraBooleanComparison" tests
