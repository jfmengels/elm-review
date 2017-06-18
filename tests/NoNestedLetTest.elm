module NoNestedLetTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoNestedLet exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : LintError
error =
    LintError "NoNestedLet" "Do not nest Let expressions directly"


tests : List Test
tests =
    [ test "should not report single let expression" <|
        \() ->
            testRule """a = let b = 1
                   in b
            """
                |> expectErrors []
    , test "should report let expression inside the body of an other let expression" <|
        \() ->
            testRule """a = let b = 1
                   in let c = 2
                      in c
            """
                |> expectErrors [ error ]
    , test "should not report let expression indirectly inside another " <|
        \() ->
            testRule """a = let b = 1
                   in if foo then
                        let c = 2 in c
                      else
                        3
            """
                |> expectErrors []
    ]


all : Test
all =
    describe "NoNestedLet" tests
