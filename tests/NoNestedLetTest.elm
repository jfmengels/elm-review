port module NoNestedLetTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoNestedLet exposing (rule)
import Lint.Types exposing (LintRule, LintError)
import TestUtil exposing (expectErrors)


error : LintError
error =
    LintError "NoNestedLet" "Do not nest Let expressions directly"


tests : List Test
tests =
    [ test "should not report single let expression" <|
        \() ->
            """a = let b = 1
                   in b
            """
                |> rule
                |> expectErrors []
    , test "should report let expression inside the body of an other let expression" <|
        \() ->
            """a = let b = 1
                   in let c = 2
                      in c
            """
                |> rule
                |> expectErrors [ error ]
    , test "should not report let expression indirectly inside another " <|
        \() ->
            """a = let b = 1
                   in if foo then
                        let c = 2 in c
                      else
                        3
            """
                |> rule
                |> expectErrors []
    ]


all : Test
all =
    describe "NoNestedLet" tests
