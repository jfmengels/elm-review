module NoUselessIfTest exposing (all)

import Test exposing (describe, test, Test)
import Lint.Rules.NoUselessIf exposing (rule)
import Lint.Types exposing (LintRule, LintError, LintResult)
import TestUtil exposing (ruleTester, expectErrors)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : LintError
error =
    LintError "NoUselessIf" "Useless if expression: It will always evaluate to the same value"


tests : List Test
tests =
    [ test "should not report If expression that has different values in both paths" <|
        \() ->
            testRule "a = if b then c else d"
                |> expectErrors []
    , test "should report If expression that has the same value in both paths" <|
        \() ->
            testRule "a = if b then c else c"
                |> expectErrors [ error ]
    , test "should report If expression that has the same complex value in both paths" <|
        \() ->
            testRule "a = if b then (foo m, bar n p) else (foo m, bar n p)"
                |> expectErrors [ error ]
    ]


all : Test
all =
    describe "NoUselessIf" tests
