port module NoNestedLetTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import Lint.Rules.NoNestedLet exposing (rule)
import Lint.Types exposing (Error)


error : Error
error =
    Error "NoNestedLet" "Do not nest Let expressions directly"


tests : List Test
tests =
    [ test "should not report single let expression" <|
        \() ->
            """a = let b = 1
                   in b
            """
                |> rule
                |> Expect.equal []
    , test "should report let expression inside the body of an other let expression" <|
        \() ->
            """a = let b = 1
                   in let c = 2
                      in c
            """
                |> rule
                |> Expect.equal [ error ]
    , test "should not report let expression indirectly inside another " <|
        \() ->
            """a = let b = 1
                   in if foo then
                        let c = 2 in c
                      else
                        3
            """
                |> rule
                |> Expect.equal []
    ]


all : Test
all =
    describe "NoNestedLet" tests
