port module NoUselessIfTest exposing (all)

import Expect
import Test exposing (describe, test, Test)
import NoUselessIf exposing (rule)
import Types exposing (Error)


error : Error
error =
    Error "NoUselessIf" "Useless if expression: It will always evaluate to the same value"


tests : List Test
tests =
    [ test "should not report If expression that has different values in both paths" <|
        \() ->
            rule "a = if b then c else d"
                |> Expect.equal []
    , test "should report If expression that has the same value in both paths" <|
        \() ->
            rule "a = if b then c else c"
                |> Expect.equal [ error ]
    , test "should report If expression that has the same complex value in both paths" <|
        \() ->
            rule "a = if b then (foo m, bar n p) else (foo m, bar n p)"
                |> Expect.equal [ error ]
    ]


all : Test
all =
    describe "NoUselessIf" tests
