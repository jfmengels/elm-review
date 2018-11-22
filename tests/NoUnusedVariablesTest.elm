module NoUnusedVariablesTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint exposing (Rule)
import Lint.Error exposing (Error)
import Lint.Rule exposing (LintResult)
import Lint.Rule.NoUnusedVariables exposing (rule)
import Test exposing (Test, describe, test)
import TestUtil exposing (expectErrors, ruleTester)


testRule : String -> LintResult
testRule =
    ruleTester rule


error : String -> Range -> Error
error =
    Error "NoUnusedVariables"


location : Int -> Int -> Int -> Range
location row columnStart columnEnd =
    { start = { row = row, column = columnStart }
    , end = { row = row, column = columnEnd }
    }


tests : List Test
tests =
    [ test "should not report exposed top-level variables" <|
        \() ->
            testRule """module A exposing (a)
a = 1"""
                |> expectErrors []
    , test "should not report used top-level variables" <|
        \() ->
            testRule """module A exposing (b)
a n = 1
b = a 1"""
                |> expectErrors []
    , test "should report unused top-level variables" <|
        \() ->
            testRule """module A exposing (b)
a = 1"""
                |> expectErrors [ error "Variable `a` is not used" (location 2 1 2) ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            testRule """module A exposing (b)
a: Int
a = 1"""
                |> expectErrors [ error "Variable `a` is not used" (location 3 1 2) ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
a n = 1
b = a 1"""
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
a = 1
b = 2"""
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            testRule """module A exposing (a, b)
a = 1
b = 2
c = 3"""
                |> expectErrors [ error "Variable `c` is not used" (location 4 1 2) ]
    , test "should not report unused top-level variables if everything is exposed (port module)" <|
        \() ->
            testRule """port module A exposing (..)
a n = 1
b = a 1"""
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
a = 1
b = 2"""
                |> expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
a = 1
b = 2
c = 3"""
                |> expectErrors [ error "Variable `c` is not used" (location 4 1 2) ]
    , test "should report unused variables from let declarations" <|
        \() ->
            testRule """module A exposing (a)
a = let b = 1
    in 2"""
                |> expectErrors [ error "Variable `b` is not used" (location 2 9 10) ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
a = let b = 1
    in 2"""
                |> expectErrors [ error "Variable `b` is not used" (location 2 9 10) ]
    , test "should report unused functions from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a)
a = let b param = 1
    in 2"""
                |> expectErrors [ error "Variable `b` is not used" (location 2 9 10) ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
a = let b = 1
    in 2"""
                |> expectErrors [ error "Variable `b` is not used" (location 2 9 10) ]
    , test "should not report top-level variables used inside a let expression" <|
        \() ->
            testRule """module A exposing (a)
b = 1
a = let c = 1
    in b + c"""
                |> expectErrors []
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            testRule """module A exposing (a)
b = 1
a = let c = b
    in c"""
                |> expectErrors []
    , test "should not report top-level variables used in nested lets" <|
        \() ->
            testRule """module A exposing (a)
b = 1
a = let
      c = b
      d = let
            e = 1
          in
            b + c + e
    in
      d"""
                |> expectErrors []
    , test "should not report variables from let declarations that are used in the expression" <|
        \() ->
            testRule """module A exposing (a)
a = let c = 1
    in c"""
                |> expectErrors []
    , test "should not report unused function parameters" <|
        \() ->
            testRule """module A exposing (a)
a n = 1"""
                |> expectErrors []
    , test "should report unused imported functions" <|
        \() ->
            testRule """module A exposing (b)
import Foo exposing (a)"""
                |> expectErrors [ error "Variable `a` is not used" (location 2 22 23) ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            testRule """module A exposing (d)
import Foo exposing (C, a, b)"""
                |> expectErrors
                    [ error "Variable `a` is not used" (location 2 25 26)
                    , error "Variable `b` is not used" (location 2 28 29)
                    ]

    -- Needs to be improved, every case should create a new scope stack
    -- Right now, every parameter is considered used, which is not great
    , test "should not report unused pattern matching parameters" <|
        \() ->
            testRule """module A exposing (a)
a = case thing of
    Foo b c -> []"""
                |> expectErrors []

    -- Should B and C be reported if they are not used? Probably.
    , test "should report unused custom type declarations" <|
        \() ->
            testRule """module A exposing (a)
type A = B | C"""
                |> expectErrors [ error "Variable `A` is not used" (location 2 6 7) ]
    , test "should report unused type aliases declarations" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }"""
                |> expectErrors [ error "Variable `A` is not used" (location 2 12 13) ]
    , test "should not report type used in a signature" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : A
a = {a = 1}"""
                |> expectErrors []
    , test "should not report type used in a signature with multiple arguments" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : String -> A
a str = {a = str}"""
                |> expectErrors []
    , test "should not report type used in a signature with parameterized types (as generic type)" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : A B
a = []"""
                |> expectErrors []
    , test "should not report type used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : List A
a = []"""
                |> expectErrors []
    , test "should not report type used in a signature with a record" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : { c: A }
a str = {c = str}"""
                |> expectErrors []
    , test "should not report type used in a signature with a generic record" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : { r | c: A }
a str = {c = str}"""
                |> expectErrors []
    , test "should not report type if it's exposed" <|
        \() ->
            testRule """module A exposing (A)
type A a = B a"""
                |> expectErrors []
    , test "should not report custom type if it's exposed with its sub-types" <|
        \() ->
            testRule """module A exposing (A(..))
type A = B | C | D"""
                |> expectErrors []
    , test "should report unused variable even if it's present in a generic type" <|
        \() ->
            testRule """module A exposing (A)
a = 1
type A a = B a"""
                |> expectErrors [ error "Variable `a` is not used" (location 2 1 2) ]
    , test "should report unused variable even if it's present in a generic record type" <|
        \() ->
            testRule """module A exposing (a)
r = 1
a : { r | c: A }
a str = {c = str}"""
                |> expectErrors [ error "Variable `r` is not used" (location 2 1 2) ]
    , test "should report unused operator import" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))"""
                |> expectErrors [ error "Variable `</>` is not used" (location 2 25 30) ]
    , test "should not report used operator (infix)" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))
a = 1 </> 2"""
                |> expectErrors []
    , test "should not report used operator (prefix)" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))
a = (</>) 2"""
                |> expectErrors []

    -- ##################################################################################################
    -- , test "should report unused opaque types" <|
    --     \() ->
    --         testRule """module A exposing (a)
    -- type A = A Int"""
    --             |> expectErrors [ error "Variable `A` is not used" (location 2 6 7) ]
    -- , test "should not report used opaque types" <|
    --     \() ->
    --         testRule """module A exposing (a)
    -- type A = A Int
    -- a : A
    -- a = 1"""
    --             |> expectErrors [ error "Variable `A` is not used" (location 2 6 7) ]
    ]


all : Test
all =
    describe "NoUnusedVariables" tests
