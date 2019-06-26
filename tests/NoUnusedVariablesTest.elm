module NoUnusedVariablesTest exposing (all)

import Elm.Syntax.Range exposing (Location, Range)
import Lint.Error as Error exposing (Error)
import Lint.Rule exposing (Rule)
import Lint.Rule.NoUnusedVariables exposing (rule)
import Test exposing (Test, describe, test)
import Lint.Test exposing (LintResult)


testRule : String -> LintResult
testRule =
    Lint.Test.ruleTester rule


tests : List Test
tests =
    [ test "should not report exposed top-level variables" <|
        \() ->
            testRule """module A exposing (a)
a = 1"""
                |> Lint.Test.expectErrors []
    , test "should not report used top-level variables" <|
        \() ->
            testRule """module A exposing (b)
a n = 1
b = a 1"""
                |> Lint.Test.expectErrors []
    , test "should report unused top-level variables" <|
        \() ->
            testRule """module A exposing (b)
a = 1"""
                |> Lint.Test.expectErrors [ Error.create "Variable `a` is not used" (Lint.Test.location ( 2, 1 ) ( 2, 2 )) ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            testRule """module A exposing (b)
a: Int
a = 1"""
                |> Lint.Test.expectErrors [ Error.create "Variable `a` is not used" (Lint.Test.location ( 3, 1 ) ( 3, 2 )) ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
a n = 1
b = a 1"""
                |> Lint.Test.expectErrors []
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
a = 1
b = 2"""
                |> Lint.Test.expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            testRule """module A exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Lint.Test.expectErrors [ Error.create "Variable `c` is not used" (Lint.Test.location ( 4, 1 ) ( 4, 2 )) ]
    , test "should not report unused top-level variables if everything is exposed (port module)" <|
        \() ->
            testRule """port module A exposing (..)
a n = 1
b = a 1"""
                |> Lint.Test.expectErrors []
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
a = 1
b = 2"""
                |> Lint.Test.expectErrors []
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            testRule """port module A exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Lint.Test.expectErrors [ Error.create "Variable `c` is not used" (Lint.Test.location ( 4, 1 ) ( 4, 2 )) ]
    , test "should report unused variables from let declarations" <|
        \() ->
            testRule """module A exposing (a)
a = let b = 1
    in 2"""
                |> Lint.Test.expectErrors [ Error.create "Variable `b` is not used" (Lint.Test.location ( 2, 9 ) ( 2, 10 )) ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a, b)
a = let b = 1
    in 2"""
                |> Lint.Test.expectErrors [ Error.create "Variable `b` is not used" (Lint.Test.location ( 2, 9 ) ( 2, 10 )) ]
    , test "should report unused functions from let even if they are exposed by name" <|
        \() ->
            testRule """module A exposing (a)
a = let b param = 1
    in 2"""
                |> Lint.Test.expectErrors [ Error.create "Variable `b` is not used" (Lint.Test.location ( 2, 9 ) ( 2, 10 )) ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            testRule """module A exposing (..)
a = let b = 1
    in 2"""
                |> Lint.Test.expectErrors [ Error.create "Variable `b` is not used" (Lint.Test.location ( 2, 9 ) ( 2, 10 )) ]
    , test "should not report top-level variables used inside a let expression" <|
        \() ->
            testRule """module A exposing (a)
b = 1
a = let c = 1
    in b + c"""
                |> Lint.Test.expectErrors []
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            testRule """module A exposing (a)
b = 1
a = let c = b
    in c"""
                |> Lint.Test.expectErrors []
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
                |> Lint.Test.expectErrors []
    , test "should not report variables from let declarations that are used in the expression" <|
        \() ->
            testRule """module A exposing (a)
a = let c = 1
    in c"""
                |> Lint.Test.expectErrors []
    , test "should not report unused function parameters" <|
        \() ->
            testRule """module A exposing (a)
a n = 1"""
                |> Lint.Test.expectErrors []
    , test "should report unused imported functions" <|
        \() ->
            testRule """module A exposing (b)
import Foo exposing (a)"""
                |> Lint.Test.expectErrors [ Error.create "Imported variable `a` is not used" (Lint.Test.location ( 2, 22 ) ( 2, 23 )) ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            testRule """module A exposing (d)
import Foo exposing (C, a, b)"""
                |> Lint.Test.expectErrors
                    [ Error.create "Imported variable `b` is not used" (Lint.Test.location ( 2, 28 ) ( 2, 29 ))
                    , Error.create "Imported variable `a` is not used" (Lint.Test.location ( 2, 25 ) ( 2, 26 ))
                    , Error.create "Imported type `C` is not used" (Lint.Test.location ( 2, 22 ) ( 2, 23 ))
                    ]

    -- Needs to be improved, every case should create a new scope stack
    -- Right now, every parameter is considered used, which is not great
    , test "should not report unused pattern matching parameters" <|
        \() ->
            testRule """module A exposing (a)
a = case thing of
    Foo b c -> []"""
                |> Lint.Test.expectErrors []

    -- Should B and C be reported if they are not used? Probably.
    , test "should report unused custom type declarations" <|
        \() ->
            testRule """module A exposing (a)
type A = B | C"""
                |> Lint.Test.expectErrors [ Error.create "Type `A` is not used" (Lint.Test.location ( 2, 6 ) ( 2, 7 )) ]
    , test "should report unused type aliases declarations" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }"""
                |> Lint.Test.expectErrors [ Error.create "Type `A` is not used" (Lint.Test.location ( 2, 12 ) ( 2, 13 )) ]
    , test "should not report type used in a signature" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : A
a = {a = 1}"""
                |> Lint.Test.expectErrors []
    , test "should not report type used in a signature with multiple arguments" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : String -> A
a str = {a = str}"""
                |> Lint.Test.expectErrors []
    , test "should not report type used in a signature with parameterized types (as generic type)" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : A B
a = []"""
                |> Lint.Test.expectErrors []
    , test "should not report type used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : List A
a = []"""
                |> Lint.Test.expectErrors []
    , test "should not report type used in a signature with a record" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : { c: A }
a str = {c = str}"""
                |> Lint.Test.expectErrors []
    , test "should not report type used in a signature with a generic record" <|
        \() ->
            testRule """module A exposing (a)
type alias A = { a : B }
a : { r | c: A }
a str = {c = str}"""
                |> Lint.Test.expectErrors []
    , test "should not report type if it's exposed" <|
        \() ->
            testRule """module A exposing (A)
type A a = B a"""
                |> Lint.Test.expectErrors []
    , test "should not report custom type if it's exposed with its sub-types" <|
        \() ->
            testRule """module A exposing (A(..))
type A = B | C | D"""
                |> Lint.Test.expectErrors []
    , test "should report unused variable even if it's present in a generic type" <|
        \() ->
            testRule """module A exposing (A)
a = 1
type A a = B a"""
                |> Lint.Test.expectErrors [ Error.create "Variable `a` is not used" (Lint.Test.location ( 2, 1 ) ( 2, 2 )) ]
    , test "should report unused variable even if it's present in a generic record type" <|
        \() ->
            testRule """module A exposing (a)
r = 1
a : { r | c: A }
a str = {c = str}"""
                |> Lint.Test.expectErrors [ Error.create "Variable `r` is not used" (Lint.Test.location ( 2, 1 ) ( 2, 2 )) ]
    , test "should report unused operator import" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))"""
                |> Lint.Test.expectErrors [ Error.create "Imported operator `</>` is not used" (Lint.Test.location ( 2, 25 ) ( 2, 30 )) ]
    , test "should not report used operator (infix)" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))
a = 1 </> 2"""
                |> Lint.Test.expectErrors []
    , test "should not report used operator (prefix)" <|
        \() ->
            testRule """module A exposing (a)
import Parser exposing ((</>))
a = (</>) 2"""
                |> Lint.Test.expectErrors []
    , test "should report unused opaque types" <|
        \() ->
            testRule """module A exposing (a)
type A = A Int"""
                |> Lint.Test.expectErrors [ Error.create "Type `A` is not used" (Lint.Test.location ( 2, 6 ) ( 2, 7 )) ]
    , test "should not report used opaque types" <|
        \() ->
            testRule """module A exposing (a)
type A = A Int
a : A
a = 1"""
                |> Lint.Test.expectErrors []
    , test "should report unused import" <|
        \() ->
            testRule """module A exposing (a)
import Html"""
                |> Lint.Test.expectErrors [ Error.create "Imported module `Html` is not used" (Lint.Test.location ( 2, 8 ) ( 2, 12 )) ]
    , test "should report unused import (multiples segments)" <|
        \() ->
            testRule """module A exposing (a)
import Html.Styled.Attributes"""
                |> Lint.Test.expectErrors [ Error.create "Imported module `Html.Styled.Attributes` is not used" (Lint.Test.location ( 2, 8 ) ( 2, 30 )) ]
    , test "should not report import if it exposes all (should be improved by detecting if any exposed value is used)" <|
        \() ->
            testRule """module A exposing (a)
import Html.Styled.Attributes exposing (..)"""
                |> Lint.Test.expectErrors []
    , test "should report unused variable even if a homonym from a module is used" <|
        \() ->
            testRule """module A exposing (a)
href = 1
a = Html.Styled.Attributes.href"""
                |> Lint.Test.expectErrors [ Error.create "Variable `href` is not used" (Lint.Test.location ( 2, 1 ) ( 2, 5 )) ]
    , test "should not report used import (function access)" <|
        \() ->
            testRule """module A exposing (a)
import Html.Styled.Attributes
a = Html.Styled.Attributes.href"""
                |> Lint.Test.expectErrors []
    , test "should not report unused import if it is aliased" <|
        \() ->
            testRule """module A exposing (a)
import Html.Styled.Attributes as Html
a = Html.href"""
                |> Lint.Test.expectErrors []
    , test "should report unused import alias" <|
        \() ->
            testRule """module A exposing (a)
import Html.Styled.Attributes as Html"""
                |> Lint.Test.expectErrors [ Error.create "Module alias `Html` is not used" (Lint.Test.location ( 2, 34 ) ( 2, 38 )) ]
    , test "should not report import that exposes a used exposed type" <|
        \() ->
            testRule """module A exposing (a)
import B exposing (C(..))
a : C
a = 1"""
                |> Lint.Test.expectErrors []
    , test "should not report import that exposes an unused exposed type (but whose subtype is potentially used)" <|
        \() ->
            testRule """module A exposing (a)
import B exposing (C(..))
a : D
a = 1"""
                |> Lint.Test.expectErrors []
    , test "should not report types that are used in ports" <|
        \() ->
            testRule """module A exposing (output, input)
import Json.Decode
import Json.Encode
port output : Json.Encode.Value -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                |> Lint.Test.expectErrors []
    , test "should report unused ports (ingoing)" <|
        \() ->
            testRule """module A exposing (a)
import Json.Decode
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                |> Lint.Test.expectErrors
                    [ Error.create "Port `input` is not used (Warning: Removing this port may break your application if it is used in the JS code)" (Lint.Test.location ( 3, 6 ) ( 3, 11 ))
                    ]
    , test "should report unused ports (outgoing)" <|
        \() ->
            testRule """module A exposing (a)
import Json.Encode
port output : Json.Encode.Value -> Cmd msg"""
                |> Lint.Test.expectErrors
                    [ Error.create "Port `output` is not used (Warning: Removing this port may break your application if it is used in the JS code)" (Lint.Test.location ( 3, 6 ) ( 3, 12 ))
                    ]
    ]


all : Test
all =
    describe "NoUnusedVariables" tests
