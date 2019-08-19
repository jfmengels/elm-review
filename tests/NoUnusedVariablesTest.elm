module NoUnusedVariablesTest exposing (all)

import Lint.Rule.NoUnusedVariables exposing (rule)
import Lint.Test exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : String -> LintResult
testRule =
    Lint.Test.run rule


details : List String
details =
    [ "Since it is not being used, I recommend removing it. It should make the code clearer to read for other people." ]


all : Test
all =
    describe "NoUnusedVariables"
        [ describe "Top-level variables" topLevelVariablesTests
        , describe "let..in" letInTests
        , describe "Top-level variables used inside a let..in" topLevelVariablesUsedInLetInTests
        , describe "Record updates" recordUpdateTests
        , describe "Function parameters" functionParameterTests
        , describe "Imports" importTests
        , describe "Pattern matching variables" patternMatchingVariablesTests
        , describe "Defined types" typeTests
        , describe "Opaque Types" opaqueTypeTests
        , describe "Operators" operatorTests
        , describe "Ports" portTests
        ]


topLevelVariablesTests : List Test
topLevelVariablesTests =
    [ test "should not report exposed top-level variables" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = 1"""
                |> Lint.Test.expectNoErrors
    , test "should not report used top-level variables" <|
        \() ->
            testRule """module SomeModule exposing (b)
a n = 1
b = a 1"""
                |> Lint.Test.expectNoErrors
    , test "should report unused top-level variables" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = 1
a = 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (b)
b = 1
"""
                    ]
    , test "should report unused top-level variables with type annotation" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = 1
a : Int
a = 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 2 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (b)
b = 1
"""
                    ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            testRule """module SomeModule exposing (b)
a: Int
a = 1
b = 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (b)

b = 2"""
                    ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            testRule """module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a, b)
a = 1
b = 2
"""
                    ]
    , test "should not report unused top-level variables if everything is exposed (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Lint.Test.whenFixed """port module SomeModule exposing (a, b)
a = 1
b = 2
"""
                    ]
    , test "should report unused variable even if a homonym from a module is used" <|
        \() ->
            testRule """module SomeModule exposing (a)
href = 1
a = Html.Styled.Attributes.href"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `href` is not used"
                        , details = details
                        , under = "href"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)

a = Html.Styled.Attributes.href"""
                    ]
    ]


letInTests : List Test
letInTests =
    [ test "should report unused variables from let declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = let b = 1
    in 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = let b = 1
        c = 2
    in c"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                        |> Lint.Test.whenFixed "module SomeModule exposing (a, b)\na = let \n        c = 2\n    in c"
                    ]
    , test "should report unused function from let even if they are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = let b param = 1
    in 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a, b)
a = 2"""
                    ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            testRule """module SomeModule exposing (..)
a = let b = 1
    in 2"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (..)
a = 2"""
                    ]
    , test "should not report variables from let declarations that are used in the expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = let c = 1
    in c"""
                |> Lint.Test.expectNoErrors
    ]


topLevelVariablesUsedInLetInTests : List Test
topLevelVariablesUsedInLetInTests =
    [ test "should not report top-level variables used inside a let expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a = let c = 1
in b + c"""
                |> Lint.Test.expectNoErrors
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a = let c = b
in c"""
                |> Lint.Test.expectNoErrors
    , test "should not report top-level variables used in nested lets" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a = let
  c = b
  d = let
        e = 1
      in
        b + c + e
in
  d"""
                |> Lint.Test.expectNoErrors
    ]


recordUpdateTests : List Test
recordUpdateTests =
    [ test "should not report variables used in a record update expression's value to be updated" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { c = 1 }
a = { b | c = 3 }"""
                |> Lint.Test.expectNoErrors
    , test "should not report variables used in a record update expression's updates" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { y = 1, z = 1 }
d = 3
e = 3
a = { b | y = d, z = e }"""
                |> Lint.Test.expectNoErrors
    , test "should report variables even if they appear as keys of a record update expression's updates" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { z = 1, c = 2 }
c = 1
a = { b | c = 3 }"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Lint.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
b = { z = 1, c = 2 }

a = { b | c = 3 }"""
                    ]
    ]


functionParameterTests : List Test
functionParameterTests =
    [ test "should not report unused function parameters" <|
        \() ->
            testRule """module SomeModule exposing (a)
a n = 1"""
                |> Lint.Test.expectNoErrors
    ]


importTests : List Test
importTests =
    [ test "should report unused imported functions" <|
        \() ->
            testRule """module SomeModule exposing (b)
import Foo exposing (a)"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (b)
import Foo """
                    ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            testRule """module SomeModule exposing (d)
import Foo exposing (C, a, b)"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (d)
import Foo exposing (a, b)"""
                    , Lint.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (d)
import Foo exposing (C, b)"""
                    , Lint.Test.error
                        { message = "Imported variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (d)
import Foo exposing (C, a)"""
                    ]
    , test "should report unused imported functions (multiple imports on several lines)" <|
        \() ->
            testRule """module SomeModule exposing (d)
import Foo
    exposing
        ( C
        , a
        , b
        )"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C\n  "
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (d)
import Foo
    exposing
        ( a
        , b
        )"""
                    , Lint.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.whenFixed
                            ("""module SomeModule exposing (d)
import Foo
    exposing
        ( C
"""
                                ++ "        "
                                ++ """
        , b
        )"""
                            )
                    , Lint.Test.error
                        { message = "Imported variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (d)
import Foo
    exposing
        ( C
        , a
        )"""
                    ]
    , test "should report unused operator import" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Parser exposing ((</>))"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported operator `</>` is not used"
                        , details = details
                        , under = "(</>)"
                        }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
import Parser """
                    ]
    , test "should report unused import" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported module `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Lint.Test.whenFixed "module SomeModule exposing (a)\n"
                    ]
    , test "should report unused import (multiples segments)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Imported module `Html.Styled.Attributes` is not used"
                        , details = details
                        , under = "Html.Styled.Attributes"
                        }
                        |> Lint.Test.whenFixed "module SomeModule exposing (a)\n"
                    ]
    , test "should not report import if it exposes all (should be improved by detecting if any exposed value is used)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes exposing (..)"""
                |> Lint.Test.expectNoErrors
    , test "should not report used import (function access)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes
a = Html.Styled.Attributes.href"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused import if it is aliased" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html
a = Html.href"""
                |> Lint.Test.expectNoErrors
    , test "should report unused import alias" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 34 }, end = { row = 2, column = 38 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
import Html.Styled.Attributes"""
                    ]
    , test "should report unused import alias even if it exposes a used type" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html exposing (Attribute)
a : Attribute
a = ()"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 34 }, end = { row = 2, column = 38 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
import Html.Styled.Attributes exposing (Attribute)
a : Attribute
a = ()"""
                    ]
    , test "should report unused import alias even if it is named like an exposed type" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled as Html exposing (Html)
a : Html
a = ()"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 27 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)
import Html.Styled exposing (Html)
a : Html
a = ()"""
                    ]
    , test "should not report import that exposes a used exposed type" <|
        \() ->
            testRule """module SomeModule exposing (a)
import B exposing (C(..))
a : C
a = 1"""
                |> Lint.Test.expectNoErrors
    , test "should not report import that exposes an unused exposed type (but whose subtype is potentially used)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import B exposing (C(..))
a : D
a = 1"""
                |> Lint.Test.expectNoErrors
    ]


patternMatchingVariablesTests : List Test
patternMatchingVariablesTests =
    [ test "should not report unused pattern matching parameters" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = case thing of
    Foo b c -> []"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused variable when used as the expression in a case expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a =
    case b of
        _ -> 2"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern" <|
        \() ->
            testRule """module SomeModule exposing (a)
type Bar = Baz

a =
    case () of
        Baz ->
            []"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern (sub-pattern)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type Bar = Baz

a =
    case () of
        Just (Baz range) ->
            []"""
                |> Lint.Test.expectNoErrors
    , test "should not report unused import when a type from it is used in a pattern matching pattern" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Bar

a =
    case () of
        Just (Bar.Baz range) ->
            []"""
                |> Lint.Test.expectNoErrors
    ]


typeTests : List Test
typeTests =
    [ test "should report unused custom type declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a = 1"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report unused custom type constructors" <|
        -- This is handled by the `NoUnusedTypeConstructors` rule
        \() ->
            testRule """module SomeModule exposing (A)
type A = B | C"""
                |> Lint.Test.expectNoErrors
    , test "should report unused type aliases declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a = 1"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 13 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report type alias used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : A
a = {a = 1}"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a signature with multiple arguments" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : String -> A
a str = {a = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : A
a = {a = 1}"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a signature with multiple arguments" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : String -> A
a str = {a = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report parameterized custom type used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type CustomMaybe a = B a | C a
a : CustomMaybe D
a = []"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : List A
a = []"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : List A
a = []"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a signature with a record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : { c: A }
a str = {c = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a signature with a record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : { c: A }
a str = {c = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a signature with a generic record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : { r | c: A }
a str = {c = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a signature with a generic record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : { r | c: A }
a str = {c = str}"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a custom type constructor definition" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type ExposedType = Something A
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a custom type constructor definition" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B
type ExposedType = Something A
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used" <|
        \() ->
            testRule """module SomeModule exposing (b)
type A = B | C | D
b = B
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used even if it was defined afterwards" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = B
type A = B | C | D
"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in type signature inside a let..in" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a = let
      b : A
      b = 1
    in
    b
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in type signature inside a let..in" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = A
a = let
      b : A
      b = 1
    in
    b
"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a type alias field" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : A }
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a type alias field" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : A }
"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias used in a type alias field's arguments " <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : Maybe A }
"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type used in a type alias field's arguments " <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : Maybe A }
"""
                |> Lint.Test.expectNoErrors
    , test "should not report type alias if it is exposed" <|
        \() ->
            testRule """module SomeModule exposing (A)
type alias A = { a : B }"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type if it is exposed" <|
        \() ->
            testRule """module SomeModule exposing (A)
type A a = B a"""
                |> Lint.Test.expectNoErrors
    , test "should not report custom type if it is exposed with its sub-types" <|
        \() ->
            testRule """module SomeModule exposing (A(..))
type A = B | C | D"""
                |> Lint.Test.expectNoErrors
    , test "should report unused variable even if it is named like a custom type parameter" <|
        \() ->
            testRule """module SomeModule exposing (A)
a = 1
type A a = B a"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (A)

type A a = B a"""
                    ]
    , test "should report unused variable even if it is present in a generic record type" <|
        \() ->
            testRule """module SomeModule exposing (a)
r = 1
a : { r | c: A }
a str = {c = str}"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Top-level variable `r` is not used"
                        , details = details
                        , under = "r"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)

a : { r | c: A }
a str = {c = str}"""
                    ]
    ]


opaqueTypeTests : List Test
opaqueTypeTests =
    [ test "should report unused opaque types" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = A Int
a = 1"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Lint.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                        |> Lint.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report used opaque types" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = A Int
a : A
a = 1"""
                |> Lint.Test.expectNoErrors
    ]


operatorTests : List Test
operatorTests =
    [ test "should not report used operator (infix)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Parser exposing ((</>))
a = 1 </> 2"""
                |> Lint.Test.expectNoErrors
    , test "should not report used operator (prefix)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Parser exposing ((</>))
a = (</>) 2"""
                |> Lint.Test.expectNoErrors
    ]


portTests : List Test
portTests =
    [ test "should not report types that are used in ports" <|
        \() ->
            testRule """port module SomeModule exposing (output, input)
import Json.Decode
import Json.Encode
port output : Json.Encode.Value -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                |> Lint.Test.expectNoErrors
    , test "should not report used ports" <|
        \() ->
            testRule """port module SomeModule exposing (a, subscriptions)
import Json.Decode
port output : () -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg

a = output ()
subscriptions = input GotInput"""
                |> Lint.Test.expectNoErrors
    , test "should report unused ports (ingoing)" <|
        \() ->
            testRule """port module SomeModule exposing (a)
a = 1
port input : (() -> msg) -> Sub msg"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Port `input` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "input"
                        }
                        |> Lint.Test.whenFixed """port module SomeModule exposing (a)
a = 1
"""
                    ]
    , test "should report unused ports (outgoing)" <|
        \() ->
            testRule """port module SomeModule exposing (a)
a = 1
port output : String -> Cmd msg"""
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = "Port `output` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "output"
                        }
                        |> Lint.Test.whenFixed """port module SomeModule exposing (a)
a = 1
"""
                    ]
    ]
