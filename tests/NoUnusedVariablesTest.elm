module NoUnusedVariablesTest exposing (all)

import Review.Rule.NoUnusedVariables exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule =
    Review.Test.run rule


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
                |> Review.Test.expectNoErrors
    , test "should not report used top-level variables" <|
        \() ->
            testRule """module SomeModule exposing (b)
a n = 1
b = a 1"""
                |> Review.Test.expectNoErrors
    , test "should report unused top-level variables" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = 1
a = 2"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)
b = 1
"""
                    ]
    , test "should report unused top-level variables with type annotation" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = 1
a : Int
a = 2"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.atExactly { start = { row = 4, column = 1 }, end = { row = 4, column = 2 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)
b = 1
"""
                    ]
    , test "should report unused top-level variables even if they are annotated" <|
        \() ->
            testRule """module SomeModule exposing (b)
a: Int
a = 1
b = 2"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)

b = 2"""
                    ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            testRule """module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a, b)
a = 1
b = 2
"""
                    ]
    , test "should not report unused top-level variables if everything is exposed (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            testRule """port module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Review.Test.whenFixed """port module SomeModule exposing (a, b)
a = 1
b = 2
"""
                    ]
    , test "should report unused variable even if a homonym from a module is used" <|
        \() ->
            testRule """module SomeModule exposing (a)
href = 1
a = Html.Styled.Attributes.href"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `href` is not used"
                        , details = details
                        , under = "href"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 5 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)

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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report unused variables from let even if they are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = let b = 1
        c = 2
    in c"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                        |> Review.Test.whenFixed "module SomeModule exposing (a, b)\na = let \n        c = 2\n    in c"
                    ]
    , test "should report unused function from let even if they are exposed by name" <|
        \() ->
            testRule """module SomeModule exposing (a, b)
a = let b param = 1
    in 2"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a, b)
a = 2"""
                    ]
    , test "should report unused variables from let even if everything is exposed" <|
        \() ->
            testRule """module SomeModule exposing (..)
a = let b = 1
    in 2"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (..)
a = 2"""
                    ]
    , test "should not report variables from let declarations that are used in the expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = let c = 1
    in c"""
                |> Review.Test.expectNoErrors
    ]


topLevelVariablesUsedInLetInTests : List Test
topLevelVariablesUsedInLetInTests =
    [ test "should not report top-level variables used inside a let expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a = let c = 1
in b + c"""
                |> Review.Test.expectNoErrors
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a = let c = b
in c"""
                |> Review.Test.expectNoErrors
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
                |> Review.Test.expectNoErrors
    ]


recordUpdateTests : List Test
recordUpdateTests =
    [ test "should not report variables used in a record update expression's value to be updated" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { c = 1 }
a = { b | c = 3 }"""
                |> Review.Test.expectNoErrors
    , test "should not report variables used in a record update expression's updates" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { y = 1, z = 1 }
d = 3
e = 3
a = { b | y = d, z = e }"""
                |> Review.Test.expectNoErrors
    , test "should report variables even if they appear as keys of a record update expression's updates" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = { z = 1, c = 2 }
c = 1
a = { b | c = 3 }"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `c` is not used"
                        , details = details
                        , under = "c"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 1 }, end = { row = 3, column = 2 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
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
                |> Review.Test.expectNoErrors
    ]


importTests : List Test
importTests =
    [ test "should report unused imported functions" <|
        \() ->
            testRule """module SomeModule exposing (b)
import Foo exposing (a)"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)
import Foo """
                    ]
    , test "should report unused imported functions (multiple imports)" <|
        \() ->
            testRule """module SomeModule exposing (d)
import Foo exposing (C, a, b)"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
import Foo exposing (a, b)"""
                    , Review.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
import Foo exposing (C, b)"""
                    , Review.Test.error
                        { message = "Imported variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C\n  "
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
import Foo
    exposing
        ( a
        , b
        )"""
                    , Review.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.whenFixed
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
                    , Review.Test.error
                        { message = "Imported variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported operator `</>` is not used"
                        , details = details
                        , under = "(</>)"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Parser """
                    ]
    , test "should report unused import" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported module `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Review.Test.whenFixed "module SomeModule exposing (a)\n"
                    ]
    , test "should report unused import (multiples segments)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported module `Html.Styled.Attributes` is not used"
                        , details = details
                        , under = "Html.Styled.Attributes"
                        }
                        |> Review.Test.whenFixed "module SomeModule exposing (a)\n"
                    ]
    , test "should not report import if it exposes all (should be improved by detecting if any exposed value is used)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes exposing (..)"""
                |> Review.Test.expectNoErrors
    , test "should not report used import (function access)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes
a = Html.Styled.Attributes.href"""
                |> Review.Test.expectNoErrors
    , test "should not report unused import if it is aliased" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html
a = Html.href"""
                |> Review.Test.expectNoErrors
    , test "should report unused import alias" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 34 }, end = { row = 2, column = 38 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Html.Styled.Attributes"""
                    ]
    , test "should report unused import alias even if it exposes a used type" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Html.Styled.Attributes as Html exposing (Attribute)
a : Attribute
a = ()"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 34 }, end = { row = 2, column = 38 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 27 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
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
                |> Review.Test.expectNoErrors
    , test "should not report import that exposes an unused exposed type (but whose subtype is potentially used)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import B exposing (C(..))
a : D
a = 1"""
                |> Review.Test.expectNoErrors
    ]


patternMatchingVariablesTests : List Test
patternMatchingVariablesTests =
    [ test "should not report unused pattern matching parameters" <|
        \() ->
            testRule """module SomeModule exposing (a)
a = case thing of
    Foo b c -> []"""
                |> Review.Test.expectNoErrors
    , test "should not report unused variable when used as the expression in a case expression" <|
        \() ->
            testRule """module SomeModule exposing (a)
b = 1
a =
    case b of
        _ -> 2"""
                |> Review.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern" <|
        \() ->
            testRule """module SomeModule exposing (a)
type Bar = Baz

a =
    case () of
        Baz ->
            []"""
                |> Review.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern (sub-pattern)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type Bar = Baz

a =
    case () of
        Just (Baz range) ->
            []"""
                |> Review.Test.expectNoErrors
    , test "should not report unused import when a type from it is used in a pattern matching pattern" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Bar

a =
    case () of
        Just (Bar.Baz range) ->
            []"""
                |> Review.Test.expectNoErrors
    ]


typeTests : List Test
typeTests =
    [ test "should report unused custom type declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a = 1"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report unused custom type constructors" <|
        -- This is handled by the `NoUnusedTypeConstructors` rule
        \() ->
            testRule """module SomeModule exposing (A)
type A = B | C"""
                |> Review.Test.expectNoErrors
    , test "should report unused type aliases declarations" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a = 1"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 13 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report type alias used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : A
a = {a = 1}"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with multiple arguments" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : String -> A
a str = {a = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : A
a = {a = 1}"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with multiple arguments" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : String -> A
a str = {a = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report parameterized custom type used in a signature" <|
        \() ->
            testRule """module SomeModule exposing (a)
type CustomMaybe a = B a | C a
a : CustomMaybe D
a = []"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : List A
a = []"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with parameterized types (as parameter)" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : List A
a = []"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with a record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : { c: A }
a str = {c = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with a record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : { c: A }
a str = {c = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with a generic record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type alias A = { a : B }
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with a generic record" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = B | C
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a custom type constructor definition" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type ExposedType = Something A
"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a custom type constructor definition" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B
type ExposedType = Something A
"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used" <|
        \() ->
            testRule """module SomeModule exposing (b)
type A = B | C | D
b = B
"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used even if it was defined afterwards" <|
        \() ->
            testRule """module SomeModule exposing (b)
b = B
type A = B | C | D
"""
                |> Review.Test.expectNoErrors
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
                |> Review.Test.expectNoErrors
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
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a type alias field" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : A }
"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a type alias field" <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : A }
"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a type alias field's arguments " <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : Maybe A }
"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a type alias field's arguments " <|
        \() ->
            testRule """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : Maybe A }
"""
                |> Review.Test.expectNoErrors
    , test "should not report type alias if it is exposed" <|
        \() ->
            testRule """module SomeModule exposing (A)
type alias A = { a : B }"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type if it is exposed" <|
        \() ->
            testRule """module SomeModule exposing (A)
type A a = B a"""
                |> Review.Test.expectNoErrors
    , test "should not report custom type if it is exposed with its sub-types" <|
        \() ->
            testRule """module SomeModule exposing (A(..))
type A = B | C | D"""
                |> Review.Test.expectNoErrors
    , test "should report unused variable even if it is named like a custom type parameter" <|
        \() ->
            testRule """module SomeModule exposing (A)
a = 1
type A a = B a"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (A)

type A a = B a"""
                    ]
    , test "should report unused variable even if it is present in a generic record type" <|
        \() ->
            testRule """module SomeModule exposing (a)
r = 1
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `r` is not used"
                        , details = details
                        , under = "r"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 2 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)

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
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)

a = 1"""
                    ]
    , test "should not report used opaque types" <|
        \() ->
            testRule """module SomeModule exposing (a)
type A = A Int
a : A
a = 1"""
                |> Review.Test.expectNoErrors
    ]


operatorTests : List Test
operatorTests =
    [ test "should not report used operator (infix)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Parser exposing ((</>))
a = 1 </> 2"""
                |> Review.Test.expectNoErrors
    , test "should not report used operator (prefix)" <|
        \() ->
            testRule """module SomeModule exposing (a)
import Parser exposing ((</>))
a = (</>) 2"""
                |> Review.Test.expectNoErrors
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
                |> Review.Test.expectNoErrors
    , test "should not report used ports" <|
        \() ->
            testRule """port module SomeModule exposing (a, subscriptions)
import Json.Decode
port output : () -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg

a = output ()
subscriptions = input GotInput"""
                |> Review.Test.expectNoErrors
    , test "should report unused ports (ingoing)" <|
        \() ->
            testRule """port module SomeModule exposing (a)
a = 1
port input : (() -> msg) -> Sub msg"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Port `input` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "input"
                        }
                        |> Review.Test.whenFixed """port module SomeModule exposing (a)
a = 1
"""
                    ]
    , test "should report unused ports (outgoing)" <|
        \() ->
            testRule """port module SomeModule exposing (a)
a = 1
port output : String -> Cmd msg"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Port `output` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "output"
                        }
                        |> Review.Test.whenFixed """port module SomeModule exposing (a)
a = 1
"""
                    ]
    ]
