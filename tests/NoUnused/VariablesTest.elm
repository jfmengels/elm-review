module NoUnused.VariablesTest exposing (all)

import NoUnused.Variables exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]


all : Test
all =
    describe "NoUnusedVariables"
        [ describe "Top-level variables" topLevelVariablesTests
        , describe "Recursive functions" recursiveFunctionsTests
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
        , describe "Operator declarations" operatorDeclarationTests
        ]


recursiveFunctionsTests : List Test
recursiveFunctionsTests =
    [ test "should report recursive functions that are not used elsewhere" <|
        \() ->
            """module SomeModule exposing (a)
fib n = fib (n - 1) + fib (n - 2)
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `fib` is not used"
                        , details = details
                        , under = "fib"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 4 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
    , test "should not report recursive functions that are used by other functions" <|
        \() ->
            """module SomeModule exposing (a)
a = fib 0
fib n = fib (n - 1) + fib (n - 2)"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report recursive functions that are exposed by the module" <|
        \() ->
            """module SomeModule exposing (fib)
fib n = fib (n - 1) + fib (n - 2)"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


topLevelVariablesTests : List Test
topLevelVariablesTests =
    [ test "should not report exposed top-level variables" <|
        \() ->
            """module SomeModule exposing (a)
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used top-level variables" <|
        \() ->
            """module SomeModule exposing (b)
a n = 1
b = a 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused top-level variables" <|
        \() ->
            """module SomeModule exposing (b)
b = 1
a = 2"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (b)
b = 1
a : Int
a = 2"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (b)
a: Int
a = 1
b = 2"""
                |> Review.Test.run rule
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
    , test "should report unused top-level variables with documentation attached" <|
        \() ->
            """module SomeModule exposing (b)
{-| Documentation
-}
unusedVar = 1
b = 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `unusedVar` is not used"
                        , details = details
                        , under = "unusedVar"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)
b = 2"""
                    ]
    , test "should report unused top-level variables with documentation attached even if they are annotated" <|
        \() ->
            """module SomeModule exposing (b)
{-| Documentation
-}
unusedVar : Int
unusedVar = 1
b = 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `unusedVar` is not used"
                        , details = details
                        , under = "unusedVar"
                        }
                        |> Review.Test.atExactly { start = { row = 5, column = 1 }, end = { row = 5, column = 10 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (b)
b = 2"""
                    ]
    , test "should not report unused top-level variables if everything is exposed" <|
        \() ->
            """module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name" <|
        \() ->
            """module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others" <|
        \() ->
            """module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Review.Test.run rule
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
            """port module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name (port module)" <|
        \() ->
            """port module SomeModule exposing (a, b)
a = 1
b = 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables that are exposed by name, but report others (port module)" <|
        \() ->
            """port module SomeModule exposing (a, b)
a = 1
b = 2
c = 3"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
href = 1
a = Html.Styled.Attributes.href"""
                |> Review.Test.run rule
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
    , test "should not report 'main' as unused, even if it's not exposed" <|
        \() ->
            """module SomeModule exposing (a)
main = Html.text "hello, world"
a = ()
            """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not remove too much" <|
        \() ->
            """module A exposing
    ( getModuleName
    , isInScope
    )


realFunctionOrType : List String -> String -> ModuleContext -> ( List String, String )
realFunctionOrType moduleName functionOrType (ModuleContext context) =
    if List.length moduleName == 1 then
        Dict.get

    else
        moduleName


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    NonemptyList.any (.names >> Dict.member name) scopes
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `realFunctionOrType` is not used"
                        , details = details
                        , under = "realFunctionOrType"
                        }
                        |> Review.Test.atExactly { start = { row = 8, column = 1 }, end = { row = 8, column = 19 } }
                        |> Review.Test.whenFixed """module A exposing
    ( getModuleName
    , isInScope
    )


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    NonemptyList.any (.names >> Dict.member name) scopes
"""
                    ]
    ]


letInTests : List Test
letInTests =
    [ test "should report unused variables from let declarations" <|
        \() ->
            """module SomeModule exposing (a)
a = let b = 1
    in 2"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a, b)
a = let b = 1
        c = 2
    in c"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a, b)
a = let b param = 1
    in 2"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (..)
a = let b = 1
    in 2"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
a = let c = 1
    in c"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report 'main' as unused, even if it's not an exception for top-level declarations" <|
        \() ->
            """module SomeModule exposing (a)
a = let main = 1
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `main` is not used"
                        , details = details
                        , under = "main"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    ]


topLevelVariablesUsedInLetInTests : List Test
topLevelVariablesUsedInLetInTests =
    [ test "should not report top-level variables used inside a let expression" <|
        \() ->
            """module SomeModule exposing (a)
b = 1
a = let c = 1
in b + c"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report top-level variables used inside let declarations" <|
        \() ->
            """module SomeModule exposing (a)
b = 1
a = let c = b
in c"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report top-level variables used in nested lets" <|
        \() ->
            """module SomeModule exposing (a)
b = 1
a = let
  c = b
  d = let
        e = 1
      in
        b + c + e
in
  d"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


recordUpdateTests : List Test
recordUpdateTests =
    [ test "should not report variables used in a record update expression's value to be updated" <|
        \() ->
            """module SomeModule exposing (a)
b = { c = 1 }
a = { b | c = 3 }"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report variables used in a record update expression's updates" <|
        \() ->
            """module SomeModule exposing (a)
b = { y = 1, z = 1 }
d = 3
e = 3
a = { b | y = d, z = e }"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report variables even if they appear as keys of a record update expression's updates" <|
        \() ->
            """module SomeModule exposing (a)
b = { z = 1, c = 2 }
c = 1
a = { b | c = 3 }"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
a n = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused import when a type is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
import Bar

a =
  \\(Bar.Baz range) -> []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type when it is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
type Baz = Baz String

a =
  \\(Baz value) -> []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias when it is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
type alias Baz = { a : String}

a =
  \\(Baz value) -> []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


importTests : List Test
importTests =
    [ test "should report unused imported functions" <|
        \() ->
            """module SomeModule exposing (b)
import Foo exposing (a)"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (d)
import Foo exposing (C, a, b)"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (d)
import Foo
    exposing
        ( C
        , a
        , b
        )"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C"
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
    , test "should report unused imported functions (multiple imports on several lines, function first)" <|
        \() ->
            """module SomeModule exposing (d)
import Foo
    exposing
        ( a
        , b
        )
d = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported variable `a` is not used"
                        , details = details
                        , under = "a"
                        }
                        |> Review.Test.whenFixed
                            """module SomeModule exposing (d)
import Foo
    exposing
        ( b
        )
d = 1"""
                    , Review.Test.error
                        { message = "Imported variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (d)
import Foo
    exposing
        ( a
        )
d = 1"""
                    ]
    , test "should report unused operator import" <|
        \() ->
            """module SomeModule exposing (a)
import Parser exposing ((</>))"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
import Html"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
import Html.Styled.Attributes"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
import Html.Styled.Attributes exposing (..)"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used import (function access)" <|
        \() ->
            """module SomeModule exposing (a)
import Html.Styled.Attributes
a = Html.Styled.Attributes.href"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused import if it is aliased" <|
        \() ->
            """module SomeModule exposing (a)
import Html.Styled.Attributes as Html
a = Html.href"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused import alias" <|
        \() ->
            """module SomeModule exposing (a)
import Html.Styled.Attributes as Html
import Foo
a= Foo.a"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `Html` is not used"
                        , details = details
                        , under = "Html"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 34 }, end = { row = 2, column = 38 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Foo
a= Foo.a"""
                    ]
    , test "should report unused import alias but not fix it if another alias is named like the original module name and we can't remove the whole import" <|
        \() ->
            """module SomeModule exposing (a)
import Html as CoreHtml exposing (div)
import Html.Styled.Attributes as Html
a= Html.a div"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `CoreHtml` is not used"
                        , details = details
                        , under = "CoreHtml"
                        }
                    ]
    , test "should report unused import alias but and fix it if another alias is named like the original module name but we can remove the whole import" <|
        \() ->
            """module SomeModule exposing (a)
import Html as CoreHtml
import Html.Styled.Attributes as Html
a= Html.a"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module alias `CoreHtml` is not used"
                        , details = details
                        , under = "CoreHtml"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Html.Styled.Attributes as Html
a= Html.a"""
                    ]
    , test "should report unused import alias even if it exposes a used type" <|
        \() ->
            """module SomeModule exposing (a)
import Html.Styled.Attributes as Html exposing (Attribute)
a : Attribute
a = ()"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
import Html.Styled as Html exposing (Html)
a : Html
a = ()"""
                |> Review.Test.run rule
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
    , test "should report import alias if the name is the same thing as the module name" <|
        \() ->
            """module SomeModule exposing (a)
import Html as Html
a = Html.div"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Module `Html` is aliased as itself"
                        , details = [ "The alias is the same as the module name, and brings no useful value" ]
                        , under = "Html"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 16 }, end = { row = 2, column = 20 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Html
a = Html.div"""
                    ]
    , test "should not report import that exposes a used exposed type" <|
        \() ->
            """module SomeModule exposing (a)
import B exposing (C(..))
a : C
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report import that exposes an unused exposed type (but whose subtype is potentially used)" <|
        \() ->
            """module SomeModule exposing (a)
import B exposing (C(..))
a : D
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused import alias but not remove it if another import is aliased as the real name of the reported import and it exposes something" <|
        \() ->
            [ """module A exposing (a)
import B as Unused exposing (b)
import C as B
a = b + B.c"""
            , """module B exposing (b)
b = C.c"""
            , """module C exposing (c)
c = Value"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Module alias `Unused` is not used"
                            , details = details
                            , under = "Unused"
                            }
                        ]
                      )
                    ]
    , test "should report unused import alias and remove it if another import is aliased as the real name of the reported import but it doesn't expose anything" <|
        \() ->
            [ """module A exposing (a)
import B as Unused
import C as B
a = B.b"""
            , """module B exposing (b)
b = 1"""
            , """module C exposing (c)
c = 1"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Module alias `Unused` is not used"
                            , details = details
                            , under = "Unused"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
import C as B
a = B.b"""
                        ]
                      )
                    ]
    , test "should not mark module as unused when using a qualified type from it" <|
        \() ->
            [ """module A exposing (foo)
import B as C
foo : C.Thing Account
foo user = 1
"""
            , """module B exposing (Thing)
type alias Thing a = {}"""
            , """module C exposing (c)
c = 1"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report unused import even if a let in variable is named the same way" <|
        \() ->
            """module SomeModule exposing (a)
import Html exposing (button, div)
a = let button = 1
    in button + div"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported variable `button` is not used"
                        , details = details
                        , under = "button"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 29 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Html exposing (div)
a = let button = 1
    in button + div"""
                    ]
    , test "should report unused import alias when two modules share the same alias" <|
        \() ->
            [ """module A exposing (a)
import B
import C as B
a = B.b"""
            , """module B exposing (b)
b = 1"""
            , """module C exposing (c)
c = 1"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Module alias `B` is not used"
                            , details = details
                            , under = "B"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 13 }, end = { row = 3, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (a)
import B
a = B.b"""
                        ]
                      )
                    ]
    ]


patternMatchingVariablesTests : List Test
patternMatchingVariablesTests =
    [ test "should not report unused pattern matching parameters" <|
        \() ->
            """module SomeModule exposing (a)
a = case thing of
    Foo b c -> []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused variable when used as the expression in a case expression" <|
        \() ->
            """module SomeModule exposing (a)
b = 1
a =
    case b of
        _ -> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern" <|
        \() ->
            """module SomeModule exposing (a)
type Bar = Baz
a =
    case () of
        Baz ->
            []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused type when it is used in a pattern matching pattern (sub-pattern)" <|
        \() ->
            """module SomeModule exposing (a)
type Bar = Baz
a =
    case () of
        Just (Baz range) ->
            []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused import when a type from it is used in a pattern matching pattern" <|
        \() ->
            """module SomeModule exposing (a)
import Bar
a =
    case () of
        Just (Bar.Baz range) ->
            []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused import when a type is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
import Bar
a (Bar.Baz range) =
    []
    """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused imports when a type is deconstructed in a function call in a let" <|
        \() ->
            """module SomeModule exposing (outer)
import Bar
outer arg =
    let
        inner (Bar.Baz range) =
            []
    in
    inner arg
    """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


typeTests : List Test
typeTests =
    [ test "should report unused custom type declarations" <|
        \() ->
            """module SomeModule exposing (a)
type UnusedType = B | C
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `UnusedType` is not used"
                        , details = details
                        , under = "UnusedType"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
    , test "should report unused custom type declarations with documentation" <|
        \() ->
            """module SomeModule exposing (a)
{-| Documentation -}
type UnusedType = B | C
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `UnusedType` is not used"
                        , details = details
                        , under = "UnusedType"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
    , test "should not report unused custom type constructors" <|
        -- This is handled by the `NoUnused.CustomTypeConstructors` rule
        \() ->
            """module SomeModule exposing (A)
type A = B | C"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused type aliases declarations" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a = 1"""
                |> Review.Test.run rule
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
    , test "should report unused type aliases declarations with documentation" <|
        \() ->
            """module SomeModule exposing (a)
{-| Documentation -}
type alias UnusedType = { a : B }
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `UnusedType` is not used"
                        , details = details
                        , under = "UnusedType"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
    , test "should not report type alias used in a signature" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a : A
a = {a = 1}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with multiple arguments" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a : String -> A
a str = {a = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
a : A
a = {a = 1}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with multiple arguments" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
a : String -> A
a str = {a = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report parameterized custom type used in a signature" <|
        \() ->
            """module SomeModule exposing (a)
type CustomMaybe a = B a | C a
a : CustomMaybe D
a = []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with parameterized types (as parameter)" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a : List A
a = []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with parameterized types (as parameter)" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
a : List A
a = []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with a record" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a : { c: A }
a str = {c = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with a record" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
a : { c: A }
a str = {c = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a signature with a generic record" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a signature with a generic record" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a custom type constructor definition" <|
        \() ->
            """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type ExposedType = Something A
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a custom type constructor definition" <|
        \() ->
            """module SomeModule exposing (ExposedType)
type A = B
type ExposedType = Something A
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used" <|
        \() ->
            """module SomeModule exposing (b)
type A = B | C | D
b = B
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type of which a constructor is used even if it was defined afterwards" <|
        \() ->
            """module SomeModule exposing (b)
b = B
type A = B | C | D
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in type signature inside a let..in" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a = let
      b : A
      b = 1
    in
    b
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in type signature inside a let..in" <|
        \() ->
            """module SomeModule exposing (a)
type A = A
a = let
      b : A
      b = 1
    in
    b
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a type alias field" <|
        \() ->
            """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : A }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a type alias field" <|
        \() ->
            """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : A }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias used in a type alias field's arguments " <|
        \() ->
            """module SomeModule exposing (ExposedType)
type alias A = { a : B }
type alias ExposedType = { a : Maybe A }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type used in a type alias field's arguments " <|
        \() ->
            """module SomeModule exposing (ExposedType)
type A = B | C
type alias ExposedType = { a : Maybe A }
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias if it is exposed" <|
        \() ->
            """module SomeModule exposing (A)
type alias A = { a : B }"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type if it is exposed" <|
        \() ->
            """module SomeModule exposing (A)
type A a = B a"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report custom type if it is exposed with its sub-types" <|
        \() ->
            """module SomeModule exposing (A(..))
type A = B | C | D"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused variable even if it is named like a custom type parameter" <|
        \() ->
            """module SomeModule exposing (A)
a = 1
type A a = B a"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
r = 1
a : { r | c: A }
a str = {c = str}"""
                |> Review.Test.run rule
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
    , test "should not report custom type when it is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
type Baz = Baz String

a (Baz range) =
    []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report type alias when it is deconstructed in a function call" <|
        \() ->
            """module SomeModule exposing (a)
type alias Baz = { a : String}
a (Baz value) =
    []"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused custom type when it is function call in a let" <|
        \() ->
            """module SomeModule exposing (outer)
type Baz = Baz String

outer arg =
    let
        inner (Baz range) =
            []
    in
    inner arg
    """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused type alias when it is function call in a let" <|
        \() ->
            """module SomeModule exposing (outer)
type alias Baz = { a: String }
outer arg =
    let
        inner (Baz range) =
            []
    in
    inner arg
    """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


opaqueTypeTests : List Test
opaqueTypeTests =
    [ test "should report unused opaque types" <|
        \() ->
            """module SomeModule exposing (a)
type A = A Int
a = 1"""
                |> Review.Test.run rule
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
            """module SomeModule exposing (a)
type A = A Int
a : A
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


operatorTests : List Test
operatorTests =
    [ test "should not report used operator (infix)" <|
        \() ->
            """module SomeModule exposing (a)
import Parser exposing ((</>))
a = 1 </> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used operator (prefix)" <|
        \() ->
            """module SomeModule exposing (a)
import Parser exposing ((</>))
a = (</>) 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused operator (infix)" <|
        \() ->
            """module SomeModule exposing (a)
import Parser exposing (something, (</>))
a = something"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported operator `</>` is not used"
                        , details = details
                        , under = "(</>)"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Parser exposing (something)
a = something"""
                    ]
    , test "should report unused operator (prefix)" <|
        \() ->
            """module SomeModule exposing (a)
import Parser exposing (something, (</>))
a = something"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported operator `</>` is not used"
                        , details = details
                        , under = "(</>)"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
import Parser exposing (something)
a = something"""
                    ]
    ]


portTests : List Test
portTests =
    [ test "should not report types that are used in ports" <|
        \() ->
            """port module SomeModule exposing (output, input)
import Json.Decode
import Json.Encode
port output : Json.Encode.Value -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used ports" <|
        \() ->
            """port module SomeModule exposing (a, subscriptions)
import Json.Decode
port output : () -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg

a = output ()
subscriptions = input GotInput"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused ports (ingoing)" <|
        \() ->
            """port module SomeModule exposing (a)
a = 1
port input : (() -> msg) -> Sub msg"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Port `input` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "input"
                        }
                    ]
    , test "should report unused ports (outgoing)" <|
        \() ->
            """port module SomeModule exposing (a)
a = 1
port output : String -> Cmd msg"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Port `output` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                        , details = details
                        , under = "output"
                        }
                    ]
    ]


operatorDeclarationTests : List Test
operatorDeclarationTests =
    [ test "should not report operator that is exposed" <|
        \() ->
            """module SomeModule exposing ((<|))
infix right 0 (<|) = apL
apL : (a -> b) -> a -> b
apL f x =
  f x"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used operator" <|
        \() ->
            """module SomeModule exposing (value)
value = apl <| 1
infix right 0 (<|) = apL
apL : (a -> b) -> a -> b
apL f x =
  f x"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused operator" <|
        \() ->
            """module SomeModule exposing (apL)
infix right 0 (<|) = apL
apL : (a -> b) -> a -> b
apL f x =
  f x"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Declared operator `<|` is not used"
                        , details = details
                        , under = "(<|)"
                        }
                    ]
    ]
