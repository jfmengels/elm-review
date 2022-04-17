module NoUnused.VariablesTest exposing (all)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import NoUnused.Variables exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Test
import Review.Test.Dependencies
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
    , test "should report recursive functions declared in a let block that are not used elsewhere" <|
        \() ->
            """module SomeModule exposing (a)
a = let fib n = fib (n - 1) + fib (n - 2)
    in 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `fib` is not used"
                        , details = details
                        , under = "fib"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
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
    , test "should not report unused top-level variables if everything is exposed (functions)" <|
        \() ->
            """module SomeModule exposing (..)
a n = 1
b = a 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables if everything is exposed (custom types)" <|
        \() ->
            """module SomeModule exposing (..)
type A = A"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report unused top-level variables if everything is exposed (type aliases)" <|
        \() ->
            """module SomeModule exposing (..)
type alias A = ()"""
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
    , test "should not report 'main' as unused for applications, even if it's not exposed" <|
        \() ->
            """module SomeModule exposing (a)
main = Html.text "hello, world"
a = ()
            """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused 'main' as unused for packages" <|
        \() ->
            """module SomeModule exposing (a)
main = Html.text "hello, world"
a = ()"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Top-level variable `main` is not used"
                        , details = details
                        , under = "main"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = ()"""
                    ]
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
a = let
        unused : number
        unused = 1
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `unused` is not used"
                        , details = details
                        , under = "unused"
                        }
                        |> Review.Test.atExactly { start = { row = 4, column = 9 }, end = { row = 4, column = 15 } }
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
                        |> Review.Test.whenFixed ("""module SomeModule exposing (a, b)
a = let$
        c = 2
    in c""" |> String.replace "$" " ")
                    ]
    , test "should report unused variables from let even if they are exposed by name (multiple ones with type annotations)" <|
        \() ->
            """module SomeModule exposing (a, b)
a = let
        b : number
        b = 1

        c : number
        c = 2
    in c"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `b` is not used"
                        , details = details
                        , under = "b"
                        }
                        |> Review.Test.atExactly { start = { row = 4, column = 9 }, end = { row = 4, column = 10 } }
                        |> Review.Test.whenFixed ("""module SomeModule exposing (a, b)
a = let
       $

        c : number
        c = 2
    in c""" |> String.replace "$" " ")
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
    , test "should report wildcard assignments" <|
        \() ->
            """module SomeModule exposing (a)
a = let _ = 1
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value assigned to `_` is unused"
                        , details =
                            [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "_"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should not report a wildcard assignment used for a Debug.log call with all arguments (simple call)" <|
        \() ->
            """module SomeModule exposing (a)
a = let _ = Debug.log "ok" ()
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report a wildcard assignment used for a Debug.log call with all arguments (using <|)" <|
        \() ->
            """module SomeModule exposing (a)
a = let _ = Debug.log "ok" <| ()
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report a wildcard assignment used for a Debug.log call with all arguments (using |>)" <|
        \() ->
            """module SomeModule exposing (a)
a = let _ = () |> Debug.log "ok"
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report a wildcard assignment when used for a Debug.log call without all the arguments" <|
        \() ->
            """module SomeModule exposing (a)
a = let _ = Debug.log "ok"
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value assigned to `_` is unused"
                        , details =
                            [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "_"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report an unused named declaration even if it uses Debug.log" <|
        \() ->
            """module SomeModule exposing (a)
a = let xyz = Debug.log "ok" ()
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "`let in` variable `xyz` is not used"
                        , details =
                            [ "You should either use this value somewhere, or remove it at the location I pointed at."
                            ]
                        , under = "xyz"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report () destructuring" <|
        \() ->
            """module SomeModule exposing (a)
a = let () = b
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unit value is unused"
                        , details =
                            [ "This value has no data, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "()"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report () destructuring even if something comes afterwards" <|
        \() ->
            """module SomeModule exposing (a)
a = let () = b
        {c} = 1
    in c"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Unit value is unused"
                        , details =
                            [ "This value has no data, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "()"
                        }
                        |> Review.Test.whenFixed ("""module SomeModule exposing (a)
a = let$
        {c} = 1
    in c""" |> String.replace "$" " ")
                    ]
    , test "should report parenthesized wildcard assignments" <|
        \() ->
            """module SomeModule exposing (a)
a = let (_) = 1
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Value assigned to `_` is unused"
                        , details =
                            [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "_"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 2"""
                    ]
    , test "should report pattern match of data-less constructor" <|
        \() ->
            """module SomeModule exposing (a)
type Foo = Foo
a = let Foo = Foo
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern doesn't introduce any variables"
                        , details =
                            [ "This value has been computed but isn't assigned to any variable, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "Foo"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 9 }, end = { row = 3, column = 12 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
type Foo = Foo
a = 2"""
                    ]
    , test "should report pattern match that doesn't introduce any variables" <|
        \() ->
            """module SomeModule exposing (a)
type Foo = Foo
a = let ( Foo, (Bar _), _ ) = x
    in 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Pattern doesn't introduce any variables"
                        , details =
                            [ "This value has been computed but isn't assigned to any variable, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        , under = "( Foo, (Bar _), _ )"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
type Foo = Foo
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
                            """module SomeModule exposing (d)
import Foo
    exposing
        ( C
        , b
        )"""
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
    , test "should not report used import (function access)" <|
        \() ->
            """module SomeModule exposing (a)
import Html.Styled.Attributes
a = Html.Styled.Attributes.href"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report used import (let destructuring)" <|
        \() ->
            [ """module SomeModule exposing (a)
import B
a = let (B.B y) = x
    in y
"""
            , """module B exposing (B)
type B = B ()"""
            ]
                |> Review.Test.runOnModules rule
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
    , test "should not report open type import when we don't know what the constructors are" <|
        \() ->
            """module SomeModule exposing (a)
import B exposing (C(..))
a : C
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report open type import when at least one of the exposed constructors are used as a value (imported local module)" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
a = C_Value"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report open type import when the exposed constructor is shadowed by a local type alias when it is a record" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
type alias C_Value = {}
a = C_Value"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported type `C` is not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed ("""module A exposing (a)
import B$
type alias C_Value = {}
a = C_Value""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report open type import when the exposed constructor is NOT shadowed by a local type alias when it is not a record" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
type alias C = B.C
a = C"""
            , """module B exposing (C(..))
type C = C
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Type alias `C` is not used"
                            , details = details
                            , under = "C"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                            |> Review.Test.whenFixed """module A exposing (a)
import B exposing (C(..))
a = C"""
                        ]
                      )
                    ]
    , test "should report open type import when the exposed constructor is shadowed by a custom type constructor" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
type Type = C
a = C"""
            , """module B exposing (C(..))
type C = C
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported type `C` is not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed ("""module A exposing (a)
import B$
type Type = C
a = C""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report type alias that doesn't alias a record when it has the same name as a constructor defined in the same file" <|
        \() ->
            """module A exposing (a)
type A = B | C
type alias B = Int
a = B
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type alias `B` is not used"
                        , details = details
                        , under = "B"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 13 } }
                        |> Review.Test.whenFixed """module A exposing (a)
type A = B | C
a = B
"""
                    ]
    , test "should not report open type import when a constructor is used but the type is locally shadowed" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
type alias C = A.C

a : C -> String
a (C s) = s
"""
            , """module B exposing (C(..))
type C = C String
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report open type import when none of its constructors is used (imported dependency)" <|
        \() ->
            """module A exposing (a)
import Dependency exposing (C(..))
a = 1"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported type `C` is not used"
                        , details = details
                        , under = "C(..)"
                        }
                        |> Review.Test.whenFixed ("""module A exposing (a)
import Dependency$
a = 1""" |> String.replace "$" " ")
                    ]
    , test "should not report open type import when at least one of the exposed constructors are used as a value (imported dependency)" <|
        \() ->
            """module A exposing (a)
import Dependency exposing (C(..))
a = C_Value"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectNoErrors
    , test "should not report open type import when the dependency it comes from is unknown" <|
        \() ->
            """module A exposing (a)
import UnknownDependency exposing (C(..))
a = 1"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectNoErrors
    , test "should not report open type import when at least one of the exposed constructors is used in a pattern" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
a = case thing of
      C_Value -> 1
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should not report open type import when at least one of the exposed constructors are used in a let expression" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
a = let foo = C_Value
    in foo
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report open type import when none of the exposed constructors are used" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..))
a = 1
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported type `C` is not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed ("""module A exposing (a)
import B$
a = 1
""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report open type import when none of the exposed constructors are used, because they have been shadowed" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..), something)
type Something = C_Value
a = C_Value + something
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported type `C` is not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
import B exposing (something)
type Something = C_Value
a = C_Value + something
"""
                        ]
                      )
                    ]
    , test "should report open type import when none of the exposed constructors are used and the type itself has been shadowed" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..), something)
type C = Local_Value
a = Local_Value + something
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported type `C` is not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
import B exposing (something)
type C = Local_Value
a = Local_Value + something
"""
                        ]
                      )
                    ]
    , test "should not report open type import when at least of the exposed constructors even when the type itself has been shadowed" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..), something)
type C = Local_Value
a = C_Value + Local_Value + something
"""
            , """module B exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report open type import when none of the exposed constructors are used, but only remove the (..) when type is used" <|
        \() ->
            [ """module A exposing (a)
import B exposing (C(..), something)
a : C
a = B.something + something
"""
            , """module B exposing (C(..), something)
type C = C_Value
something = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported constructors for `C` are not used"
                            , details = details
                            , under = "C(..)"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
import B exposing (C, something)
a : C
a = B.something + something
"""
                        ]
                      )
                    ]
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
    , test "should report unused import even if a used let..in variable is named the same way" <|
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
    , test "should report unused import even if a used function param is named in the same way" <|
        \() ->
            [ """module A exposing (identity)
import Used exposing (shadowed)
identity shadowed = shadowed
"""
            , """module Used exposing (shadowed)
shadowed = ""
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (identity)
import Used$
identity shadowed = shadowed
""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report unused import even if a used let..in function param is named in the same way" <|
        \() ->
            [ """module A exposing (identity)
import Used exposing (shadowed)
identity x = 
    let
        identityHelp shadowed = shadowed
    in
    identityHelp x
"""
            , """module Used exposing (shadowed)
shadowed = ""
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (identity)
import Used$
identity x = 
    let
        identityHelp shadowed = shadowed
    in
    identityHelp x
""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report unused import even if a used lambda param is named in the same way" <|
        \() ->
            [ """module A exposing (identity)
import Used exposing (shadowed)
identity x = 
    (\\shadowed -> shadowed) x
"""
            , """module Used exposing (shadowed)
shadowed = ""
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (identity)
import Used$
identity x = 
    (\\shadowed -> shadowed) x
""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should not report used import even if a used lambda param is named in the same way elsewhere" <|
        \() ->
            """module A exposing (list)
import Html exposing (Html, label, text)

list : List (Html msg)
list =
    [ label [] []
    , Maybe.map
        (\\label ->
            text label
        )
        (Just "string")
        |> Maybe.withDefault (text "")
    ]
"""
                |> Review.Test.runWithProjectData
                    (Review.Test.Dependencies.projectWithElmCore
                        |> Project.addDependency Review.Test.Dependencies.elmHtml
                    )
                    rule
                |> Review.Test.expectNoErrors
    , test "should not report used import even if a used let variable is named in the same way elsewhere" <|
        \() ->
            """module A exposing (list)
import Html exposing (Html, label, text)

list : List (Html msg)
list =
    [ label [] []
    , let
        label =
            "string"
      in
      text label
    ]
"""
                |> Review.Test.runWithProjectData
                    (Review.Test.Dependencies.projectWithElmCore
                        |> Project.addDependency Review.Test.Dependencies.elmHtml
                    )
                    rule
                |> Review.Test.expectNoErrors
    , test "should report unused import even if a variant arg is named in the same way" <|
        \() ->
            [ """module A exposing (identity)
import Used exposing (shadowed)
identity x = 
    case Just x of
        Nothing -> x
        Just shadowed -> shadowed
"""
            , """module Used exposing (shadowed)
shadowed = ""
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (identity)
import Used$
identity x = 
    case Just x of
        Nothing -> x
        Just shadowed -> shadowed
""" |> String.replace "$" " ")
                        ]
                      )
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
    , test "should report unused imports even if everything is exposed" <|
        \() ->
            """module SomeModule exposing (..)
import Unused
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported module `Unused` is not used"
                        , details = details
                        , under = "Unused"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (..)
a = 1"""
                    ]
    , test "should report unused import from local module that exposes everything" <|
        \() ->
            [ """module A exposing (a)
import Unused exposing (..)
a = 1"""
            , """module Unused exposing (b)
b = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported module `Unused` is not used"
                            , details = details
                            , under = "Unused"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
a = 1"""
                        ]
                      )
                    ]
    , test "should not report import if it exposes all and its contents are unknown" <|
        \() ->
            """module SomeModule exposing (a)
import Unknown exposing (..)
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report imported type if it exposes the constructors and the module is unknown" <|
        \() ->
            """module SomeModule exposing (a)
import Unknown exposing (A(..))
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused import from dependency that exposes everything" <|
        \() ->
            """module SomeModule exposing (..)
import Dependency exposing (..)
a = 1"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported module `Dependency` is not used"
                        , details = details
                        , under = "Dependency"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (..)
a = 1"""
                    ]
    , test "should report unused aliased import from dependency that exposes everything" <|
        \() ->
            """module SomeModule exposing (..)
import Dependency as D exposing (..)
a = 1"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Imported module `Dependency` is not used"
                        , details = details
                        , under = "Dependency"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (..)
a = 1"""
                    ]
    , test "should report unused exposing from dependency that exposes everything when it is used with qualified imports" <|
        \() ->
            """module SomeModule exposing (a)
import Dependency exposing (..)
a = Dependency.C_Value"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "No imported elements from `Dependency` are used"
                        , details = details
                        , under = "exposing (..)"
                        }
                        |> Review.Test.whenFixed ("""module SomeModule exposing (a)
import Dependency$
a = Dependency.C_Value""" |> String.replace "$" " ")
                    ]
    , test "should report unused exposing from aliased dependency that exposes everything when it is used with qualified imports" <|
        \() ->
            """module SomeModule exposing (a)
import Dependency as D exposing (..)
a = D.C_Value"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "No imported elements from `Dependency` are used"
                        , details = details
                        , under = "exposing (..)"
                        }
                        |> Review.Test.whenFixed ("""module SomeModule exposing (a)
import Dependency as D$
a = D.C_Value""" |> String.replace "$" " ")
                    ]
    , test "should not report used exposing from dependency module that exposes everything" <|
        \() ->
            """module SomeModule exposing (..)
import Dependency exposing (..)
a = C_Value"""
                |> Review.Test.runWithProjectData packageProject rule
                |> Review.Test.expectNoErrors
    , test "should not report used exposing from local module that exposes everything (using function)" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (..)
a = b"""
            , """module Used exposing (b)
b = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should not report used exposing from local module that exposes everything (using type)" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (..)
a : C
a = 1"""
            , """module Used exposing (C)
type alias C = Int
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should not report used exposing from local module that exposes everything (using case of pattern)" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (..)
a = case () of
    C_Value -> 1"""
            , """module Used exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should not report used exposing from local module that exposes everything (using destructuring pattern)" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (..)
a C_Value = 1"""
            , """module Used exposing (C(..))
type C = C_Value
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report unused module alias from module exposing everything and where something is used implicitly" <|
        \() ->
            [ """module A exposing (a)
import Used as UnusedAlias exposing (..)
a = b"""
            , """module Used exposing (b)
b = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Module alias `UnusedAlias` is not used"
                            , details = details
                            , under = "UnusedAlias"
                            }
                            |> Review.Test.whenFixed """module A exposing (a)
import Used exposing (..)
a = b"""
                        ]
                      )
                    ]
    , test "should not report used exposing from local module that exposes everything that is aliased and alias is also used" <|
        \() ->
            [ """module A exposing (a)
import Used as U exposing (..)
a = U.b + b"""
            , """module Used exposing (b)
b = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
    , test "should report unused imported value if it is redefined" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (shadowed)
shadowed = 1
a = shadowed"""
            , """module Used exposing (shadowed)
shadowed = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (a)
import Used$
shadowed = 1
a = shadowed""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should report unused imported value if it is redefined, and should not report the top-level one even if used before declaration" <|
        \() ->
            [ """module A exposing (a)
import Used exposing (shadowed)
a = shadowed
shadowed = 1"""
            , """module Used exposing (shadowed)
shadowed = 1
"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectErrorsForModules
                    [ ( "A"
                      , [ Review.Test.error
                            { message = "Imported variable `shadowed` is not used"
                            , details = details
                            , under = "shadowed"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 31 } }
                            |> Review.Test.whenFixed ("""module A exposing (a)
import Used$
a = shadowed
shadowed = 1""" |> String.replace "$" " ")
                        ]
                      )
                    ]
    , test "should not report imported type as unused when it's used in a type annotation, and the name conflicts with an imported custom type constructor" <|
        \() ->
            [ """module Main exposing (thing, main)
import ModuleA exposing (A)
import ModuleB exposing (Variants(..))

thing : Variants
thing = A

main : A
main = ()
"""
            , """module ModuleA exposing (A)
type alias A = ()"""
            , """module ModuleB exposing (Variants(..))
type Variants = A"""
            ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expectNoErrors
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
    , test "should report unused custom type declaration even when it references itself" <|
        \() ->
            """module SomeModule exposing (a)
type Node = Node Int (List (Node))
a = 1"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `Node` is not used"
                        , details = details
                        , under = "Node"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 10 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1"""
                    ]
    , test "should report unused custom type declaration even if another constructor with the same name is used" <|
        \() ->
            """module SomeModule exposing (a)
type A = B | C
type Something = A
a = A"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type `A` is not used"
                        , details = details
                        , under = "A"
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 7 } }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
type Something = A
a = A"""
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
    , test "should not report type alias used in a let constant's signature" <|
        \() ->
            """module SomeModule exposing (a)
type alias A = { a : B }
a =
  let
    b : A
    b = {a = 1}
  in b"""
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
    , test "should not report unused type alias when it is used in a function call in a let expression" <|
        \() ->
            """module SomeModule exposing (outer)
type alias Baz = { a: String }
outer arg =
    let
        inner = Baz range
    in
    inner arg
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report unused type alias when it aliases something else than a record" <|
        \() ->
            """module SomeModule exposing (a)
type alias UnusedType = String
a = 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Type alias `UnusedType` is not used"
                        , details = details
                        , under = "UnusedType"
                        }
                        |> Review.Test.whenFixed """module SomeModule exposing (a)
a = 1
"""
                    ]
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
    , test "should not report exposed ports" <|
        \() ->
            """port module SomeModule exposing (output, input)
import Json.Decode
port output : () -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report exposed ports using (..)" <|
        \() ->
            """port module SomeModule exposing (..)
import Json.Decode
port output : () -> Cmd msg
port input : (Json.Decode.Value -> msg) -> Sub msg"""
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
                        |> Review.Test.whenFixed """module SomeModule exposing (apL)

apL : (a -> b) -> a -> b
apL f x =
  f x"""
                    ]
    ]


packageProject : Project
packageProject =
    Project.new
        |> Project.addElmJson (createElmJson rawPackageElmJson)
        |> Project.addDependency packageWithFoo


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawPackageElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)


rawPackageElmJson : String
rawPackageElmJson =
    """{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "package/author": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


packageWithFoo : Dependency
packageWithFoo =
    let
        modules : List Elm.Docs.Module
        modules =
            [ { name = "Dependency"
              , comment = ""
              , unions =
                    [ { name = "C"
                      , comment = ""
                      , args = []
                      , tags = [ ( "C_Value", [ Elm.Type.Var "a" ] ) ]
                      }
                    ]
              , aliases = []
              , values = []
              , binops = []
              }
            ]

        elmJson : Elm.Project.Project
        elmJson =
            .project <| createElmJson """
  {
      "type": "package",
      "name": "author/package-with-foo",
      "summary": "Summary",
      "license": "BSD-3-Clause",
      "version": "1.0.0",
      "exposed-modules": [
          "Dependency"
      ],
      "elm-version": "0.19.0 <= v < 0.20.0",
      "dependencies": {
          "elm/core": "1.0.0 <= v < 2.0.0"
      },
      "test-dependencies": {}
  }"""
    in
    Dependency.create
        "author/package-with-foo"
        elmJson
        modules
