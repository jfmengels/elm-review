module NoImportingEverythingTest exposing (all)

import NoImportingEverything exposing (Configuration, rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : Configuration -> String -> ReviewResult
testRule options =
    Review.Test.run (rule options)


details : List String
details =
    [ "Exposing `(..)` from a module means making all its exposed functions and types available in the file's namespace. This makes it hard to tell which module a function or type comes from."
    , "A preferred pattern is to import functions by name (`import Html exposing (div, span)`) or to use qualified imports (`import Html`, then `Html.div`). If the module name is too long, you can give an alias to the imported module (`import Html.Attributes as Attr`)."
    ]


tests : List Test
tests =
    [ test "should not report imports that do not expose anything" <|
        \() ->
            """module A exposing (..)
import Html
import Http"""
                |> testRule { exceptions = [] }
                |> Review.Test.expectNoErrors
    , test "should not report imports that expose functions by name" <|
        \() ->
            """module A exposing (..)
import Html exposing (a)
import Http exposing (a, b)"""
                |> testRule { exceptions = [] }
                |> Review.Test.expectNoErrors
    , test "should report imports that expose everything" <|
        \() ->
            """module A exposing (..)
import Html exposing (..)"""
                |> testRule { exceptions = [] }
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not expose everything from Html"
                        , details = details
                        , under = ".."
                        }
                        |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 25 } }
                    ]
    , test "should report imports from sub-modules" <|
        \() ->
            """module A exposing (a)
import Html.App exposing (..)"""
                |> testRule { exceptions = [] }
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not expose everything from Html.App"
                        , details = details
                        , under = ".."
                        }
                    ]
    , test "should report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [] }
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not expose everything from Html.Foo.Bar"
                        , details = details
                        , under = ".."
                        }
                    ]
    , test "should not report imports that expose everything that are in the exception list" <|
        \() ->
            """module A exposing (a)
import Html exposing (..)"""
                |> testRule { exceptions = [ "Html" ] }
                |> Review.Test.expectNoErrors
    , test "should not report imports from sub-modules that are in the exception list" <|
        \() ->
            """module A exposing (a)
import Html.App exposing (..)"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> Review.Test.expectNoErrors
    , test "should not report imports from sub-modules (multiple dots)" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [ "Html.Foo.Bar" ] }
                |> Review.Test.expectNoErrors
    , test "should report imports whose parent is ignored" <|
        \() ->
            """module A exposing (a)
import Html.Foo.Bar exposing (..)"""
                |> testRule { exceptions = [ "Html" ] }
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not expose everything from Html.Foo.Bar"
                        , details = details
                        , under = ".."
                        }
                    ]
    , test "should report imports whose sub-module is ignored" <|
        \() ->
            """module A exposing (a)
import Html exposing (..)"""
                |> testRule { exceptions = [ "Html.App" ] }
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not expose everything from Html"
                        , details = details
                        , under = ".."
                        }
                    ]
    ]


all : Test
all =
    describe "NoImportingEverything" tests
