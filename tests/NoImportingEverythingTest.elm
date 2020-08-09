module NoImportingEverythingTest exposing (all)

import NoImportingEverything exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoImportingEverything"
        [ test "should not report imports without exposing clause" <|
            \_ ->
                """module A exposing (thing)
import Html
import Html as B
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report imports that expose some elements" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (B, c)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should not report imports that expose all constructors of a type" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (B(..))
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectNoErrors
        , test "should report imports that expose everything" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (..)
"""
                    |> Review.Test.run (rule [])
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Prefer listing what you wish to import and/or using qualified imports"
                            , details = [ "When you import everything from a module it becomes harder to know where a function or a type comes from." ]
                            , under = "(..)"
                            }
                        ]
        , test "should not report imports that are in the exceptions list" <|
            \_ ->
                """module A exposing (thing)
import Html exposing (..)
import Thing.Foo as Foo exposing (..)
"""
                    |> Review.Test.run (rule [ "Html", "Thing.Foo" ])
                    |> Review.Test.expectNoErrors
        ]
