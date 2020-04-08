module NoMissingTypeAnnotationTest exposing (all)

import NoMissingTypeAnnotation exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
    ]


all : Test
all =
    describe "NoMissingTypeAnnotation"
        [ test "should not report anything when all top-level declarations have a type signature" <|
            \_ ->
                """module A exposing (..)
hasTypeAnnotation : Int
hasTypeAnnotation = 1

alsoHasTypeAnnotation : String -> List Things
alsoHasTypeAnnotation = doSomething
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report when a declaration named `all...` that is of type `List <CustomTypeName>` does not have all the type constructors in its value (1)" <|
            \_ ->
                """module A exposing (..)
hasNoTypeAnnotation = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation`"
                            , details = details
                            , under = "hasNoTypeAnnotation"
                            }
                        ]
        , test "should not report anything for custom type declarations" <|
            \_ ->
                """module A exposing (..)
type A = B | C
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything for type alias declarations" <|
            \_ ->
                """module A exposing (..)
type alias A = { a : String }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything for port declarations" <|
            \_ ->
                """module A exposing (..)
port toJavaScript : Int -> Cmd msg
port fromJavaScript : (Int -> msg) -> Sub msg
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
