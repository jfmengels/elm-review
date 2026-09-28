module NoMissingTypeAnnotationInLetInTest exposing (all)

import NoMissingTypeAnnotationInLetIn exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


details : List String
details =
    [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
    ]


all : Test
all =
    describe "NoMissingTypeAnnotationInLetIn"
        [ test "should not report anything for top-level declarations even if they have no type annotation" <|
            \_ ->
                """module A exposing (..)
hasTypeAnnotation : Int
hasTypeAnnotation = 1

alsoHasTypeAnnotation str = []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report anything when all let in declarations have a type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      b : number
      b = 1

      c : number
      c = 1
    in
    b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a let in declaration has no type annotation" <|
            \_ ->
                """module A exposing (..)
a = let
      hasNoTypeAnnotation_1 = 1
      hasNoTypeAnnotation_2 = ""
    in
    2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_1`"
                            , details = details
                            , under = "hasNoTypeAnnotation_1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = let
      hasNoTypeAnnotation_1 : number
      hasNoTypeAnnotation_1 = 1
      hasNoTypeAnnotation_2 = ""
    in
    2
"""
                        , Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation_2`"
                            , details = details
                            , under = "hasNoTypeAnnotation_2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = let
      hasNoTypeAnnotation_1 = 1
      hasNoTypeAnnotation_2 : String
      hasNoTypeAnnotation_2 = ""
    in
    2
"""
                        ]
        , test "should not report anything for let..in destructuring" <|
            \_ ->
                """module A exposing (..)
type Thing = Thing Int
a = let
      (b, c) = (1, 2)
      {e, f} = { e = 1, f = 2 }
      (Thing thing) = Thing 1
    in
    b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
