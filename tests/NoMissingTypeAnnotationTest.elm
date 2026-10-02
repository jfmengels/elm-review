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
        [ test "should not report anything when all top-level declarations have a type annotation" <|
            \_ ->
                """module A exposing (..)
hasTypeAnnotation : Int
hasTypeAnnotation = 1

alsoHasTypeAnnotation : String -> List Int
alsoHasTypeAnnotation str = []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report a missing type annotation" <|
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
                            |> Review.Test.whenFixed """module A exposing (..)
hasNoTypeAnnotation : number
hasNoTypeAnnotation = 1
"""
                        ]
        , test "should report a missing type annotation and qualify types according to existing imports" <|
            \_ ->
                """module A exposing (..)
import Dict
import Set exposing (Set)
hasNoTypeAnnotation = Dict.singleton "" Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation`"
                            , details = details
                            , under = "hasNoTypeAnnotation"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
import Set exposing (Set)
hasNoTypeAnnotation : Dict.Dict String (Set a)
hasNoTypeAnnotation = Dict.singleton "" Set.empty
"""
                        ]
        , test "should report a missing type annotation and add missing imports" <|
            \_ ->
                [ """module A exposing (..)
import B
import Set

hasNoTypeAnnotation = B.value
""", """module B exposing (..)
import Dict
import Set exposing (Set)

type alias X = Int

value : Dict.Dict String (Set X)
value = Dict.singleton "" Set.empty
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expect
                        [ Review.Test.moduleErrors "A"
                            [ Review.Test.error
                                { message = "Missing type annotation for `hasNoTypeAnnotation`"
                                , details = details
                                , under = "hasNoTypeAnnotation"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Dict
import B
import Set

hasNoTypeAnnotation : Dict.Dict String (Set.Set B.X)
hasNoTypeAnnotation = B.value
"""
                            ]
                        ]
        , test "should report a missing type annotation for record" <|
            \_ ->
                """module A exposing (..)
hasNoTypeAnnotation = { a = "", b = [ True ] }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Missing type annotation for `hasNoTypeAnnotation`"
                            , details = details
                            , under = "hasNoTypeAnnotation"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
hasNoTypeAnnotation : { a : String, b : List Bool }
hasNoTypeAnnotation = { a = "", b = [ True ] }
"""
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
