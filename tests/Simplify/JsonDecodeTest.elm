module Simplify.JsonDecodeTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    Test.describe "Json.Decode"
        [ jsonDecodeMapTests
        , jsonDecodeMapNTests
        , jsonDecodeAndThenTests
        , jsonDecodeOneOfTests
        ]


jsonDecodeMapTests : Test
jsonDecodeMapTests =
    describe "Json.Decode.map"
        [ test "should not report Json.Decode.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f json decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.map f (Json.Decode.fail z) by (Json.Decode.fail z)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f (Json.Decode.fail z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a failing decoder will result in the given failing decoder"
                            , details = [ "You can replace this call by the given failing decoder." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail z
"""
                        ]
        , test "should replace Json.Decode.map f <| Json.Decode.fail z by Json.Decode.fail z" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f <| Json.Decode.fail z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a failing decoder will result in the given failing decoder"
                            , details = [ "You can replace this call by the given failing decoder." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail z
"""
                        ]
        , test "should replace Json.Decode.fail z |> Json.Decode.map f by Json.Decode.fail z" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.fail z |> Json.Decode.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a failing decoder will result in the given failing decoder"
                            , details = [ "You can replace this call by the given failing decoder." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail z
"""
                        ]
        , test "should replace Json.Decode.map f << Json.Decode.fail by Json.Decode.fail" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f << Json.Decode.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a failing decoder will result in the unchanged failing decoder"
                            , details = [ "You can replace this composition by Json.Decode.fail." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail
"""
                        ]
        , test "should replace Json.Decode.map identity json decoder by json decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map identity json decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map with an identity function will always return the same given json decoder"
                            , details = [ "You can replace this call by the json decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = json decoder
"""
                        ]
        , test "should replace Json.Decode.map identity <| json decoder by json decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map identity <| json decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map with an identity function will always return the same given json decoder"
                            , details = [ "You can replace this call by the json decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = json decoder
"""
                        ]
        , test "should replace json decoder |> Json.Decode.map identity by json decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = json decoder |> Json.Decode.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map with an identity function will always return the same given json decoder"
                            , details = [ "You can replace this call by the json decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = json decoder
"""
                        ]
        , test "should replace Json.Decode.map identity by identity" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map with an identity function will always return the same given json decoder"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = identity
"""
                        ]
        , test "should replace Json.Decode.map f (Json.Decode.succeed x) by (Json.Decode.succeed (f x))" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f (Json.Decode.succeed x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = (Json.Decode.succeed (f x))
"""
                        ]
        , test "should replace Json.Decode.map f <| Json.Decode.succeed x by Json.Decode.succeed (f <| x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f <| Json.Decode.succeed x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed (f <| x)
"""
                        ]
        , test "should replace Json.Decode.succeed x |> Json.Decode.map f by Json.Decode.succeed (x |> f)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed x |> Json.Decode.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed (x |> f)
"""
                        ]
        , test "should replace x |> Json.Decode.succeed |> Json.Decode.map f by (x |> f) |> Json.Decode.succeed" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = x |> Json.Decode.succeed |> Json.Decode.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = (x |> f) |> Json.Decode.succeed
"""
                        ]
        , test "should replace Json.Decode.map f <| Json.Decode.succeed <| x by Json.Decode.succeed <| (f <| x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f <| Json.Decode.succeed <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed <| (f <| x)
"""
                        ]
        , test "should replace Json.Decode.map f << Json.Decode.succeed by Json.Decode.succeed << f" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map f << Json.Decode.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map on a succeeding decoder will result in Json.Decode.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function directly applied to the value inside the succeeding decoder itself." ]
                            , under = "Json.Decode.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed << f
"""
                        ]
        ]


jsonDecodeMapNTests : Test
jsonDecodeMapNTests =
    -- testing behavior only with representatives for 2-8
    describe "Json.Decode.mapN"
        [ test "should not report Json.Decode.map3 with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3
b = Json.Decode.map3 f
c = Json.Decode.map3 f decoder0
d = Json.Decode.map3 f decoder0 decoder1
e = Json.Decode.map3 f decoder0 decoder1 decoder2
f = Json.Decode.map3 f (Json.Decode.succeed h) decoder1 decoder2 -- because this is a code style choice
f = Json.Decode.map3 f (Json.Decode.succeed h)
g = Json.Decode.map3 f decoder0 decoder1 (Json.Decode.fail x) -- because decoder0/1 can have an earlier failing decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) (Json.Decode.succeed c) by Json.Decode.succeed (f a b c)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) (Json.Decode.succeed c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where each json decoder is a succeeding decoder will result in Json.Decode.succeed on the values inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function applied to the values inside each succeeding decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed (f a b c)
"""
                        ]
        , test "should replace c |> g |> Json.Decode.succeed |> Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) by (c |> g) |> f a b |> Json.Decode.succeed" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = c |> g |> Json.Decode.succeed |> Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where each json decoder is a succeeding decoder will result in Json.Decode.succeed on the values inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function applied to the values inside each succeeding decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = (c |> g) |> f a b |> Json.Decode.succeed
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) <| Json.Decode.succeed <| g <| c by Json.Decode.succeed <| f a b <| (g <| c)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) <| Json.Decode.succeed <| g <| c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where each json decoder is a succeeding decoder will result in Json.Decode.succeed on the values inside"
                            , details = [ "You can replace this call by Json.Decode.succeed with the function applied to the values inside each succeeding decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed <| f a b <| (g <| c)
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x) decoder2 by (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x) decoder2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where we know the first failing decoder will result in that failing decoder"
                            , details = [ "You can replace this call by the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail x
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x) by always (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where we know the first failing decoder will result in that failing decoder"
                            , details = [ "You can replace this call by always with the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = always (Json.Decode.fail x)
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.fail x) decoder1 decoder2 by (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.fail x) decoder1 decoder2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where we know the first failing decoder will result in that failing decoder"
                            , details = [ "You can replace this call by the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail x
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.fail x) decoder1 by always (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.fail x) decoder1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where we know the first failing decoder will result in that failing decoder"
                            , details = [ "You can replace this call by always with the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = always (Json.Decode.fail x)
"""
                        ]
        , test "should replace Json.Decode.map3 f (Json.Decode.fail x) by (\\_ _ -> (Json.Decode.fail x))" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f (Json.Decode.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 where we know the first failing decoder will result in that failing decoder"
                            , details = [ "You can replace this call by \\_ _ -> with the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = (\\_ _ -> Json.Decode.fail x)
"""
                        ]
        , test "should replace Json.Decode.map3 f decoder0 (Json.Decode.fail x) decoder2 by Json.Decode.map2 f decoder0 (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map3 f decoder0 (Json.Decode.fail x) decoder2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map3 with a failing decoder early will ignore later arguments"
                            , details = [ "You can replace this call by Json.Decode.map2 with the same arguments until the first failing decoder." ]
                            , under = "Json.Decode.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.map2 f decoder0 (Json.Decode.fail x)
"""
                        ]
        , test "should replace Json.Decode.map4 f decoder0 (Json.Decode.fail x) decoder3 by always (Json.Decode.map2 f decoder0 (Json.Decode.fail x))" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map4 f decoder0 (Json.Decode.fail x) decoder3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map4 with a failing decoder early will ignore later arguments"
                            , details = [ "You can replace this call by always with Json.Decode.map2 with the same arguments until the first failing decoder." ]
                            , under = "Json.Decode.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = always (Json.Decode.map2 f decoder0 (Json.Decode.fail x))
"""
                        ]
        , test "should replace Json.Decode.map4 f decoder0 (Json.Decode.fail x) by (\\_ _ -> Json.Decode.map2 f decoder0 (Json.Decode.fail x))" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.map4 f decoder0 (Json.Decode.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.map4 with a failing decoder early will ignore later arguments"
                            , details = [ "You can replace this call by \\_ _ -> with Json.Decode.map2 with the same arguments until the first failing decoder." ]
                            , under = "Json.Decode.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = (\\_ _ -> Json.Decode.map2 f decoder0 (Json.Decode.fail x))
"""
                        ]
        ]


jsonDecodeAndThenTests : Test
jsonDecodeAndThenTests =
    describe "Json.Decode.andThen"
        [ test "should not report Json.Decode.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen
b = Json.Decode.andThen f
c = Json.Decode.andThen f decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.andThen f (Json.Decode.fail x) by (Json.Decode.fail x)" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen f (Json.Decode.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen on a failing decoder will result in the given failing decoder"
                            , details = [ "You can replace this call by the given failing decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail x
"""
                        ]
        , test "should replace Json.Decode.andThen f << Json.Decode.fail by Json.Decode.fail" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen f << Json.Decode.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen on a failing decoder will result in the unchanged failing decoder"
                            , details = [ "You can replace this composition by Json.Decode.fail." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.fail
"""
                        ]
        , test "should not report Json.Decode.andThen (always (Json.Decode.fail x)) decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen (always (Json.Decode.fail x)) decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.andThen Json.Decode.succeed decoder by decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen Json.Decode.succeed decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen with a function equivalent to Json.Decode.succeed will always return the same given json decoder"
                            , details = [ "You can replace this call by the json decoder itself." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = decoder
"""
                        ]
        , test "should replace Json.Decode.andThen (\\b -> Json.Decode.succeed c) decoder by Json.Decode.map (\\b -> c) decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen (\\b -> Json.Decode.succeed c) decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen with a function that always returns a succeeding decoder is the same as Json.Decode.map with the function returning the value inside"
                            , details = [ "You can replace this call by Json.Decode.map with the function returning the value inside the succeeding decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.map (\\b -> c) decoder
"""
                        ]
        , test "should replace Json.Decode.andThen (\\b -> let y = 1 in Json.Decode.succeed y) decoder by Json.Decode.map (\\b -> let y = 1 in y) decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen (\\b -> let y = 1 in Json.Decode.succeed y) decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen with a function that always returns a succeeding decoder is the same as Json.Decode.map with the function returning the value inside"
                            , details = [ "You can replace this call by Json.Decode.map with the function returning the value inside the succeeding decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.map (\\b -> let y = 1 in y) decoder
"""
                        ]
        , test "should replace Json.Decode.andThen (\\b -> if cond then Json.Decode.succeed b else Json.Decode.succeed c) decoder by Json.Decode.map (\\b -> if cond then b else c) decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen (\\b -> if cond then Json.Decode.succeed b else Json.Decode.succeed c) decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen with a function that always returns a succeeding decoder is the same as Json.Decode.map with the function returning the value inside"
                            , details = [ "You can replace this call by Json.Decode.map with the function returning the value inside the succeeding decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = Json.Decode.map (\\b -> if cond then b else c) decoder
"""
                        ]
        , test "should not report Json.Decode.andThen (\\b -> if cond then Json.Decode.succeed b else Json.Decode.fail c) decoder" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen (\\b -> if cond then Json.Decode.succeed b else Json.Decode.fail c) decoder
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.andThen f (Json.Decode.succeed a) by f a" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen f (Json.Decode.succeed a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen on a succeeding decoder is the same as applying the function to the value from the succeeding decoder"
                            , details = [ "You can replace this call by the function directly applied to the value inside the succeeding decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = f a
"""
                        ]
        , test "should replace Json.Decode.succeed a |> Json.Decode.andThen f by a |> f" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.succeed a |> Json.Decode.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen on a succeeding decoder is the same as applying the function to the value from the succeeding decoder"
                            , details = [ "You can replace this call by the function directly applied to the value inside the succeeding decoder." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = a |> f
"""
                        ]
        , test "should replace Json.Decode.andThen f << Json.Decode.succeed by f" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.andThen f << Json.Decode.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.andThen on a succeeding decoder is the same as applying the function to the value from the succeeding decoder"
                            , details = [ "You can replace this composition by the function given to Json.Decode.andThen." ]
                            , under = "Json.Decode.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = f
"""
                        ]
        ]


jsonDecodeOneOfTests : Test
jsonDecodeOneOfTests =
    describe "Json.Decode.oneOf"
        [ test "should not report Json.Decode.oneOf used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.oneOf x
b = Json.Decode.oneOf [ y, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Decode.oneOf [ x ] by x" <|
            \() ->
                """module A exposing (..)
import Json.Decode
a = Json.Decode.oneOf [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Json.Decode.oneOf on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Json.Decode.oneOf"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Decode
a = x
"""
                        ]
        ]
