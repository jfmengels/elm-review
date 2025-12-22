module Simplify.ResultTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Result"
        [ resultMapTests
        , resultMapNTests
        , resultMapErrorTests
        , resultAndThenTests
        , resultWithDefaultTests
        , resultToMaybeTests
        , resultFromMaybeTests
        ]


resultMapTests : Test
resultMapTests =
    describe "Result.map"
        [ test "should not report Result.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.map f (Err z) by (Err z)" <|
            \() ->
                """module A exposing (..)
a = Result.map f (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an error will result in the given error"
                            , details = [ "You can replace this call by the given error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err z)
"""
                        ]
        , test "should replace Result.map f <| Err z by Err z" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Err z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an error will result in the given error"
                            , details = [ "You can replace this call by the given error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err z
"""
                        ]
        , test "should replace Err z |> Result.map f by Err z" <|
            \() ->
                """module A exposing (..)
a = Err z |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an error will result in the given error"
                            , details = [ "You can replace this call by the given error." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err z
"""
                        ]
        , test "should replace Result.andThen f << Err by Err" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an error will result in the unchanged error"
                            , details = [ "You can replace this composition by Err." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err
"""
                        ]
        , test "should replace Result.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Result.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Result.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Result.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Result.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Result.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Result.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.map f (Ok x) by (Ok (f x))" <|
            \() ->
                """module A exposing (..)
a = Result.map f (Ok x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Ok (f x))
"""
                        ]
        , test "should replace Result.map f <| Ok x by Ok (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Ok x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (f <| x)
"""
                        ]
        , test "should replace Ok x |> Result.map f by Ok (x |> f)" <|
            \() ->
                """module A exposing (..)
a = Ok x |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (x |> f)
"""
                        ]
        , test "should replace x |> Ok |> Result.map f by (x |> f) |> Ok" <|
            \() ->
                """module A exposing (..)
a = x |> Ok |> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) |> Ok
"""
                        ]
        , test "should replace Result.map f <| Ok <| x by Ok <| (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Result.map f <| Ok <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok <| (f <| x)
"""
                        ]
        , test "should replace Result.map f << Ok by Ok << f" <|
            \() ->
                """module A exposing (..)
a = Result.map f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok << f
"""
                        ]
        , test "should replace Ok >> Result.map f by f >> Ok" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Ok
"""
                        ]
        , test "should replace Result.map f << Ok << a by Ok << f << a" <|
            \() ->
                """module A exposing (..)
a = Result.map f << Ok << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok << f << a
"""
                        ]
        , test "should replace g << Result.map f << Ok by g << Ok << f" <|
            \() ->
                """module A exposing (..)
a = g << Result.map f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Ok << f
"""
                        ]
        , test "should replace Ok >> Result.map f >> g by f >> Ok >> g" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.map f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map on an okay result will result in Ok with the function applied to the value inside"
                            , details = [ "You can replace this call by Ok with the function directly applied to the value inside the okay result itself." ]
                            , under = "Result.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Ok >> g
"""
                        ]
        ]


resultMapNTests : Test
resultMapNTests =
    -- testing behavior only with representatives for 2-5
    describe "Result.mapN"
        [ test "should not report Result.map3 with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.map3
b = Result.map3 f
c = Result.map3 f result0
d = Result.map3 f result0 result1
e = Result.map3 f result0 result1 result2
f = Result.map3 f (Ok h) result1 result2 -- because this is a code style choice
f = Result.map3 f (Ok h)
g = Result.map3 f result0 result1 (Err x) -- because result0/1 can have an earlier error
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.map3 f (Ok a) (Ok b) (Ok c) by Ok (f a b c)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Ok a) (Ok b) (Ok c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where each result is an okay result will result in Ok on the values inside"
                            , details = [ "You can replace this call by Ok with the function applied to the values inside each okay result." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok (f a b c)
"""
                        ]
        , test "should replace c |> g |> Ok |> Result.map3 f (Ok a) (Ok b) by (c |> g) |> f a b |> Ok" <|
            \() ->
                """module A exposing (..)
a = c |> g |> Ok |> Result.map3 f (Ok a) (Ok b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where each result is an okay result will result in Ok on the values inside"
                            , details = [ "You can replace this call by Ok with the function applied to the values inside each okay result." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (c |> g) |> f a b |> Ok
"""
                        ]
        , test "should replace Result.map3 f (Ok a) (Ok b) <| Ok <| g <| c by Ok <| f a b <| (g <| c)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Ok a) (Ok b) <| Ok <| g <| c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where each result is an okay result will result in Ok on the values inside"
                            , details = [ "You can replace this call by Ok with the function applied to the values inside each okay result." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok <| f a b <| (g <| c)
"""
                        ]
        , test "should replace Result.map3 f (Ok a) (Err x) result2 by (Err x)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Ok a) (Err x) result2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where we know the first error will result in that error"
                            , details = [ "You can replace this call by the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err x)
"""
                        ]
        , test "should replace Result.map3 f (Ok a) (Err x) by always (Err x)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Ok a) (Err x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where we know the first error will result in that error"
                            , details = [ "You can replace this call by always with the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always (Err x)
"""
                        ]
        , test "should replace Result.map3 f (Err x) result1 result2 by (Err x)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Err x) result1 result2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where we know the first error will result in that error"
                            , details = [ "You can replace this call by the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err x)
"""
                        ]
        , test "should replace Result.map3 f (Err x) result1 by always (Err x)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Err x) result1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where we know the first error will result in that error"
                            , details = [ "You can replace this call by always with the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always (Err x)
"""
                        ]
        , test "should replace Result.map3 f (Err x) by (\\_ _ -> (Err x))" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f (Err x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 where we know the first error will result in that error"
                            , details = [ "You can replace this call by \\_ _ -> with the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> (Err x))
"""
                        ]
        , test "should replace Result.map3 f result0 (Err x) result2 by Result.map2 f result0 (Err x)" <|
            \() ->
                """module A exposing (..)
a = Result.map3 f result0 (Err x) result2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map3 with an error early will ignore later arguments"
                            , details = [ "You can replace this call by Result.map2 with the same arguments until the first error." ]
                            , under = "Result.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map2 f result0 (Err x)
"""
                        ]
        , test "should replace Result.map4 f result0 (Err x) result3 by always (Result.map2 f result0 (Err x))" <|
            \() ->
                """module A exposing (..)
a = Result.map4 f result0 (Err x) result3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map4 with an error early will ignore later arguments"
                            , details = [ "You can replace this call by always with Result.map2 with the same arguments until the first error." ]
                            , under = "Result.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always (Result.map2 f result0 (Err x))
"""
                        ]
        , test "should replace Result.map4 f result0 (Err x) by (\\_ _ -> Result.map2 f result0 (Err x))" <|
            \() ->
                """module A exposing (..)
a = Result.map4 f result0 (Err x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.map4 with an error early will ignore later arguments"
                            , details = [ "You can replace this call by \\_ _ -> with Result.map2 with the same arguments until the first error." ]
                            , under = "Result.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> Result.map2 f result0 (Err x))
"""
                        ]
        ]


resultMapErrorTests : Test
resultMapErrorTests =
    describe "Result.mapError"
        [ test "should not report Result.mapError used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.mapError
b = Result.mapError f
c = Result.mapError f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.mapError f (Ok z) by (Ok z)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f (Ok z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an okay result will result in the given okay result"
                            , details = [ "You can replace this call by the given okay result." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Ok z)
"""
                        ]
        , test "should replace Result.mapError f <| Ok z by Ok z" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Ok z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an okay result will result in the given okay result"
                            , details = [ "You can replace this call by the given okay result." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok z
"""
                        ]
        , test "should replace Ok z |> Result.mapError f by Ok z" <|
            \() ->
                """module A exposing (..)
a = Ok z |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an okay result will result in the given okay result"
                            , details = [ "You can replace this call by the given okay result." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok z
"""
                        ]
        , test "should replace Result.mapError f << Ok by Ok" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an okay result will result in the unchanged okay result"
                            , details = [ "You can replace this composition by Ok." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok
"""
                        ]
        , test "should replace Result.mapError identity x by x" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.mapError identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Result.mapError identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Result.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.mapError identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.mapError <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Result.mapError <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Result.mapError by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Result.mapError
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError with an identity function will always return the same given result"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Result.mapError f (Err x) by (Err (f x))" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f (Err x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err (f x))
"""
                        ]
        , test "should replace Result.mapError f <| Err x by Err (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Err x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err (f <| x)
"""
                        ]
        , test "should replace Err x |> Result.mapError f by Err (x |> f)" <|
            \() ->
                """module A exposing (..)
a = Err x |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err (x |> f)
"""
                        ]
        , test "should replace x |> Err |> Result.mapError f by (x |> f) |> Err" <|
            \() ->
                """module A exposing (..)
a = x |> Err |> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) |> Err
"""
                        ]
        , test "should replace Result.mapError f <| Err <| x by Err <| (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f <| Err <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err <| (f <| x)
"""
                        ]
        , test "should replace Result.mapError f << Err by Err << f" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should replace Err >> Result.mapError f by f >> Err" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Err
"""
                        ]
        , test "should replace Result.mapError f << Err << a by Err << f << a" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << Err << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f << a
"""
                        ]
        , test "should replace g << Result.mapError f << Err by g << Err << f" <|
            \() ->
                """module A exposing (..)
a = g << Result.mapError f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Err << f
"""
                        ]
        , test "should replace Err >> Result.mapError f >> g by f >> Err >> g" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.mapError f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Err >> g
"""
                        ]
        ]


resultAndThenTests : Test
resultAndThenTests =
    describe "Result.andThen"
        [ test "should not report Result.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen f (Err z) by (Err z)" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an error will result in the given error"
                            , details = [ "You can replace this call by the given error." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Err z)
"""
                        ]
        , test "should replace Result.andThen f << Err by Err" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an error will result in the unchanged error"
                            , details = [ "You can replace this composition by Err." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err
"""
                        ]
        , test "should not report Result.andThen (always (Err z)) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (always (Err z)) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen Ok x by x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen Ok x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen with a function equivalent to Ok will always return the same given result"
                            , details = [ "You can replace this call by the result itself." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.andThen (\\b -> Ok c) x by Result.map (\\b -> c) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> Ok c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen with a function that always returns an okay result is the same as Result.map with the function returning the value inside"
                            , details = [ "You can replace this call by Result.map with the function returning the value inside the okay result." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> c) x
"""
                        ]
        , test "should replace Result.andThen (\\b -> let y = 1 in Ok y) x by Result.map (\\b -> let y = 1 in y) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> let y = 1 in Ok y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen with a function that always returns an okay result is the same as Result.map with the function returning the value inside"
                            , details = [ "You can replace this call by Result.map with the function returning the value inside the okay result." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> let y = 1 in y) x
"""
                        ]
        , test "should replace Result.andThen (\\b -> if cond then Ok b else Ok c) x by Result.map (\\b -> if cond then b else c) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> if cond then Ok b else Ok c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen with a function that always returns an okay result is the same as Result.map with the function returning the value inside"
                            , details = [ "You can replace this call by Result.map with the function returning the value inside the okay result." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Result.map (\\b -> if cond then b else c) x
"""
                        ]
        , test "should not report Result.andThen (\\b -> if cond then Ok b else Err c) x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen (\\b -> if cond then Ok b else Err c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.andThen f (Ok x) by f x" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f (Ok x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an okay result is the same as applying the function to the value from the okay result"
                            , details = [ "You can replace this call by the function directly applied to the value inside the okay result." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f x
"""
                        ]
        , test "should replace Ok x |> Result.andThen f by x |> f" <|
            \() ->
                """module A exposing (..)
a = Ok x |> Result.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an okay result is the same as applying the function to the value from the okay result"
                            , details = [ "You can replace this call by the function directly applied to the value inside the okay result." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace Result.andThen f << Ok by f" <|
            \() ->
                """module A exposing (..)
a = Result.andThen f << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.andThen on an okay result is the same as applying the function to the value from the okay result"
                            , details = [ "You can replace this composition by the function given to Result.andThen." ]
                            , under = "Result.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        ]


resultWithDefaultTests : Test
resultWithDefaultTests =
    describe "Result.withDefault"
        [ test "should not report Result.withDefault used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x y
b = Result.withDefault << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.withDefault x (Err z) by x" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x (Err z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an error will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Result.withDefault x (Ok y) by y" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x (Ok y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an okay result will result in the value inside"
                            , details = [ "You can replace this call by the value inside the okay result." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace Result.withDefault x <| (Ok y) by y" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault x <| (Ok y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an okay result will result in the value inside"
                            , details = [ "You can replace this call by the value inside the okay result." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace (Ok y) |> Result.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = (Ok y) |> Result.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an okay result will result in the value inside"
                            , details = [ "You can replace this call by the value inside the okay result." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace y |> Ok |> Result.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = y |> Ok |> Result.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an okay result will result in the value inside"
                            , details = [ "You can replace this call by the value inside the okay result." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace Result.withDefault a << Ok by identity" <|
            \() ->
                """module A exposing (..)
a = Result.withDefault b << Ok
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.withDefault on an okay result will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Result.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


resultToMaybeTests : Test
resultToMaybeTests =
    describe "Result.toMaybe"
        [ test "should not report Result.toMaybe used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe
b = Result.toMaybe x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Result.toMaybe (Err a) by Nothing" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe (Err b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an error will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Result.toMaybe (Ok a) by Just (a)" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe (Ok b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an okay result will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the okay result." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace Result.toMaybe <| Ok a by Just a" <|
            \() ->
                """module A exposing (..)
a = Result.toMaybe <| Ok b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an okay result will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the okay result." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace Ok a |> Result.toMaybe by Just a" <|
            \() ->
                """module A exposing (..)
a = Ok b |> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an okay result will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the okay result." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace a |> Ok |> Result.toMaybe by Just a" <|
            \() ->
                """module A exposing (..)
a = b |> Ok |> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an okay result will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the okay result." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace Ok >> Result.toMaybe by Just" <|
            \() ->
                """module A exposing (..)
a = Ok >> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an okay result will always result in Just the value inside"
                            , details = [ "You can replace this call by Just." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        , test "should replace Err >> Result.toMaybe by always Nothing" <|
            \() ->
                """module A exposing (..)
a = Err >> Result.toMaybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.toMaybe on an error will result in Nothing"
                            , details = [ "You can replace this call by always Nothing." ]
                            , under = "Result.toMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Nothing
"""
                        ]
        ]


resultFromMaybeTests : Test
resultFromMaybeTests =
    describe "Result.fromMaybe"
        [ test "should not report Result.toMaybe used with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Result.fromMaybe
a1 = Result.fromMaybe x
a2 = Result.fromMaybe x result
a3 = Result.fromMaybe Nothing result
a4 = Result.fromMaybe (Just a) result
a5 = Result.fromMaybe (Err x) result
a6 = Result.fromMaybe (Ok a) result
a7 = Result.fromMaybe << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Nothing |> Result.fromMaybe x by Err x" <|
            \() ->
                """module A exposing (..)
a = Nothing |> Result.fromMaybe x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.fromMaybe on Nothing will result in Err with the given first value"
                            , details = [ "You can replace this call by Err with the given first value." ]
                            , under = "Result.fromMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err x
"""
                        ]
        , test "should replace a |> Just |> Result.fromMaybe x by a |> Ok" <|
            \() ->
                """module A exposing (..)
a = b |> Just |> Result.fromMaybe x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.fromMaybe on a just maybe will result in Ok with the value inside"
                            , details = [ "You can replace this call by Ok with the value inside the given just maybe." ]
                            , under = "Result.fromMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Ok
"""
                        ]
        , test "should replace Result.fromMaybe x <| if cond then Just a else Just b by if cond then Ok a else Ok b" <|
            \() ->
                """module A exposing (..)
a =
    Result.fromMaybe x <|
        if cond then
            Just b

        else
            Just c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.fromMaybe on a just maybe will result in Ok with the value inside"
                            , details = [ "You can replace this call by Ok with the value inside the given just maybe." ]
                            , under = "Result.fromMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if cond then
            Ok b

        else
            Ok c
"""
                        ]
        , test "should replace Result.fromMaybe x << Just by Ok" <|
            \() ->
                """module A exposing (..)
a = Result.fromMaybe x << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.fromMaybe on a just maybe will result in Ok with the value inside"
                            , details = [ "You can replace this call by Ok." ]
                            , under = "Result.fromMaybe"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Ok
"""
                        ]
        ]
