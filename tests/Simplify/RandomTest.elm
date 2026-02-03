module Simplify.RandomTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    Test.describe "Random"
        [ randomUniformTests
        , randomWeightedChecks
        , randomListTests
        , randomMapTests
        , randomAndThenTests
        ]


randomUniformTests : Test
randomUniformTests =
    Test.describe "Random.uniform"
        [ test "should not report Random.uniform used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.uniform
b = Random.uniform []
c = Random.uniform first
d = Random.uniform head tail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Random.uniform a [] by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.uniform a []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.uniform with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.uniform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        , test "should replace Random.uniform a <| [] by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.uniform a <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.uniform with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.uniform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        , test "should replace [] |> Random.uniform a by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = [] |> Random.uniform a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.uniform with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.uniform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        ]


randomWeightedChecks : Test
randomWeightedChecks =
    Test.describe "Random.weighted"
        [ test "should not report Random.uniform used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.weighted
b = Random.weighted []
c = Random.weighted first
d = Random.weighted head tail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Random.weighted ( w, a ) [] by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.weighted ( w, a ) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        , test "should replace Random.weighted ( w, a ) <| [] by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.weighted ( w, a ) <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        , test "should replace [] |> Random.weighted ( w, a ) by Random.constant a" <|
            \() ->
                """module A exposing (..)
import Random
a = [] |> Random.weighted ( w, a )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant a
"""
                        ]
        , test "should replace Random.weighted tuple [] by Random.constant (Tuple.first tuple)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.weighted tuple []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (Tuple.first tuple)
"""
                        ]
        , test "should replace Random.weighted tuple <| [] by Random.constant (Tuple.first tuple)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.weighted tuple <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (Tuple.first tuple)
"""
                        ]
        , test "should replace [] |> Random.weighted tuple by Random.constant (Tuple.first tuple)" <|
            \() ->
                """module A exposing (..)
import Random
a = [] |> Random.weighted tuple
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.weighted with one possible value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the first given value." ]
                            , under = "Random.weighted"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (Tuple.first tuple)
"""
                        ]
        ]


randomListTests : Test
randomListTests =
    Test.describe "Random.list"
        [ test "should not report Random.uniform used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list
b = Random.list n
c = Random.list 2 generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Random.list 1 by Random.map List.singleton" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list 1 will result in Random.map List.singleton"
                            , details = [ "You can replace this call by Random.map List.singleton." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map List.singleton
"""
                        ]
        , test "should replace 1 |> Random.list by Random.map List.singleton" <|
            \() ->
                """module A exposing (..)
import Random
a = 1 |> Random.list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list 1 will result in Random.map List.singleton"
                            , details = [ "You can replace this call by Random.map List.singleton." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map List.singleton
"""
                        ]
        , test "should replace Random.list 1 generator by Random.map List.singleton generator" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 1 generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list 1 will result in Random.map List.singleton"
                            , details = [ "You can replace this call by Random.map List.singleton." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map List.singleton generator
"""
                        ]
        , test "should replace Random.list 1 <| generator by Random.map List.singleton <| generator" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 1 <| generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list 1 will result in Random.map List.singleton"
                            , details = [ "You can replace this call by Random.map List.singleton." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map List.singleton <| generator
"""
                        ]
        , test "should replace generator |> Random.list 1 by generator |> Random.map List.singleton" <|
            \() ->
                """module A exposing (..)
import Random
a = generator |> Random.list 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list 1 will result in Random.map List.singleton"
                            , details = [ "You can replace this call by Random.map List.singleton." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = generator |> Random.map List.singleton
"""
                        ]
        , test "should replace Random.list 0 generator by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 0 generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with length 0 will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace Random.list 0 <| generator by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 0 <| generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with length 0 will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace generator |> Random.list 0 by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = generator |> Random.list 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with length 0 will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace Random.list 0 by always (Random.constant [])" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with length 0 will always result in Random.constant []"
                            , details = [ "You can replace this call by always (Random.constant [])." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant [])
"""
                        ]
        , test "should replace Random.list -1 generator by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list -1 generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with negative length will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace Random.list -1 <| generator by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list -1 <| generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with negative length will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace generator |> Random.list -1 by Random.constant []" <|
            \() ->
                """module A exposing (..)
import Random
a = generator |> Random.list -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with negative length will always result in Random.constant []"
                            , details = [ "You can replace this call by Random.constant []." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant []
"""
                        ]
        , test "should replace Random.list -1 by always (Random.constant [])" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with negative length will always result in Random.constant []"
                            , details = [ "You can replace this call by always (Random.constant [])." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant [])
"""
                        ]
        , test "should replace Random.list n (Random.constant el) by Random.constant (List.repeat n el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n (Random.constant el)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n el)
"""
                        ]
        , test "should replace Random.list n (Random.constant <| el) by Random.constant (List.repeat n el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n (Random.constant <| el)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n el)
"""
                        ]
        , test "should replace Random.list n (el |> Random.constant) by Random.constant (List.repeat n el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n (el |> Random.constant)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n el)
"""
                        ]
        , test "should replace Random.list n <| Random.constant el by Random.constant (List.repeat n <| el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n <| Random.constant el
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n <| el)
"""
                        ]
        , test "should replace Random.list n <| Random.constant <| el by Random.constant (List.repeat n <| el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n <| Random.constant <| el
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n <| el)
"""
                        ]
        , test "should replace Random.list n <| (el |> Random.constant) by Random.constant (List.repeat n <| el)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.list n <| (el |> Random.constant)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (List.repeat n <| el)
"""
                        ]
        , test "should replace Random.constant el |> Random.list n by Random.constant (el |> List.repeat n)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.constant el |> Random.list n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (el |> List.repeat n)
"""
                        ]
        , test "should replace el |> Random.constant |> Random.list n by Random.constant (el |> List.repeat n)" <|
            \() ->
                """module A exposing (..)
import Random
a = el |> Random.constant |> Random.list n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (el |> List.repeat n)
"""
                        ]
        , test "should replace (Random.constant <| el) |> Random.list n by Random.constant (el |> List.repeat n)" <|
            \() ->
                """module A exposing (..)
import Random
a = (Random.constant <| el) |> Random.list n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.list with a constant generator will result in Random.constant with List.repeat with the value in that constant generator"
                            , details = [ "You can replace the call by Random.constant with List.repeat with the same length and the value inside the given constant generator." ]
                            , under = "Random.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (el |> List.repeat n)
"""
                        ]
        ]


randomMapTests : Test
randomMapTests =
    describe "Random.map"
        [ test "should not report Random.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map
b = Random.map f
c = Random.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Random.map identity x by x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by the random generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace Random.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by the random generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace x |> Random.map identity by x" <|
            \() ->
                """module A exposing (..)
import Random
a = x |> Random.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by the random generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace Random.map identity by identity" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = identity
"""
                        ]
        , test "should replace Random.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = identity
"""
                        ]
        , test "should replace identity |> Random.map by identity" <|
            \() ->
                """module A exposing (..)
import Random
a = identity |> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with an identity function will always return the same given random generator"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = identity
"""
                        ]
        , test "should replace Random.map (\\_ -> x) generator by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (\\_ -> x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace Random.map (always x) generator by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace Random.map (always <| x) generator by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always <| x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace Random.map (x |> always) generator by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (x |> always) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace Random.map (always x) <| generator by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always x) <| generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace generator |> Random.map (always x) by Random.constant x" <|
            \() ->
                """module A exposing (..)
import Random
a = generator |> Random.map (always x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant x
"""
                        ]
        , test "should replace Random.map (\\_ -> f x) generator by Random.constant (f x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (\\_ -> f x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (f x)
"""
                        ]
        , test "should replace Random.map (always <| f x) generator by Random.constant (f x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always <| f x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (f x)
"""
                        ]
        , test "should replace Random.map (f x |> always) generator by Random.constant (f x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (f x |> always) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (f x)
"""
                        ]
        , test "should replace Random.map (\\_ -> x) by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (\\_ -> x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace Random.map <| \\_ -> x by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map <| \\_ -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace (\\_ -> x) |> Random.map by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = (\\_ -> x) |> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace Random.map (always x) by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace Random.map (always <| x) by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always <| x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace Random.map (x |> always) by always (Random.constant x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (x |> always)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant x)
"""
                        ]
        , test "should replace Random.map (\\_ -> f x) generator by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (\\_ -> f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map <| \\_ -> f x by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map <| \\_ -> f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace (\\_ -> f x) |> Random.map by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = (\\_ -> f x) |> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map (always <| f x) by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (always <| f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map <| always <| f x by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map <| always <| f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace (always <| f x) |> Random.map by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = (always <| f x) |> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map (f x |> always) by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map (f x |> always)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map <| (f x |> always) by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map <| (f x |> always)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace f x |> always |> Random.map by always (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = f x |> always |> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value will always result in Random.constant with that value"
                            , details = [ "You can replace this call by Random.constant with the value produced by the mapper function." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (Random.constant (f x))
"""
                        ]
        , test "should replace always >> Random.map by Random.constant >> always" <|
            \() ->
                """module A exposing (..)
import Random
a = always >> Random.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value is equivalent to Random.constant, then `always`"
                            , details = [ "You can replace this call by Random.constant, then `always`." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant >> always
"""
                        ]
        , test "should replace Random.map << always by always << Random.constant" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map << always
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map with a function that always maps to the same value is equivalent to Random.constant, then `always`"
                            , details = [ "You can replace this call by Random.constant, then `always`." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always << Random.constant
"""
                        ]
        , test "should replace Random.map f (Random.constant x) by (Random.constant (f x))" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map f (Random.constant x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = (Random.constant (f x))
"""
                        ]
        , test "should replace Random.map f <| Random.constant x by Random.constant (f <| x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map f <| Random.constant x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (f <| x)
"""
                        ]
        , test "should replace Random.constant x |> Random.map f by Random.constant (x |> f)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.constant x |> Random.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant (x |> f)
"""
                        ]
        , test "should replace x |> Random.constant |> Random.map f by (x |> f) |> Random.constant" <|
            \() ->
                """module A exposing (..)
import Random
a = x |> Random.constant |> Random.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = (x |> f) |> Random.constant
"""
                        ]
        , test "should replace Random.map f <| Random.constant <| x by Random.constant <| (f <| x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map f <| Random.constant <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant <| (f <| x)
"""
                        ]
        , test "should replace Random.map f << Random.constant by Random.constant << f" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map f << Random.constant
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant << f
"""
                        ]
        , test "should replace Random.constant >> Random.map f by f >> Random.constant" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.constant >> Random.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = f >> Random.constant
"""
                        ]
        , test "should replace Random.map f << Random.constant << a by Random.constant << f << a" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.map f << Random.constant << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.constant << f << a
"""
                        ]
        , test "should replace g << Random.map f << Random.constant by g << Random.constant << f" <|
            \() ->
                """module A exposing (..)
import Random
a = g << Random.map f << Random.constant
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = g << Random.constant << f
"""
                        ]
        , test "should replace Random.constant >> Random.map f >> g by f >> Random.constant >> g" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.constant >> Random.map f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.map on a constant generator will result in Random.constant with the function applied to the value inside"
                            , details = [ "You can replace this call by Random.constant with the function directly applied to the value inside the constant generator itself." ]
                            , under = "Random.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = f >> Random.constant >> g
"""
                        ]
        ]


randomAndThenTests : Test
randomAndThenTests =
    describe "Random.andThen"
        [ test "should not report Random.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen
b = Random.andThen f
c = Random.andThen f generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Random.andThen Random.constant x by x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen Random.constant x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function equivalent to Random.constant will always return the same given random generator"
                            , details = [ "You can replace this call by the random generator itself." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace Random.andThen (\\b -> Random.constant c) x by Random.map (\\b -> c) x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (\\b -> Random.constant c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns a constant generator is the same as Random.map with the function returning the value inside"
                            , details = [ "You can replace this call by Random.map with the function returning the value inside the constant generator." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map (\\b -> c) x
"""
                        ]
        , test "should replace Random.andThen (\\b -> if cond then Random.constant b else Random.constant c) x by Random.map (\\b -> if cond then b else c) x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (\\b -> if cond then Random.constant b else Random.constant c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns a constant generator is the same as Random.map with the function returning the value inside"
                            , details = [ "You can replace this call by Random.map with the function returning the value inside the constant generator." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = Random.map (\\b -> if cond then b else c) x
"""
                        ]
        , test "should replace Random.andThen f (Random.constant x) by f x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen f (Random.constant x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen on a constant generator is the same as applying the function to the value from the constant generator"
                            , details = [ "You can replace this call by the function directly applied to the value inside the constant generator." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = f x
"""
                        ]
        , test "should replace Random.constant x |> Random.andThen f by f (x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.constant x |> Random.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen on a constant generator is the same as applying the function to the value from the constant generator"
                            , details = [ "You can replace this call by the function directly applied to the value inside the constant generator." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x |> f
"""
                        ]
        , test "should replace Random.andThen f << Random.constant by f" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen f << Random.constant
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen on a constant generator is the same as applying the function to the value from the constant generator"
                            , details = [ "You can replace this composition by the function given to Random.andThen." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = f
"""
                        ]
        , test "should replace Random.andThen (\\_ -> x) generator by x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (\\_ -> x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace Random.andThen (always x) generator by x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (always x) generator
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = x
"""
                        ]
        , test "should replace Random.andThen (always (f x) by always (f x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (always (f x))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by always with the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always (f x)
"""
                        ]
        , test "should replace Random.andThen (f x |> always) by (f x |> always)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (f x |> always)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by always with the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = (f x |> always)
"""
                        ]
        , test "should replace (f x |> always) |> Random.andThen by f x |> always" <|
            \() ->
                """module A exposing (..)
import Random
a = (f x |> always) |> Random.andThen
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by always with the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = f x |> always
"""
                        ]
        , test "should replace Random.andThen (always <| f x) by (always <| f x)" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen (always <| f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by always with the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = (always <| f x)
"""
                        ]
        , test "should replace Random.andThen <| (always <| f x) by always <| f x" <|
            \() ->
                """module A exposing (..)
import Random
a = Random.andThen <| (always <| f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Random.andThen with a function that always returns to the same random generator will result in that random generator"
                            , details = [ "You can replace this call by always with the random generator produced by the function." ]
                            , under = "Random.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Random
a = always <| f x
"""
                        ]
        ]
