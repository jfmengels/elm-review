module Simplify.MaybeTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Maybe"
        [ maybeMapTests
        , maybeMapNTests
        , maybeAndThenTests
        , maybeWithDefaultTests
        ]


maybeMapTests : Test
maybeMapTests =
    describe "Maybe.map"
        [ test "should not report Maybe.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.map f Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map f <| Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Nothing |> Maybe.map f by Nothing" <|
            \() ->
                """module A exposing (..)
a = Nothing |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Maybe.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Maybe.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by the maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Maybe.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Maybe.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Maybe.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Maybe.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Maybe.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map with an identity function will always return the same given maybe"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Maybe.map f (Just x) by (Just (f x))" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f (Just x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Just (f x))
"""
                        ]
        , test "should replace Maybe.map f <| Just x by Just (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f <| x)
"""
                        ]
        , test "should replace Just x |> Maybe.map f by Just (x |> f)" <|
            \() ->
                """module A exposing (..)
a = Just x |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (x |> f)
"""
                        ]
        , test "should replace x |> Just |> Maybe.map f by (x |> f) |> Just" <|
            \() ->
                """module A exposing (..)
a = x |> Just |> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) |> Just
"""
                        ]
        , test "should replace Maybe.map f <| Just <| x by Just <| (f <| x)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f <| Just <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| (f <| x)
"""
                        ]
        , test "should replace Maybe.map f << Just by Just << f" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just << f
"""
                        ]
        , test "should replace Just >> Maybe.map f by f >> Just" <|
            \() ->
                """module A exposing (..)
a = Just >> Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Just
"""
                        ]
        , test "should replace Maybe.map f << Just << a by Just << f << a" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f << Just << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just << f << a
"""
                        ]
        , test "should replace g << Maybe.map f << Just by g << Just << f" <|
            \() ->
                """module A exposing (..)
a = g << Maybe.map f << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = g << Just << f
"""
                        ]
        , test "should replace Just >> Maybe.map f >> g by f >> Just >> g" <|
            \() ->
                """module A exposing (..)
a = Just >> Maybe.map f >> g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map on a just maybe will result in Just with the function applied to the value inside"
                            , details = [ "You can replace this call by Just with the function directly applied to the value inside the just maybe itself." ]
                            , under = "Maybe.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f >> Just >> g
"""
                        ]
        ]


maybeMapNTests : Test
maybeMapNTests =
    -- testing behavior only with representatives for 2-5
    Test.describe "Maybe.mapN"
        [ test "should not report Task.map3 with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Maybe.map2
a1 = Maybe.map2 f
a2 = Maybe.map2 f maybe0
a3 = Maybe.map2 f maybe0 maybe1
a4 = Maybe.map2 f (Just a) maybe1 -- because this is a code style choice
a5 = Maybe.map2 f (Just a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.map3 f (Just a) (Just b) (Just c) by Just (f a b c)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f (Just a) (Just b) (Just c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 where each maybe is a just maybe will result in Just on the values inside"
                            , details = [ "You can replace this call by Just with the function applied to the values inside each just maybe." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f a b c)
"""
                        ]
        , test "should replace c |> g |> Just |> Maybe.map3 f (Just a) (Just b) by (c |> g) |> f a b |> Just" <|
            \() ->
                """module A exposing (..)
a = c |> g |> Just |> Maybe.map3 f (Just a) (Just b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 where each maybe is a just maybe will result in Just on the values inside"
                            , details = [ "You can replace this call by Just with the function applied to the values inside each just maybe." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (c |> g) |> f a b |> Just
"""
                        ]
        , test "should replace Maybe.map3 f (Just a) (Just b) <| Just <| g <| c by Just <| f a b <| (g <| c)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f (Just a) (Just b) <| Just <| g <| c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 where each maybe is a just maybe will result in Just on the values inside"
                            , details = [ "You can replace this call by Just with the function applied to the values inside each just maybe." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| f a b <| (g <| c)
"""
                        ]
        , test "should replace Maybe.map3 f Nothing maybe1 maybe2 by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f Nothing maybe1 maybe2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 with any maybe being Nothing will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map3 f Nothing maybe1 by always Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f Nothing maybe1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 with any maybe being Nothing will always result in Nothing"
                            , details = [ "You can replace this call by always Nothing." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always Nothing
"""
                        ]
        , test "should replace Maybe.map3 f Nothing maybe1 by (\\_ _ -> Nothing)" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 with any maybe being Nothing will always result in Nothing"
                            , details = [ "You can replace this call by (\\_ _ -> Nothing)." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> Nothing)
"""
                        ]
        , test "should replace Maybe.map3 f maybe0 Nothing maybe2 by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f maybe0 Nothing maybe2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 with any maybe being Nothing will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.map3 f maybe0 maybe1 Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.map3 f maybe0 maybe1 Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map3 with any maybe being Nothing will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        ]


maybeAndThenTests : Test
maybeAndThenTests =
    describe "Maybe.andThen"
        [ test "should not report Maybe.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Maybe.andThen f x
a1 = Maybe.andThen << Just -- f arg missing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen f Nothing by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen on Nothing will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.andThen Just x by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function equivalent to Just will always return the same given maybe"
                            , details = [ "You can replace this call by the maybe itself." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.andThen (always Nothing) x by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (always Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that will always return Nothing will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace Maybe.andThen (\\a -> Nothing) x by Nothing" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\a -> Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that will always return Nothing will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should not replace Maybe.andThen (\\a b -> Nothing) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\a b -> Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen (\\b -> Just c) x by Maybe.map (\\b -> c) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> Just c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (\\b -> c) x
"""
                        ]
        , test "should replace Maybe.andThen (\\b -> if cond then Just b else Just c) x by Maybe.map (\\b -> if cond then b else c) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> if cond then Just b else Just c) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (\\b -> if cond then b else c) x
"""
                        ]
        , test "should not report Maybe.andThen (\\b -> if cond then Just b else Nothing) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (\\b -> if cond then Just b else Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen (\\b -> case b of C -> Just b ; D -> Just c) x by Maybe.map (\\b -> case b of C -> b ; D -> c) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        case b of
            C -> Just b
            D -> Just c
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (
    \\b ->
        case b of
            C -> b
            D -> c
    ) x
"""
                        ]
        , test "should replace Maybe.andThen (\\b -> let y = 1 in Just y) x by Maybe.map (\\b -> let y = 1 in y) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        let
            y = 1
        in
        Just y
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (
    \\b ->
        let
            y = 1
        in
        y
    ) x
"""
                        ]
        , test "should not report Maybe.andThen (\\b -> case b of C -> Just b ; D -> Nothing) x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (
    \\b ->
        case b of
            C -> Just b
            D -> Nothing
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report Maybe.andThen (f << Just) maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (f << Just) maybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.andThen (Just << f) maybe by Maybe.map (f) maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (Just << f) maybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (f) maybe
"""
                        ]
        , test "should replace Maybe.andThen (Just << f) by Maybe.map (f)" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen (Just << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen with a function that always returns a just maybe is the same as Maybe.map with the function returning the value inside"
                            , details = [ "You can replace this call by Maybe.map with the function returning the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.map (f)
"""
                        ]
        , test "should replace Maybe.andThen f (Just x) by f x" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f (Just x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen on a just maybe is the same as applying the function to the value from the just maybe"
                            , details = [ "You can replace this call by the function directly applied to the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f x
"""
                        ]
        , test "should replace Just x |> Maybe.andThen f by f (x)" <|
            \() ->
                """module A exposing (..)
a = Just x |> Maybe.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen on a just maybe is the same as applying the function to the value from the just maybe"
                            , details = [ "You can replace this call by the function directly applied to the value inside the just maybe." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace Maybe.andThen f << Just by f" <|
            \() ->
                """module A exposing (..)
a = Maybe.andThen f << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.andThen on a just maybe is the same as applying the function to the value from the just maybe"
                            , details = [ "You can replace this composition by the function given to Maybe.andThen." ]
                            , under = "Maybe.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        ]


maybeWithDefaultTests : Test
maybeWithDefaultTests =
    describe "Maybe.withDefault"
        [ test "should not report Maybe.withDefault used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x y
b = Maybe.withDefault << Just
c = Maybe.withDefault Nothing << Maybe.andThen f
d = Maybe.withDefault nothing << Maybe.map f
e = Maybe.withDefault NotFromMaybe.Nothing << Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report Maybe.withDefault with local Nothing on Maybe.map" <|
            \() ->
                """module A exposing (..)
type LocalNothing = Nothing
a = Maybe.withDefault Nothing << Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Maybe.withDefault x Nothing by x" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on Nothing will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Maybe.withDefault x (Just y) by y" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x (Just y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on a just maybe will result in the value inside"
                            , details = [ "You can replace this call by the value inside the just maybe." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace Maybe.withDefault x <| (Just y) by y" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault x <| (Just y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on a just maybe will result in the value inside"
                            , details = [ "You can replace this call by the value inside the just maybe." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace (Just y) |> Maybe.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = (Just y) |> Maybe.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on a just maybe will result in the value inside"
                            , details = [ "You can replace this call by the value inside the just maybe." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace y |> Just |> Maybe.withDefault x by y" <|
            \() ->
                """module A exposing (..)
a = y |> Just |> Maybe.withDefault x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on a just maybe will result in the value inside"
                            , details = [ "You can replace this call by the value inside the just maybe." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y
"""
                        ]
        , test "should replace Maybe.withDefault a << Just by identity" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault b << Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.withDefault on a just maybe will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Maybe.withDefault Nothing <| Maybe.map f maybe by Maybe.andThen f maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault Nothing <| Maybe.map f maybe
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this call by Maybe.andThen with the same arguments given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.andThen f maybe
"""
                        ]
        , test "should replace Maybe.map f maybe |> Maybe.withDefault Nothing by Maybe.andThen f maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f maybe |> Maybe.withDefault Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this call by Maybe.andThen with the same arguments given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.andThen f maybe
"""
                        ]
        , test "should replace Maybe.withDefault Nothing (Maybe.map f maybe) maybe by Maybe.andThen f maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault Nothing (Maybe.map f maybe)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this call by Maybe.andThen with the same arguments given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Maybe.andThen f maybe)
"""
                        ]
        , test "should replace Maybe.withDefault Maybe.Nothing (Maybe.map f maybe) maybe by Maybe.andThen f maybe" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault Maybe.Nothing (Maybe.map f maybe)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this call by Maybe.andThen with the same arguments given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Maybe.andThen f maybe)
"""
                        ]
        , test "should replace Maybe.withDefault <import alias for Maybe>.Nothing (Maybe.map f maybe) maybe by <import alias for Maybe>.andThen f maybe" <|
            \() ->
                """module A exposing (..)
import Maybe as M
a = M.withDefault M.Nothing (M.map f maybe)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this call by Maybe.andThen with the same arguments given to Maybe.map which is meant for this exact purpose." ]
                            , under = "M.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Maybe as M
a = (M.andThen f maybe)
"""
                        ]
        , test "should replace Maybe.withDefault Nothing << Maybe.map f by Maybe.andThen f" <|
            \() ->
                """module A exposing (..)
a = Maybe.withDefault Nothing << Maybe.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this composition by Maybe.andThen with the same argument given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.andThen f
"""
                        ]
        , test "should replace Maybe.map f >> Maybe.withDefault Nothing f by Maybe.andThen f" <|
            \() ->
                """module A exposing (..)
a = Maybe.map f >> Maybe.withDefault Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Maybe.map, then Maybe.withDefault with Nothing can be combined into Maybe.andThen"
                            , details = [ "You can replace this composition by Maybe.andThen with the same argument given to Maybe.map which is meant for this exact purpose." ]
                            , under = "Maybe.withDefault"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.andThen f
"""
                        ]
        ]
