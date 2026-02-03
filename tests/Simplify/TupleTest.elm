module Simplify.TupleTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Tuple"
        [ tuplePairTests
        , tupleFirstTests
        , tupleSecondTests
        ]


tuplePairTests : Test
tuplePairTests =
    describe "Tuple.pair"
        [ test "should not report Tuple.pair used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.pair
b = Tuple.pair first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.pair first second by ( first, second )" <|
            \() ->
                """module A exposing (..)
a = Tuple.pair first second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Fully constructed Tuple.pair can be replaced by tuple literal"
                            , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                            , under = "Tuple.pair"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( first, second )
"""
                        ]
        , test "should replace multiline Tuple.pair (let x = y in first) <| let x = y in second by ( (let x = y in first), let x = y in second )" <|
            \() ->
                """module A exposing (..)
a =
    Tuple.pair
        (let
            x =
                y
         in
         first
        )
    <|
        let
            x =
                y
        in
        second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Fully constructed Tuple.pair can be replaced by tuple literal"
                            , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                            , under = "Tuple.pair"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (
        (let
            x =
                y
         in
         first
        )
    ,
        let
            x =
                y
        in
        second
    )
"""
                        ]
        ]


tupleFirstTests : Test
tupleFirstTests =
    describe "Tuple.first"
        [ test "should not report Tuple.first used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.first
b = Tuple.first tuple
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.first ( first |> f, second ) by (first |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first ( first |> f, second )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on a known tuple will result in the tuple's first part"
                            , details = [ "You can replace this call by the tuple's first part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (first |> f)
"""
                        ]
        , test "should replace Tuple.first (second |> Tuple.pair first) by first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (second |> Tuple.pair first)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on a known tuple will result in the tuple's first part"
                            , details = [ "You can replace this call by the tuple's first part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = first
"""
                        ]
        , test "should replace Tuple.first << (first |> f |> Tuple.pair) by always (first |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << (first |> f |> Tuple.pair)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.pair with a first part, then Tuple.first will always result in that first part"
                            , details = [ "You can replace this call by always with the first argument given to Tuple.pair." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always (first |> f)
"""
                        ]
        , test "should replace Tuple.mapSecond changeSecond tuple |> Tuple.first by tuple |> Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapSecond changeSecond tuple |> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapSecond before Tuple.first"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapSecond call by the unchanged tuple." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = tuple |> Tuple.first
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapSecond changeSecond by Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapSecond changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapSecond before Tuple.first"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can remove the Tuple.mapSecond call." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.first
"""
                        ]
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first by changeFirst (Tuple.first tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 1st function argument of the the Tuple.mapBoth call and call it with the accessed tuple part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = changeFirst (Tuple.first tuple)
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapBoth changeFirst changeSecond by changeFirst << Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 1st function argument of the the Tuple.mapBoth call and compose it after the tuple part access." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (changeFirst << Tuple.first)
"""
                        ]
        , test "should replace Tuple.first (List.partition f list) by (List.filter f list)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (List.partition f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition, then Tuple.first can be combined into List.filter"
                            , details = [ "You can replace this call by List.filter with the same arguments given to List.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filter f list)
"""
                        ]
        , test "should replace Tuple.first (Set.partition f set) by (Set.filter f set)" <|
            \() ->
                """module A exposing (..)
import Set
a = Tuple.first (Set.partition f set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition, then Tuple.first can be combined into Set.filter"
                            , details = [ "You can replace this call by Set.filter with the same arguments given to Set.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.filter f set)
"""
                        ]
        , test "should replace Tuple.first (Dict.partition f dict) by (Dict.filter f dict)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Tuple.first (Dict.partition f dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition, then Tuple.first can be combined into Dict.filter"
                            , details = [ "You can replace this call by Dict.filter with the same arguments given to Dict.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.filter f dict)
"""
                        ]
        , test "should replace Tuple.first << List.partition f by List.filter f" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << List.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition, then Tuple.first can be combined into List.filter"
                            , details = [ "You can replace this composition by List.filter with the same argument given to List.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filter f
"""
                        ]
        , test "should replace Tuple.first << Set.partition f by Set.filter f" <|
            \() ->
                """module A exposing (..)
import Set
a = Tuple.first << Set.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition, then Tuple.first can be combined into Set.filter"
                            , details = [ "You can replace this composition by Set.filter with the same argument given to Set.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.filter f
"""
                        ]
        , test "should replace Tuple.first << Dict.partition f by Dict.filter f" <|
            \() ->
                """module A exposing (..)
import Dict
a = Tuple.first << Dict.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition, then Tuple.first can be combined into Dict.filter"
                            , details = [ "You can replace this composition by Dict.filter with the same argument given to Dict.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.filter f
"""
                        ]
        , test "should replace Tuple.first (Tuple.mapFirst f tuple) by f (Tuple.first tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (Tuple.mapFirst f tuple)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapFirst can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapFirst call and call it with the accessed tuple part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (Tuple.first tuple)
"""
                        ]
        , test "should replace Tuple.first (Tuple.mapFirst f <| g tuple) by f <| Tuple.first (g tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (Tuple.mapFirst f <| g tuple)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapFirst can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapFirst call and call it with the accessed tuple part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f <| (Tuple.first (g tuple)))
"""
                        ]
        , test "should replace Tuple.first (g tuple |> Tuple.mapFirst f) by Tuple.first (g tuple) |> f" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (g tuple |> Tuple.mapFirst f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapFirst can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapFirst call and call it with the accessed tuple part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((Tuple.first (g tuple)) |> f)
"""
                        ]
        , test "should replace Tuple.mapFirst f >> Tuple.first by Tuple.first >> f" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapFirst f >> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapFirst can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapFirst call and compose it after the tuple part access." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.first >> f)
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapFirst f by f << Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapFirst f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on Tuple.mapFirst can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapFirst call and compose it after the tuple part access." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f << Tuple.first)
"""
                        ]
        ]


tupleSecondTests : Test
tupleSecondTests =
    describe "Tuple.second"
        [ test "should not report Tuple.second used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.second
b = Tuple.second tuple
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.second ( first, second |> f ) by (second |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.second ( first, second |> f )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on a known tuple will result in the tuple's second part"
                            , details = [ "You can replace this call by the tuple's second part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (second |> f)
"""
                        ]
        , test "should replace Tuple.second (second |> Tuple.pair first) by second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second (second |> Tuple.pair first)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on a known tuple will result in the tuple's second part"
                            , details = [ "You can replace this call by the tuple's second part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = second
"""
                        ]
        , test "should replace Tuple.second << Tuple.pair first by identity" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << (second |> f |> Tuple.pair)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.pair with a first part, then Tuple.second will always result in the incoming second part"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Tuple.mapFirst changeSecond tuple |> Tuple.second by tuple |> Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapFirst changeSecond tuple |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapFirst before Tuple.second"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapFirst call by the unchanged tuple." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = tuple |> Tuple.second
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapFirst changeSecond by Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapFirst before Tuple.second"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can remove the Tuple.mapFirst call." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.second
"""
                        ]
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second by changeSecond (Tuple.second tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 2nd function argument of the the Tuple.mapBoth call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = changeSecond (Tuple.second tuple)
"""
                        ]
        , test "should replace tuple |> Tuple.mapBoth changeFirst changeSecond |> Tuple.second by Tuple.second tuple |> changeSecond" <|
            \() ->
                """module A exposing (..)
a = tuple |> Tuple.mapBoth changeFirst changeSecond |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 2nd function argument of the the Tuple.mapBoth call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.second tuple) |> changeSecond
"""
                        ]
        , test "should replace (Tuple.mapBoth changeFirst changeSecond <| tuple) |> Tuple.second by changeSecond <| Tuple.second tuple" <|
            \() ->
                """module A exposing (..)
a = (Tuple.mapBoth changeFirst changeSecond <| tuple) |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 2nd function argument of the the Tuple.mapBoth call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (changeSecond <| (Tuple.second tuple))
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapBoth changeFirst changeSecond by changeSecond << Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapBoth can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the 2nd function argument of the the Tuple.mapBoth call and compose it after the tuple part access." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (changeSecond << Tuple.second)
"""
                        ]
        , test "should replace Tuple.second (Tuple.mapSecond f tuple) by f (Tuple.second tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.second (Tuple.mapSecond f tuple)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapSecond can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapSecond call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (Tuple.second tuple)
"""
                        ]
        , test "should replace Tuple.second (Tuple.mapSecond f <| g tuple) by f <| Tuple.second (g tuple)" <|
            \() ->
                """module A exposing (..)
a = Tuple.second (Tuple.mapSecond f <| g tuple)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapSecond can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapSecond call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f <| (Tuple.second (g tuple)))
"""
                        ]
        , test "should replace Tuple.second (g tuple |> Tuple.mapSecond f) by Tuple.second (g tuple) |> f" <|
            \() ->
                """module A exposing (..)
a = Tuple.second (g tuple |> Tuple.mapSecond f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapSecond can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapSecond call and call it with the accessed tuple part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((Tuple.second (g tuple)) |> f)
"""
                        ]
        , test "should replace Tuple.mapSecond f >> Tuple.second by Tuple.second >> f" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapSecond f >> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapSecond can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapSecond call and compose it after the tuple part access." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.second >> f)
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapSecond f by f << Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapSecond f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on Tuple.mapSecond can be replaced by directly calling the given function on the accessed tuple part"
                            , details = [ "You can take the function argument of the the Tuple.mapSecond call and compose it after the tuple part access." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f << Tuple.second)
"""
                        ]
        ]
