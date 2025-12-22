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
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first by Tuple.mapFirst changeFirst tuple |> Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.first is the same as Tuple.mapFirst"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapFirst with the same first mapping and tuple." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.mapFirst changeFirst tuple |> Tuple.first
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapBoth changeFirst changeSecond by Tuple.first << Tuple.mapFirst changeFirst" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.first is the same as Tuple.mapFirst"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapFirst with the same first mapping." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.first << Tuple.mapFirst changeFirst
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
                            , details = [ "You can replace this composition by List.filter with the same arguments given to List.partition which is meant for this exact purpose." ]
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
                            , details = [ "You can replace this composition by Set.filter with the same arguments given to Set.partition which is meant for this exact purpose." ]
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
                            , details = [ "You can replace this composition by Dict.filter with the same arguments given to Dict.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.filter f
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
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second by Tuple.mapFirst changeFirst tuple |> Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.second is the same as Tuple.mapSecond"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapSecond with the same second mapping and tuple." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.mapSecond changeSecond tuple |> Tuple.second
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapBoth changeFirst changeSecond by Tuple.second << Tuple.mapSecond changeSecond" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.second is the same as Tuple.mapSecond"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapSecond with the same second mapping." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.second << Tuple.mapSecond changeSecond
"""
                        ]
        ]
