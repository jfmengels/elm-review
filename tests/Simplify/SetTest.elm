module Simplify.SetTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults, whenNotExpectingNaN)


all : Test
all =
    describe "Set"
        [ setMapTests
        , setFilterTests
        , setIsEmptyTests
        , setSizeTests
        , setFromListTests
        , setToListTests
        , setPartitionTests
        , setRemoveTests
        , setMemberTests
        , setIntersectTests
        , setDiffTests
        , setUnionTests
        , setInsertTests
        , setFoldlTests
        , setFoldrTests
        ]


setMapTests : Test
setMapTests =
    describe "Set.map"
        [ test "should not report Set.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map f set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.map f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.map f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.map f by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.map identity set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map identity set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.map identity <| set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map identity <| set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace set |> Set.map identity by set" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.map identity by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace Set.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace identity |> Set.map by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = identity |> Set.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.map with an identity function will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        ]


setFilterTests : Test
setFilterTests =
    describe "Set.filter"
        [ test "should not report Set.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f set
b = Set.filter f (Set.filter g set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.filter f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.filter f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.filter f by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.filter f (Set.filter f set) by Set.filter f set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f (Set.filter f set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.filter after equivalent Set.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.filter f set
"""
                        ]
        , test "should replace Set.filter f (Set.filter f <| set) by Set.filter f <| set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f (Set.filter f <| set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.filter after equivalent Set.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.filter f <| set)
"""
                        ]
        , test "should replace Set.filter f (set |> Set.filter f) by set |> Set.filter f" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f (set |> Set.filter f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.filter after equivalent Set.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (set |> Set.filter f)
"""
                        ]
        , test "should replace Set.filter f >> Set.filter f by Set.filter f" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f >> Set.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.filter after equivalent Set.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 21 }, end = { row = 3, column = 31 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.filter f
"""
                        ]
        , test "should replace Set.filter f << Set.filter f by Set.filter f" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter f << Set.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.filter after equivalent Set.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.filter f
"""
                        ]
        , test "should replace Set.filter (always True) set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (always True) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return True will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.filter (\\_ -> True) set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (\\_ -> True) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return True will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return True will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace Set.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return True will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace always True |> Set.filter by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = always True |> Set.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return True will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace Set.filter (always False) set by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (always False) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (\\_ -> False) set by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (\\_ -> False) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) <| set by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (always False) <| set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace set |> Set.filter (always False) by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by always Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always Set.empty
"""
                        ]
        , test "should replace Set.filter <| (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by always Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always Set.empty
"""
                        ]
        , test "should replace always False |> Set.filter by always Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = always False |> Set.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.filter with a function that will always return False will always result in Set.empty"
                            , details = [ "You can replace this call by always Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always Set.empty
"""
                        ]
        , test "should replace Set.filter ((/=) <| f <| x) set by always Set.remove (f <| x) set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter ((/=) <| f <| x) set
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.filter checking for inequality with a specific value is the same as Set.remove"
                            , details = [ "You can replace this call by Set.remove with the specific value you compared against which is meant for this exact purpose and will also be faster." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove (f <| x) set
"""
                        ]
        , test "should replace Set.filter (\\k -> k /= (f <| x)) set by always Set.remove (f <| x) set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (\\k -> k /= (f <| x)) set
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.filter checking for inequality with a specific value is the same as Set.remove"
                            , details = [ "You can replace this call by Set.remove with the specific value you compared against which is meant for this exact purpose and will also be faster." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove (f <| x) set
"""
                        ]
        , test "should replace Set.filter (\\k -> (f <| x) /= k) by always Set.remove (f <| x)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.filter (\\k -> (f <| x) /= k)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.filter checking for inequality with a specific value is the same as Set.remove"
                            , details = [ "You can replace this call by Set.remove with the specific value you compared against which is meant for this exact purpose and will also be faster." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove (f <| x)
"""
                        ]
        ]


setSizeTests : Test
setSizeTests =
    describe "Set.size"
        [ test "should not report Set.size used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size
a = Set.size b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.size Set.empty by 0" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 0
"""
                        ]
        , test "should not replace Set.size (Set.fromList [b, c, d])" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [b, c, d])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.size (Set.fromList []) by 0" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on [] will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.empty)
"""
                        , Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 0
"""
                        ]
        , test "should replace Set.size (Set.fromList [a]) by 1" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [a])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 1"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 1
"""
                        , Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.singleton a)
"""
                        ]
        , test "should replace Set.size (Set.fromList [1, 2, 3]) by 3" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [1, 2, 3])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 3
"""
                        ]
        , test "should replace Set.size (Set.fromList [1, 2, 3, 3, 0x3]) by 3" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [1, 2, 3, 3, 0x3])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 3
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "3"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 35 }, end = { row = 3, column = 36 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList [1, 2, 3, 0x3])
"""
                        ]
        , test "should replace Set.size (Set.fromList [2, -2, -2]) by 2" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [2, -2, -2])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 2
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "-2"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 32 }, end = { row = 3, column = 34 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList [2, -2])
"""
                        ]
        , test "should replace Set.size (Set.fromList [1.3, -1.3, 2.1, 2.1]) by 3" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [1.3, -1.3, 2.1, 2.1])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 3"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 3
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "2.1"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 40 }, end = { row = 3, column = 43 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList [1.3, -1.3, 2.1])
"""
                        ]
        , test "should replace Set.size (Set.fromList [\"foo\", \"bar\", \"foo\"]) by 2" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList ["foo", "bar", "foo"])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 2
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "\"foo\""
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 29 }, end = { row = 3, column = 34 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList ["bar", "foo"])
"""
                        ]
        , test "should replace Set.size (Set.fromList ['a', 'b', ('a')]) by 2" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList ['a', 'b', ('a')])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 2
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "'a'"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 29 }, end = { row = 3, column = 32 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList ['b', ('a')])
"""
                        ]
        , test "should replace Set.size (Set.fromList [([1, 2], [3, 4]), ([1, 2], [3, 4]), ([], [1])]) by 2" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.size (Set.fromList [([1, 2], [3, 4]), ([1, 2], [3, 4]), ([], [1])])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 2"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 2
"""
                        , Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "([1, 2], [3, 4])"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 29 }, end = { row = 3, column = 45 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size (Set.fromList [([1, 2], [3, 4]), ([], [1])])
"""
                        ]
        , test "should replace Set.empty |> Set.size by 0" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 0"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 0
"""
                        ]
        , test "should replace Set.singleton set |> Set.size by 1" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.singleton set |> Set.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the set is 1"
                            , details = [ "The size of the set can be determined by looking at the code." ]
                            , under = "Set.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = 1
"""
                        ]
        ]


setIsEmptyTests : Test
setIsEmptyTests =
    describe "Set.isEmpty"
        [ test "should not report Set.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty
b = Set.isEmpty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.isEmpty Set.empty by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on Set.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Set.isEmpty (Set.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty (Set.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on this set will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        , Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty (Set.singleton x)
"""
                        ]
        , test "should replace Set.isEmpty (Set.fromList []) by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty (Set.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on Set.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        , Review.Test.error
                            { message = "Set.fromList on [] will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty (Set.empty)
"""
                        ]
        , test "should replace Set.isEmpty (Set.singleton x) by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty (Set.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on this set will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace Set.isEmpty (Set.fromList ([a,b] ++ rest)) by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty (Set.fromList ([b,c] ++ rest))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on this set will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace Set.isEmpty (Set.map f set) by Set.isEmpty set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.isEmpty (Set.map f set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.map before Set.isEmpty"
                            , details = [ "Mapping an empty set will result in an empty set and otherwise creating a new set with any element inserted will not be empty, even if all new elements are the same. You can replace the Set.map call by the unchanged set." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty set
"""
                        ]
        , test "should replace Set.singleton set |> Set.isEmpty by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.singleton set |> Set.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.isEmpty on this set will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace list |> Set.fromList |> Set.isEmpty by list |> List.isEmpty" <|
            \() ->
                """module A exposing (..)
import Set
a = list |> Set.fromList |> Set.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList, then Set.isEmpty can be combined into List.isEmpty"
                            , details = [ "You can replace this call by List.isEmpty with the same argument given to Set.fromList which is meant for this exact purpose." ]
                            , under = "Set.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = list |> List.isEmpty
"""
                        ]
        ]


setFromListTests : Test
setFromListTests =
    describe "Set.fromList"
        [ test "should not report Set.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList
b = Set.fromList list
c = Set.fromList (x :: ys)
d = Set.fromList [x, y]
e = Set.fromList
f = Set.fromList list
g = Set.fromList << fun << Set.toList
h = (Set.fromList << fun) << Set.toList
i = Set.fromList << (fun << Set.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.fromList [] by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on [] will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.fromList [ a ] by Set.singleton a" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton b
"""
                        ]
        , test "should replace Set.fromList [ f a ] by Set.singleton (f a)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton (f b)
"""
                        ]
        , test "should replace Set.fromList (List.singleton a) by Set.singleton a" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton b
"""
                        ]
        , test "should replace Set.fromList <| List.singleton a by Set.singleton <| a" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton <| b
"""
                        ]
        , test "should replace List.singleton a |> Set.fromList by a |> Set.singleton" <|
            \() ->
                """module A exposing (..)
import Set
a = List.singleton b |> Set.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton with the value inside the singleton list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = b |> Set.singleton
"""
                        ]
        , test "should replace List.singleton >> Set.fromList by Set.singleton" <|
            \() ->
                """module A exposing (..)
import Set
a = List.singleton >> Set.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton
"""
                        ]
        , test "should replace Set.fromList << List.singleton by Set.singleton" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a singleton list will result in Set.singleton with the value inside"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton
"""
                        ]
        , test "should replace x |> f |> Set.toList |> Set.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
import Set
a = x |> f |> Set.toList |> Set.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to Set.toList." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = x |> f
"""
                        ]
        , test "should replace Set.fromList << Set.toList by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace Set.fromList << (Set.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << (Set.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f)
"""
                        ]
        , test "should replace Set.fromList << (Set.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << (Set.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (g << f)
"""
                        ]
        , test "should replace Set.fromList << (f >> Set.toList) by (f)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << (f >> Set.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f)
"""
                        ]
        , test "should replace (f << Set.fromList) << Set.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Set
a = (f << Set.fromList) << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f)
"""
                        ]
        , test "should replace (Set.fromList >> f) << Set.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Set
a = (Set.fromList >> f) << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f)
"""
                        ]
        , test "should replace (Set.fromList >> f >> g) << Set.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Set
a = (Set.fromList >> f >> g) << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then Set.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f >> g)
"""
                        ]
        , test "should replace Set.fromList (List.reverse list) by Set.fromList list" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.reverse list)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can replace the List.reverse call by the unchanged list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList list
"""
                        ]
        , test "should replace (List.reverse <| f <| x) |> Set.fromList by (f <| x) |> Set.fromList" <|
            \() ->
                """module A exposing (..)
import Set
a = (List.reverse <| f <| x) |> Set.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can replace the List.reverse call by the unchanged list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (f <| x) |> Set.fromList
"""
                        ]
        , test "should replace Set.fromList << List.reverse by Set.fromList" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << List.reverse
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can remove the List.reverse call." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList
"""
                        ]
        , test "should replace List.reverse >> Set.fromList by Set.fromList" <|
            \() ->
                """module A exposing (..)
import Set
a = List.reverse >> Set.fromList
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can remove the List.reverse call." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList
"""
                        ]
        , test "should replace Set.fromList (List.sort list) by Set.fromList list" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.sort list)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.sort before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can replace the List.sort call by the unchanged list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList list
"""
                        ]
        , test "should replace Set.fromList (List.sortBy f list) by Set.fromList list" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.sortBy f list)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can replace the List.sortBy call by the unchanged list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList list
"""
                        ]
        , test "should replace Set.fromList (List.sortWith f list) by Set.fromList list" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.sortWith f list)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Unnecessary List.sortWith before Set.fromList"
                            , details = [ "Reordering a list does not affect its final representation as a set. You can replace the List.sortWith call by the unchanged list." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList list
"""
                        ]
        , test "should replace Set.fromList (List.repeat n a) by if n >= 1 then Set.singleton a else Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList (List.repeat n b)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.fromList on List.repeat will result in Set.singleton the repeated element if the count is positive and Set.empty otherwise"
                            , details = [ "You can replace this call by if (the count argument given to List.repeat) >= 1 then Set.singleton (the element to repeat argument given to List.repeat) else Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (if n >= 1 then
                                Set.singleton b

    else
                                Set.empty)
"""
                        ]
        , test "should replace Set.fromList << List.repeat n by if n >= 1 then Set.singleton else always Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList << List.repeat n
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.fromList on List.repeat will result in Set.singleton the repeated element if the count is positive and Set.empty otherwise"
                            , details = [ "You can replace this composition by if (the count argument given to List.repeat) >= 1 then Set.singleton else always Set.empty." ]
                            , under = "Set.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (if n >= 1 then
        Set.singleton

    else
        always Set.empty)
"""
                        ]
        ]


setToListTests : Test
setToListTests =
    describe "Set.toList"
        [ test "should not report Set.toList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.toList
b = Set.toList set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.toList Set.empty by []" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.toList Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList on Set.empty will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Set.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = []
"""
                        ]
        , test "should not report Set.fromList with duplicate keys when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ ( 1, "", [ 'a', b ] ), ( 1, "", [ 'a', b ] ) ]
"""
                    |> Review.Test.run TestHelpers.ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace Set.fromList [ key, key ] by Set.fromList [ key ]" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ key, key ]
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "key"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 20 }, end = { row = 3, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [ key ]
"""
                        ]
        , test "should replace Set.fromList [ 0, 0 ] by Set.fromList [ 0 ] even when expecting NaN because keys are literals" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ 0, 0 ]
"""
                    |> Review.Test.run TestHelpers.ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "0"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 20 }, end = { row = 3, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [ 0 ]
"""
                        ]
        , test "should replace Set.fromList [ ( 1, \"\", [ 'a' ] ), ( 1, \"\", [ 'a' ] ) ] by Set.fromList [ ( 1, \"\", [ 'a' ] ) ]" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ ( 1, "", [ 'a' ] ), ( 1, "", [ 'a' ] ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = """( 1, "", [ 'a' ] )"""
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 20 }, end = { row = 3, column = 38 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [ ( 1, "", [ 'a' ] ) ]
"""
                        ]
        , test "should replace Set.fromList [ 'a', thing, 'a' ] by Set.fromList [ thing, 'a' ]" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [ 'a', thing, 'a' ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                            , under = "'a'"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 20 }, end = { row = 3, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [ thing, 'a' ]
"""
                        ]
        ]


setPartitionTests : Test
setPartitionTests =
    describe "Set.partition"
        [ test "should not report Set.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition f set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.partition f Set.empty by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition f Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.partition f <| Set.empty by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition f <| Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.empty |> Set.partition f by ( Set.empty, Set.empty )" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition on Set.empty will result in ( Set.empty, Set.empty )"
                            , details = [ "You can replace this call by ( Set.empty, Set.empty )." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ( Set.empty, Set.empty )
"""
                        ]
        , test "should replace Set.partition (always True) set by ( set, Set.empty )" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition (always True) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first set"
                            , details = [ "Since the predicate function always returns True, the second set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ( set, Set.empty )
"""
                        ]
        , test "should not replace Set.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
import Set
a = Set.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.partition (always False) set by ( Set.empty, set )" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition (always False) set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ( Set.empty, set )
"""
                        ]
        , test "should replace Set.partition (always False) by (Tuple.pair Set.empty)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Tuple.pair Set.empty)
"""
                        ]
        , test "should replace Set.partition <| (always False) by (Tuple.pair Set.empty)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Tuple.pair Set.empty)
"""
                        ]
        , test "should replace always False |> Set.partition by Tuple.pair Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = always False |> Set.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second set"
                            , details = [ "Since the predicate function always returns False, the first set will always be Set.empty." ]
                            , under = "Set.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Tuple.pair Set.empty)
"""
                        ]
        ]


setRemoveTests : Test
setRemoveTests =
    describe "Set.remove"
        [ test "should not report Set.remove used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.remove x set
b = Set.remove k0 (Set.remove k1 set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.remove x Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.remove x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.remove on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.remove"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.remove k (Set.remove k set) by Set.remove k set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.remove k (Set.remove k set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.remove after equivalent Set.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove k set
"""
                        ]
        , test "should replace Set.remove k >> Set.remove k by Set.remove k" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.remove k >> Set.remove k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.remove after equivalent Set.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 21 }, end = { row = 3, column = 31 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove k
"""
                        ]
        , test "should replace Set.remove k << Set.remove k by Set.remove k" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.remove k << Set.remove k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.remove after equivalent Set.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.remove k
"""
                        ]
        ]


setMemberTests : Test
setMemberTests =
    describe "Set.member"
        [ test "should not report Set.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member x set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.member x Set.empty by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on Set.empty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace Set.member b (Set.singleton c) by b == c" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member b (Set.singleton c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on a singleton set is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton set are equal." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = b == c
"""
                        ]
        , test "should replace Set.member 1 (Set.fromList [ 0, 1, 2 ]) by True" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 1 (Set.fromList [ 0, 1, 2 ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on a set which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Set.member 1 (Set.fromList [ 0, 1, 2 ]) by True even when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 1 (Set.fromList [ 0, 1, 2 ])
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on a set which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = True
"""
                        ]
        , test "should replace Set.member 0 (Set.fromList [ 2, 3, 1 ]) by False" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 0 (Set.fromList [ 2, 3, 1 ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on a set which does not contain the given element will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should replace Set.member 0 (Set.fromList [ 2, 3, 1 ]) by False even when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 0 (Set.fromList [ 2, 3, 1 ])
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.member on a set which does not contain the given element will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = False
"""
                        ]
        , test "should not replace Set.member 2 (Set.fromList list) when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 2 (Set.fromList list)
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not replace Set.member 2 << Set.fromList when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member 2 << Set.fromList
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace Set.member x (Set.fromList list) by List.member x list when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member x (Set.fromList list)
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "To check for a list member, you don't need to convert to a set"
                            , details = [ "Using List.member directly is meant for this exact purpose and will also be faster." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = List.member x list
"""
                        ]
        , test "should replace Set.member x << Set.fromList by Set.member x when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.member x << Set.fromList
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "To check for a list member, you don't need to convert to a set"
                            , details = [ "Using List.member directly is meant for this exact purpose and will also be faster." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = List.member x
"""
                        ]
        , test "should replace Set.fromList >> Set.member x by Set.member x when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList >> Set.member x
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "To check for a list member, you don't need to convert to a set"
                            , details = [ "Using List.member directly is meant for this exact purpose and will also be faster." ]
                            , under = "Set.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = List.member x
"""
                        ]
        ]


setIntersectTests : Test
setIntersectTests =
    describe "Set.intersect"
        [ test "should not report Set.intersect used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a0 = Set.intersect
a1 = Set.intersect set0
a2 = Set.intersect set0 set1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.intersect Set.empty set by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.intersect Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.intersect on Set.empty will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.intersect Set.empty by always Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.intersect Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.intersect on Set.empty will always result in Set.empty"
                            , details = [ "You can replace this call by always Set.empty." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always Set.empty
"""
                        ]
        , test "should replace Set.intersect set Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.intersect set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.intersect on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.intersect set set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.intersect set set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.intersect with two equal sets can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace value.field |> Set.intersect (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Set
a = value.field |> Set.intersect (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.intersect with two equal sets can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = value.field
"""
                        ]
        , test "should replace intersect (intersect (intersect set2 set3) set0) (intersect set1 set0) by intersect (intersect set2 set3) (intersect set1 set0)" <|
            \() ->
                """module A exposing (..)
import Set exposing (intersect)
a = Set.intersect (intersect (intersect set2 set3) set0) (intersect set1 set0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Set.intersect contains unnecessary equal sets across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (intersect)
a = Set.intersect (intersect set2 set3) (intersect set1 set0)
"""
                        ]
        , test "should replace intersect set0 << intersect set0 by intersect set0" <|
            \() ->
                """module A exposing (..)
import Set exposing (intersect)
a = Set.intersect set0 << intersect set0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.intersect on Set.intersect with an equal set"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (intersect)
a = intersect set0
"""
                        ]
        , test "should replace intersect set0 >> intersect set0 by intersect set0" <|
            \() ->
                """module A exposing (..)
import Set exposing (intersect)
a = intersect set0 >> Set.intersect set0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.intersect on Set.intersect with an equal set"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Set.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (intersect)
a = intersect set0
"""
                        ]
        ]


setDiffTests : Test
setDiffTests =
    describe "Set.diff"
        [ test "should not report Set.diff used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.diff set1 set2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.diff Set.empty set by Set.empty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.diff Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.diff Set.empty will always result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.empty
"""
                        ]
        , test "should replace Set.diff set Set.empty by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.diff set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.diff with Set.empty"
                            , details = [ "You can replace this call by the given first set." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.empty |> Set.diff set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.diff set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.diff with Set.empty"
                            , details = [ "You can replace this call by the given first set." ]
                            , under = "Set.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        ]


setUnionTests : Test
setUnionTests =
    describe "Set.union"
        [ test "should not report Set.union used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union set1 set2
b = Set.union (Set.union set0 set1) (Set.union set2 set3)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.union Set.empty set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union Set.empty set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union Set.empty will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.union set Set.empty by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union set Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.union with Set.empty"
                            , details = [ "You can replace this call by the given first set." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace Set.empty |> Set.union set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.union set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.union with Set.empty"
                            , details = [ "You can replace this call by the given first set." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace set |> Set.union Set.empty by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.union set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.union with Set.empty"
                            , details = [ "You can replace this call by the given first set." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should report Set.union applied on two set literals" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.fromList [b,c]) (Set.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.fromList [b,c,d,e])
"""
                        ]
        , test "should report Set.union applied on two set literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.fromList [ b, z ]) (Set.fromList [c,d,0])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.fromList [ b, z ,c,d,0])
"""
                        ]
        , test "should report Set.union <| on two set literals" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.fromList [b, c]) <| Set.fromList [d,e]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [b, c,d,e]
"""
                        ]
        , test "should report Set.union |> on two set literals" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [d,e] |> Set.union (Set.fromList [b,c])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [b,c,d,e]
"""
                        ]
        , test "should report Set.union |> on two set literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.fromList [c,d,0] |> Set.union (Set.fromList [ b, z ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.fromList [ b, z ,c,d,0]
"""
                        ]
        , test "should replace Set.union ([ b, c ] |> Set.fromList) (Set.fromList [ d, e ]) by (Set.fromList [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union ([ b, c ] |> Set.fromList) (Set.fromList [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.fromList [ b, c , d, e ])
"""
                        ]
        , test "should replace Set.union ([ b, c ] |> Set.fromList) (Set.fromList <| [ d, e ]) by (Set.fromList <| [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union ([ b, c ] |> Set.fromList) (Set.fromList <| [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.fromList <| [ b, c , d, e ])
"""
                        ]
        , test "should replace Set.union (Set.fromList <| [ b, c ]) ([ d, e ] |> Set.fromList) by ([ b, c , d, e ] |> Set.fromList)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.fromList <| [ b, c ]) ([ d, e ] |> Set.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = ([ b, c , d, e ] |> Set.fromList)
"""
                        ]
        , test "should replace [ d, e ] |> Set.fromList |> Set.union (Set.fromList <| [ b, c ]) by [ b, c , d, e ] |> Set.fromList" <|
            \() ->
                """module A exposing (..)
import Set
a = [ d, e ] |> Set.fromList |> Set.union (Set.fromList <| [ b, c ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union on Set.fromList calls can be turned into a single Set.fromList call"
                            , details = [ "Try moving all the elements into a single Set.fromList call." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = [ b, c , d, e ] |> Set.fromList
"""
                        ]
        , test "should replace Set.union set set by set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union set set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with two equal sets can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        , test "should replace value.field |> Set.union (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Set
a = value.field |> Set.union (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with two equal sets can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = value.field
"""
                        ]
        , test "should replace union (union (union set2 set3) set0) (union set1 set0) by union (union set2 set3) (union set1 set0)" <|
            \() ->
                """module A exposing (..)
import Set exposing (union)
a = Set.union (union (union set2 set3) set0) (union set1 set0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Set.union contains unnecessary equal sets across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (union)
a = Set.union (union set2 set3) (union set1 set0)
"""
                        ]
        , test "should replace union set0 << union set0 by union set0" <|
            \() ->
                """module A exposing (..)
import Set exposing (union)
a = Set.union set0 << union set0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.union on Set.union with an equal set"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (union)
a = union set0
"""
                        ]
        , test "should replace union set0 >> union set0 by union set0" <|
            \() ->
                """module A exposing (..)
import Set exposing (union)
a = union set0 >> Set.union set0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.union on Set.union with an equal set"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (union)
a = union set0
"""
                        ]
        , test "should replace Set.union (Set.singleton k) set by (Set.insert k)" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.singleton k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.insert k)
"""
                        ]
        , test "should replace Set.union (Set.singleton k) <| set by (Set.insert k) <| set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union (Set.singleton k) <| set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.insert k) <| set
"""
                        ]
        , test "should replace set |> Set.union (Set.singleton k) by set |> (Set.insert k)" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.union (Set.singleton k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set |> (Set.insert k)
"""
                        ]
        , test "should replace set |> Set.union (Set.singleton <| k) by set |> (Set.insert <| k)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Set
a = set |> Set.union (Set.singleton <| k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set |> (Set.insert <| k)
"""
                        ]
        , test "should replace set |> (Set.union <| Set.singleton <| k) by set |> (Set.insert <| k)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Set
a = set |> (Set.union <| Set.singleton <| k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set |> (Set.insert <| k)
"""
                        ]
        , test "should replace Set.union set (Set.singleton k) by Set.insert k set" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Set
a = Set.union set (Set.singleton k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert k set
"""
                        ]
        , test "should replace Set.union set (Set.singleton <| ..multiline..) by Set.insert (..multiline with original indent preserved..) set" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Set
a = Set.union set (Set.singleton <| let _ = 0
                                        _ = 1 in k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert
                                   (let _ = 0
                                        _ = 1 in k) set
"""
                        ]
        , test "should replace Set.union set <| Set.singleton <| k by Set.insert k set" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Set
a = Set.union set <| Set.singleton <| k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this call by Set.insert with the same element given to Set.singleton which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert k set
"""
                        ]
        , test "should replace Set.union << Set.singleton by Set.insert" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.union << Set.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this composition by Set.insert which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert
"""
                        ]
        , test "should replace Set.singleton >> Set.union by Set.insert" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.singleton >> Set.union
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.union with a singleton set can be combined into Set.insert"
                            , details = [ "You can replace this composition by Set.insert which is meant for this exact purpose." ]
                            , under = "Set.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert
"""
                        ]
        ]


setInsertTests : Test
setInsertTests =
    describe "Set.insert"
        [ test "should not report Set.insert used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.insert x set
b = Set.insert k0 (Set.insert k1 set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.insert x Set.empty by Set.singleton x" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.insert x Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Set.singleton instead of inserting in Set.empty"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton x
"""
                        ]
        , test "should replace Set.empty |> Set.insert x by Set.singleton x" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.empty |> Set.insert x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use Set.singleton instead of inserting in Set.empty"
                            , details = [ "You can replace this call by Set.singleton." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.singleton x
"""
                        ]
        , test "should replace Set.insert k (Set.insert k set) by Set.insert k set" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.insert k (Set.insert k set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.insert after equivalent Set.insert"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert k set
"""
                        ]
        , test "should replace Set.insert k >> Set.insert k by Set.insert k" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.insert k >> Set.insert k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.insert after equivalent Set.insert"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 21 }, end = { row = 3, column = 31 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert k
"""
                        ]
        , test "should replace Set.insert k << Set.insert k by Set.insert k" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.insert k << Set.insert k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Set.insert after equivalent Set.insert"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Set.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.insert k
"""
                        ]
        ]


setFoldlTests : Test
setFoldlTests =
    describe "Set.foldl"
        [ test "should not report Set.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl
b = Set.foldl (\\el soFar -> soFar - el)
c = Set.foldl (\\el soFar -> soFar - el) 20
d = Set.foldl (\\el soFar -> soFar - el) 20 set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.foldl f initial Set.empty by initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl f initial Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl on Set.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = initial
"""
                        ]
        , test "should replace Set.foldl (always identity) initial set by initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl (always identity) initial set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = initial
"""
                        ]
        , test "should replace Set.foldl (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always initial
"""
                        ]
        , test "should replace Set.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which set is supplied next." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always
"""
                        ]
        , test "should replace Set.foldl Set.insert Set.empty by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldl Set.insert Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl Set.insert Set.empty will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace set |> Set.foldl (\\e s -> s |> Set.insert e) Set.empty by set" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.foldl (\\e s -> s |> Set.insert e) Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldl Set.insert Set.empty will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        ]


setFoldrTests : Test
setFoldrTests =
    describe "Set.foldr"
        [ test "should not report Set.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr
b = Set.foldr (\\el soFar -> soFar - el)
c = Set.foldr (\\el soFar -> soFar - el) 20
d = Set.foldr (\\el soFar -> soFar - el) 20 set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Set.foldr f initial Set.empty by initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr f initial Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr on Set.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = initial
"""
                        ]
        , test "should replace Set.foldr (always identity) initial set by initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr (always identity) initial set
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = initial
"""
                        ]
        , test "should replace Set.foldr (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always initial
"""
                        ]
        , test "should replace Set.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which set is supplied next." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = always
"""
                        ]
        , test "should replace Set.foldr (::) [] by Set.toList" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr (::) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr (::) [] is the same as Set.toList"
                            , details = [ "You can replace this call by Set.toList which is meant for this exact purpose." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.toList
"""
                        ]
        , test "should replace set |> Set.foldr (\\h t -> h :: t) [] by set |> Set.toList" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.foldr (\\h t -> h :: t) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr (::) [] is the same as Set.toList"
                            , details = [ "You can replace this call by Set.toList which is meant for this exact purpose." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set |> Set.toList
"""
                        ]
        , test "should replace Set.foldr Set.insert Set.empty by identity" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.foldr Set.insert Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr Set.insert Set.empty will always return the same given set"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = identity
"""
                        ]
        , test "should replace set |> Set.foldr (\\e s -> s |> Set.insert e) Set.empty by set" <|
            \() ->
                """module A exposing (..)
import Set
a = set |> Set.foldr (\\e s -> s |> Set.insert e) Set.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.foldr Set.insert Set.empty will always return the same given set"
                            , details = [ "You can replace this call by the set itself." ]
                            , under = "Set.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = set
"""
                        ]
        ]
