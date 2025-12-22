module Simplify.ArrayTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Array"
        [ arrayToListTests
        , arrayToIndexedListTests
        , arrayFromListTests
        , arrayMapTests
        , arrayIndexedMapTests
        , arrayFilterTests
        , arrayIsEmptyTests
        , arrayRepeatTests
        , arrayInitializeTests
        , arrayLengthTests
        , arrayAppendTests
        , arrayGetTests
        , arraySetTests
        , arraySliceTests
        , arrayFoldlTests
        , arrayFoldrTests
        ]


arrayToListTests : Test
arrayToListTests =
    describe "Array.toList"
        [ test "should not report Array.toList that contains a variable" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList
b = Array.toList array
c = Array.toList << f << Array.fromList
d = (Array.toList << f) << Array.fromList
e = Array.toList << (f << Array.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.toList Array.empty by []" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList on Array.empty will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = []
"""
                        ]
        , test "should replace x |> f |> Array.fromList |> Array.toList by x |> f" <|
            \() ->
                """module A exposing (..)
import Array
a = x |> f |> Array.fromList |> Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can replace this call by the argument given to Array.fromList." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = x |> f
"""
                        ]
        , test "should replace Array.toList << Array.fromList by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.toList << (Array.fromList << f) by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList << (Array.fromList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace Array.toList << (Array.fromList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList << (Array.fromList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (g << f)
"""
                        ]
        , test "should replace Array.toList << (f >> Array.fromList) by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList << (f >> Array.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace Array.toList << (f >> g >> Array.fromList) by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList << (f >> g >> Array.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f >> g)
"""
                        ]
        , test "should replace (f << Array.toList) << Array.fromList by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = (f << Array.toList) << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace (g << f << Array.toList) << Array.fromList by (g << f)" <|
            \() ->
                """module A exposing (..)
import Array
a = (g << f << Array.toList) << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (g << f)
"""
                        ]
        , test "should replace (Array.toList >> f) << Array.fromList by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = (Array.toList >> f) << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace (Array.toList >> f >> g) << Array.fromList by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Array
a = (Array.toList >> f >> g) << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f >> g)
"""
                        ]
        , test "should replace a |> Array.repeat n |> Array.toList by a |> List.repeat n" <|
            \() ->
                """module A exposing (..)
import Array
a = b |> Array.repeat n |> Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat, then Array.toList can be combined into List.repeat"
                            , details = [ "You can replace this call by List.repeat with the same arguments given to Array.repeat which is meant for this exact purpose." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = b |> List.repeat n
"""
                        ]
        , test "should replace Array.repeat n >> Array.toList by List.repeat n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat n >> Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat, then Array.toList can be combined into List.repeat"
                            , details = [ "You can replace this composition by List.repeat with the same arguments given to Array.repeat which is meant for this exact purpose." ]
                            , under = "Array.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = List.repeat n
"""
                        ]
        ]


arrayToIndexedListTests : Test
arrayToIndexedListTests =
    describe "Array.toIndexedMap"
        [ test "should not report Array.toList that contains a variable" <|
            \() ->
                """module A exposing (..)
import Array
a0 = Array.toIndexedList
a1 = Array.toIndexedList array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.toIndexedList Array.empty by []" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList on Array.empty will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Array.toIndexedList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = []
"""
                        ]
        ]


arrayFromListTests : Test
arrayFromListTests =
    describe "Array.fromList"
        [ test "should not report Array.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList
b = Array.fromList list
c = Array.fromList (x :: ys)
d = Array.fromList [x, y]
e = Array.fromList << f << Array.toList
f = (Array.fromList << f) << Array.toList
g = Array.fromList << (f << Array.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.fromList [] by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList on [] will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace x |> f |> Array.toList |> Array.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
import Array
a = x |> f |> Array.toList |> Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to Array.toList." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = x |> f
"""
                        ]
        , test "should replace Array.fromList << Array.toList by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.fromList << (Array.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList << (Array.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace Array.fromList << (Array.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList << (Array.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (g << f)
"""
                        ]
        , test "should replace Array.fromList << (f >> Array.toList) by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList << (f >> Array.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace (f << Array.fromList) << Array.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = (f << Array.fromList) << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace (Array.fromList >> f) << Array.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Array
a = (Array.fromList >> f) << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f)
"""
                        ]
        , test "should replace (Array.fromList >> f >> g) << Array.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Array
a = (Array.fromList >> f >> g) << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then Array.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (f >> g)
"""
                        ]
        ]


arrayMapTests : Test
arrayMapTests =
    describe "List.map"
        [ test "should not report Array.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map
b = Array.map f
c = Array.map f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.map f Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map f Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.map f <| Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map f <| Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.empty |> Array.map f by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.empty |> Array.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.map identity array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map identity array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.map identity <| array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map identity <| array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace array |> Array.map identity by array" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.map identity by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace identity |> Array.map by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = identity |> Array.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.map with an identity function will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        ]


arrayIndexedMapTests : Test
arrayIndexedMapTests =
    describe "Array.indexedMap"
        [ test "should not report Array.indexedMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap f array
b = Array.indexedMap (\\i value -> i + value) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.indexedMap f Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap f Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.indexedMap (\\_ y -> y) array by Array.map (\\y -> y) array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (\\_ y -> y) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map (\\y -> y) array
"""
                        ]
        , test "should replace Array.indexedMap (\\(_) (y) -> y) array by Array.map (\\(y) -> y) array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (\\(_) (y) -> y) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map (\\(y) -> y) array
"""
                        ]
        , test "should replace Array.indexedMap (\\_ -> f) array by Array.map f array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (\\_ -> f) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map f array
"""
                        ]
        , test "should replace Array.indexedMap (always f) array by Array.map f array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (always f) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map f array
"""
                        ]
        , test "should replace Array.indexedMap (always <| f y) array by Array.map (f y) array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (always <| f y) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map (f y) array
"""
                        ]
        , test "should replace Array.indexedMap (f y |> always) array by Array.map (f y) array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.indexedMap (f y |> always) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.indexedMap with a function that ignores the first argument is the same as Array.map"
                            , details = [ "You can replace this call by Array.map." ]
                            , under = "Array.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.map (f y) array
"""
                        ]
        ]


arrayFilterTests : Test
arrayFilterTests =
    describe "Array.filter"
        [ test "should not report Array.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f array
b = Array.filter f (Array.filter g array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.filter f Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.filter f <| Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f <| Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.empty |> Array.filter f by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.empty |> Array.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.filter f (Array.filter f array) by Array.filter f array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f (Array.filter f array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.filter after equivalent Array.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.filter f array)
"""
                        ]
        , test "should replace Array.filter f >> Array.filter f by Array.filter f" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f >> Array.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.filter after equivalent Array.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 35 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.filter f
"""
                        ]
        , test "should replace Array.filter f << Array.filter f by Array.filter f" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter f << Array.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.filter after equivalent Array.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.filter f
"""
                        ]
        , test "should replace Array.filter (always True) array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (always True) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return True will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.filter (\\_ -> True) array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (\\_ -> True) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return True will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return True will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return True will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace always True |> Array.filter by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = always True |> Array.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return True will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.filter (always False) array by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (always False) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.filter (\\_ -> False) array by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (\\_ -> False) array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.filter (always False) <| array by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (always False) <| array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace array |> Array.filter (always False) by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.filter (always False) by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.filter <| (always False) by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace always False |> Array.filter by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = always False |> Array.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.filter with a function that will always return False will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        ]


arrayIsEmptyTests : Test
arrayIsEmptyTests =
    describe "Array.isEmpty"
        [ test "should not report Array.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty
b = Array.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.isEmpty Array.empty by True" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.isEmpty on Array.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        ]
        , test "should replace Array.isEmpty (Array.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.isEmpty on this array will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = False
"""
                        ]
        , test "should replace Array.isEmpty (Array.fromList []) by True" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.isEmpty on Array.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = True
"""
                        , Review.Test.error
                            { message = "Array.fromList on [] will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.isEmpty (Array.empty)
"""
                        ]
        , test "should replace Array.isEmpty (Array.repeat 2 x) by False" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.repeat 2 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.isEmpty on this array will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = False
"""
                        ]
        , test "should replace list |> Array.fromList |> Array.isEmpty by list |> List.isEmpty" <|
            \() ->
                """module A exposing (..)
import Array
a = list |> Array.fromList |> Array.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.isEmpty can be combined into List.isEmpty"
                            , details = [ "You can replace this call by List.isEmpty with the same arguments given to Array.fromList which is meant for this exact purpose." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = list |> List.isEmpty
"""
                        ]
        , test "should not report Array.isEmpty (Array.repeat n x)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.repeat n x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.isEmpty (Array.initialize 2 f) by False" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.initialize 2 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.isEmpty on this array will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Array.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = False
"""
                        ]
        , test "should not report Array.isEmpty (Array.initialize n f)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.isEmpty (Array.initialize n f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace comparisons to the empty array with Array.isEmpty" <|
            \() ->
                """module A exposing (..)

a = x == Array.empty"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "== Array.empty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Array.isEmpty x"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (in parens)" <|
            \() ->
                """module A exposing (..)

a = x == (Array.empty)"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "== (Array.empty)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Array.isEmpty x"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (reverse order)" <|
            \() ->
                """module A exposing (..)

a = Array.empty == x"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "Array.empty =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Array.isEmpty x"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (negated)" <|
            \() ->
                """module A exposing (..)

a = x /= Array.empty"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "/= Array.empty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (Array.isEmpty x)"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (negated, reverse order)" <|
            \() ->
                """module A exposing (..)

a = Array.empty /= x"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "Array.empty /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (Array.isEmpty x)"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (needs parentheses)" <|
            \() ->
                """module A exposing (..)

a = x ++ y == Array.empty"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "== Array.empty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Array.isEmpty (x ++ y)"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (needs parentheses, reverse order)" <|
            \() ->
                """module A exposing (..)

a = Array.empty == x ++ y"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "Array.empty =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = Array.isEmpty (x ++ y)"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (needs parentheses, negated)" <|
            \() ->
                """module A exposing (..)

a = x ++ y /= Array.empty"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "/= Array.empty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (Array.isEmpty (x ++ y))"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (needs parentheses, negated, reverse order)" <|
            \() ->
                """module A exposing (..)

a = Array.empty /= x ++ y"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "Array.empty /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (Array.isEmpty (x ++ y))"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (infix)" <|
            \() ->
                """module A exposing (..)

a = x |> (==) Array.empty"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "(==) Array.empty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = x |> Array.isEmpty"""
                        ]
        , test "should replace comparisons to the empty array with Array.isEmpty (infix, with parens)" <|
            \() ->
                """module A exposing (..)

a = x |> (==) (Array.empty)"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty array can be replaced by a call to Array.isEmpty"
                            , details = [ "You can replace this comparison to an empty array with a call to Array.isEmpty, which is more efficient." ]
                            , under = "(==) (Array.empty)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = x |> Array.isEmpty"""
                        ]
        ]


arrayRepeatTests : Test
arrayRepeatTests =
    describe "Array.repeat"
        [ test "should not report Array.repeat used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat n
b = Array.repeat 2
c = Array.repeat n x
d = Array.repeat 5 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace Array.repeat n x by x" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat n x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.repeat 0 x by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat 0 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.repeat 0 by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.repeat -5 x by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat -5 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat with negative length will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        ]


arrayInitializeTests : Test
arrayInitializeTests =
    describe "Array.initialize"
        [ test "should not report Array.initialize used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.initialize n
b = Array.initialize 2
c = Array.initialize n f
d = Array.initialize 5 f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace Array.initialize n f by f" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.initialize n f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.initialize 0 f by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.initialize 0 f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.initialize with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.initialize"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.initialize 0 by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.initialize 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.initialize with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.initialize"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.initialize -5 f by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.initialize -5 f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.initialize with negative length will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.initialize"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        ]


arrayLengthTests : Test
arrayLengthTests =
    describe "Array.length"
        [ test "should not report Array.length used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length
b = Array.length array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.length Array.empty by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.fromList [b, c, d]) by 3" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.fromList [b, c, d])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the array is 3"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 3
"""
                        ]
        , test "should replace Array.empty |> Array.length by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.empty |> Array.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.repeat 1 x) by 1" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.repeat 1 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the array is 1"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 1
"""
                        ]
        , test "should replace Array.length (Array.repeat n x) by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.repeat n x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace Array.length (Array.repeat n x) by Basics.max 0 n (when max is already in scope)" <|
            \() ->
                """module A exposing (..)
import Array
max = 1
a = Array.length (Array.repeat n x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
max = 1
a = Basics.max 0 n
"""
                        ]
        , test "should replace Array.length <| Array.repeat n x by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length <| Array.repeat n x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace Array.length <| Array.repeat n <| x by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length <| Array.repeat n <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace Array.repeat n x |> Array.length by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.repeat n x |> Array.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace x |> Array.repeat n |> Array.length by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = x |> Array.repeat n |> Array.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.repeat with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace Array.length (Array.repeat 0 x) by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.repeat 0 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length (Array.empty)
"""
                        , Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.repeat -1 x) by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.repeat -1 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.repeat with negative length will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length (Array.empty)
"""
                        , Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.initialize 1 f) by 1" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.initialize 1 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the array is 1"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 1
"""
                        ]
        , test "should replace Array.length (Array.initialize 0 f) by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.initialize 0 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.initialize with length 0 will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.initialize"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length (Array.empty)
"""
                        , Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.initialize -1 f) by 0" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.initialize -1 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.initialize with negative length will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.initialize"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length (Array.empty)
"""
                        , Review.Test.error
                            { message = "The length of the array is 0"
                            , details = [ "The length of the array can be determined by looking at the code." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = 0
"""
                        ]
        , test "should replace Array.length (Array.initialize n f) by max 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.length (Array.initialize n f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.length on an array created by Array.initialize with a given length will result in that length"
                            , details = [ "You can replace this call by max 0 with the given length. max 0 makes sure that negative given lengths return 0." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = max 0 n
"""
                        ]
        , test "should replace Array.fromList list |> Array.length with List.length list" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList list |> Array.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.fromList, then Array.length can be combined into List.length"
                            , details = [ "You can replace this call by List.length with the same arguments given to Array.fromList which is meant for this exact purpose." ]
                            , under = "Array.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = List.length list
"""
                        ]
        ]


arrayAppendTests : Test
arrayAppendTests =
    describe "Array.append"
        [ test "should not report Array.append with an array variable" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append
b = Array.append (Array.fromList [ 1 ])
c = Array.append (Array.fromList [ 1 ]) array
d = Array.append array (Array.fromList [ 1 ])
e = Array.append array1 array2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.append Array.empty array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append Array.empty array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append Array.empty will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.append Array.empty <| array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append Array.empty <| array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append Array.empty will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace array |> Array.append Array.empty by array" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.append Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append Array.empty will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.append Array.empty by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append Array.empty will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.append array Array.empty by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append array Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.append with Array.empty"
                            , details = [ "You can replace this call by the given first array." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.append array <| Array.empty by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append array <| Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.append with Array.empty"
                            , details = [ "You can replace this call by the given first array." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.empty |> Array.append array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.empty |> Array.append array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Array.append with Array.empty"
                            , details = [ "You can replace this call by the given first array." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should report Array.append applied on two array literals" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append (Array.fromList [b]) (Array.fromList [c,d,0])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList [b,c,d,0])
"""
                        ]
        , test "should report Array.append applied on two array literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append (Array.fromList [ b, z ]) (Array.fromList [c,d,0])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList [ b, z ,c,d,0])
"""
                        ]
        , test "should report Array.append <| on two array literals" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append (Array.fromList [b]) <| Array.fromList [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.fromList [b,c,d,0]
"""
                        ]
        , test "should report Array.append |> on two array literals" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList [c,d,0] |> Array.append (Array.fromList [b])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.fromList [b,c,d,0]
"""
                        ]
        , test "should report Array.append |> on two array literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList [c,d,0] |> Array.append (Array.fromList [ b, z ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.fromList [ b, z ,c,d,0]
"""
                        ]
        , test "should replace Array.append ([ b, c ] |> Array.fromList) (Array.fromList [ d, e ]) by (Array.fromList [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append ([ b, c ] |> Array.fromList) (Array.fromList [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList [ b, c , d, e ])
"""
                        ]
        , test "should replace Array.append ([ b, c ] |> Array.fromList) (Array.fromList <| [ d, e ]) by (Array.fromList <| [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append ([ b, c ] |> Array.fromList) (Array.fromList <| [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList <| [ b, c , d, e ])
"""
                        ]
        , test "should replace Array.append (Array.fromList <| [ b, c ]) ([ d, e ] |> Array.fromList) by ([ b, c , d, e ] |> Array.fromList)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.append (Array.fromList <| [ b, c ]) ([ d, e ] |> Array.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = ([ b, c , d, e ] |> Array.fromList)
"""
                        ]
        , test "should replace [ d, e ] |> Array.fromList |> Array.append (Array.fromList <| [ b, c ]) by [ b, c , d, e ] |> Array.fromList" <|
            \() ->
                """module A exposing (..)
import Array
a = [ d, e ] |> Array.fromList |> Array.append (Array.fromList <| [ b, c ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.append on Array.fromList calls can be turned into a single Array.fromList call"
                            , details = [ "Try moving all the elements into a single Array.fromList call." ]
                            , under = "Array.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = [ b, c , d, e ] |> Array.fromList
"""
                        ]
        ]


arrayGetTests : Test
arrayGetTests =
    describe "Array.get"
        [ test "should not report Array.get with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get n array
b = Array.get 0 array
c = Array.get n (Array.fromList [ 1 ])
d = Array.get n (Array.repeat m x)
e = Array.get 0 (Array.repeat m x)
f = Array.get 0 (Array.initialize m func)
g = Array.get n (Array.initialize m func)
h = Array.get 0 (Array.fromList (xs ++ [1]))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.get n Array.empty by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get n Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.get on Array.empty will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Nothing
"""
                        ]
        , test "should replace Array.get -1 array by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get -1 array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.get with negative index will always result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Nothing
"""
                        ]
        , test "should replace Array.get 1 (Array.fromList [ b, c, d ]) by Just c" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 1 (Array.fromList [ b, c, d ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Just c
"""
                        ]
        , test "should replace Array.get 1 (Array.fromList [ b 0, c 0, d 0 ]) by Just (c 0)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 1 (Array.fromList [ b 0, c 0, d 0 ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Just (c 0)
"""
                        ]
        , test "should replace Array.fromList [ b, c, d ] |> Array.get 1 |> f by c |> Just |> f" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList [ b, c, d ] |> Array.get 1 |> f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = c |> Just |> f
"""
                        ]
        , test "should replace Array.fromList [ b, g <| c, d ] |> Array.get 1 |> f by (g <| c) |> Just |> f" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.fromList [ b, g <| c, d ] |> Array.get 1 |> f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (g <| c) |> Just |> f
"""
                        ]
        , test "should replace f <| Array.get 1 <| Array.fromList [ b, c, d ] by f <| Just <| c" <|
            \() ->
                """module A exposing (..)
import Array
a = f <| Array.get 1 <| Array.fromList [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = f <| Just <| c
"""
                        ]
        , test "should replace f <| Array.get 1 <| Array.fromList [ b, c |> g, d ] by f <| Just <| (c |> g)" <|
            \() ->
                """module A exposing (..)
import Array
a = f <| Array.get 1 <| Array.fromList [ b, c |> g, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the targeted element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = f <| Just <| (c |> g)
"""
                        ]
        , test "should replace Array.get 100 (Array.fromList [ b, c, d ]) by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 100 (Array.fromList [ b, c, d ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.get with an index out of bounds of the given array will always return Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Nothing
"""
                        ]
        , test "should replace Array.get 2 (Array.repeat 10 x) by Just x" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 2 (Array.repeat 10 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the repeated element." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Just x
"""
                        ]
        , test "should replace Array.get 100 (Array.repeat 10 a) by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 100 (Array.repeat 10 x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.get with an index out of bounds of the given array will always return Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Nothing
"""
                        ]
        , test "should replace Array.get 100 (Array.initialize 10 f) by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 100 (Array.initialize 10 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.get with an index out of bounds of the given array will always return Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Nothing
"""
                        ]
        , test "should replace Array.get 2 (Array.initialize 10 f) by Just (f x)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.get 2 (Array.initialize 10 f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The element returned by Array.get is known"
                            , details = [ "You can replace this call by Just the function directly applied to the index." ]
                            , under = "Array.get"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Just (f 2)
"""
                        ]
        ]


arraySetTests : Test
arraySetTests =
    describe "Array.set"
        [ test "should not report Array.set with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set n x array
b = Array.set 0 x array
c = Array.set n x
d = Array.set 1
e = Array.set 1 x
f = Array.set n x (Array.fromList [ 1 ])
g = Array.set i v1 (Array.set j v0 array)
h = Array.set i v1 << Array.set j v0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.set n x Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set n x Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.set i v1 (Array.set i v0 array) by Array.set i v1 array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set i v1 (Array.set i v0 array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set on Array.set with the same index makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.set i v1 array
"""
                        ]
        , test "Array.set i v1 (Array.set i v0 <| f <| x) by Array.set i v1 (f <| x)" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set i v1 (Array.set i v0 <| f <| x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set on Array.set with the same index makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.set i v1 (f <| x)
"""
                        ]
        , test "should replace Array.set i v1 << Array.set i v0 by Array.set i v1" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set i v1 << Array.set i v0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set on Array.set with the same index makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.set i v1
"""
                        ]
        , test "should replace Array.set i v0 >> Array.set i v1 by Array.set i v1" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set i v0 >> Array.set i v1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set on Array.set with the same index makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 32 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.set i v1
"""
                        ]
        , test "should replace Array.set -1 x array by array" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set -1 x array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set with negative index will always return the same given array"
                            , details = [ "You can replace this call by the array itself." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array
"""
                        ]
        , test "should replace Array.set -1 x by identity" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set -1 x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set with negative index will always return the same given array"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = identity
"""
                        ]
        , test "should replace Array.set -1 by always" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set with negative index will always return the same given array"
                            , details = [ "You can replace this call by always identity." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always identity
"""
                        ]
        , test "should replace Array.set 1 x (Array.fromList [ b, c, d ]) by (Array.fromList [ b, x, d ])" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set 1 x (Array.fromList [ b, c, d ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set will replace a known element"
                            , details = [ "You can directly replace the element at the given index in the array." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList [ b, x, d ])
"""
                        ]
        , test "should replace Array.set 100 x (Array.fromList [ b, c, d ]) by Nothing" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.set 100 x (Array.fromList [ b, c, d ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.set with an index out of bounds of the given array will always return the same given array"
                            , details = [ "You can replace this call by the given array." ]
                            , under = "Array.set"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = (Array.fromList [ b, c, d ])
"""
                        ]
        ]


arraySliceTests : Test
arraySliceTests =
    describe "Array.slice"
        [ test "should not report Array.slice that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report Array.slice 0 n" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice 0
b = Array.slice 0 n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.slice b 0 by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice b 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with end index 0 will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.slice b 0 str by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice b 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with end index 0 will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.slice n n by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with equal start and end index will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.slice n n str by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice n n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with equal start and end index will always result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.slice a z Array.empty by Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice a z Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice on Array.empty will result in Array.empty"
                            , details = [ "You can replace this call by Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.empty
"""
                        ]
        , test "should replace Array.slice with natural start >= natural end by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice 2 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with a start index greater than the end index will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should replace Array.slice with negative start >= negative end by always Array.empty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice -1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.slice with a negative start index closer to the right than the negative end index will always result in Array.empty"
                            , details = [ "You can replace this call by always Array.empty." ]
                            , under = "Array.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always Array.empty
"""
                        ]
        , test "should not report Array.slice with negative start, natural end" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice -1 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        , test "should not report Array.slice with natural start, negative end" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.slice 1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        ]


arrayFoldlTests : Test
arrayFoldlTests =
    describe "Array.foldl"
        [ test "should not report Array.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldl
b = Array.foldl (\\el soFar -> soFar - el)
c = Array.foldl (\\el soFar -> soFar - el) 20
d = Array.foldl (\\el soFar -> soFar - el) 20 array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.foldl f initial Array.empty by initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldl f initial Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldl on Array.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = initial
"""
                        ]
        , test "should replace Array.foldl (always identity) initial array by initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldl (always identity) initial array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = initial
"""
                        ]
        , test "should replace Array.foldl (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldl (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always initial
"""
                        ]
        , test "should replace Array.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which array is supplied next." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always
"""
                        ]
        , test "should replace Array.foldl f x (Array.fromList list) by List.foldl f x list" <|
            \() ->
                """module A exposing (..)
a = Array.foldl f x (Array.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldl f x list
"""
                        ]
        , test "should replace Array.foldl f x << Array.fromList by Array.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Array.foldl f x << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldl f x
"""
                        ]
        , test "should replace Array.fromList >> Array.foldl f x by Array.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Array.fromList >> Array.foldl f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldl f x
"""
                        ]
        ]


arrayFoldrTests : Test
arrayFoldrTests =
    describe "Array.foldr"
        [ test "should not report Array.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldr
b = Array.foldr (\\el soFar -> soFar - el)
c = Array.foldr (\\el soFar -> soFar - el) 20
d = Array.foldr (\\el soFar -> soFar - el) 20 array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Array.foldr f initial Array.empty by initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldr f initial Array.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldr on Array.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = initial
"""
                        ]
        , test "should replace Array.foldr (always identity) initial array by initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldr (always identity) initial array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = initial
"""
                        ]
        , test "should replace Array.foldr (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldr (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always initial
"""
                        ]
        , test "should replace Array.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which array is supplied next." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = always
"""
                        ]
        , test "should replace Array.foldr f x (Array.fromList list) by List.foldr f x list" <|
            \() ->
                """module A exposing (..)
a = Array.foldr f x (Array.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldr f x list
"""
                        ]
        , test "should replace Array.foldr f x << Array.fromList by Array.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Array.foldr f x << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldr f x
"""
                        ]
        , test "should replace Array.fromList >> Array.foldr f x by Array.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Array.fromList >> Array.foldr f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a list, you don't need to convert to an array"
                            , details = [ "Using List.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "Array.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.foldr f x
"""
                        ]
        ]
