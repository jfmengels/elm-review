module Simplify.DictTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "Dict"
        [ dictIsEmptyTests
        , dictFromListTests
        , dictToListTests
        , dictSizeTests
        , dictMemberTests
        , dictInsertTests
        , dictRemoveTests
        , dictUpdateTests
        , dictFilterTests
        , dictPartitionTests
        , dictMapTests
        , dictUnionTests
        , dictIntersectTests
        , dictDiffTests
        , dictFoldlTests
        , dictFoldrTests
        ]


dictIsEmptyTests : Test
dictIsEmptyTests =
    describe "Dict.isEmpty"
        [ test "should not report Dict.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty
b = Dict.isEmpty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.isEmpty Dict.empty by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on Dict.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList []) by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on Dict.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        , Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.empty)
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.singleton x) by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.singleton x y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace x :: xs |> Dict.isEmpty by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.singleton x y |> Dict.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace list |> Dict.fromList |> Dict.isEmpty by list |> List.isEmpty" <|
            \() ->
                """module A exposing (..)
import Dict
a = list |> Dict.fromList |> Dict.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList, then Dict.isEmpty can be combined into List.isEmpty"
                            , details = [ "You can replace this call by List.isEmpty with the same arguments given to Dict.fromList which is meant for this exact purpose." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = list |> List.isEmpty
"""
                        ]
        ]


dictFromListTests : Test
dictFromListTests =
    describe "Dict.fromList"
        [ test "should not report Dict.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList
b = Dict.fromList list
c = Dict.fromList [x]
d = Dict.fromList [x, y]
e = Dict.fromList
f = Dict.fromList list
g = Dict.fromList << fun << Dict.toList
h = (Dict.fromList << fun) << Dict.toList
i = Dict.fromList << (fun << Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.fromList [] by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace x |> f |> Dict.toList |> Dict.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
import Dict
a = x |> f |> Dict.toList |> Dict.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to Dict.toList." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = x |> f
"""
                        ]
        , test "should replace Dict.fromList << Dict.toList by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace Dict.fromList << (Dict.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (Dict.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace Dict.fromList << (Dict.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (Dict.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (g << f)
"""
                        ]
        , test "should replace Dict.fromList << (f >> Dict.toList) by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (f >> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (f << Dict.fromList) << Dict.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (f << Dict.fromList) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (Dict.fromList >> f) << Dict.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (Dict.fromList >> f) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (Dict.fromList >> f >> g) << Dict.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (Dict.fromList >> f >> g) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f >> g)
"""
                        ]
        , test "should not report Dict.fromList with duplicate keys when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ ( key, v0 ), ( key, v1 ) ]
b = Dict.fromList [ ( ( 1, "", [ 'a', b ] ), v0 ), ( ( 1, "", [ 'a', b ] ), v1 ) ]
"""
                    |> Review.Test.run TestHelpers.ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not replace Dict.fromList [ x, x ] when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ x, x ]
"""
                    |> Review.Test.run TestHelpers.ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.fromList [ ( key, v0 ), ( key, v1 ) ] by Dict.fromList [ ( key, v1 ) ]" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ ( key, v0 ), ( key, v1 ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "key"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 23 }, end = { row = 3, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.fromList [ ( key, v1 ) ]
"""
                        ]
        , test "should replace Dict.fromList [ ( 0, v0 ), ( 0, v1 ) ] by Dict.fromList [ ( 0, v1 ) ] when expecting NaN because keys are literals" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ ( 0, v0 ), ( 0, v1 ) ]
"""
                    |> Review.Test.run TestHelpers.ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "0"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 23 }, end = { row = 3, column = 24 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.fromList [ ( 0, v1 ) ]
"""
                        ]
        , test "should replace Dict.fromList [ ( key, v0 ), thing, ( key, v1 ) ] by Dict.fromList [ thing, ( key, v1 ) ]" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ ( key, v0 ), thing, ( key, v1 ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "key"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 23 }, end = { row = 3, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.fromList [ thing, ( key, v1 ) ]
"""
                        ]
        , test "should replace Dict.fromList [ ( ( 1, \"\", [ 'a' ] ), v0 ), ( ( 1, \"\", [ 'a' ] ), v1 ) ] by Dict.fromList [ ( ( 1, \"\", [ 'a' ] ), v1 ) ]" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ ( ( 1, "", [ 'a' ] ), v0 ), ( ( 1, "", [ 'a' ] ), v1 ) ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = """( 1, "", [ 'a' ] )"""
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 23 }, end = { row = 3, column = 41 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.fromList [ ( ( 1, "", [ 'a' ] ), v1 ) ]
"""
                        ]
        , test "should replace Dict.fromList [ x, x ] by Dict.fromList [ x ]" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ x, x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on a list with a duplicate entry will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 21 }, end = { row = 3, column = 22 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.fromList [ x ]
"""
                        ]
        , test "should replace Dict.fromList [ let a = 0 in ( 0, 0 ), let a = 0 in ( 0, 0 ) ] by Dict.fromList [ let a = 0 in ( 0, 0 ) ] even when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [ let a = 0 in ( 0, 0 ), let a = 0 in ( 0, 0 ) ]
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on a list with a duplicate entry will only keep one of them"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "let a = 0 in ( 0, 0 )"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 21 }, end = { row = 3, column = 42 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a = Dict.fromList [ let a = 0 in ( 0, 0 ) ]
"""
                        ]
        ]


dictToListTests : Test
dictToListTests =
    describe "Dict.toList"
        [ test "should not report Dict.toList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList
b = Dict.toList dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.toList Dict.empty by []" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList on Dict.empty will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Dict.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = []
"""
                        ]
        ]


dictSizeTests : Test
dictSizeTests =
    describe "Dict.size"
        [ test "should not report Dict.size used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size
a = Dict.size b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace Dict.size Dict.fromList with unknown keys because they could contain duplicate keys which would make the final dict size smaller" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [b, c, d])
a = Dict.size (Dict.fromList [(b, 'b'), (c,'c'), (d,'d')])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.size Dict.empty by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.empty |> Dict.size by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.singleton x y |> Dict.size by 1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.singleton x y |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 1"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 1
"""
                        ]
        , test "should replace Dict.size (Dict.fromList []) by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size (Dict.empty)
"""
                        , Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [a]) by 1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [a])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 1"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 1
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1,1), (2,1), (3,1)]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1,1), (2,1), (3,1)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1,1), (2,1), (3,1), (3,2), (0x3,2)]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1,1), (2,1), (3,1), (3,2), (0x3,2)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        , Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "3"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 46 }, end = { row = 3, column = 47 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1,1), (2,1), (3,2), (0x3,2)])
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1.3,()), (-1.3,()), (2.1,()), (2.1,())]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1.3,()), (-1.3,()), (2.1,()), (2.1,())])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        , Review.Test.error
                            { message = "Dict.fromList on entries with a duplicate key will only keep the last entry"
                            , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                            , under = "2.1"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 3, column = 53 }, end = { row = 3, column = 56 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1.3,()), (-1.3,()), (2.1,())])
"""
                        ]
        ]


dictMemberTests : Test
dictMemberTests =
    describe "Dict.member"
        [ test "should not report Dict.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.member x Dict.empty by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member x Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.empty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace Dict.member 1 (Dict.fromList [ ( 0, () ), ( 1, () ), ( 2, () ) ]) by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 1 (Dict.fromList [ ( 0, () ), ( 1, () ), ( 2, () ) ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on a dict which contains the given key will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Dict.member 1 (Dict.fromList [ ( 0, () ), ( 1, () ), ( 2, () ) ]) by True when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 1 (Dict.fromList [ ( 0, () ), ( 1, () ), ( 2, () ) ])
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on a dict which contains the given key will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Dict.member 0 (Dict.fromList [ ( 2, () ), ( 3, () ), ( 1, () ) ]) by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 0 (Dict.fromList [ ( 2, () ), ( 3, () ), ( 1, () ) ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on a dict which does not contain the given key will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace Dict.member 0 (Dict.fromList [ ( 2, () ), ( 3, () ), ( 1, () ) ]) by False when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 0 (Dict.fromList [ ( 2, () ), ( 3, () ), ( 1, () ) ])
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on a dict which does not contain the given key will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should not simplify Dict.member reference (Dict.fromList [ ( 2, () ), ( 3, () ), ( reference, () ) ]) when expecting NaN" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member reference (Dict.fromList [ ( 2, () ), ( 3, () ), ( reference, () ) ])
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.member 0 (Dict.fromList list) by List.any (Tuple.first >> (==) 0) list when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 0 (Dict.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.fromList can be replaced by List.any with a function comparing the key"
                            , details = [ "You can replace these calls by List.any comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a = List.any (Tuple.first >> (==) 0) list
"""
                        ]
        , test "should replace Dict.member (let ...) (Dict.fromList list) by List.any (Tuple.first >> (==) (let ...)) list _with correct let indentation_ when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member (let x = 0
                     y = 1 in x + y) (Dict.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.fromList can be replaced by List.any with a function comparing the key"
                            , details = [ "You can replace these calls by List.any comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a = List.any (Tuple.first >> (==)
                (let x = 0
                     y = 1 in x + y)) list
"""
                        ]
        , test "should replace Dict.member 0 << Dict.fromList by List.any (Tuple.first >> (==) (0)) when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member 0 << Dict.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.fromList can be replaced by List.any with a function comparing the key"
                            , details = [ "You can replace this composition by List.any comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a = List.any (Tuple.first >> (==) (0))
"""
                        ]
        , test "should replace (Dict.member <| let ...) << Dict.fromList by List.any (Tuple.first >> (==) (let ...)) _with correct let indentation and inserted parens_ when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Dict
a=(Dict.member<| let x = 0
                     y = 1 in x + y) << Dict.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.fromList can be replaced by List.any with a function comparing the key"
                            , details = [ "You can replace this composition by List.any comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a=(List.any<| (Tuple.first >> (==) (
                 let x = 0
                     y = 1 in x + y)))
"""
                        ]
        , test "should replace Dict.fromList >> (Dict.member <| let ...) by List.any (Tuple.first >> (==) (let ...)) _with correct let indentation and inserted parens_ when expectNaN is not enabled" <|
            \() ->
                """module A exposing (..)
import Dict
a=Dict.fromList >>
  (Dict.member<| let x = 0
                     y = 1 in x + y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.fromList can be replaced by List.any with a function comparing the key"
                            , details = [ "You can replace this composition by List.any comparing the key which is both simpler and faster. The automatic fix suggests Tuple.first >> (==) ... to only evaluate the member to check for once. However, if it is a variable, a simpler alternative might be using a lambda like \\( k, _ ) -> k == ..." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Dict
a=(List.any<| (Tuple.first >> (==) (
                 let x = 0
                     y = 1 in x + y)))
"""
                        ]
        ]


dictInsertTests : Test
dictInsertTests =
    describe "Dict.insert"
        [ test "should not report Dict.remove used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.insert
a1 = Dict.insert k
a2 = Dict.insert k dict
a3 = Dict.insert k1 v1 (Dict.insert k0 v0 dict)
a4 = Dict.insert k1 v1 << Dict.insert k0 v0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.insert k v1 (Dict.insert k v0 dict) by Dict.insert k v1 dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.insert k v1 (Dict.insert k v0 dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.insert on Dict.insert with the same key makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Dict.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v1 dict
"""
                        ]
        , test "Dict.insert k v1 (Dict.insert k v0 <| f <| x) by Dict.insert k v1 (f <| x)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.insert k v1 (Dict.insert k v0 <| f <| x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.insert on Dict.insert with the same key makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Dict.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v1 (f <| x)
"""
                        ]
        , test "should replace Dict.insert k v1 << Dict.insert k v0 by Dict.insert k v1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.insert k v1 << Dict.insert k v0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.insert on Dict.insert with the same key makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Dict.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v1
"""
                        ]
        , test "should replace Dict.insert k v0 >> Dict.insert k v1 by Dict.insert k v1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.insert k v0 >> Dict.insert k v1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.insert on Dict.insert with the same key makes the earlier operation unnecessary"
                            , details = [ "You can remove the earlier operation." ]
                            , under = "Dict.insert"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 25 }, end = { row = 3, column = 36 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v1
"""
                        ]
        ]


dictRemoveTests : Test
dictRemoveTests =
    describe "Dict.remove"
        [ test "should not report Dict.remove used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.remove
a1 = Dict.remove k
a2 = Dict.remove k dict
a3 = Dict.remove k0 (Dict.remove k1 dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.remove k Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.remove k Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.remove on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.remove"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.remove k (Dict.remove k dict) by Dict.remove k dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.remove k (Dict.remove k dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.remove after equivalent Dict.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.remove k dict)
"""
                        ]
        , test "should replace Dict.remove k >> Dict.remove k by Dict.remove k" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.remove k >> Dict.remove k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.remove after equivalent Dict.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 22 }, end = { row = 3, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k
"""
                        ]
        , test "should replace Dict.remove k << Dict.remove k by Dict.remove k" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.remove k << Dict.remove k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.remove after equivalent Dict.remove"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.remove"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k
"""
                        ]
        ]


dictUpdateTests : Test
dictUpdateTests =
    describe "Dict.update"
        [ test "should not report Dict.update used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.update
a1 = Dict.update k
a2 = Dict.update k f
a3 = Dict.update k f dict
-- not Dict.update k f dict because e.g. f = Maybe.map ((+) 1)
a4 = Dict.update k f (Dict.update k f dict)
a5 = Dict.update k (\\v -> Just (f v)) dict
a6 = Dict.update k f Dict.empty -- because f could insert by returning Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.update k identity dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k identity dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an identity update function will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.update k identity by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an identity update function will always return the same given dict"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace Dict.update k (\\_ -> Nothing) dict by Dict.remove k dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k (\\_ -> Nothing) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Nothing is the same as Dict.remove"
                            , details = [ "You can replace this call by Dict.remove with the same given key." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k dict
"""
                        ]
        , test "should replace Dict.update k (\\_ -> Nothing) by Dict.remove k" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k (\\_ -> Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Nothing is the same as Dict.remove"
                            , details = [ "You can replace this call by Dict.remove with the same given key." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k
"""
                        ]
        , test "should replace Dict.update k <| (\\_ -> Nothing) by Dict.remove k" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k <| (\\_ -> Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Nothing is the same as Dict.remove"
                            , details = [ "You can replace this call by Dict.remove with the same given key." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k
"""
                        ]
        , test "should replace (\\_ -> Nothing) |> Dict.update k by Dict.remove k" <|
            \() ->
                """module A exposing (..)
import Dict
a = (\\_ -> Nothing) |> Dict.update k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Nothing is the same as Dict.remove"
                            , details = [ "You can replace this call by Dict.remove with the same given key." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.remove k
"""
                        ]
        , test "should replace Dict.update k (\\_ -> Just v) dict by Dict.insert k v dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k (\\_ -> Just v) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Just a value is the same as Dict.insert with that value"
                            , details = [ "You can replace this call by Dict.insert with the same given key and the value inside the Just variant." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v dict
"""
                        ]
        , test "should replace Dict.update k (\\_ -> v |> f |> Just) dict by Dict.insert k (v |> f) dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k (\\_ -> v |> f |> Just) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Just a value is the same as Dict.insert with that value"
                            , details = [ "You can replace this call by Dict.insert with the same given key and the value inside the Just variant." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k (v |> f) dict
"""
                        ]
        , test "should replace Dict.update k (\\_ -> Just v) by Dict.insert k v" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k (\\_ -> Just v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Just a value is the same as Dict.insert with that value"
                            , details = [ "You can replace this call by Dict.insert with the same given key and the value inside the Just variant." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k v
"""
                        ]
        , test "should replace Dict.update k <| (\\_ -> Just v) by Dict.insert k <| v" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.update k <| (\\_ -> Just v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Just a value is the same as Dict.insert with that value"
                            , details = [ "You can replace this call by Dict.insert with the same given key and the value inside the Just variant." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k <| v
"""
                        ]
        , test "should replace (\\_ -> Just v) |> Dict.update k by v |> Dict.insert k" <|
            \() ->
                """module A exposing (..)
import Dict
a = (\\_ -> Just v) |> Dict.update k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.update with an update function that is always Just a value is the same as Dict.insert with that value"
                            , details = [ "You can replace this call by Dict.insert with the same given key and the value inside the Just variant." ]
                            , under = "Dict.update"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = v |> Dict.insert k
"""
                        ]
        ]


dictFilterTests : Test
dictFilterTests =
    describe "Dict.filter"
        [ test "should not report Dict.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.filter
a1 = Dict.filter f
a2 = Dict.filter f dict
a3 = Dict.filter f (Dict.filter g dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.filter f Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.filter (always (always True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter f (Dict.filter f dict) by Dict.filter f dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter f (Dict.filter f dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.filter after equivalent Dict.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.filter f dict)
"""
                        ]
        , test "should replace Dict.filter f >> Dict.filter f by Dict.filter f" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter f >> Dict.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.filter after equivalent Dict.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 22 }, end = { row = 3, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.filter f
"""
                        ]
        , test "should replace Dict.filter f << Dict.filter f by Dict.filter f" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter f << Dict.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.filter after equivalent Dict.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.filter f
"""
                        ]
        , test "should replace Dict.filter (always (\\_ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ -> (\\_ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ -> (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ -> (always True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ -> (always True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ _ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ _ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (always (always True)) by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always True))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace Dict.filter (always (always False)) dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always False)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return False will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.filter (always (always False)) dict by always Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always False))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return False will always result in Dict.empty"
                            , details = [ "You can replace this call by always Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always Dict.empty
"""
                        ]
        ]


dictPartitionTests : Test
dictPartitionTests =
    describe "Dict.partition"
        [ test "should not report Dict.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.partition
a1 = Dict.partition f
a2 = Dict.partition f dict
a3 = Dict.partition (\\_ _ -> bool)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition f Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition f <| Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition f <| Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.empty |> Dict.partition f by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (always (always True)) x by ( x, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always True)) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( x, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (always (\\_ -> True)) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ _ -> True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ _ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ -> \\_ -> True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ -> \\_ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ -> always True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ -> always True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should not replace Dict.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always True))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition (always (always False)) dict by ( Dict.empty, dict )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always False)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second dict"
                            , details = [ "Since the predicate function always returns False, the first dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, dict )
"""
                        ]
        , test "should replace Dict.partition (always (always False)) by (Tuple.pair Dict.empty)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always False))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second dict"
                            , details = [ "Since the predicate function always returns False, the first dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Tuple.pair Dict.empty)
"""
                        ]
        ]


dictIntersectTests : Test
dictIntersectTests =
    describe "Dict.intersect"
        [ test "should not report Dict.intersect used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect
b = Dict.intersect x
c = Dict.intersect x y
d = Dict.intersect (Dict.intersect dict0 dict1) (Dict.union dict2 dict3)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.intersect Dict.empty dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.intersect Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by always Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always Dict.empty
"""
                        ]
        , test "should replace Dict.intersect dict Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.intersect dict dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect dict dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect with two equal dicts can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace value.field |> Dict.intersect (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Dict
a = value.field |> Dict.intersect (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect with two equal dicts can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = value.field
"""
                        ]
        , test "should replace intersect (intersect (intersect dict2 dict3) dict0) (intersect dict1 dict0) by intersect (intersect dict2 dict3) (intersect dict1 dict0)" <|
            \() ->
                """module A exposing (..)
import Dict exposing (intersect)
a = Dict.intersect (intersect (intersect dict2 dict3) dict0) (intersect dict1 dict0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Dict.intersect contains unnecessary equal dicts across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (intersect)
a = Dict.intersect (intersect dict2 dict3) (intersect dict1 dict0)
"""
                        ]
        , test "should replace intersect dict0 << intersect dict0 by intersect dict0" <|
            \() ->
                """module A exposing (..)
import Dict exposing (intersect)
a = Dict.intersect dict0 << intersect dict0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.intersect on Dict.intersect with an equal dict"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (intersect)
a = intersect dict0
"""
                        ]
        , test "should replace intersect dict0 >> intersect dict0 by intersect dict0" <|
            \() ->
                """module A exposing (..)
import Dict exposing (intersect)
a = intersect dict0 >> Dict.intersect dict0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.intersect on Dict.intersect with an equal dict"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (intersect)
a = intersect dict0
"""
                        ]
        ]


dictDiffTests : Test
dictDiffTests =
    describe "Dict.diff"
        [ test "should not report Dict.diff used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.diff Dict.empty dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.diff Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.diff dict Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.diff with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.empty |> Dict.diff dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.diff dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.diff with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        ]


dictMapTests : Test
dictMapTests =
    describe "Dict.map"
        [ test "should not report Dict.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.map
a1 = Dict.map f
a2 = Dict.map f dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.map f Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.map (always identity) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (always identity) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (always (\\v -> v)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (always (\\v -> v)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (\\_ -> identity) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (\\_ -> identity) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (\\_ v -> v) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (\\_ v -> v) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        ]


dictUnionTests : Test
dictUnionTests =
    describe "Dict.union"
        [ test "should not report Dict.union used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union x y
b = Dict.union (Dict.union dict0 dict1) (Dict.union dict2 dict3)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.union Dict.empty dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.union dict Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.empty |> Dict.union dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.union dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace dict |> Dict.union Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.union dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should report Dict.union applied on two dict literals" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [b,c]) (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b,c])
"""
                        ]
        , test "should report Dict.union applied on two dict literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [ b, c ]) (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e, b, c ])
"""
                        ]
        , test "should report Dict.union <| on two dict literals" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [b,c]) <| Dict.fromList [d,e]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b,c])
"""
                        ]
        , test "should replace Dict.fromList [d,e] |> Dict.union (Dict.fromList [b, c]) by (Dict.fromList [d,e,b, c])" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [d,e] |> Dict.union (Dict.fromList [b, c])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b, c])
"""
                        ]
        , test "should replace Dict.fromList [b,c] |> Dict.union (Dict.fromList [d,e]) by (Dict.fromList [b,c,d,e])" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [b,c] |> Dict.union (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [b,c,d,e])
"""
                        ]
        , test "should replace Dict.union ([ b, c ] |> Dict.fromList) (Dict.fromList [ d, e ]) by ([ d, e , b, c ] |> Dict.fromList)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union ([ b, c ] |> Dict.fromList) (Dict.fromList [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ([ d, e , b, c ] |> Dict.fromList)
"""
                        ]
        , test "should replace Dict.union ([ b, c ] |> Dict.fromList) (Dict.fromList <| [ d, e ]) by ([ d, e , b, c ] |> Dict.fromList)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union ([ b, c ] |> Dict.fromList) (Dict.fromList <| [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ([ d, e , b, c ] |> Dict.fromList)
"""
                        ]
        , test "should replace Dict.union (Dict.fromList <| [ b, c ]) ([ d, e ] |> Dict.fromList) by (Dict.fromList <| [ d, e , b, c ])" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList <| [ b, c ]) ([ d, e ] |> Dict.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList <| [ d, e , b, c ])
"""
                        ]
        , test "should replace [ d, e ] |> Dict.fromList |> Dict.union (Dict.fromList <| [ b, c ]) by (Dict.fromList <| [ d, e , b, c ])" <|
            \() ->
                """module A exposing (..)
import Dict
a = [ d, e ] |> Dict.fromList |> Dict.union (Dict.fromList <| [ b, c ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList <| [ d, e , b, c ])
"""
                        ]
        , test "should replace Dict.union dict dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union dict dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with two equal dicts can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace value.field |> Dict.union (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Dict
a = value.field |> Dict.union (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with two equal dicts can be replaced by one of them"
                            , details = [ "You can replace this call by one of its arguments." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = value.field
"""
                        ]
        , test "should replace union (union (union dict2 dict3) dict0) (union dict1 dict0) by union (union dict2 dict3) (union dict1 dict0)" <|
            \() ->
                """module A exposing (..)
import Dict exposing (union)
a = Dict.union (union (union dict2 dict3) dict0) (union dict1 dict0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "nested Dict.union contains unnecessary equal dicts across both arguments"
                            , details = [ "You can replace the call that has an equal argument by its other argument." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (union)
a = Dict.union (union dict2 dict3) (union dict1 dict0)
"""
                        ]
        , test "should replace union dict0 << union dict0 by union dict0" <|
            \() ->
                """module A exposing (..)
import Dict exposing (union)
a = Dict.union dict0 << union dict0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union on Dict.union with an equal dict"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (union)
a = union dict0
"""
                        ]
        , test "should replace union dict0 >> union dict0 by union dict0" <|
            \() ->
                """module A exposing (..)
import Dict exposing (union)
a = union dict0 >> Dict.union dict0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union on Dict.union with an equal dict"
                            , details = [ "You can replace this composition by either its left or right function as both are equivalent." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict exposing (union)
a = union dict0
"""
                        ]
        , test "should replace Dict.union (Dict.singleton k v) dict by (Dict.insert k v)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.singleton k v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this call by Dict.insert with the same key and value given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.insert k v)
"""
                        ]
        , test "should replace Dict.union (Dict.singleton k v) <| dict by (Dict.insert k v) <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.singleton k v) <| dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this call by Dict.insert with the same key and value given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.insert k v) <| dict
"""
                        ]
        , test "should replace dict |> Dict.union (Dict.singleton k v) by dict |> (Dict.insert k v)" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.union (Dict.singleton k v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this call by Dict.insert with the same key and value given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> (Dict.insert k v)
"""
                        ]
        , test "should replace dict |> Dict.union (Dict.singleton k <| v) by dict |> (Dict.insert k <| v)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Dict
a = dict |> Dict.union (Dict.singleton k <| v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this call by Dict.insert with the same key and value given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> (Dict.insert k <| v)
"""
                        ]
        , test "should replace dict |> (Dict.union <| Dict.singleton k <| v) by dict |> (Dict.insert k <| v)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Dict
a = dict |> (Dict.union <| Dict.singleton k <| v)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this call by Dict.insert with the same key and value given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> (Dict.insert k <| v)
"""
                        ]
        , test "should replace Dict.union << Dict.singleton k by Dict.insert k" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Dict
a = Dict.union << Dict.singleton k
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this composition by Dict.insert with the same argument given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.insert k
"""
                        ]
        , test "should replace Dict.union << (Dict.singleton <| k) by (Dict.insert <| k)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Dict
a = Dict.union << (Dict.singleton <| k)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this composition by Dict.insert with the same argument given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.insert <| k)
"""
                        ]
        , test "should replace Dict.union << (k |> Dict.singleton) by (k |> Dict.insert)" <|
            \() ->
                -- note that the parenthesis are mandatory
                """module A exposing (..)
import Dict
a = Dict.union << (k |> Dict.singleton)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union with a singleton dict can be combined into Dict.insert"
                            , details = [ "You can replace this composition by Dict.insert with the same argument given to Dict.singleton which is meant for this exact purpose." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (k |> Dict.insert)
"""
                        ]
        ]


dictFoldlTests : Test
dictFoldlTests =
    describe "Dict.foldl"
        [ test "should not report Dict.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.foldl
a1 = Dict.foldl (\\_ el soFar -> soFar - el)
a2 = Dict.foldl (\\_ el soFar -> soFar - el) 20
a3 = Dict.foldl (\\_ el soFar -> soFar - el) 20 dict
a4 = Dict.foldl (always identity) initial dict
a5 = Dict.foldl (\\_ -> identity) initial dict
a6 = Dict.foldl (\\_ v -> v) initial dict
a7 = Dict.foldl (always (\\v -> v)) initial dict
a8 = Dict.foldl (always (::)) [] dict
a9 = Dict.foldl (\\k _ ks -> k :: ks)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.foldl f initial Dict.empty by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl f initial Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl on Dict.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) initial dict by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity)) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (always (\\_ -> identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (\\_ -> identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ -> (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ -> (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ _ -> identity) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ _ -> identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ _ soFar -> soFar) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ _ soFar -> soFar)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl Dict.insert Dict.empty by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl Dict.insert Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace dict |> Dict.foldl (\\k -> k |> Dict.insert) Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldl (\\k -> k |> Dict.insert) Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace dict |> Dict.foldl (\\k v -> d |> Dict.insert k) Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldl (\\k v -> v |> Dict.insert k) Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace dict |> Dict.foldl (\\k v d -> d |> Dict.insert k v) Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldl (\\k v d -> d |> Dict.insert k v) Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        ]


dictFoldrTests : Test
dictFoldrTests =
    describe "Dict.foldr"
        [ test "should not report Dict.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.foldr
a1 = Dict.foldr (\\_ el soFar -> soFar - el)
a2 = Dict.foldr (\\_ el soFar -> soFar - el) 20
a3 = Dict.foldr (\\_ el soFar -> soFar - el) 20 dict
a4 = Dict.foldr (always identity) initial dict
a5 = Dict.foldr (\\_ -> identity) initial dict
a6 = Dict.foldr (\\_ v -> v) initial dict
a6 = Dict.foldr (always (\\v -> v)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.foldr f initial Dict.empty by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr f initial Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr on Dict.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) initial dict by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity)) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (always (\\_ -> identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (\\_ -> identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ -> (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ -> (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ _ -> identity) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ _ -> identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ _ soFar -> soFar) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ _ soFar -> soFar)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr Dict.insert Dict.empty by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr Dict.insert Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\k v d -> d |> Dict.insert k v) Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\k v d -> d |> Dict.insert k v) Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr Dict.insert Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.foldr (always (::)) [] by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (::)) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\_ v vs -> v :: vs) [] is the same as Dict.values"
                            , details = [ "You can replace this call by Dict.values which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\_ v -> (::) v) [] by dict |> Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\_ v -> (::) v) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\_ v vs -> v :: vs) [] is the same as Dict.values"
                            , details = [ "You can replace this call by Dict.values which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.values
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\_ v vs -> v :: vs) [] by dict |> Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\_ v vs -> v :: vs) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\_ v vs -> v :: vs) [] is the same as Dict.values"
                            , details = [ "You can replace this call by Dict.values which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.values
"""
                        ]
        , test "should replace Dict.foldr (always << (::)) [] by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always << (::)) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k _ ks -> k :: ks) [] is the same as Dict.keys"
                            , details = [ "You can replace this call by Dict.keys which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.foldr (\\k -> always ((::) k)) [] by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\k -> always ((::) k)) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k _ ks -> k :: ks) [] is the same as Dict.keys"
                            , details = [ "You can replace this call by Dict.keys which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\k _ ks -> k :: ks) [] by dict |> Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\k _ ks -> k :: ks) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k _ ks -> k :: ks) [] is the same as Dict.keys"
                            , details = [ "You can replace this call by Dict.keys which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.keys
"""
                        ]
        , test "should replace Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) [] by Dict.toList" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) [] is the same as Dict.toList"
                            , details = [ "You can replace this call by Dict.toList which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.toList
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\k v -> (::) <| ( k, v )) [] by dict |> Dict.toList" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\k v -> (::) <| ( k, v )) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) [] is the same as Dict.toList"
                            , details = [ "You can replace this call by Dict.toList which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.toList
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\k -> (::) << Tuple.pair k) [] by dict |> Dict.toList" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\k -> (::) << Tuple.pair k) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) [] is the same as Dict.toList"
                            , details = [ "You can replace this call by Dict.toList which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.toList
"""
                        ]
        , test "should replace dict |> Dict.foldr (\\k -> Tuple.pair k >> (::)) [] by dict |> Dict.toList" <|
            \() ->
                """module A exposing (..)
import Dict
a = dict |> Dict.foldr (\\k -> Tuple.pair k >> (::)) []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr (\\k v kvs -> ( k, v ) :: kvs) [] is the same as Dict.toList"
                            , details = [ "You can replace this call by Dict.toList which is meant for this exact purpose." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.toList
"""
                        ]
        ]
