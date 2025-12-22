module Simplify.ListTest exposing (all)

import Review.Test
import Simplify
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "List"
        [ usingConsTests
        , listAppendTests
        , listConcatTests
        , listConcatMapTests
        , listHeadTests
        , listTailTests
        , listMemberTests
        , listMapTests
        , listFilterTests
        , listFilterMapTests
        , listIndexedMapTests
        , listIsEmptyTests
        , listSumTests
        , listProductTests
        , listMinimumTests
        , listMaximumTests
        , listFoldlTests
        , listFoldrTests
        , listAllTests
        , listAnyTests
        , listRangeTests
        , listLengthTests
        , listRepeatTests
        , listPartitionTests
        , listSortTests
        , listSortByTests
        , listSortWithTests
        , listReverseTests
        , listTakeTests
        , listDropTests
        , listIntersperseTests
        , listMap2Tests
        , listMap3Tests
        , listMap4Tests
        , listMap5Tests
        , listUnzipTests
        ]


usingConsTests : Test
usingConsTests =
    describe "(::)"
        [ test "should not report using :: to a variable or expression" <|
            \() ->
                """module A exposing (..)
a = 1 :: list
b = 1 :: foo bar
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report using :: to a list literal" <|
            \() ->
                """module A exposing (..)
a = 1::[ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1, 2, 3 ]
"""
                        ]
        , test
            "should report using :: to a list literal, between is additional white space"
          <|
            \() ->
                """module A exposing (..)
a =
  1


    ::     [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1


    ,      2, 3 ]
"""
                        ]
        , test "should report using :: to a list literal, between is additional white space and different comments" <|
            \() ->
                """module A exposing (..)
a =
  1
    {- -- comment {- nested -} here we go! -}
    -- important
    ::     [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1
    {- -- comment {- nested -} here we go! -}
    -- important
    ,      2, 3 ]
"""
                        ]
        , test "should report using :: to a list literal, between is additional white space and different comments that use the operator symbol" <|
            \() ->
                """module A exposing (..)
a =
  1
    {- -- comment {-
     nested :: -} here we :: go!
         -}
    -- important: ::
    ::
    -- why is there a rabbit in here ::)
           [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 8, column = 5 }
                                , end = { row = 8, column = 7 }
                                }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1
    {- -- comment {-
     nested :: -} here we :: go!
         -}
    -- important: ::
    ,
    -- why is there a rabbit in here ::)
            2, 3 ]
"""
                        ]
        , test "should report using :: to [] literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 ]
"""
                        ]
        , test "should replace a :: (List.singleton <| b + c) by [ a, b + c ]" <|
            \() ->
                """module A exposing (..)
a = a
    :: (List.singleton <| b + c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "You can replace this operation by a list that contains both the added element and the value inside the singleton list." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, b + c ]
"""
                        ]
        ]


listAppendTests : Test
listAppendTests =
    describe "List.append"
        [ test "should not report List.append with a list variable" <|
            \() ->
                """module A exposing (..)
a = List.append
b = List.append list
c = List.append [ 1 ]
d = List.append [ 1 ] list
e = List.append list [ 1 ]
f = List.append list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.append applied on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append applied on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = List.append [ b, z ] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should report List.append <| on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] <| [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [b]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [ b, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should replace List.append [] list by list" <|
            \() ->
                """module A exposing (..)
a = List.append [] list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append [] <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.append [] <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace list |> List.append [] by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append [] by identity" <|
            \() ->
                """module A exposing (..)
a = List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.append list [] by list" <|
            \() ->
                """module A exposing (..)
a = List.append list []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append list <| [] by list" <|
            \() ->
                """module A exposing (..)
a = List.append list <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace [] |> List.append list by list" <|
            \() ->
                """module A exposing (..)
a = [] |> List.append list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        ]


listConcatTests : Test
listConcatTests =
    describe "List.concat"
        [ test "should not report List.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.concat [ foo, bar ]
b = List.concat [ [ 1 ], foo ]
c = List.concat [ foo, [ 1 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concat with no items" <|
            \() ->
                """module A exposing (..)
a = List.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should report List.concat with a single item" <|
            \() ->
                """module A exposing (..)
a = List.concat [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace List.concat << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.concat << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should report List.concat that only contains list literals" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6 ]
"""
                        ]
        , test "should report List.concat that only contains list literals, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6  ]
"""
                        ]
        , test "should report List.concat that only contains list literals, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6  ]
"""
                        ]
        , test "should concatenate consecutive list literals in passed to List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ], [ 4, 5, 6], [7], c, [8], [9 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Consecutive list literals can be merged"
                            , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ,  4, 5, 6, 7], c, [8, 9 ] ]
"""
                        ]
        , test "should remove empty list literals passed to List.concat (last item)" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a list containing an irrelevant []"
                            , details = [ "Including [] in the list does not change the result of this call. You can remove the [] element." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a ]
"""
                        ]
        , test "should remove empty list literals passed to List.concat (first item)" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [], b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a list containing an irrelevant []"
                            , details = [ "Including [] in the list does not change the result of this call. You can remove the [] element." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ b ]
"""
                        ]
        , test "should replace List.concat [ list0, List.concat [ list1, list2 ], list3, List.concat [ list4, list5 ] ] by List.concat [ list0, list1, list2, list3, list4, list5 ]" <|
            \() ->
                """module A exposing (..)
a = List.concat [ list0, List.concat [ list1, list2 ], list3, List.concat [ list4, list5 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Nested List.concat calls can be spread"
                            , details = [ "You can move the elements from the inner List.concat calls to inside this outer List.concat call." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ list0,  list1, list2 , list3,  list4, list5  ]
"""
                        ]
        , test "should replace List.concat (List.map f x) by List.concatMap f x" <|
            \() ->
                """module A exposing (..)
a = List.concat (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.concatMap f x)
"""
                        ]
        , test "should replace List.concat <| List.map f <| x by List.concatMap f <| x" <|
            \() ->
                """module A exposing (..)
a = List.concat <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f <| x
"""
                        ]
        , test "should replace x |> List.map f |> List.concat by x |> List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> List.concatMap f
"""
                        ]
        ]


listConcatMapTests : Test
listConcatMapTests =
    describe "List.concatMap"
        [ test "should replace List.concatMap identity x by List.concat x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should replace List.concatMap identity by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat
"""
                        ]
        , test "should replace List.concatMap (\\x->x) by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->x) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should not report List.concatMap with a non-identity lambda" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.concatMap without an identity function by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concatMap with no items" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should not report List.concatMap f [ a, [], b ]" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ a, [], b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.concatMap (always []) x by []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always []) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that will always return [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) by always []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that will always return [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.concatMap List.singleton x by x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap List.singleton x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function equivalent to List.singleton will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.concatMap List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.concatMap List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function equivalent to List.singleton will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.concatMap (\\_ -> [a]) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> ([a])) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> List.singleton a) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> List.singleton a) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> if cond then [a] else [b]) x by List.map (\\_ -> if cond then a else b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> if cond then [a] else [b]) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> if cond then a else b) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> case y of A -> [a] ; B -> [b]) x by List.map (\\_ -> case y of A -> a ; B -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap
    (\\_ ->
        case y of
            A -> [a]
            B -> [b]
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map
    (\\_ ->
        case y of
            A -> a
            B -> b
    ) x
"""
                        ]
        , test "should replace List.concatMap f [ a ] by f a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f a
"""
                        ]
        , test "should replace List.concatMap f [ b c ] by f (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ b c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (b c)
"""
                        ]
        , test "should replace List.concatMap f <| [ a ] by f <| a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f <| [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f <| a
"""
                        ]
        , test "should replace List.concatMap f <| [ b c ] by f <| (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f <| [ b c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f <| (b c)
"""
                        ]
        , test "should replace [ c ] |> List.concatMap f by c |> f" <|
            \() ->
                """module A exposing (..)
a = [ c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c |> f
"""
                        ]
        , test "should replace [ b c ] |> List.concatMap f by (b c) |> f" <|
            \() ->
                """module A exposing (..)
a = [ b c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b c) |> f
"""
                        ]
        , test "should replace List.concatMap f << List.singleton by f" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this composition by the function given to List.concatMap." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace List.map f >> List.concat by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this composition by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        , test "should replace List.concat << List.map f by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.concat << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this composition by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        ]


listHeadTests : Test
listHeadTests =
    describe "List.head"
        [ test "should not report List.head used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.head
b = List.head list
c = List.head (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.head [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.head []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.head (List.singleton b) by Just b" <|
            \() ->
                """module A exposing (..)
a = List.head (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head <| List.singleton b by Just <| b" <|
            \() ->
                """module A exposing (..)
a = List.head <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| b
"""
                        ]
        , test "should replace List.singleton b |> List.head by b |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.head
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Just
"""
                        ]
        , test "should replace List.head [ b ] by Just b" <|
            \() ->
                """module A exposing (..)
a = List.head [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f b ] by Just (f b)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head [ b, c, d ] by Just b" <|
            \() ->
                """module A exposing (..)
a = List.head [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f b, c, d ] by Just (f b)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head (b :: bToZ) by Just b" <|
            \() ->
                """module A exposing (..)
a = List.head (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.sort list |> List.head by List.minimum list" <|
            \() ->
                """module A exposing (..)
a = List.sort list |> List.head
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort, then List.head can be combined into List.minimum"
                            , details = [ "You can replace this call by List.minimum with the same arguments given to List.sort which is meant for this exact purpose." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.minimum list
"""
                        ]
        , test "should replace List.sort list |> List.reverse |> List.head by List.maximum list" <|
            \() ->
                """module A exposing (..)
a = List.sort list |> List.reverse |> List.head
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort, then List.reverse, then List.head can be combined into List.maximum"
                            , details = [ "You can replace this call by List.maximum with the same list given to List.sort which is meant for this exact purpose." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.maximum
"""
                        ]
        , test "should replace List.head (List.reverse (List.sort list)) by List.maximum list" <|
            \() ->
                """module A exposing (..)
a = List.head (List.reverse (List.sort list))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort, then List.reverse, then List.head can be combined into List.maximum"
                            , details = [ "You can replace this call by List.maximum with the same list given to List.sort which is meant for this exact purpose." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.maximum list
"""
                        ]
        ]


listTailTests : Test
listTailTests =
    describe "List.tail"
        [ test "should not report List.tail used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.tail
b = List.tail list
c = List.tail (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.tail [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.tail []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.tail (List.singleton b) by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a singleton list will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail <| List.singleton b by Just <| []" <|
            \() ->
                """module A exposing (..)
a = List.tail <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a singleton list will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| []
"""
                        ]
        , test "should replace List.singleton b |> List.tail by [] |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.tail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a singleton list will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [] |> Just
"""
                        ]
        , test "should replace List.tail [ b ] by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a singleton list will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail [ b, c, d ] by Just [ c, d ]" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just [ c, d ]
"""
                        ]
        , test "should replace List.tail (b :: bToZ) by Just bToZ" <|
            \() ->
                """module A exposing (..)
a = List.tail (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just cToZ
"""
                        ]
        ]


listMemberTests : Test
listMemberTests =
    describe "List.member"
        [ test "should not report List.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.member
b = List.member g
c = List.member g list
d = List.member g (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.member used with okay arguments when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = List.member 0 (0 :: 1 :: unknown)
b = List.member g [ 0, 1 ]
c = List.member 0 list
d = List.member 0 [ 0, 1, 2, g ]
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace List.member a [] by False" <|
            \() ->
                """module A exposing (..)
a = List.member a []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on [] will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.member a (List.singleton a) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b (List.singleton a) by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b (List.singleton b) by b == b when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = List.member b (List.singleton b)
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b == b
"""
                        ]
        , test "should replace List.member a [ a ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member b [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b <| [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b [ f a ] by b == (f a)" <|
            \() ->
                """module A exposing (..)
a = List.member c [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == (f b)
"""
                        ]
        , test "should replace [ a ] |> List.member b by a == b" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.member c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b == c
"""
                        ]
        , test "should replace List.member c [ a, b, c ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member d [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member 0 [ 2, 0, 1 ] by True when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = List.member 0 [ 2, 0, 1 ]
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member c ([ a, b, c] ++ dToZ ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member d ([ b, c, d ] ++ eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d [ a, b, c ]" <|
            \() ->
                """module A exposing (..)
a = List.member e [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member a (a :: bToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member c (a :: b :: c :: dToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member d (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d (a :: b :: c :: dToZ)" <|
            \() ->
                """module A exposing (..)
a = List.member e (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member 0 [ 2, 3, 1 ] by False" <|
            \() ->
                """module A exposing (..)
a = List.member 0 [ 2, 3, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which does not contain the given element will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.member 0 [ 2, 3, 1 ] by False when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = List.member 0 [ 2, 3, 1 ]
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which does not contain the given element will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listMapTests : Test
listMapTests =
    describe "List.map"
        [ test "should not report List.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
import Dict
a0 = List.map
a1 = List.map f
a2 = List.map f x
a3 = List.map f (Dict.fromList dict)
a4 = List.map (\\( a, b ) -> a + b) (Dict.fromList dict)
a5 = List.map (f >> Tuple.first) (Dict.fromList dict)
a6 = List.map f << Dict.fromList
a7 = List.map (\\( a, b ) -> a + b) << Dict.fromList
a8 = List.map (f >> Tuple.first) << Dict.fromList
a9 = List.map f (Array.toIndexedList array)
a10 = List.map (\\( a, b ) -> a) (Array.toIndexedList array)
a11 = List.map Tuple.first (Array.toIndexedList array)
a12 = List.map (f >> Tuple.second) (Array.toIndexedList array)
a13 = List.map Tuple.first << Array.toIndexedList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.map f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> List.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> List.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map f [ x ] by [ (f x) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f x) ]
"""
                        ]
        , test "should replace List.map f [ g x ] by [ (f (g x)) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f [ g x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f (g x)) ]
"""
                        ]
        , test "should replace List.map f (if cond then [ x ] else [ y ]) by [ f (if cond then x else y) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f (if cond then [ x ] else [ y ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f (if cond then x else y) ]
"""
                        ]
        , test "should replace List.map f <| if cond then [ x ] else [ y ] by [ f <| if cond then x else y ]" <|
            \() ->
                """module A exposing (..)
a = List.map f <| if cond then [ x ] else [ y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f <| if cond then x else y ]
"""
                        ]
        , test "should replace (if cond then [ x ] else [ y ]) |> List.map f by [ (if cond then x else y) |> f ]" <|
            \() ->
                """module A exposing (..)
a = (if cond then [ x ] else [ y ]) |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (if cond then x else y) |> f ]
"""
                        ]
        , test "should replace List.map f (if cond then List.singleton x else List.singleton y) by [ f (if cond then x else y) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f (if cond then List.singleton x else List.singleton y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f (if cond then x else y) ]
"""
                        ]
        , test "should replace List.map f (List.singleton x) by (List.singleton (f x))" <|
            \() ->
                """module A exposing (..)
a = List.map f (List.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton (f x))
"""
                        ]
        , test "should replace List.map f <| [ x ] by [ (f <| x) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f <| [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f <| x) ]
"""
                        ]
        , test "should replace [ x ] |> List.map f by [ (x |> f) ]" <|
            \() ->
                """module A exposing (..)
a = [ x ] |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (x |> f) ]
"""
                        ]
        , test "should replace List.map f << List.singleton by List.singleton << f" <|
            \() ->
                """module A exposing (..)
a = List.map f << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in List.singleton with the function applied to the value inside"
                            , details = [ "You can replace this call by List.singleton with the function directly applied to the value inside the singleton list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton << f
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this composition by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( part0, _ ) -> part0) by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( part0, _ ) -> part0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this composition by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace List.map Tuple.first << Dict.toList Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this composition by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.second can be combined into Dict.values"
                            , details = [ "You can replace this composition by Dict.values with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( _, part1 ) -> part1) by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.second can be combined into Dict.values"
                            , details = [ "You can replace this composition by Dict.values with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.second << Dict.toList Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.second << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.second can be combined into Dict.values"
                            , details = [ "You can replace this composition by Dict.values with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList dict) by (Dict.keys dict)" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.keys dict)
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList <| dict) by (Dict.keys <| dict)" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.keys <| dict)
"""
                        ]
        , test "should replace List.map Tuple.first (dict |> Dict.toList) by (dict |> Dict.keys)" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (dict |> Dict.keys)
"""
                        ]
        , test "should replace List.map Tuple.first <| Dict.toList dict by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| Dict.toList dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first <| (Dict.toList <| dict) by (Dict.keys <| dict)" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.keys <| dict)
"""
                        ]
        , test "should replace List.map Tuple.first <| (dict |> Dict.toList) by (dict |> Dict.keys)" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (dict |> Dict.keys)
"""
                        ]
        , test "should replace Dict.toList dict |> List.map Tuple.first by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList dict |> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map with Tuple.first can be combined into Dict.keys"
                            , details = [ "You can replace this call by Dict.keys with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace array |> Array.toIndexedList |> List.map Tuple.second by array |> Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.toIndexedList |> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map with Tuple.second can be combined into Array.toList"
                            , details = [ "You can replace this call by Array.toList with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array |> Array.toList
"""
                        ]
        , test "should replace array |> Array.toIndexedList |> List.map (\\( _, part1 ) -> part1) by array |> Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.toIndexedList |> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map with Tuple.second can be combined into Array.toList"
                            , details = [ "You can replace this call by Array.toList with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array |> Array.toList
"""
                        ]
        , test "should replace Array.toIndexedList >> List.map Tuple.second by Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList >> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map with Tuple.second can be combined into Array.toList"
                            , details = [ "You can replace this composition by Array.toList with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.toList
"""
                        ]
        , test "should replace Array.toIndexedList >> List.map (\\( _, part1 ) -> part1) by Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList >> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map with Tuple.second can be combined into Array.toList"
                            , details = [ "You can replace this composition by Array.toList with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.toList
"""
                        ]
        ]


listFilterTests : Test
listFilterTests =
    describe "List.filter"
        [ test "should not report List.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filter f x
b = List.filter f (List.filter g x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filter f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filter f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter f (List.filter f list) by List.filter f list" <|
            \() ->
                """module A exposing (..)
a = List.filter f (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.filter after equivalent List.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filter f list)
"""
                        ]
        , test "should replace List.filter f >> List.filter f by List.filter f" <|
            \() ->
                """module A exposing (..)
a = List.filter f >> List.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.filter after equivalent List.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 22 }, end = { row = 2, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filter f
"""
                        ]
        , test "should replace List.filter f << List.filter f by List.filter f" <|
            \() ->
                """module A exposing (..)
a = List.filter f << List.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.filter after equivalent List.filter"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filter f
"""
                        ]
        , test "should replace List.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (\\_ -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\_ -> True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> List.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter (always False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (\\_ -> False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\_ -> False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filter (always False) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filter <| (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always False |> List.filter by always []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listFilterMapTests : Test
listFilterMapTests =
    describe "List.filterMap"
        [ test "should not report List.filterMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filterMap f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filterMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filterMap (always Nothing) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap <| always Nothing by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap <| always Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always Nothing |> List.filterMap by always []" <|
            \() ->
                """module A exposing (..)
a = always Nothing |> List.filterMap
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap Just x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.filterMap Just by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filterMap (\\a -> Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just b) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b) x
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just b) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b)
"""
                        ]
        , test "should replace List.map (\\a -> Just b) x by List.filterMap (\\a -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b) x
"""
                        ]
        , test "should replace List.filterMap identity (List.map f x) by List.filterMap f x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.filterMap with an identity function can be combined into List.filterMap"
                            , details = [ "You can replace this call by List.filterMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filterMap f x)
"""
                        ]
        , test "should replace List.filterMap identity <| List.map f <| x by List.filterMap f <| x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.filterMap with an identity function can be combined into List.filterMap"
                            , details = [ "You can replace this call by List.filterMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f <| x
"""
                        ]
        , test "should replace x |> List.map f |> List.filterMap identity by x |> List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.filterMap with an identity function can be combined into List.filterMap"
                            , details = [ "You can replace this call by List.filterMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> List.filterMap f
"""
                        ]
        , test "should replace List.map f >> List.filterMap identity by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.filterMap with an identity function can be combined into List.filterMap"
                            , details = [ "You can replace this composition by List.filterMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should not report List.filterMap f [ a, Nothing, b ] with f not being an identity function" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f [ a, Nothing, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap identity [ a, Nothing, b ] by List.filterMap identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity [ a, Nothing, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with an identity function on a list containing an irrelevant Nothing"
                            , details = [ "Including Nothing in the list does not change the result of this call. You can remove the Nothing element." ]
                            , under = "Nothing"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap identity [ a, b ]
"""
                        ]
        , test "should replace List.filterMap identity << List.map f by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.filterMap with an identity function can be combined into List.filterMap"
                            , details = [ "You can replace this composition by List.filterMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should replace List.filterMap identity [ Just x, Just y ] by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity [ Just x, Just y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                        ]
        , test "should replace [ Just x, Just y ] |> List.filterMap identity by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = [ Just x, Just y ] |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
"""
                        ]
        ]


listIndexedMapTests : Test
listIndexedMapTests =
    describe "List.indexedMap"
        [ test "should not report List.indexedMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f list
b = List.indexedMap (\\i value -> i + value) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.indexedMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.indexedMap (\\_ y -> y) list by List.map (\\y -> y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ y -> y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\y -> y) list
"""
                        ]
        , test "should replace List.indexedMap (\\(_) (y) -> y) list by List.map (\\(y) -> y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\(_) (y) -> y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\(y) -> y) list
"""
                        ]
        , test "should replace List.indexedMap (\\_ -> f) list by List.map f list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ -> f) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map f list
"""
                        ]
        , test "should replace List.indexedMap (always f) list by List.map f list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always f) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map f list
"""
                        ]
        , test "should replace List.indexedMap (always <| f y) list by List.map (f y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always <| f y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) list
"""
                        ]
        , test "should replace List.indexedMap (f y |> always) list by List.map (f y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (f y |> always) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) list
"""
                        ]
        ]


listIsEmptyTests : Test
listIsEmptyTests =
    describe "List.isEmpty"
        [ test "should not report List.isEmpty with a non-literal argument" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.isEmpty [] by True" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on [] will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.isEmpty [x] by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty [x]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (x :: xs) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (x :: xs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> List.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = x :: xs |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (List.singleton x) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (List.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (a |> List.singleton) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (b |> List.singleton)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace Set.toList set |> List.isEmpty by Set.isEmpty" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.toList set |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then List.isEmpty can be combined into Set.isEmpty"
                            , details = [ "You can replace this call by Set.isEmpty with the same arguments given to Set.toList which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.isEmpty set
"""
                        ]
        , test "should replace Dict.toList dict |> List.isEmpty by Dict.isEmpty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList dict |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.isEmpty can be combined into Dict.isEmpty"
                            , details = [ "You can replace this call by Dict.isEmpty with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty dict
"""
                        ]
        , test "should replace Dict.values dict |> List.isEmpty by Dict.isEmpty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.values dict |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.values, then List.isEmpty can be combined into Dict.isEmpty"
                            , details = [ "You can replace this call by Dict.isEmpty with the same arguments given to Dict.values which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty dict
"""
                        ]
        , test "should replace Dict.keys dict |> List.isEmpty by Dict.isEmpty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.keys dict |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.keys, then List.isEmpty can be combined into Dict.isEmpty"
                            , details = [ "You can replace this call by Dict.isEmpty with the same arguments given to Dict.keys which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty dict
"""
                        ]
        , test "should replace Array.toList array |> List.isEmpty by Array.isEmpty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList array |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then List.isEmpty can be combined into Array.isEmpty"
                            , details = [ "You can replace this call by Array.isEmpty with the same arguments given to Array.toList which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.isEmpty array
"""
                        ]
        , test "should replace Array.toIndexedList array |> List.isEmpty by Array.isEmpty" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList array |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.isEmpty can be combined into Array.isEmpty"
                            , details = [ "You can replace this call by Array.isEmpty with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.isEmpty array
"""
                        ]
        ]


listSumTests : Test
listSumTests =
    describe "List.sum"
        [ test "should not report List.sum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sum
b = List.sum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sum (a :: bToZ)
b = List.sum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sum [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.sum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on [] will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.sum [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.sum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should replace List.sum << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.sum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sum [ a, 0, b ] by List.sum [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b, c ]
"""
                        ]
        , test "should replace List.sum [ a, 0.0, b ] by List.sum [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0.0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b, c ]
"""
                        ]
        , test "should replace List.sum [ a, 0 ] by List.sum [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b ]
"""
                        ]
        , test "should replace List.sum [ 0, a ] by List.sum [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ 0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b ]
"""
                        ]
        , test "should replace [ a, 0 / 0.0, b ] |> List.sum by (0 / 0.0) when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = [ a, 0 / 0.0, b ] |> List.sum
"""
                    |> Review.Test.run (Simplify.rule (Simplify.defaults |> Simplify.expectNaN))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list with NaN will result in NaN"
                            , details = [ "You can replace this call by (0 / 0)." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (0 / 0.0)
"""
                        ]
        ]


listProductTests : Test
listProductTests =
    describe "List.product"
        [ test "should not report List.product on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.product
b = List.product list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.product on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.product (a :: bToZ)
b = List.product [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.product [] by 1" <|
            \() ->
                """module A exposing (..)
a = List.product []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on [] will result in 1"
                            , details = [ "You can replace this call by 1." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should replace List.product [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.product [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should replace List.product << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.product << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.product [ a, 1, b ] by List.product [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b, c ]
"""
                        ]
        , test "should replace List.product [ a, 1.0, b ] by List.product [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1.0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b, c ]
"""
                        ]
        , test "should replace List.product [ a, 1 ] by List.product [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b ]
"""
                        ]
        , test "should replace List.product [ 1, a ] by List.product [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ 1, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b ]
"""
                        ]
        , test "should replace List.product [ a, 0, b ] by 0" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.product (a :: 0 :: bs) by 0" <|
            \() ->
                """module A exposing (..)
a = List.product (a :: 0 :: bs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.product [ a, 0.0, b ] by 0.0" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0.0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0.0
"""
                        ]
        , test "should not report List.product [ a, 0.0, 0, b ] when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0.0, 0, b ]
"""
                    |> Review.Test.run (Simplify.rule (Simplify.defaults |> Simplify.expectNaN))
                    |> Review.Test.expectNoErrors
        , test "should replace [ a, 0 / 0.0, b ] |> List.product by (0 / 0.0) when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = [ a, 0 / 0.0, b ] |> List.product
"""
                    |> Review.Test.run (Simplify.rule (Simplify.defaults |> Simplify.expectNaN))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with NaN will result in NaN"
                            , details = [ "You can replace this call by (0 / 0)." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (0 / 0.0)
"""
                        ]
        ]


listMinimumTests : Test
listMinimumTests =
    describe "List.minimum"
        [ test "should not report List.minimum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.minimum
b = List.minimum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.minimum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.minimum (a :: bToZ)
b = List.minimum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.minimum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.minimum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.minimum [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.minimum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        , test "should replace List.minimum [ f a ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.minimum [ f a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f a)
"""
                        ]
        , test "should replace List.minimum (if c then [ a ] else [ b ]) by Just (if c then a else b)" <|
            \() ->
                """module A exposing (..)
a = List.minimum (if c then [ a ] else [ b ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (if c then a else b)
"""
                        ]
        , test "should replace List.minimum << List.singleton by Just" <|
            \() ->
                """module A exposing (..)
a = List.minimum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will always result in Just the value inside"
                            , details = [ "You can replace this call by Just." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        ]


listMaximumTests : Test
listMaximumTests =
    describe "List.maximum"
        [ test "should not report List.maximum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.maximum
b = List.maximum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.maximum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.maximum (a :: bToZ)
b = List.maximum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.maximum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.maximum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.maximum [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.maximum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        , test "should replace List.maximum << List.singleton by Just" <|
            \() ->
                """module A exposing (..)
a = List.maximum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on a singleton list will always result in Just the value inside"
                            , details = [ "You can replace this call by Just." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        ]


listFoldlTests : Test
listFoldlTests =
    describe "List.foldl"
        [ test "should not report List.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldl f x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl on [] will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which list is supplied next." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        , test "should replace List.foldl f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x set
"""
                        ]
        , test "should replace List.foldl f x << Set.toList by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should replace Set.toList >> List.foldl f x by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldl f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should replace List.foldl f x (Array.toList array) by Array.foldl f x array" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x (Array.toList array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldl f x array
"""
                        ]
        , test "should replace List.foldl f x << Array.toList by Array.foldl f x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldl f x
"""
                        ]
        , test "should replace Array.toList >> List.foldl f x by Array.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Array.toList >> List.foldl f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldl f x
"""
                        ]
        , listFoldlSumTests
        , listFoldlProductTests
        , listFoldlAllTests
        , listFoldlAnyTests
        ]


listFoldlAnyTests : Test
listFoldlAnyTests =
    describe "any"
        [ test "should replace List.foldl (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldl (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldl (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldl (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldlAllTests : Test
listFoldlAllTests =
    describe "all"
        [ test "should replace List.foldl (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldl (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldl (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldl (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldlSumTests : Test
listFoldlSumTests =
    describe "sum"
        [ test "should replace List.foldl (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldl (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldl (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldl (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldl (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldl (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldlProductTests : Test
listFoldlProductTests =
    describe "product"
        [ test "should replace List.foldl (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldl (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldl (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldl (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldl (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldl (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listFoldrTests : Test
listFoldrTests =
    describe "List.foldr"
        [ test "should not report List.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldr fn x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr fn x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr on [] will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which list is supplied next." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        , test "should replace List.foldr (always identity) initial data extraData by initial extraArgument" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) initial data extraArgument
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial extraArgument
"""
                        ]
        , test "should replace List.foldr f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x set
"""
                        ]
        , test "should replace List.foldr f x << Set.toList by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , test "should replace Set.toList >> List.foldr f x by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldr f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a list"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , test "should replace List.foldr f x (Array.toList array) by Array.foldr f x array" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x (Array.toList array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldr f x array
"""
                        ]
        , test "should replace List.foldr f x << Array.toList by Array.foldr f x" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldr f x
"""
                        ]
        , test "should replace Array.toList >> List.foldr f x by Array.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Array.toList >> List.foldr f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold an array, you don't need to convert to a list"
                            , details = [ "Using Array.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Array.foldr f x
"""
                        ]
        , listFoldrSumTests
        , listFoldrProductTests
        , listFoldrAllTests
        , listFoldrAnyTests
        ]


listFoldrAnyTests : Test
listFoldrAnyTests =
    describe "any"
        [ test "should replace List.foldr (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldr (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldr (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldr (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldrAllTests : Test
listFoldrAllTests =
    describe "all"
        [ test "should replace List.foldr (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldr (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldr (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldr (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldrSumTests : Test
listFoldrSumTests =
    describe "sum"
        [ test "should replace List.foldr (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldr (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldr (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldr (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldr (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldr (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldrProductTests : Test
listFoldrProductTests =
    describe "product"
        [ test "should replace List.foldr (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldr (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldr (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldr (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldr (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldr (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listAllTests : Test
listAllTests =
    describe "List.all"
        [ test "should not report List.all used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.all f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.all f [] by True" <|
            \() ->
                """module A exposing (..)
a = List.all f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all on [] will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) list by True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) by always True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should not report List.all on list with False but non-identity and non-not function" <|
            \() ->
                """module A exposing (..)
a = List.all f [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.all identity [ a, True, b ] by List.all identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.all identity [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list containing an irrelevant True"
                            , details = [ "Including True in the list does not change the result of this call. You can remove the True element." ]
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity [ b, c ]
"""
                        ]
        , test "should replace List.all identity [ a, False, b ] by False" <|
            \() ->
                """module A exposing (..)
a = List.all identity [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list with False will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.all identity (a :: False :: bs) by False" <|
            \() ->
                """module A exposing (..)
a = List.all identity (b :: False :: cs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list with False will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.all not [ a, False, b ] by List.all not [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.all not [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with `not` on a list containing an irrelevant False"
                            , details = [ "Including False in the list does not change the result of this call. You can remove the False element." ]
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all not [ b, c ]
"""
                        ]
        , test "should replace List.all not [ a, True, b ] by False" <|
            \() ->
                """module A exposing (..)
a = List.all not [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with `not` on a list with True will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listAnyTests : Test
listAnyTests =
    describe "List.any"
        [ test "should not report List.any used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.any f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.any f [] by False" <|
            \() ->
                """module A exposing (..)
a = List.any f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any on [] will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) list by False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) by always False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.any ((==) x) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any ((==) x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should replace List.any (\\y -> y == x) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any (\\y -> y == x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should replace List.any (\\y -> x == y) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any (\\y -> x == y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should not replace List.any (\\z -> x == y)" <|
            \() ->
                """module A exposing (..)
a = List.any (\\z -> x == y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace List.any (\\x -> x == (f x))" <|
            \() ->
                """module A exposing (..)
a = List.any (\\x -> x == (f x))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.any on list with True but non-identity and non-not function" <|
            \() ->
                """module A exposing (..)
a = List.any f [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.any identity [ a, False, b ] and by List.any identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.any identity [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with an identity function on a list containing an irrelevant False"
                            , details = [ "Including False in the list does not change the result of this call. You can remove the False element." ]
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity [ b, c ]
"""
                        ]
        , test "should replace List.any identity [ a, True, b ] by True" <|
            \() ->
                """module A exposing (..)
a = List.any identity [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with an identity function on a list with True will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.any not [ a, True, b ] by List.any not [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.any not [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with `not` on a list containing an irrelevant True"
                            , details = [ "Including True in the list does not change the result of this call. You can remove the True element." ]
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any not [ b, c ]
"""
                        ]
        , test "should replace List.any not [ a, False, b ] by True" <|
            \() ->
                """module A exposing (..)
a = List.any not [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with `not` on a list with False will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]


listRangeTests : Test
listRangeTests =
    describe "List.range"
        [ test "should not report List.range used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.range
a = List.range 5
a = List.range 5 10
a = List.range 5 0xF
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.range 10 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 10 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.range 0xF 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 0xF 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace 5 |> List.range 10 by []" <|
            \() ->
                """module A exposing (..)
a = 5 |> List.range 10
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ test "should not report List.length used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.length
a = List.length b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.length [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.length []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.length [b, c, d] by 3" <|
            \() ->
                """module A exposing (..)
a = List.length [b, c, d]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 3"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace [] |> List.length by 0" <|
            \() ->
                """module A exposing (..)
a = [] |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.length (a :: List.repeat 3 b) by 4" <|
            \() ->
                """module A exposing (..)
a = List.length (b :: List.repeat 3 c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 4"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 4
"""
                        ]
        , test "should replace List.length (List.range 3 7) by 4" <|
            \() ->
                """module A exposing (..)
a = List.length (List.range 3 7)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 4"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 4
"""
                        ]
        , test "should replace Set.toList set |> List.length with Set.size" <|
            \() ->
                """module A exposing (..)
import Set
a = Set.toList set |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.toList, then List.length can be combined into Set.size"
                            , details = [ "You can replace this call by Set.size with the same arguments given to Set.toList which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.size set
"""
                        ]
        , test "should replace Dict.toList dict |> List.length with Dict.size" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList dict |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.length can be combined into Dict.size"
                            , details = [ "You can replace this call by Dict.size with the same arguments given to Dict.toList which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size dict
"""
                        ]
        , test "should replace Dict.values dict |> List.length with Dict.size" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.values dict |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.values, then List.length can be combined into Dict.size"
                            , details = [ "You can replace this call by Dict.size with the same arguments given to Dict.values which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size dict
"""
                        ]
        , test "should replace Dict.keys dict |> List.length with Dict.size" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.keys dict |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.keys, then List.length can be combined into Dict.size"
                            , details = [ "You can replace this call by Dict.size with the same arguments given to Dict.keys which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size dict
"""
                        ]
        , test "should replace Array.toList array |> List.length with Dict.size" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toList array |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toList, then List.length can be combined into Array.length"
                            , details = [ "You can replace this call by Array.length with the same arguments given to Array.toList which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length array
"""
                        ]
        , test "should replace Array.toIndexedList array |> List.length with Array.length" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList array |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.length can be combined into Array.length"
                            , details = [ "You can replace this call by Array.length with the same arguments given to Array.toIndexedList which is meant for this exact purpose." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.length array
"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty" <|
            \() ->
                """module A exposing (..)

a = x == []"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "== []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = List.isEmpty x"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (reverse order)" <|
            \() ->
                """module A exposing (..)

a = [] == x"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "[] =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = List.isEmpty x"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (negated)" <|
            \() ->
                """module A exposing (..)

a = x /= []"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "/= []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (List.isEmpty x)"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (negated, reverse order)" <|
            \() ->
                """module A exposing (..)

a = [] /= x"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "[] /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (List.isEmpty x)"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (needs parentheses)" <|
            \() ->
                """module A exposing (..)

a = x ++ y == []"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "== []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = List.isEmpty (x ++ y)"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (needs parentheses, reverse order)" <|
            \() ->
                """module A exposing (..)

a = [] == x ++ y"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "[] =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = List.isEmpty (x ++ y)"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (needs parentheses, negated)" <|
            \() ->
                """module A exposing (..)

a = x ++ y /= []"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "/= []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (List.isEmpty (x ++ y))"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (needs parentheses, negated, reverse order)" <|
            \() ->
                """module A exposing (..)

a = [] /= x ++ y"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "[] /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = not (List.isEmpty (x ++ y))"""
                        ]
        , test "should replace comparisons to the empty list with List.isEmpty (infix)" <|
            \() ->
                """module A exposing (..)

a = x |> (==) []"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Comparison with an empty list can be replaced by a call to List.isEmpty"
                            , details = [ "You can replace this comparison to an empty list with a call to List.isEmpty, which is more efficient." ]
                            , under = "(==) []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)

a = x |> List.isEmpty"""
                        ]
        ]


listRepeatTests : Test
listRepeatTests =
    describe "List.repeat"
        [ test "should not report List.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.repeat n list
b = List.repeat 5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace List.repeat n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.repeat 0 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 0 will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.repeat 0 by always []" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 0 will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.repeat -5 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat -5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with negative length will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.repeat 1 by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace 1 |> List.repeat by List.singleton" <|
            \() ->
                """module A exposing (..)
a = 1 |> List.repeat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.repeat 1 element by List.singleton element" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 element
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton element
"""
                        ]
        , test "should replace List.repeat 1 <| element by List.singleton <| element" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 <| element
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton <| element
"""
                        ]
        , test "should replace element |> List.repeat 1 by element |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = element |> List.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = element |> List.singleton
"""
                        ]
        ]


listSortTests : Test
listSortTests =
    describe "List.sort"
        [ test "should not report List.sort on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sort
b = List.sort list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sort on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sort (a :: bToZ)
b = List.sort [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sort [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sort []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sort [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sort [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a ]
"""
                        ]
        , test "should replace List.sort (List.sort list) by (List.sort list)" <|
            \() ->
                """module A exposing (..)
a = List.sort (List.sort list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort after List.sort"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.sort list)
"""
                        ]
        , test "should replace List.sort << List.sort by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sort << List.sort
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort after List.sort"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sort (List.repeat n a) by List.repeat n a" <|
            \() ->
                """module A exposing (..)
a = List.sort (List.repeat n b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort on a List.repeat call"
                            , details = [ "You can replace this call by the given List.repeat call." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.repeat n b)
"""
                        ]
        , test "should replace List.sort << List.repeat n by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.sort << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        , test "should replace List.repeat n >> List.sort by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.repeat n >> List.sort
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        ]


listSortByTests : Test
listSortByTests =
    describe "List.sortBy"
        [ test "should not report List.sortBy used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn
b = List.sortBy fn list
c = List.sortBy << List.sortBy fn
d = List.sortBy fn << List.sortWith gn
e = List.sortBy fn << List.sort
f = List.sortBy (\\(x,y) -> (y,x))
g = List.sortBy (\\({x,y} as r) -> {r|y=y})
h = List.sortBy (\\({x,y} as r) -> {r|x=y,y=x})
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function that reconstructs all destructured record fields, as record patterns do not have to be exhaustive, e.g. {x,y} can match {x=x,y=y,z=z} values" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\{x,y} -> {x=x,y=y})
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function that reconstructs the destructured variant with a curried call" <|
            \() ->
                """module A exposing (..)
type ChoiceTypeWithoutPhantomType used = Good Int used
a = List.sortBy (\\(Good b c) -> Good b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function that reconstructs a variant whose origin type has parameters not used in the variant values, as therefore the function could change phantom types of input and output" <|
            \() ->
                """module A exposing (..)
type ChoiceTypeWithPhantomType unused = Bad Int Int
a = List.sortBy (\\(Bad b c) -> Bad b c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn (a :: bToZ)
b = List.sortBy fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortBy fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortBy fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortBy fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortBy (always a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with a function that always returns the same constant will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (always a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with a function that always returns the same constant will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with a function that always returns the same constant will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with a function that always returns the same constant will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy identity by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\() -> ()) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\() -> ())
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(((a))) -> ((((a))))) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\(((b))) -> ((((b)))))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(a,b,c) -> (a,b,c)) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\(a,b,c) -> (a,b,c))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(a,(b,c)) -> (a,Tuple.pair b c)) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\(b,(c,d)) -> (b,Tuple.pair c d))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        , Review.Test.error
                            { message = "Fully constructed Tuple.pair can be replaced by tuple literal"
                            , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                            , under = "Tuple.pair"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy (\\(b,(c,d)) -> (b,( c, d )))
"""
                        ]
        , test "should replace List.sortBy (\\Variant -> Variant) by List.sort when origin choice type parameters are all used in variants" <|
            \() ->
                """module A exposing (..)
type ChoiceType = Variant
a = List.sortBy (\\Variant -> Variant)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type ChoiceType = Variant
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(Variant a b) -> Variant a b) by List.sort when origin choice type has no parameters" <|
            \() ->
                """module A exposing (..)
type ChoiceType = Variant Int Int
a = List.sortBy (\\(Variant a b) -> Variant a b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type ChoiceType = Variant Int Int
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(Variant a b) -> Variant a b) by List.sort when origin choice type parameters are all used in variants" <|
            \() ->
                """module A exposing (..)
type ChoiceType a b = Variant a (Maybe (ChoiceType a b))
a = List.sortBy (\\(Variant a b) -> Variant a b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type ChoiceType a b = Variant a (Maybe (ChoiceType a b))
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\(Imported.Variant a b) -> Imported.Variant a b) by List.sort when origin choice type parameters are all used in variants" <|
            \() ->
                [ """module A exposing (..)
import Imported
a = List.sortBy (\\(Imported.Variant a b) -> Imported.Variant a b)
"""
                , """module Imported exposing (..)
type ChoiceType a b = Variant a (Maybe (ChoiceType a b))
"""
                ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "List.sortBy with an identity function is the same as List.sort"
                                , details = [ "You can replace this call by List.sort." ]
                                , under = "List.sortBy"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import Imported
a = List.sort
"""
                            ]
                          )
                        ]
        , test "should replace List.sortBy (\\({a,b} as r) -> {r|a=a,b=b}) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\({x,y} as r) -> {r|x=x,y=y})
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\({b,a} as r) -> {r|a=a,b=b}) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\({y,x} as r) -> {r|x=x,y=y})
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\({a,b} as r) -> {r|b=b,a=a}) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\({x,y} as r) -> {r|y=y,x=x})
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\((b,((c))) as unused, (), (Variant, _ as d)) -> ((((b)),c), (), (Variant, d))) by List.sort" <|
            \() ->
                """module A exposing (..)
type ChoiceType = Variant
a = List.sortBy (\\((b,((c))) as unused, (), (Variant, _ as d)) -> ((((b)),c), (), (Variant, d)))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type ChoiceType = Variant
a = List.sort
"""
                        ]
        , test "should replace List.sortBy identity list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        , test "should replace List.sortBy f (List.sortBy f list) by (List.sortBy f list)" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f (List.sortBy f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy after equivalent List.sortBy"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.sortBy f list)
"""
                        ]
        , test "should replace List.sortBy f << List.sortBy f by List.sortBy f" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f << List.sortBy f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy after equivalent List.sortBy"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy f
"""
                        ]
        , test "should replace List.sortBy f (List.repeat n a) by List.repeat n a" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f (List.repeat n b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy on a List.repeat call"
                            , details = [ "You can replace this call by the given List.repeat call." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.repeat n b)
"""
                        ]
        , test "should replace List.sortBy f << List.repeat n by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        , test "should replace List.repeat n >> List.sortBy f by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.repeat n >> List.sortBy f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        ]


listSortWithTests : Test
listSortWithTests =
    describe "List.sortWith"
        [ test "should not report List.sortWith with a function variable and a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn
b = List.sortWith fn list
b = List.sortWith (always fn) list
b = List.sortWith (always (always fn)) list
e = List.sortWith f (List.sortWith f list) -- because e.g. List.sortWith (\\_ _ -> LT) is equivalent to List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortWith with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn (a :: bToZ)
b = List.sortWith fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortWith fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortWith fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortWith fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns GT will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns GT will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns GT will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> GT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns GT will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns GT will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns EQ will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns EQ will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns EQ will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> EQ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns EQ will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns EQ will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse list
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse <| list
"""
                        ]
        , test "should replace list |> List.sortWith (always (always LT)) by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> LT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith with a comparison that always returns LT is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith f (List.repeat n a) by List.repeat n a" <|
            \() ->
                """module A exposing (..)
a = List.sortWith f (List.repeat n b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortWith on a List.repeat call"
                            , details = [ "You can replace this call by the given List.repeat call." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.repeat n b)
"""
                        ]
        , test "should replace List.sortWith f << List.repeat n by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.sortWith f << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortWith on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        , test "should replace List.repeat n >> List.sortWith f by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.repeat n >> List.sortWith f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortWith on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        ]


listReverseTests : Test
listReverseTests =
    describe "List.reverse"
        [ test "should not report List.reverse with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.reverse
b = List.reverse list
c = (List.reverse << f) << List.reverse
d = List.reverse << (f << List.reverse)
e = List.repeat n (List.reverse list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.reverse [] by []" <|
            \() ->
                """module A exposing (..)
a = List.reverse []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.reverse [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.reverse [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b ]
"""
                        ]
        , test "should replace List.reverse (List.singleton a) by (List.singleton a)" <|
            \() ->
                """module A exposing (..)
a = List.reverse (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton b)
"""
                        ]
        , test "should replace a |> List.singleton |> List.reverse by a |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = b |> List.singleton |> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> List.singleton
"""
                        ]
        , test "should replace List.reverse << List.singleton by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.reverse << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.singleton >> List.reverse by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.reverse (List.repeat n a) by List.repeat n a" <|
            \() ->
                """module A exposing (..)
a = List.reverse (List.repeat n b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse on a List.repeat call"
                            , details = [ "You can replace this call by the given List.repeat call." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.repeat n b)
"""
                        ]
        , test "should replace List.reverse << List.repeat n by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.reverse << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        , test "should replace List.repeat n >> List.reverse by List.repeat n" <|
            \() ->
                """module A exposing (..)
a = List.repeat n >> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.reverse on a List.repeat call"
                            , details = [ "You can replace this composition by the given List.repeat call." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.repeat n
"""
                        ]
        , test "should replace List.reverse <| List.reverse <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.reverse <| List.reverse <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can replace this call by the argument given to List.reverse." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should simplify List.reverse >> List.reverse to identity" <|
            \() ->
                """module A exposing (..)
a = List.reverse >> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 21 }, end = { row = 2, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify (f << List.reverse) << List.reverse to (f)" <|
            \() ->
                """module A exposing (..)
a = (f << List.reverse) << List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should simplify List.reverse << (List.reverse << f) to (f)" <|
            \() ->
                """module A exposing (..)
a = List.reverse << (List.reverse << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        ]


listTakeTests : Test
listTakeTests =
    describe "List.take"
        [ test "should not report List.take that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.take 2 list
b = List.take y [ 1, 2, 3 ]
b = List.take n0 (List.take n1 list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.take n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.take n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 list by []" <|
            \() ->
                """module A exposing (..)
a = List.take 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with length 0 will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 by always []" <|
            \() ->
                """module A exposing (..)
a = List.take 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with length 0 will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.take n (List.take n list) by List.take n list" <|
            \() ->
                """module A exposing (..)
a = List.take n (List.take n list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.take after equivalent List.take"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.take"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.take n list)
"""
                        ]
        , test "should replace List.take n >> List.take n by List.take n" <|
            \() ->
                """module A exposing (..)
a = List.take n >> List.take n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.take after equivalent List.take"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.take"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 29 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.take n
"""
                        ]
        , test "should replace List.take n << List.take n by List.take n" <|
            \() ->
                """module A exposing (..)
a = List.take n << List.take n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.take after equivalent List.take"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.take"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.take n
"""
                        ]
        , test "should replace List.take -literal by always []" <|
            \() ->
                """module A exposing (..)
a = List.take -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with negative length will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listDropTests : Test
listDropTests =
    describe "List.drop"
        [ test "should not report List.drop that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.drop 2 list
b = List.drop y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.drop n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.drop n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.drop 0 list by list" <|
            \() ->
                """module A exposing (..)
a = List.drop 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with count 0 will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace list |> List.drop 0 by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with count 0 will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.drop 0 by identity" <|
            \() ->
                """module A exposing (..)
a = List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with count 0 will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace list |> List.drop -1 by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.drop -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with negative count will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.drop -1 by identity" <|
            \() ->
                """module A exposing (..)
a = List.drop -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with negative count will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace [a, b, c] |> List.drop 2 by [c]" <|
            \() ->
                """module A exposing (..)
a = [b, c, d] |> List.drop 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with a count less than the given list's length will remove these elements"
                            , details = [ "You can remove the first 2 elements from the list literal." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [d]
"""
                        ]
        , test "should replace List.singleton a |> List.drop 1 by []" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.drop 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with a count greater than or equal to the given list's length will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [ a, b ] |> List.drop 2 by []" <|
            \() ->
                """module A exposing (..)
a = [ b, c ] |> List.drop 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop with a count greater than or equal to the given list's length will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listPartitionTests : Test
listPartitionTests =
    describe "List.partition"
        [ test "should not report List.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.partition f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition f [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition f <| [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace [] |> List.partition f by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = [] |> List.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition (always True) list by ( list, [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always True) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first list"
                            , details = [ "Since the predicate function always returns True, the second list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( list, [] )
"""
                        ]
        , test "should not replace List.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
a = List.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition (always False) list by ( [], list )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], list )
"""
                        ]
        , test "should replace List.partition (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace List.partition <| (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace always False |> List.partition by Tuple.pair []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        ]


listIntersperseTests : Test
listIntersperseTests =
    describe "List.intersperse"
        [ test "should not report List.intersperse that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.intersperse 2 list
b = List.intersperse y [ 1, 2, 3 ]
c = List.intersperse << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.intersperse x [] by []" <|
            \() ->
                """module A exposing (..)
a = List.intersperse x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.intersperse s [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b ]
"""
                        ]
        , test "should replace List.intersperse s (List.singleton a) by (List.singleton a)" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton b)
"""
                        ]
        , test "should replace a |> List.singleton |> List.intersperse s by a |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = b |> List.singleton |> List.intersperse s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> List.singleton
"""
                        ]
        , test "should replace List.intersperse s << List.singleton by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.singleton >> List.intersperse s by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> List.intersperse s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        ]


listUnzipTests : Test
listUnzipTests =
    describe "List.unzip"
        [ test "should not report List.unzip on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.unzip
b = List.unzip list
c = List.unzip [ h ]
d = List.unzip (h :: t)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.unzip [] by []" <|
            \() ->
                """module A exposing (..)
a = List.unzip []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.unzip on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.unzip"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        ]


listMap2Tests : Test
listMap2Tests =
    describe "List.map2"
        [ test "should not report List.map2 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map2 f
a = List.map2 f list0
b = List.map2 f list0 list1
c = List.map2 f [ h ] list1
d = List.map2 f (h :: t) list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map2 f [] list1 by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map2 f [] by always []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map2 f list0 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f list0 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap3Tests : Test
listMap3Tests =
    describe "List.map3"
        [ test "should not report List.map3 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map3 f
a = List.map3 f list0
b = List.map3 f list0 list1
b = List.map3 f list0 list1 list2
c = List.map3 f [ h ] list1 list2
d = List.map3 f (h :: t) list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map3 f [] list1 list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f [] list1 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map3 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map3 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map3 f list0 [] list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 [] list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f list0 list1 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 list1 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap4Tests : Test
listMap4Tests =
    describe "List.map4"
        [ test "should not report List.map4 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map4 f
a = List.map4 f list0
b = List.map4 f list0 list1
b = List.map4 f list0 list1 list2
b = List.map4 f list0 list1 list2 list3
c = List.map4 f [ h ] list1 list2 list3
d = List.map4 f (h :: t) list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map4 f [] list1 list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f [] list1 list2 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map4 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map4 f [] by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map4 f list0 [] list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 [] list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 [] list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 [] list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 list2 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 list2 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap5Tests : Test
listMap5Tests =
    describe "List.map5"
        [ test "should not report List.map5 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map5 f
a = List.map5 f list0
b = List.map5 f list0 list1
b = List.map5 f list0 list1 list2
b = List.map5 f list0 list1 list2 list3
b = List.map5 f list0 list1 list2 list3 list4
c = List.map5 f [ h ] list1 list2 list3 list4
d = List.map5 f (h :: t) list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map5 f [] list1 list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 list3 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] list1 by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] by (\\_ _ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f list0 [] list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 [] list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 [] list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 [] list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 [] list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 [] list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 list3 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 list3 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]
