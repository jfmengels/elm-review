module Simplify.RecordAccessTest exposing (all)

import Review.Project
import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Simplify.RecordAccess"
        [ test "should simplify record accesses for explicit records" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records (using access function application)" <|
            \() ->
                """module A exposing (..)
a = .b { b = 3 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records (using access function application with extra argument)" <|
            \() ->
                """module A exposing (..)
a = .b { b = f } extraArgument
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f extraArgument
"""
                        ]
        , test "should simplify record accesses for explicit records (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = .b <| { b = 3 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = { b = 3 } |> .b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records and add parens when necessary" <|
            \() ->
                """module A exposing (..)
a = { b = f n }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        , test "should simplify record accesses for explicit records in parentheses" <|
            \() ->
                """module A exposing (..)
a = (({ b = 3 })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "shouldn't simplify record accesses for explicit records if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for record updates" <|
            \() ->
                """module A exposing (..)
a = foo { d | b = f x y }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates in parentheses" <|
            \() ->
                """module A exposing (..)
a = foo (({ d | b = f x y })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates (using access function application)" <|
            \() ->
                """module A exposing (..)
a = foo <| .b { d | b = f x y }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo <| (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates (using access function application with extra argument)" <|
            \() ->
                """module A exposing (..)
a = foo <| .b { d | b = f x y } extraArgument
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo <| (f x y) extraArgument
"""
                        ]
        , test "should simplify record accesses for record updates (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = foo <| .b <| { d | b = f x y }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo <| (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = { d | b = f x y } |> .b |> foo
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f x y) |> foo
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            , under = ".c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field (using access function application)" <|
            \() ->
                """module A exposing (..)
a = .c { d | b = 3 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            , under = ".c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = .c <| { d | b = 3 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            , under = ".c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 } |> .c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            , under = ".c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should replace constructing record composition into field access function by contructing that field's value" <|
            \() ->
                """module A exposing (..)
a = .b << (\\x -> { b = f <| x })
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\x -> (f <| x))
"""
                        ]
        , test "should replace constructing record update composition into field access function by contructing the updated field" <|
            \() ->
                """module A exposing (..)
a = .d << (\\x -> { b | d = f <| x, c = 1 })
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\x -> (f <| x))
"""
                        ]
        , test "should replace constructing record update composition into unrelated field access function by contructing the unchanged record" <|
            \() ->
                """module A exposing (..)
a = .e << (\\x -> { x | b = y })
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\x -> x.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = 3 }).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = 3 }.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression" <|
            \() ->
                """module A exposing (..)
a = (let b = c in f x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression (using access function application)" <|
            \() ->
                """module A exposing (..)
a = .e (let b = c in f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = .e <| let b = c in f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = let b = c in (f x).e
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in f x) |> .e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression, without adding unnecessary parentheses" <|
            \() ->
                """module A exposing (..)
a = (let b = c in x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in x.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions in parentheses" <|
            \() ->
                """module A exposing (..)
a = (((let b = c in {e = 2}))).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (((let b = c in {e = 2}.e)))
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (inner)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = { f = 2 } }).e.f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = { f = 2 } }.e).f
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (outer)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in (f x).e).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside a let...in will result in accessing it in its result"
                            , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e.f)
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions (using access function application)" <|
            \() ->
                """module A exposing (..)
a = .f (if x then { f = 3 } else { z | f = 3 })
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions (using access function <|)" <|
            \() ->
                """module A exposing (..)
a = .f <| if x then { f = 3 } else { z | f = 3 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if x then { f = 3 }.f else { z | f = 3 }.f
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions (using access function |>)" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else { z | f = 3 }) |> .f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should not simplify record accesses if some branches are not records" <|
            \() ->
                """module A exposing (..)
a = (if x then a else { f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for nested if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then { z | f = 4 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then { z | f = 4 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should simplify record accesses for mixed if/then/else and case expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then {f = 2} else
            case b of Nothing -> { f = 4 }
                      Just _ -> { f = 5 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then {f = 2}.f else
            case b of Nothing -> { f = 4 }.f
                      Just _ -> { f = 5 }.f)
"""
                        ]
        , test "should simplify record access for module-locally declared record type alias" <|
            \() ->
                """module A exposing (..)
a =
    (second |> (Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "You can replace accessing this record by just that field's value." ]
                            , under = ".second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                        ]
        , test "should simplify record access for project-locally declared record type alias" <|
            \() ->
                [ """module A exposing (..)
import B

a =
    (second |> (B.Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                , """module B exposing (Record)

type alias Record =
    { first : Int, second : Int }
"""
                ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Accessing a field of a record where we know that field's value will return that field's value"
                                , details = [ "You can replace accessing this record by just that field's value." ]
                                , under = ".second"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B

a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                            ]
                          )
                        ]
        , test "should simplify record access for dependency declared record type alias" <|
            \() ->
                """module A exposing (..)
import B

a =
    (second |> (B.Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                    |> Review.Test.runWithProjectData
                        (Review.Project.new
                            |> Review.Project.addModule
                                { path = "src/B.elm"
                                , source =
                                    """module B exposing (Record)

type alias Record =
    { first : Int, second : Int }
"""
                                }
                        )
                        ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Accessing a field of a record where we know that field's value will return that field's value"
                                , details = [ "You can replace accessing this record by just that field's value." ]
                                , under = ".second"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B

a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                            ]
                          )
                        ]
        , test "should replace record type alias constructor into last field access function by identity" <|
            \() ->
                """module A exposing (..)
type alias Record =
    { first : Int, second : Int }
a =
    .second << Record first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary constructing a record around a field that is then being accessed"
                            , details = [ "This composition will construct a record with the incoming value stored in the field `second`. This exact field is then immediately accessed which means you can replace this composition by identity." ]
                            , under = ".second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type alias Record =
    { first : Int, second : Int }
a =
    identity
"""
                        ]
        , test "should replace record type alias constructor into known field access function by always that field value" <|
            \() ->
                """module A exposing (..)
type alias Record =
    { first : Int, second : Int }
a =
    .first << Record first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                            , details = [ "This composition will construct a record where we known the value of the field `first`. This exact field is then immediately accessed which means you can replace this composition by `always` with that value." ]
                            , under = ".first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type alias Record =
    { first : Int, second : Int }
a =
    always first
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions as record type alias constructions" <|
            \() ->
                """module A exposing (..)
type alias F =
    { f : Int }
a =
    (if x then F 3 else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Accessing a field outside an if...then...else will result in accessing it in each branch"
                            , details = [ "You can replace accessing this record outside an if...then...else by accessing the record inside each branch." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type alias F =
    { f : Int }
a =
    (if x then (F 3).f else { z | f = 3 }.f)
"""
                        ]
        ]
