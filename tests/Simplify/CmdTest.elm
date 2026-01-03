module Simplify.CmdTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Cmd"
        [ cmdBatchTests
        , cmdMapTests
        ]


cmdBatchTests : Test
cmdBatchTests =
    describe "Cmd.batch"
        [ test "should not report Cmd.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch
a = Cmd.batch b
a = Cmd.batch [ b, x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Cmd.batch [] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on [] will result in Cmd.none"
                            , details = [ "You can replace this call by Cmd.none." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.batch [ a, Cmd.none, b ] by Cmd.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ a, Cmd.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a list containing an irrelevant Cmd.none"
                            , details = [ "Including Cmd.none in the list does not change the result of this call. You can remove the Cmd.none element." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ a, b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none ] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace Cmd.batch << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Cmd.batch [ b, Cmd.none ] by Cmd.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b, Cmd.none ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a list containing an irrelevant Cmd.none"
                            , details = [ "Including Cmd.none in the list does not change the result of this call. You can remove the Cmd.none element." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none, b ] by Cmd.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Cmd.batch on a list containing an irrelevant Cmd.none"
                            , details = [ "Including Cmd.none in the list does not change the result of this call. You can remove the Cmd.none element." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.batch [ command0, Cmd.batch [ command1, command2 ], command3, Cmd.batch [ command4, command5 ] ] by Cmd.batch [ command0, command1, command2, command3, command4, command5 ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ command0, Cmd.batch [ command1, command2 ], command3, Cmd.batch [ command4, command5 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Nested Cmd.batch calls can be spread"
                            , details = [ "You can move the elements from the inner Cmd.batch calls to inside this outer Cmd.batch call." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ command0,  command1, command2 , command3,  command4, command5  ]
"""
                        ]
        ]


cmdMapTests : Test
cmdMapTests =
    describe "Cmd.map"
        [ test "should replace Cmd.map identity cmd by cmd" <|
            \() ->
                """module A exposing (..)
a = Cmd.map identity cmd
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map with an identity function will always return the same given command"
                            , details = [ "You can replace this call by the command itself." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = cmd
"""
                        ]
        , test "should replace Cmd.map f Cmd.none by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.map f Cmd.none
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Cmd.none will result in Cmd.none"
                            , details = [ "You can replace this call by Cmd.none." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.map f (Task.perform identity task) by Task.perform f task" <|
            \() ->
                """module A exposing (..)
import Task
a = Cmd.map f (Task.perform identity task)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.perform with an identity function can be combined"
                            , details = [ "You can replace these operations by Task.perform with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.perform f task
"""
                        ]
        , test "should replace Cmd.map f << Task.perform identity by Task.perform f" <|
            \() ->
                """module A exposing (..)
import Task
a = Cmd.map f << Task.perform identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.perform with an identity function can be combined"
                            , details = [ "You can replace this composition by Task.perform with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.perform f
"""
                        ]
        , test "should replace Task.perform identity >> Cmd.map f by Task.perform f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.perform identity >> Cmd.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.perform with an identity function can be combined"
                            , details = [ "You can replace this composition by Task.perform with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.perform f
"""
                        ]
        , test "should replace (Cmd.map <| f <| x) << Task.perform identity by Task.perform <| f <| x" <|
            \() ->
                """module A exposing (..)
import Task
a = (Cmd.map <| f <| x) << Task.perform identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.perform with an identity function can be combined"
                            , details = [ "You can replace this composition by Task.perform with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.perform <| f <| x)
"""
                        ]
        , test "should replace Cmd.map f (Task.attempt identity task) by Task.attempt f task" <|
            \() ->
                """module A exposing (..)
import Task
a = Cmd.map f (Task.attempt identity task)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.attempt with an identity function can be combined"
                            , details = [ "You can replace these operations by Task.attempt with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.attempt f task
"""
                        ]
        , test "should replace Cmd.map f << Task.attempt identity by Task.attempt f" <|
            \() ->
                """module A exposing (..)
import Task
a = Cmd.map f << Task.attempt identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.attempt with an identity function can be combined"
                            , details = [ "You can replace this composition by Task.attempt with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.attempt f
"""
                        ]
        , test "should replace Task.attempt identity >> Cmd.map f by Task.attempt f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.attempt identity >> Cmd.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Platform.Cmd.map on Task.attempt with an identity function can be combined"
                            , details = [ "You can replace this composition by Task.attempt with the function given to Cmd.map." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.attempt f
"""
                        ]
        ]
