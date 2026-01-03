module Simplify.TaskTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    Test.describe "Task"
        [ taskMapTests
        , taskMapNTests
        , taskAndThenTests
        , taskMapErrorTests
        , taskOnErrorTests
        , taskSequenceTests
        , attemptTests
        , performTests
        ]


taskMapTests : Test
taskMapTests =
    describe "Task.map"
        [ test "should not report Task.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.map f (Task.fail z) by (Task.fail z)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f (Task.fail z)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a failing task will result in the given failing task"
                            , details = [ "You can replace this call by the given failing task." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.fail z)
"""
                        ]
        , test "should replace Task.map f <| Task.fail z by Task.fail z" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f <| Task.fail z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a failing task will result in the given failing task"
                            , details = [ "You can replace this call by the given failing task." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail z
"""
                        ]
        , test "should replace Task.fail z |> Task.map f by Task.fail z" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.fail z |> Task.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a failing task will result in the given failing task"
                            , details = [ "You can replace this call by the given failing task." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail z
"""
                        ]
        , test "should replace Task.map f << Task.fail by Task.fail" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f << Task.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a failing task will result in the unchanged failing task"
                            , details = [ "You can replace this composition by Task.fail." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail
"""
                        ]
        , test "should replace Task.map identity task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map identity task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.map identity <| task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map identity <| task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace task |> Task.map identity by task" <|
            \() ->
                """module A exposing (..)
import Task
a = task |> Task.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.map identity by identity" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map with an identity function will always return the same given task"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = identity
"""
                        ]
        , test "should replace Task.map f (Task.succeed x) by (Task.succeed (f x))" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f (Task.succeed x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.succeed (f x))
"""
                        ]
        , test "should replace Task.map f <| Task.succeed x by Task.succeed (f <| x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f <| Task.succeed x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed (f <| x)
"""
                        ]
        , test "should replace Task.succeed x |> Task.map f by Task.succeed (x |> f)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.succeed x |> Task.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed (x |> f)
"""
                        ]
        , test "should replace x |> Task.succeed |> Task.map f by (x |> f) |> Task.succeed" <|
            \() ->
                """module A exposing (..)
import Task
a = x |> Task.succeed |> Task.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (x |> f) |> Task.succeed
"""
                        ]
        , test "should replace Task.map f <| Task.succeed <| x by Task.succeed <| (f <| x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f <| Task.succeed <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed <| (f <| x)
"""
                        ]
        , test "should replace Task.map f << Task.succeed by Task.succeed << f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f << Task.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map on a succeeding task will result in Task.succeed with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.succeed with the function directly applied to the value inside the succeeding task itself." ]
                            , under = "Task.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed << f
"""
                        ]
        ]


taskMapNTests : Test
taskMapNTests =
    -- testing behavior only with representatives for 2-5
    describe "Task.mapN"
        [ test "should not report Task.map3 with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3
b = Task.map3 f
c = Task.map3 f task0
d = Task.map3 f task0 task1
e = Task.map3 f task0 task1 task2
f = Task.map3 f (Task.succeed h) task1 task2 -- because this is a code style choice
f = Task.map3 f (Task.succeed h)
g = Task.map3 f task0 task1 (Task.fail x) -- because task0/1 can have an earlier failing task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c) by Task.succeed (f a b c)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where each task is a succeeding task will result in Task.succeed on the values inside"
                            , details = [ "You can replace this call by Task.succeed with the function applied to the values inside each succeeding task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed (f a b c)
"""
                        ]
        , test "should replace c |> g |> Task.succeed |> Task.map3 f (Task.succeed a) (Task.succeed b) by (c |> g) |> f a b |> Task.succeed" <|
            \() ->
                """module A exposing (..)
import Task
a = c |> g |> Task.succeed |> Task.map3 f (Task.succeed a) (Task.succeed b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where each task is a succeeding task will result in Task.succeed on the values inside"
                            , details = [ "You can replace this call by Task.succeed with the function applied to the values inside each succeeding task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (c |> g) |> f a b |> Task.succeed
"""
                        ]
        , test "should replace Task.map3 f (Task.succeed a) (Task.succeed b) <| Task.succeed <| g <| c by Task.succeed <| f a b <| (g <| c)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.succeed a) (Task.succeed b) <| Task.succeed <| g <| c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where each task is a succeeding task will result in Task.succeed on the values inside"
                            , details = [ "You can replace this call by Task.succeed with the function applied to the values inside each succeeding task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed <| f a b <| (g <| c)
"""
                        ]
        , test "should replace Task.map3 f (Task.succeed a) (Task.fail x) task2 by (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.succeed a) (Task.fail x) task2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where we know the first failing task will result in that failing task"
                            , details = [ "You can replace this call by the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.fail x)
"""
                        ]
        , test "should replace Task.map3 f (Task.succeed a) (Task.fail x) by always (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.succeed a) (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where we know the first failing task will result in that failing task"
                            , details = [ "You can replace this call by always with the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = always (Task.fail x)
"""
                        ]
        , test "should replace Task.map3 f (Task.fail x) task1 task2 by (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.fail x) task1 task2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where we know the first failing task will result in that failing task"
                            , details = [ "You can replace this call by the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.fail x)
"""
                        ]
        , test "should replace Task.map3 f (Task.fail x) task1 by always (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.fail x) task1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where we know the first failing task will result in that failing task"
                            , details = [ "You can replace this call by always with the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = always (Task.fail x)
"""
                        ]
        , test "should replace Task.map3 f (Task.fail x) by (\\_ _ -> (Task.fail x))" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 where we know the first failing task will result in that failing task"
                            , details = [ "You can replace this call by \\_ _ -> with the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (\\_ _ -> (Task.fail x))
"""
                        ]
        , test "should replace Task.map3 f task0 (Task.fail x) task2 by Task.map2 f task0 (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map3 f task0 (Task.fail x) task2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map3 with a failing task early will ignore later arguments"
                            , details = [ "You can replace this call by Task.map2 with the same arguments until the first failing task." ]
                            , under = "Task.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map2 f task0 (Task.fail x)
"""
                        ]
        , test "should replace Task.map4 f task0 (Task.fail x) task3 by always (Task.map2 f task0 (Task.fail x))" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map4 f task0 (Task.fail x) task3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map4 with a failing task early will ignore later arguments"
                            , details = [ "You can replace this call by always with Task.map2 with the same arguments until the first failing task." ]
                            , under = "Task.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = always (Task.map2 f task0 (Task.fail x))
"""
                        ]
        , test "should replace Task.map4 f task0 (Task.fail x) by (\\_ _ -> Task.map2 f task0 (Task.fail x))" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map4 f task0 (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map4 with a failing task early will ignore later arguments"
                            , details = [ "You can replace this call by \\_ _ -> with Task.map2 with the same arguments until the first failing task." ]
                            , under = "Task.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (\\_ _ -> Task.map2 f task0 (Task.fail x))
"""
                        ]
        ]


taskMapErrorTests : Test
taskMapErrorTests =
    describe "Task.mapError"
        [ test "should not report Task.mapError used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError
b = Task.mapError f
c = Task.mapError f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.mapError f (Task.succeed a) by (Task.succeed a)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f (Task.succeed a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a succeeding task will result in the given succeeding task"
                            , details = [ "You can replace this call by the given succeeding task." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.succeed a)
"""
                        ]
        , test "should replace Task.mapError f <| Task.succeed a by Task.succeed a" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f <| Task.succeed a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a succeeding task will result in the given succeeding task"
                            , details = [ "You can replace this call by the given succeeding task." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed a
"""
                        ]
        , test "should replace Task.succeed a |> Task.mapError f by Task.succeed a" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.succeed a |> Task.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a succeeding task will result in the given succeeding task"
                            , details = [ "You can replace this call by the given succeeding task." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed a
"""
                        ]
        , test "should replace Task.mapError f << Task.succeed by Task.succeed" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f << Task.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a succeeding task will result in the unchanged succeeding task"
                            , details = [ "You can replace this composition by Task.succeed." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed
"""
                        ]
        , test "should replace Task.mapError identity task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError identity task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.mapError identity <| task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError identity <| task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace task |> Task.mapError identity by task" <|
            \() ->
                """module A exposing (..)
import Task
a = task |> Task.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError with an identity function will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.mapError identity by identity" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError with an identity function will always return the same given task"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = identity
"""
                        ]
        , test "should replace Task.mapError f (Task.fail x) by (Task.fail (f x))" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.fail (f x))
"""
                        ]
        , test "should replace Task.mapError f <| Task.fail x by Task.fail (f <| x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f <| Task.fail x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail (f <| x)
"""
                        ]
        , test "should replace Task.fail x |> Task.mapError f by Task.fail (x |> f)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.fail x |> Task.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail (x |> f)
"""
                        ]
        , test "should replace x |> Task.fail |> Task.mapError f by (x |> f) |> Task.fail" <|
            \() ->
                """module A exposing (..)
import Task
a = x |> Task.fail |> Task.mapError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (x |> f) |> Task.fail
"""
                        ]
        , test "should replace Task.mapError f <| Task.fail <| x by Task.fail <| (f <| x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f <| Task.fail <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail <| (f <| x)
"""
                        ]
        , test "should replace Task.mapError f << Task.fail by Task.fail << f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.mapError f << Task.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.mapError on a failing task will result in Task.fail with the function applied to the value inside"
                            , details = [ "You can replace this call by Task.fail with the function directly applied to the value inside the failing task itself." ]
                            , under = "Task.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail << f
"""
                        ]
        ]


taskAndThenTests : Test
taskAndThenTests =
    describe "Task.andThen"
        [ test "should not report Task.andThen used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen
b = Task.andThen f
c = Task.andThen f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.andThen f (Task.fail x) by (Task.fail x)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen f (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen on a failing task will result in the given failing task"
                            , details = [ "You can replace this call by the given failing task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.fail x)
"""
                        ]
        , test "should replace Task.andThen f << Task.fail by Task.fail" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen f << Task.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen on a failing task will result in the unchanged failing task"
                            , details = [ "You can replace this composition by Task.fail." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.fail
"""
                        ]
        , test "should not report Task.andThen (always (Task.fail x)) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen (always (Task.fail x)) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.andThen Task.succeed task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen Task.succeed task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen with a function equivalent to Task.succeed will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.andThen (\\b -> Task.succeed c) task by Task.map (\\b -> c) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen (\\b -> Task.succeed c) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen with a function that always returns a succeeding task is the same as Task.map with the function returning the value inside"
                            , details = [ "You can replace this call by Task.map with the function returning the value inside the succeeding task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map (\\b -> c) task
"""
                        ]
        , test "should replace Task.andThen (\\b -> let y = 1 in Task.succeed y) task by Task.map (\\b -> let y = 1 in y) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen (\\b -> let y = 1 in Task.succeed y) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen with a function that always returns a succeeding task is the same as Task.map with the function returning the value inside"
                            , details = [ "You can replace this call by Task.map with the function returning the value inside the succeeding task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map (\\b -> let y = 1 in y) task
"""
                        ]
        , test "should replace Task.andThen (\\b -> if cond then Task.succeed b else Task.succeed c) task by Task.map (\\b -> if cond then b else c) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen (\\b -> if cond then Task.succeed b else Task.succeed c) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen with a function that always returns a succeeding task is the same as Task.map with the function returning the value inside"
                            , details = [ "You can replace this call by Task.map with the function returning the value inside the succeeding task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map (\\b -> if cond then b else c) task
"""
                        ]
        , test "should not report Task.andThen (\\b -> if cond then Task.succeed b else Task.fail c) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen (\\b -> if cond then Task.succeed b else Task.fail c) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.andThen f (Task.succeed a) by f a" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen f (Task.succeed a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen on a succeeding task is the same as applying the function to the value from the succeeding task"
                            , details = [ "You can replace this call by the function directly applied to the value inside the succeeding task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = f a
"""
                        ]
        , test "should replace Task.succeed a |> Task.andThen f by a |> f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.succeed a |> Task.andThen f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen on a succeeding task is the same as applying the function to the value from the succeeding task"
                            , details = [ "You can replace this call by the function directly applied to the value inside the succeeding task." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = a |> f
"""
                        ]
        , test "should replace Task.andThen f << Task.succeed by f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.andThen f << Task.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.andThen on a succeeding task is the same as applying the function to the value from the succeeding task"
                            , details = [ "You can replace this composition by the function given to Task.andThen." ]
                            , under = "Task.andThen"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = f
"""
                        ]
        ]


taskOnErrorTests : Test
taskOnErrorTests =
    describe "Task.onError"
        [ test "should not report Task.onError used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError
b = Task.onError f
c = Task.onError f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.onError f (Task.succeed a) by (Task.succeed a)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError f (Task.succeed a)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError on a succeeding task will result in the given succeeding task"
                            , details = [ "You can replace this call by the given succeeding task." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.succeed a)
"""
                        ]
        , test "should replace Task.onError f << Task.succeed by Task.succeed" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError f << Task.succeed
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError on a succeeding task will result in the unchanged succeeding task"
                            , details = [ "You can replace this composition by Task.succeed." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed
"""
                        ]
        , test "should not report Task.onError (always (Task.succeed a)) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError (always (Task.succeed a)) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.onError Task.fail task by task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError Task.fail task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError with a function equivalent to Task.fail will always return the same given task"
                            , details = [ "You can replace this call by the task itself." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = task
"""
                        ]
        , test "should replace Task.onError (\\x -> Task.fail y) task by Task.mapError (\\x -> y) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError (\\x -> Task.fail y) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError with a function that always returns a failing task is the same as Task.mapError with the function returning the value inside"
                            , details = [ "You can replace this call by Task.mapError with the function returning the value inside the failing task." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.mapError (\\x -> y) task
"""
                        ]
        , test "should replace Task.onError (\\x -> if cond then Task.fail x else Task.fail y) task by Task.mapError (\\x -> if cond then x else y) task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError (\\x -> if cond then Task.fail x else Task.fail y) task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError with a function that always returns a failing task is the same as Task.mapError with the function returning the value inside"
                            , details = [ "You can replace this call by Task.mapError with the function returning the value inside the failing task." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.mapError (\\x -> if cond then x else y) task
"""
                        ]
        , test "should replace Task.onError f (Task.fail x) by f x" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError f (Task.fail x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError on a failing task is the same as applying the function to the value from the failing task"
                            , details = [ "You can replace this call by the function directly applied to the value inside the failing task." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = f x
"""
                        ]
        , test "should replace Task.fail x |> Task.onError f by x |> f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.fail x |> Task.onError f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError on a failing task is the same as applying the function to the value from the failing task"
                            , details = [ "You can replace this call by the function directly applied to the value inside the failing task." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = x |> f
"""
                        ]
        , test "should replace Task.onError f << Task.fail by f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.onError f << Task.fail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.onError on a failing task is the same as applying the function to the value from the failing task"
                            , details = [ "You can replace this composition by the function given to Task.onError." ]
                            , under = "Task.onError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = f
"""
                        ]
        ]


taskSequenceTests : Test
taskSequenceTests =
    describe "Task.sequence"
        [ test "should not report Task.sequence used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence
b = Task.sequence list
c = Task.sequence [ Task.succeed d, e ]
c = Task.sequence [ d, Task.fail x ] -- because d could be a failing task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.sequence [] by Task.succeed []" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on [] will result in Task.succeed []"
                            , details = [ "You can replace this call by Task.succeed []." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed []
"""
                        ]
        , test "should replace Task.sequence [ f a ] by Task.map List.singleton (f a)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence [ f a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on a singleton list is the same as Task.map List.singleton on the value inside"
                            , details = [ "You can replace this call by Task.map List.singleton on the value inside the singleton list." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map List.singleton (f a)
"""
                        ]
        , test "should replace Task.sequence << List.singleton by Task.map List.singleton" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on a singleton list is the same as Task.map List.singleton on the value inside"
                            , details = [ "You can replace this call by Task.map List.singleton." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.map List.singleton
"""
                        ]
        , test "should replace Task.sequence [ Task.succeed a, Task.succeed b ] by Task.succeed [ a, b ]" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence [ Task.succeed a, Task.succeed b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on a list where each element is a succeeding task will result in Task.succeed on the values inside"
                            , details = [ "You can replace this call by Task.succeed on a list where each element is replaced by its value inside the succeeding task." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.succeed [ a, b ]
"""
                        ]
        , test "should replace Task.sequence [ Task.succeed a, x |> Task.fail, Task.fail y ] by (x |> Task.fail)" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence [ Task.succeed a, x |> Task.fail, Task.fail y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on a list containing a failing task will result in the first failing task"
                            , details = [ "You can replace this call by the first failing task in the list." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (x |> Task.fail)
"""
                        ]
        , test "should replace Task.sequence [ a, Task.fail x, b ] by Task.sequence [ a, Task.fail x]" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.sequence [ a, Task.fail x, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.sequence on a list containing a failing task early will ignore later elements"
                            , details = [ "You can remove all list elements after the first failing task." ]
                            , under = "Task.sequence"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.sequence [ a, Task.fail x]
"""
                        ]
        ]


attemptTests : Test
attemptTests =
    describe "Task.attempt"
        [ test "should not report Task.attempt used with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Task.attempt
a1 = Task.attempt f
a2 = Task.attempt f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.attempt identity (Task.map f task) by Task.attempt f task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.attempt identity (Task.map f task)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.attempt with an identity function can be combined into Task.attempt"
                            , details = [ "You can replace this call by Task.attempt with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.attempt"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.attempt f task)
"""
                        ]
        , test "should replace Task.attempt identity << Task.map f by Task.attempt f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.attempt identity << Task.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.attempt with an identity function can be combined into Task.attempt"
                            , details = [ "You can replace this composition by Task.attempt with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.attempt"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.attempt f
"""
                        ]
        , test "should replace Task.map f >> Task.attempt identity by Task.attempt f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f >> Task.attempt identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.attempt with an identity function can be combined into Task.attempt"
                            , details = [ "You can replace this composition by Task.attempt with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.attempt"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.attempt f
"""
                        ]
        ]


performTests : Test
performTests =
    describe "Task.perform"
        [ test "should not report Task.perform used with okay arguments" <|
            \() ->
                """module A exposing (..)
a0 = Task.perform
a1 = Task.perform f
a2 = Task.perform f task
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Task.perform identity (Task.map f task) by Task.perform f task" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.perform identity (Task.map f task)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.perform with an identity function can be combined into Task.perform"
                            , details = [ "You can replace this call by Task.perform with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.perform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = (Task.perform f task)
"""
                        ]
        , test "should replace Task.perform identity << Task.map f by Task.perform f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.perform identity << Task.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.perform with an identity function can be combined into Task.perform"
                            , details = [ "You can replace this composition by Task.perform with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.perform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.perform f
"""
                        ]
        , test "should replace Task.map f >> Task.perform identity by Task.perform f" <|
            \() ->
                """module A exposing (..)
import Task
a = Task.map f >> Task.perform identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Task.map, then Task.perform with an identity function can be combined into Task.perform"
                            , details = [ "You can replace this composition by Task.perform with the same arguments given to Task.map which is meant for this exact purpose." ]
                            , under = "Task.perform"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Task
a = Task.perform f
"""
                        ]
        ]
