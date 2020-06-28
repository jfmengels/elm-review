module NoDebug.TodoOrToStringTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoDebug.TodoOrToString exposing (rule)
import Review.Project
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoDebug.TodoOrToString" tests


todoMessage : String
todoMessage =
    "Remove the use of `Debug.todo` before shipping to production"


todoDetails : List String
todoDetails =
    [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ]


toStringMessage : String
toStringMessage =
    "Remove the use of `Debug.toString` before shipping to production"


toStringDetails : List String
toStringDetails =
    [ "`Debug.toString` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
    ]


tests : List Test
tests =
    [ test "should not report normal function calls" <|
        \() ->
            """module A exposing (..)
a = foo n
b = bar.foo n
c = debug
c = toString
c = List.toString
d = debug.todo n
e = debug.toString n
            """
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report Debug.log calls" <|
        \() ->
            """module A exposing (..)
a = Debug.log n
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report Debug.todo use" <|
        \() ->
            """module A exposing (..)
a = Debug.todo"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = todoMessage
                        , details = todoDetails
                        , under = "Debug.todo"
                        }
                    ]
    , test "should report Debug.toString use" <|
        \() ->
            """module A exposing (..)
a = Debug.toString"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = toStringMessage
                        , details = toStringDetails
                        , under = "Debug.toString"
                        }
                    ]
    , test "should not report calls from a module containing Debug but that is not Debug" <|
        \() ->
            """module A exposing (..)
a = Foo.Debug.todo 1
b = Debug.Foo.todo 1
a = Foo.Debug.toString 1
b = Debug.Foo.toString 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report the import of the Debug module" <|
        \() ->
            """module A exposing (..)
import Debug"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report the use of `todo` when `todo` has been explicitly imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (todo)
a = todo ""
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = todoMessage
                        , details = todoDetails
                        , under = "todo"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 9 } }
                    ]
    , test "should report the use of `todo` when `todo` has been implicitly imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (..)
a = todo "" 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = todoMessage
                        , details = todoDetails
                        , under = "todo"
                        }
                    ]
    , test "should not report the use of `todo` when it has not been imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (log)
a = todo "" 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report the use of `toString` when `toString` has been explicitly imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (toString)
a = toString ""
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = toStringMessage
                        , details = toStringDetails
                        , under = "toString"
                        }
                        |> Review.Test.atExactly { start = { row = 3, column = 5 }, end = { row = 3, column = 13 } }
                    ]
    , test "should report the use of `toString` when `toString` has been implicitly imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (..)
a = toString "" 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = toStringMessage
                        , details = toStringDetails
                        , under = "toString"
                        }
                    ]
    , test "should not report the use of `toString` when it has not been imported" <|
        \() ->
            """module A exposing (..)
import Debug exposing (log)
a = toString "" 1
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report the use of `toString` or `todo` in files outside the source directories (TODO only with flag)" <|
        \() ->
            let
                project : Review.Project.Project
                project =
                    Review.Project.addModule { path = "tests/FooTest.elm", source = """module A exposing (..)
a = Debug.todo Debug.toString""" } applicationProject
            in
            Review.Test.runOnModulesWithProjectData project rule []
                |> Review.Test.expectNoErrors
    , test "should report the use of `toString` or `todo` in files inside the source directories (TODO only with flag)" <|
        \() ->
            let
                project : Review.Project.Project
                project =
                    Review.Project.addModule { path = "src/Foo.elm", source = """module A exposing (..)
a = Debug.todo Debug.toString""" } applicationProject
            in
            Review.Test.runOnModulesWithProjectData project rule []
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = todoMessage
                        , details = todoDetails
                        , under = "Debug.todo"
                        }
                    , Review.Test.error
                        { message = toStringMessage
                        , details = toStringDetails
                        , under = "Debug.toString"
                        }
                    ]
    ]


applicationProject : Review.Project.Project
applicationProject =
    Review.Project.new
        |> withDebugTodoElmJson Debug.todo rawApplicationElmJson


rawApplicationElmJson : String
rawApplicationElmJson =
    """
{
    "type": "package",
    "name": "author/dependency",
    "summary": "Summary",
    "license": "MIT",
    "version": "1.0.0",
    "exposed-modules": [
      "Foo"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
      "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
"""


withDebugTodoElmJson : (String -> Never) -> String -> Review.Project.Project -> Review.Project.Project
withDebugTodoElmJson debugTodo rawElmJson project =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJsonProject ->
            Review.Project.addElmJson
                { path = "elm.json"
                , raw = rawElmJson
                , project = elmJsonProject
                }
                project

        Err _ ->
            let
                _ =
                    debugTodo "Invalid elm.json supplied to test"
            in
            withDebugTodoElmJson debugTodo rawElmJson project
