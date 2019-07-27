module NoDebugTest exposing (all)

import Lint.Rule.NoDebug exposing (rule)
import Lint.Test exposing (LintResult)
import Test exposing (Test, describe, test)


testRule : String -> LintResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Lint.Test.run rule


message : String
message =
    "Remove the use of `Debug` before shipping to production"


details : List String
details =
    [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing it's use before committing and attempting to push to production."
    ]


tests : List Test
tests =
    [ test "should not report normal function calls" <|
        \() ->
            testRule """
a = foo n
b = bar.foo n
c = debug
e = debug.log n
d = debug.log n
            """
                |> Lint.Test.expectNoErrors
    , test "should report Debug.log use" <|
        \() ->
            testRule "a = Debug.log"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug.log calls" <|
        \() ->
            testRule "a = Debug.log z"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report multiple Debug.log calls" <|
        \() ->
            testRule """
a = Debug.log z
b = Debug.log z
            """
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                        |> Lint.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                    , Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                        |> Lint.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                    ]
    , test "should report Debug.todo calls" <|
        \() ->
            testRule "a = Debug.todo 1"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.todo"
                        }
                    ]
    , test "should report any Debug method" <|
        \() ->
            testRule "a = Debug.foo 1"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.foo"
                        }
                    ]
    , test "should report Debug in a binary expression" <|
        \() ->
            testRule "a = ( Debug.log z ) + 2"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a << binary expression" <|
        \() ->
            testRule "a = fn << Debug.log"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a pipe expression" <|
        \() ->
            testRule "a = fn |> Debug.log z"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an list expression" <|
        \() ->
            testRule "a = [ Debug.log z y ]"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an record expression" <|
        \() ->
            testRule "a = { foo = Debug.log z y }"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an record update expression" <|
        \() ->
            testRule "a = { model | foo = Debug.log z y }"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an lambda expression" <|
        \() ->
            testRule "a = (\\foo -> Debug.log z foo)"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression condition" <|
        \() ->
            testRule "a = if Debug.log a b then True else False"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression then branch" <|
        \() ->
            testRule "a = if True then Debug.log a b else False"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in an if expression else branch" <|
        \() ->
            testRule "a = if True then True else Debug.log a b"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a case value" <|
        \() ->
            testRule """
a = case Debug.log a b of
  _ -> []
            """
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in a case body" <|
        \() ->
            testRule """
a = case a of
  _ -> Debug.log a b
            """
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in let declaration section" <|
        \() ->
            testRule """
a = let b = Debug.log a b
    in b
            """
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should report Debug in let body" <|
        \() ->
            testRule """
a = let b = c
    in Debug.log a b
            """
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug.log"
                        }
                    ]
    , test "should not report calls from a module containing Debug but that is not Debug" <|
        \() ->
            testRule """
a = Foo.Debug.log 1
b = Debug.Foo.log 1
            """
                |> Lint.Test.expectNoErrors
    , test "should report the import of the Debug module" <|
        \() ->
            testRule "import Debug"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug"
                        }
                    ]
    , test "should report the import of the Debug module (with exposing of some things)" <|
        \() ->
            testRule "import Debug exposing (log)"
                |> Lint.Test.expectErrors
                    [ Lint.Test.error
                        { message = message
                        , details = details
                        , under = "Debug"
                        }
                    ]
    , test "should not report imports of modules containing Debug but that is not Debug" <|
        \() ->
            testRule """
import Foo.Debug
import Debug.Foo
            """
                |> Lint.Test.expectNoErrors
    ]


all : Test
all =
    describe "NoDebug" tests
