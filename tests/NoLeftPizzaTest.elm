module NoLeftPizzaTest exposing (tests)

import NoLeftPizza
import Review.Test
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "NoLeftPizza"
        [ anyTests
        , redundantTests
        ]


anyTests : Test
anyTests =
    describe "NoLeftPizza.Any"
        [ test "Simple pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError "foo <| bar"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo bar"""
                        ]
        , test "Nested pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar <| baz"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            "foo <| bar <| baz"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (bar <| baz)"""
                        , makeAnyError "bar <| baz"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo <| bar baz"""
                        ]
        , test "Fixes operator precedence" <|
            \_ ->
                """module A exposing (..)

a = foo <| 1 + 1"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            "foo <| 1 + 1"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (1 + 1)"""
                        ]
        , test "Fixes more operator precedence" <|
            \_ ->
                """module A exposing (..)

a = foo <| 1 + 1 / 2"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            "foo <| 1 + 1 / 2"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo (1 + 1 / 2)"""
                        ]
        , test "Why isn't this fixed?" <|
            \_ ->
                """module A exposing (..)

f =
    List.map .x <| y
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            "List.map .x <| y"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

f =
    List.map .x y
"""
                        ]
        , test "Why isn't _this_ fixed, pt2?" <|
            \_ ->
                """module A exposing (..)

f =
    String.join " " <|
        List.map x y
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            """String.join " " <|
        List.map x y"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

f =
    String.join " " (List.map x y)
"""
                        ]
        , test "Why isn't _this_ fixed, pt3?" <|
            \_ ->
                """module A exposing (..)

f =
    String.join " " <|
        List.map
            (\\x ->
                 x
            )
            y
        """
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError
                            """String.join " " <|
        List.map
            (\\x ->
                 x
            )
            y"""
                            |> Review.Test.whenFixed
                                """module A exposing (..)

f =
    String.join " " (List.map (\\x -> x)
     y)
        """
                        ]
        , test "handle parser operators with pizza" <|
            \() ->
                """
module MyParser exposing (..)
numberToken =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isDigit
            |. Parser.chompWhile Char.isDigit
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError """Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isDigit
            |. Parser.chompWhile Char.isDigit"""
                            |> Review.Test.whenFixed
                                """
module MyParser exposing (..)
numberToken =
    Parser.getChompedString (Parser.succeed () |. Parser.chompIf Char.isDigit |. Parser.chompWhile Char.isDigit)
"""
                        ]
        , test "handle logic operators with pizza" <|
            \() ->
                """
module A exposing (..)
f =
    if isTrue <| True || False then
        True
    else
        False
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                    |> Review.Test.expectErrors
                        [ makeAnyError "isTrue <| True || False"
                            |> Review.Test.whenFixed
                                """
module A exposing (..)
f =
    if isTrue (True || False) then
        True
    else
        False
"""
                        ]
        , describe "mixed pizzas" mixedPizzaTests
        ]


mixedPizzaTests : List Test
mixedPizzaTests =
    [ test "a <| (b |> c)" <|
        \() ->
            """
module A exposing (..)
f =
    a <| (b |> c)
"""
                |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                |> Review.Test.expectErrors
                    [ makeAnyError "a <| (b |> c)"
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
f =
    a (b |> c)
"""
                    ]
    , test "(a <| b) |> c)" <|
        \() ->
            """
module A exposing (..)
f =
    (a <| b) |> c
"""
                |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                |> Review.Test.expectErrors
                    [ makeAnyError "a <| b"
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
f =
    (a b) |> c
"""
                    ]
    , test "a |> (b <| c)" <|
        \() ->
            """
module A exposing (..)
f =
    a |> (b <| c)
"""
                |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                |> Review.Test.expectErrors
                    [ makeAnyError "b <| c"
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
f =
    a |> (b c)
"""
                    ]
    , test "(a |> b) <| c" <|
        \() ->
            """
module A exposing (..)
f =
    (a |> b) <| c
"""
                |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Any)
                |> Review.Test.expectErrors
                    [ makeAnyError "(a |> b) <| c"
                        |> Review.Test.whenFixed
                            """
module A exposing (..)
f =
    (a |> b) c
"""
                    ]
    ]


redundantTests : Test
redundantTests =
    describe "NoLeftPizza.Redundant"
        [ test "Simple pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "foo <| bar"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo bar"""
                        ]
        , test "Nested pizza" <|
            \_ ->
                """module A exposing (..)

a = foo <| bar <| baz"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError
                            "bar <| baz"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

a = foo <| bar baz"""
                        ]
        , test "Fixes operator precedence" <|
            \_ ->
                """module A exposing (..)

a = foo <| 1 + 1"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectNoErrors
        , test "Why isn't this fixed?" <|
            \_ ->
                """module A exposing (..)

f =
    List.map .x <| y
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError
                            "List.map .x <| y"
                            |> Review.Test.whenFixed
                                """module A exposing (..)

f =
    List.map .x y
"""
                        ]
        , test "Why isn't _this_ fixed, pt2?" <|
            \_ ->
                """module A exposing (..)

f =
    String.join " " <|
        List.map x y
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectNoErrors
        , test "Why isn't _this_ fixed, pt3?" <|
            \_ ->
                """module A exposing (..)

f =
    String.join " " <|
        List.map
            (\\x ->
                 x
            )
            y
        """
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectNoErrors
        , test "handle parser operators with pizza" <|
            \() ->
                """
module MyParser exposing (..)
numberToken =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isDigit
            |. Parser.chompWhile Char.isDigit
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectNoErrors
        , test "handle logic operators with pizza" <|
            \() ->
                """
module A exposing (..)
f =
    if isTrue <| True || False then
        True
    else
        False
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectNoErrors
        , test "Parenthesized expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| (abc xyz)
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| (abc xyz)"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar (abc xyz)
"""
                        ]
        , test "Unit expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| ()
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| ()"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar ()
"""
                        ]
        , test "Qualified function" <|
            \() ->
                """
module A exposing (..)

foo = bar <| Foo.Bar.xyz
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| Foo.Bar.xyz"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar Foo.Bar.xyz
"""
                        ]
        , test "Prefix operator" <|
            \() ->
                """
module A exposing (..)

foo = bar <| (++)
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| (++)"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar (++)
"""
                        ]
        , test "Integer value" <|
            \() ->
                """
module A exposing (..)

foo = bar <| 123
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| 123"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar 123
"""
                        ]
        , test "Floatable value" <|
            \() ->
                """
module A exposing (..)

foo = bar <| 123.123
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| 123.123"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar 123.123
"""
                        ]
        , test "Negated value" <|
            \() ->
                """
module A exposing (..)

foo = bar <| -123.123
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| -123.123"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar -123.123
"""
                        ]
        , test "String literal" <|
            \() ->
                """
module A exposing (..)

foo = bar <| "hello there"
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| \"hello there\""
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar "hello there"
"""
                        ]
        , test "Character literal" <|
            \() ->
                """
module A exposing (..)

foo = bar <| 'a'
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| 'a'"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar 'a'
"""
                        ]
        , test "Tupled expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| ( a, b, abc xyz )
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| ( a, b, abc xyz )"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar (a, b, abc xyz)
"""
                        ]
        , test "Record expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| { foo = 123 }
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| { foo = 123 }"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar {foo = 123}
"""
                        ]
        , test "Record update expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| { r | foo = 123 }
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| { r | foo = 123 }"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar { r | foo = 123 }
"""
                        ]
        , test "Record access expression" <|
            \() ->
                """
module A exposing (..)

foo = bar <| (foo bar).foo.bar
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| (foo bar).foo.bar"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar (foo bar).foo.bar
"""
                        ]
        , test "Record access function" <|
            \() ->
                """
module A exposing (..)

foo = bar <| .bar
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| .bar"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar .bar
"""
                        ]
        , test "List literal" <|
            \() ->
                """
module A exposing (..)

foo = bar <| [ 123, 456 ]
"""
                    |> Review.Test.run (NoLeftPizza.rule NoLeftPizza.Redundant)
                    |> Review.Test.expectErrors
                        [ makeRedundantError "bar <| [ 123, 456 ]"
                            |> Review.Test.whenFixed """
module A exposing (..)

foo = bar [123, 456]
"""
                        ]
        ]


makeAnyError : String -> Review.Test.ExpectedError
makeAnyError under =
    Review.Test.error
        { message = "That's a left pizza (<|) operator application there!"
        , details =
            [ "We prefer using either parenthesized function application like `Html.text (context.translate Foo.Bar)` or right pizza's like `foo |> bar`."
            , "The proposed fix rewrites the expression to a simple parenthesized expression, however, this may not always be what you want. Use your best judgement!"
            ]
        , under = under
        }


makeRedundantError : String -> Review.Test.ExpectedError
makeRedundantError under =
    Review.Test.error
        { message = "Redundant left pizza (<|) operator application"
        , details =
            [ "This left pizza operator can be removed without any further changes, without changing the semantics of your code."
            , "Using `<|` like this adds visual noise to code that can make it harder to read."
            ]
        , under = under
        }
