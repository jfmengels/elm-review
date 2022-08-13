module Simplify.InferTest exposing (all)

import AssocList
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Expect exposing (Expectation)
import Simplify.Infer exposing (..)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Infer"
        [ simpleTests
        , detailedTests
        , deduceNewFactsTests
        ]


simpleTests : Test
simpleTests =
    describe "get"
        [ test "should infer a is true when a is True" <|
            \() ->
                empty
                    |> infer [ FunctionOrValue [] "a" ] True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer a is true when a is False" <|
            \() ->
                empty
                    |> infer [ FunctionOrValue [] "a" ] False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer a is 1 when a == 1 is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just (Floatable 1))
        , test "should not infer a when a == 1 is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should infer a is true when a && b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer b is true when a && b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should not infer a when a || b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should not infer b when a || b is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        , test "should infer a is false when a || b is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is false when a || b is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is true when a || b is True and a is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should infer b is true when b || a is True and a is False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "a"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should not infer b when a || b is True and a is True" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        True
                    |> get (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        ]


detailedTests : Test
detailedTests =
    describe "infer"
        [ test "should infer a when True" <|
            \() ->
                infer
                    [ FunctionOrValue [] "a" ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DTrue
                              )
                            ]
                        }
        , test "should infer a when False" <|
            \() ->
                infer
                    [ FunctionOrValue [] "a" ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DFalse
                              )
                            ]
                        }
        , test "should infer a == True when True" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                trueExpr
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DTrue
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , DTrue
                              )
                            ]
                        }
        , test "should infer a == True when False" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                falseExpr
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DFalse
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , DFalse
                              )
                            ]
                        }
        , test "should infer a == 1 when True" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , DNumber 1
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , DTrue
                              )
                            ]
                        }
        , test "should infer a == 1 when False" <|
            \() ->
                infer
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ NotEquals
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , DFalse
                              )
                            ]
                        }
        , test "should infer a && b when True" <|
            \() ->
                infer
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") trueExpr
                            , Equals (FunctionOrValue [] "a") trueExpr
                            , Equals
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DTrue )
                            , ( FunctionOrValue [] "a", DTrue )
                            , ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DTrue
                              )
                            ]
                        }
        , test "should infer a && b when False" <|
            \() ->
                infer
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Or
                                (Equals
                                    (FunctionOrValue [] "a")
                                    falseExpr
                                )
                                (Equals (FunctionOrValue [] "b") falseExpr)
                            , Equals
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DFalse
                              )
                            ]
                        }
        , test "should infer a || b when True" <|
            \() ->
                infer
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty
                    |> expectEqual
                        { facts =
                            [ Or
                                (Equals
                                    (FunctionOrValue [] "a")
                                    trueExpr
                                )
                                (Equals
                                    (FunctionOrValue [] "b")
                                    trueExpr
                                )
                            , Equals
                                (OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , DTrue
                              )
                            ]
                        }
        , test "should infer a || b when False" <|
            \() ->
                infer
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") falseExpr
                            , Equals (FunctionOrValue [] "a") falseExpr
                            , Equals (OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b"))) falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DFalse )
                            , ( FunctionOrValue [] "a", DFalse )
                            , ( OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")), DFalse )
                            ]
                        }
        , test "should infer a || b when True and a when False" <|
            \() ->
                empty
                    |> infer
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer [ FunctionOrValue [] "a" ]
                        False
                    |> expectEqual
                        { facts =
                            [ Equals (FunctionOrValue [] "b") (FunctionOrValue [ "Basics" ] "True")
                            , Equals (FunctionOrValue [] "a") (FunctionOrValue [ "Basics" ] "False")
                            , Or (Equals (FunctionOrValue [] "a") (FunctionOrValue [ "Basics" ] "True")) (Equals (FunctionOrValue [] "b") (FunctionOrValue [ "Basics" ] "True"))
                            , Equals (OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b"))) (FunctionOrValue [ "Basics" ] "True")
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", DTrue )
                            , ( FunctionOrValue [] "a", DFalse )
                            , ( OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")), DTrue )
                            ]
                        }
        ]


deduceNewFactsTests : Test
deduceNewFactsTests =
    describe "deduceNewFacts"
        [ test "should not deduce anything when facts don't share anything (a == True, b == True)" <|
            \() ->
                deduceNewFacts
                    (Equals (FunctionOrValue [] "b") trueExpr)
                    [ Equals (FunctionOrValue [] "a") trueExpr ]
                    |> Expect.equal []
        , test "should deduce b is True when (a || b) and (a == False)" <|
            \() ->
                deduceNewFacts
                    (Equals (FunctionOrValue [] "a") falseExpr)
                    [ Or
                        (Equals
                            (FunctionOrValue [] "a")
                            trueExpr
                        )
                        (Equals
                            (FunctionOrValue [] "b")
                            trueExpr
                        )
                    ]
                    |> Expect.equal
                        [ Equals (FunctionOrValue [] "b") trueExpr ]
        ]


expectEqual :
    { facts : List Fact
    , deduced : List ( Expression, DeducedValue )
    }
    -> Inferred
    -> Expectation
expectEqual record (Inferred inferred) =
    { facts = inferred.facts
    , deduced = AssocList.toList inferred.deduced
    }
        |> Expect.equal record


n : Expression -> Node Expression
n =
    Node Range.emptyRange
