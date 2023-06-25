module Simplify.NormalizeTest exposing (all)

import Dict
import Elm.Dependency
import Elm.Interface as Interface
import Elm.Parser as Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range as Range exposing (Range)
import Expect
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Simplify.Infer as Infer
import Simplify.Normalize as Normalize
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Normalize"
        [ simpleNormalizationTests
        , moduleNameTests
        , inferTests
        ]


simpleNormalizationTests : Test
simpleNormalizationTests =
    describe "Simple normalizations"
        [ test "should remove parentheses" <|
            \() ->
                "(1)"
                    |> normalizeAndExpect (Integer 1)
        , test "should remove ranges of 'f a'" <|
            \() ->
                "f a"
                    |> normalizeAndExpect
                        (Application
                            [ n (FunctionOrValue [] "f")
                            , n (FunctionOrValue [] "a")
                            ]
                        )
        , test "should turn '.field a' into 'a.field'" <|
            \() ->
                ".field a"
                    |> normalizeAndExpect
                        (RecordAccess
                            (n (FunctionOrValue [] "a"))
                            (n "field")
                        )
        , test "should turn '.field <| a' into 'a.field'" <|
            \() ->
                "(.field) <| (a)"
                    |> normalizeAndExpect
                        (RecordAccess
                            (n (FunctionOrValue [] "a"))
                            (n "field")
                        )
        , test "should turn '(a).field' into 'a.field'" <|
            \() ->
                "(a).field"
                    |> normalizeAndExpect
                        (RecordAccess
                            (n (FunctionOrValue [] "a"))
                            (n "field")
                        )
        , test "should turn '.field a b' into 'a.field b'" <|
            \() ->
                ".field a b"
                    |> normalizeAndExpect
                        (Application
                            [ n
                                (RecordAccess
                                    (n (FunctionOrValue [] "a"))
                                    (n "field")
                                )
                            , n (FunctionOrValue [] "b")
                            ]
                        )
        , test "should normalize a function and function arguments" <|
            \() ->
                "(a) (b) (c)"
                    |> normalizeAndExpect
                        (Application
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            , n (FunctionOrValue [] "c")
                            ]
                        )
        , test "should remove <|" <|
            \() ->
                "a b <| c"
                    |> normalizeAndExpect
                        (Application
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            , n (FunctionOrValue [] "c")
                            ]
                        )
        , test "should remove |>" <|
            \() ->
                "c |> a b"
                    |> normalizeAndExpect
                        (Application
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            , n (FunctionOrValue [] "c")
                            ]
                        )
        , test "should remove <| when the left is not already an application " <|
            \() ->
                "(a) <| (b)"
                    |> normalizeAndExpect
                        (Application
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            ]
                        )
        , test "should remove <| when the left is a let expression" <|
            \() ->
                "(let a = (fn) in a) <| (b)"
                    |> normalizeAndExpect
                        (LetExpression
                            { declarations =
                                [ n
                                    (LetFunction
                                        { declaration =
                                            n
                                                { arguments = []
                                                , expression = n (FunctionOrValue [] "fn")
                                                , name = n "a"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , expression =
                                n
                                    (Application
                                        [ n (FunctionOrValue [] "a")
                                        , n (FunctionOrValue [] "b")
                                        ]
                                    )
                            }
                        )
        , test "should remove <| when the left is an if expression" <|
            \() ->
                "(if (cond) then (fn1) else (fn2)) <| (b)"
                    |> normalizeAndExpect
                        (IfBlock (n (FunctionOrValue [] "cond"))
                            (n
                                (Application
                                    [ n (FunctionOrValue [] "fn1")
                                    , n (FunctionOrValue [] "b")
                                    ]
                                )
                            )
                            (n
                                (Application
                                    [ n (FunctionOrValue [] "fn2")
                                    , n (FunctionOrValue [] "b")
                                    ]
                                )
                            )
                        )
        , test "should remove <| when the left is a case expression" <|
            \() ->
                """
  (case (x) of
     1 -> (fn1)
     2 -> (fn2)
  ) <| (b)
"""
                    |> normalizeAndExpect
                        (CaseExpression
                            { expression = n (FunctionOrValue [] "x")
                            , cases =
                                [ ( n (IntPattern 1)
                                  , n
                                        (Application
                                            [ n (FunctionOrValue [] "fn1")
                                            , n (FunctionOrValue [] "b")
                                            ]
                                        )
                                  )
                                , ( n (IntPattern 2)
                                  , n
                                        (Application
                                            [ n (FunctionOrValue [] "fn2")
                                            , n (FunctionOrValue [] "b")
                                            ]
                                        )
                                  )
                                ]
                            }
                        )
        , test "should normalize an operator application" <|
            \() ->
                """
  [ (a) < (b)
  , (a) <= (b)
  , (a) == (b)
  , (a) /= (b)
  , (a) ++ (b)
  , (a) + (b)
  , (a) - (b)
  , (a) * (b)
  , (a) / (b)
  , (a) // (b)
  , (a) ^ (b)
  , (a) && (b)
  , (a) || (b)
  , (a) </> (b)
  , (a) <?> (b)
  , (a) |. (b)
  , (a) |= (b)
  ]"""
                    |> normalizeAndExpect
                        (ListExpr
                            [ n (OperatorApplication "<" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "<=" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "==" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "/=" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "++" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "+" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "-" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "*" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "/" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "//" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "^" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "&&" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "||" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "</>" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "<?>" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "|." Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "|=" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            ]
                        )
        , test "should re-order operands of '+', '*', '||', '&&', '==', '/=' alphabetically" <|
            \() ->
                """
  [ (b) + (a)
  , (b) * (a)
  , (b) || (a)
  , (b) && (a)
  , (b) == (a)
  , (b) /= (a)
  ]"""
                    |> normalizeAndExpect
                        (ListExpr
                            [ n (OperatorApplication "+" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "*" Infix.Left (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "||" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "&&" Infix.Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "==" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            , n (OperatorApplication "/=" Infix.Non (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")))
                            ]
                        )
        , test "should replace > by <" <|
            \() ->
                "(a) > (b)"
                    |> normalizeAndExpect
                        (OperatorApplication
                            "<"
                            Infix.Non
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "a"))
                        )
        , test "should replace >= by <=" <|
            \() ->
                "(a) >= (b)"
                    |> normalizeAndExpect
                        (OperatorApplication
                            "<="
                            Infix.Non
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "a"))
                        )
        , test "should normalize uncons with a list literal into a list literal" <|
            \() ->
                "(a) :: [(b)]"
                    |> normalizeAndExpect
                        (ListExpr
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            ]
                        )
        , test "should normalize uncons with a non-list literal into a cons operation still" <|
            \() ->
                "(a) :: (b)"
                    |> normalizeAndExpect
                        (OperatorApplication
                            "::"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        )
        , test "should normalize an if expression" <|
            \() ->
                "if (a) then (b) else (c)"
                    |> normalizeAndExpect
                        (IfBlock
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "c"))
                        )
        , test "should remove the `not` calls in the condition by switching the branches" <|
            \() ->
                "if (Basics.not a) then (b) else (c)"
                    |> normalizeAndExpect
                        (IfBlock
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "c"))
                            (n (FunctionOrValue [] "b"))
                        )
        , test "should remove the `not` calls in the condition by switching the branches multiple times" <|
            \() ->
                "if (Basics.not <| Basics.not a) then (b) else (c)"
                    |> normalizeAndExpect
                        (IfBlock
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                            (n (FunctionOrValue [] "c"))
                        )
        , test "should normalize an integer negation" <|
            \() ->
                "-1"
                    |> normalizeAndExpect (Integer -1)
        , test "should normalize a float negation" <|
            \() ->
                "-1.1"
                    |> normalizeAndExpect (Floatable -1.1)
        , test "should normalize negations of something else" <|
            \() ->
                "-(a)"
                    |> normalizeAndExpect
                        (Negation (n (FunctionOrValue [] "a")))
        , test "should normalize tuples" <|
            \() ->
                "( (a), (b) )"
                    |> normalizeAndExpect
                        (TupledExpression
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            ]
                        )
        , test "should normalize lists" <|
            \() ->
                "[ (a), (b) ]"
                    |> normalizeAndExpect
                        (ListExpr
                            [ n (FunctionOrValue [] "a")
                            , n (FunctionOrValue [] "b")
                            ]
                        )
        , test "should normalize lambdas" <|
            \() ->
                "\\(a) -> (a)"
                    |> normalizeAndExpect
                        (LambdaExpression
                            { args = [ n (VarPattern "a") ]
                            , expression = n (FunctionOrValue [] "a")
                            }
                        )
        , test "should normalize records, and sort fields alphabetically" <|
            \() ->
                "{ field = (2), a = (1), z = (3) }"
                    |> normalizeAndExpect
                        (RecordExpr
                            [ n ( n "a", n (Integer 1) )
                            , n ( n "field", n (Integer 2) )
                            , n ( n "z", n (Integer 3) )
                            ]
                        )
        , test "should normalize record updates, and sort fields alphabetically" <|
            \() ->
                "{ record | field = (2), a = (1), z = (3) }"
                    |> normalizeAndExpect
                        (RecordUpdateExpression
                            (n "record")
                            [ n ( n "a", n (Integer 1) )
                            , n ( n "field", n (Integer 2) )
                            , n ( n "z", n (Integer 3) )
                            ]
                        )
        , test "should normalize hex literals to integers" <|
            \() ->
                "0x100"
                    |> normalizeAndExpect
                        (Integer 256)
        , test "should normalize let expressions" <|
            \() ->
                """
  let
      (a) = (1)
      f : toBeRemoved
      f (n) = (2)
  in
  (2)
"""
                    |> normalizeAndExpect
                        (LetExpression
                            { declarations =
                                [ n
                                    (LetDestructuring
                                        (n (VarPattern "a"))
                                        (n (Integer 1))
                                    )
                                , n
                                    (LetFunction
                                        { declaration =
                                            n
                                                { arguments =
                                                    [ n (VarPattern "n")
                                                    ]
                                                , expression = n (Integer 2)
                                                , name = n "f"
                                                }
                                        , documentation = Nothing
                                        , signature = Nothing
                                        }
                                    )
                                ]
                            , expression = n (Integer 2)
                            }
                        )
        , test "should normalize case expressions" <|
            \() ->
                """case (x) of
  ( (0x100), (b) ) :: (c) -> (1)
  a :: [ (b) ] -> (2)
  { field, a, z } -> (3)
  Basics.Just True -> (4)
  ((a) as b) -> (5)
"""
                    |> normalizeAndExpect
                        (CaseExpression
                            { cases =
                                [ ( n
                                        (UnConsPattern
                                            (n
                                                (TuplePattern
                                                    [ n (IntPattern 256)
                                                    , n (VarPattern "b")
                                                    ]
                                                )
                                            )
                                            (n (VarPattern "c"))
                                        )
                                  , n (Integer 1)
                                  )
                                , ( n
                                        (ListPattern
                                            [ n (VarPattern "a")
                                            , n (VarPattern "b")
                                            ]
                                        )
                                  , n (Integer 2)
                                  )
                                , ( n
                                        (RecordPattern
                                            [ n "a"
                                            , n "field"
                                            , n "z"
                                            ]
                                        )
                                  , n (Integer 3)
                                  )
                                , ( n
                                        (NamedPattern
                                            { moduleName = [ "Basics" ]
                                            , name = "Just"
                                            }
                                            [ n (NamedPattern { moduleName = [], name = "True" } []) ]
                                        )
                                  , n (Integer 4)
                                  )
                                , ( n (AsPattern (n (VarPattern "a")) (n "b"))
                                  , n (Integer 5)
                                  )
                                ]
                            , expression = n (FunctionOrValue [] "x")
                            }
                        )
        , test "should not change anything about >>" <|
            \() ->
                "a >> b >> c >> d"
                    |> normalizeAndExpect
                        (OperatorApplication ">>"
                            Right
                            (n (FunctionOrValue [] "a"))
                            (n
                                (OperatorApplication ">>"
                                    Right
                                    (n (FunctionOrValue [] "b"))
                                    (n
                                        (OperatorApplication ">>"
                                            Right
                                            (n (FunctionOrValue [] "c"))
                                            (n (FunctionOrValue [] "d"))
                                        )
                                    )
                                )
                            )
                        )
        , test "should switch << to use >>" <|
            \() ->
                normalizeSourceCode [] Infer.empty "d << c << b << a"
                    |> Expect.equal (normalizeSourceCode [] Infer.empty "a >> b >> c >> d")
        ]


moduleNameTests : Test
moduleNameTests =
    describe "Module name normalization"
        [ test "should normalize module name in expression (unknown)" <|
            \() ->
                "A.b"
                    |> normalizeWithModuleNamesAndExpect []
                        (FunctionOrValue [ "A" ] "b")
        , test "should normalize module name in expression (unchanged)" <|
            \() ->
                "A.b"
                    |> normalizeWithModuleNamesAndExpect
                        [ ( { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } }
                          , [ "A" ]
                          )
                        ]
                        (FunctionOrValue [ "A" ] "b")
        , test "should normalize module name in expression (aliased)" <|
            \() ->
                "A.b"
                    |> normalizeWithModuleNamesAndExpect
                        [ ( { start = { row = 2, column = 9 }, end = { row = 2, column = 12 } }
                          , [ "Something", "Else" ]
                          )
                        ]
                        (FunctionOrValue [ "Something", "Else" ] "b")
        , test "should normalize module name in expression (unqualified)" <|
            \() ->
                "b"
                    |> normalizeWithModuleNamesAndExpect
                        [ ( { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                          , [ "A" ]
                          )
                        ]
                        (FunctionOrValue [ "A" ] "b")
        , test "should normalize module name in patterns" <|
            \() ->
                """case x of
  B.Just (a) -> (1)
  Nothing -> (2)
  _ -> (3)
"""
                    |> normalizeWithModuleNamesAndExpect
                        [ ( { start = { row = 3, column = 3 }, end = { row = 3, column = 13 } }
                          , [ "Basics" ]
                          )
                        , ( { start = { row = 4, column = 3 }, end = { row = 4, column = 10 } }
                          , [ "Basics" ]
                          )
                        ]
                        (CaseExpression
                            { cases =
                                [ ( n (NamedPattern { moduleName = [ "Basics" ], name = "Just" } [ n (VarPattern "a") ])
                                  , n (Integer 1)
                                  )
                                , ( n (NamedPattern { moduleName = [ "Basics" ], name = "Nothing" } [])
                                  , n (Integer 2)
                                  )
                                , ( n AllPattern
                                  , n (Integer 3)
                                  )
                                ]
                            , expression = n (FunctionOrValue [] "x")
                            }
                        )
        ]


inferTests : Test
inferTests =
    describe "Normalization through inferred values"
        [ test "should not replace anything when nothing has been inferred" <|
            \() ->
                "a"
                    |> normalizeWithInferredAndExpect
                        []
                        []
                        (FunctionOrValue [] "a")
        , test "should replace reference when its value is known" <|
            \() ->
                "a"
                    |> normalizeWithInferredAndExpect
                        [ ( { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                          , [ "A" ]
                          )
                        ]
                        [ ( FunctionOrValue [ "A" ] "a", Infer.DTrue ) ]
                        (FunctionOrValue [ "Basics" ] "True")
        , test "should not replace reference when module name is unknown" <|
            \() ->
                "a"
                    |> normalizeWithInferredAndExpect
                        []
                        [ ( FunctionOrValue [ "A" ] "a", Infer.DTrue ) ]
                        (FunctionOrValue [] "a")
        , test "should not replace reference when module name is not the same" <|
            \() ->
                "a"
                    |> normalizeWithInferredAndExpect
                        [ ( { start = { row = 2, column = 9 }, end = { row = 2, column = 10 } }
                          , [ "B" ]
                          )
                        ]
                        [ ( FunctionOrValue [ "A" ] "a", Infer.DTrue ) ]
                        (FunctionOrValue [ "B" ] "a")
        ]


n : a -> Node a
n =
    Node Range.emptyRange


normalizeAndExpect : Expression -> String -> Expect.Expectation
normalizeAndExpect expected source =
    normalizeBase [] Infer.empty expected source


normalizeWithModuleNamesAndExpect : List ( Range, ModuleName ) -> Expression -> String -> Expect.Expectation
normalizeWithModuleNamesAndExpect moduleNames expected source =
    normalizeBase moduleNames Infer.empty expected source


normalizeWithInferredAndExpect : List ( Range, ModuleName ) -> List ( Expression, Infer.DeducedValue ) -> Expression -> String -> Expect.Expectation
normalizeWithInferredAndExpect moduleNames inferredList expected source =
    normalizeBase moduleNames (Infer.fromList inferredList) expected source


normalizeBase : List ( Range, ModuleName ) -> Infer.Inferred -> Expression -> String -> Expect.Expectation
normalizeBase moduleNames inferred expected source =
    normalizeSourceCode moduleNames inferred source
        |> Expect.equal expected


normalizeSourceCode : List ( Range, ModuleName ) -> Infer.Inferred -> String -> Expression
normalizeSourceCode moduleNames inferred source =
    ("module A exposing (..)\nvalue = " ++ source)
        |> parse
        |> getValue
        |> Normalize.normalize
            { lookupTable = ModuleNameLookupTable.createForTests [ "A" ] moduleNames
            , inferredConstants = ( inferred, [] )
            }
        |> Node.value


{-| Parse source code into a AST.
-}
parse : String -> File
parse source =
    case Parser.parse source of
        Ok ast ->
            Elm.Processing.process elmProcessContext ast

        Err _ ->
            Debug.todo "Source code given to test contained invalid syntax"


getValue : File -> Node Expression
getValue file =
    case findValueDeclaration file.declarations of
        Just expression ->
            expression

        Nothing ->
            Debug.todo "Source code did not contain a value declaration"


findValueDeclaration : List (Node Declaration) -> Maybe (Node Expression)
findValueDeclaration declarations =
    findMap
        (\node ->
            case Node.value node of
                Declaration.FunctionDeclaration { declaration } ->
                    if Node.value (Node.value declaration).name == "value" then
                        Just (Node.value declaration).expression

                    else
                        Nothing

                _ ->
                    Nothing
        )
        declarations


elmProcessContext : Elm.Processing.ProcessContext
elmProcessContext =
    Elm.Processing.init
        |> Elm.Processing.addDependency elmCore


elmCore : Elm.Dependency.Dependency
elmCore =
    { name = "elm/core"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Basics" ]
              , [ -- infix right 0 (<|) = apL
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 0
                    , operator = Node.Node Range.emptyRange "<|"
                    , function = Node.Node Range.emptyRange "apL"
                    }
                , -- infix left  0 (|>) = apR
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 0
                    , operator = Node.Node Range.emptyRange "|>"
                    , function = Node.Node Range.emptyRange "apR"
                    }
                , -- infix right 2 (||) = or
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 2
                    , operator = Node.Node Range.emptyRange "||"
                    , function = Node.Node Range.emptyRange "or"
                    }
                , -- infix right 3 (&&) = and
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 3
                    , operator = Node.Node Range.emptyRange "&&"
                    , function = Node.Node Range.emptyRange "and"
                    }
                , -- infix non   4 (==) = eq
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange "=="
                    , function = Node.Node Range.emptyRange "eq"
                    }
                , -- infix non   4 (/=) = neq
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange "/="
                    , function = Node.Node Range.emptyRange "neq"
                    }
                , -- infix non   4 (<)  = lt
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange "<"
                    , function = Node.Node Range.emptyRange "lt"
                    }
                , -- infix non   4 (>)  = gt
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange ">"
                    , function = Node.Node Range.emptyRange "gt"
                    }
                , -- infix non   4 (<=) = le
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange "<="
                    , function = Node.Node Range.emptyRange "le"
                    }
                , -- infix non   4 (>=) = ge
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Non
                    , precedence = Node.Node Range.emptyRange 4
                    , operator = Node.Node Range.emptyRange ">="
                    , function = Node.Node Range.emptyRange "ge"
                    }
                , -- infix right 5 (++) = append
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 5
                    , operator = Node.Node Range.emptyRange "++"
                    , function = Node.Node Range.emptyRange "append"
                    }
                , -- infix left  6 (+)  = add
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 6
                    , operator = Node.Node Range.emptyRange "+"
                    , function = Node.Node Range.emptyRange "add"
                    }
                , -- infix left  6 (-)  = sub
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 6
                    , operator = Node.Node Range.emptyRange "-"
                    , function = Node.Node Range.emptyRange "sub"
                    }
                , -- infix left  7 (*)  = mul
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 7
                    , operator = Node.Node Range.emptyRange "*"
                    , function = Node.Node Range.emptyRange "mul"
                    }
                , -- infix left  7 (/)  = fdiv
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 7
                    , operator = Node.Node Range.emptyRange "/"
                    , function = Node.Node Range.emptyRange "fdiv"
                    }
                , -- infix left  7 (//) = idiv
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 7
                    , operator = Node.Node Range.emptyRange "//"
                    , function = Node.Node Range.emptyRange "idiv"
                    }
                , -- infix right 8 (^)  = pow
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 8
                    , operator = Node.Node Range.emptyRange "^"
                    , function = Node.Node Range.emptyRange "pow"
                    }
                , -- infix left  9 (<<) = composeL
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 9
                    , operator = Node.Node Range.emptyRange "<<"
                    , function = Node.Node Range.emptyRange "composeL"
                    }
                , -- infix right 9 (>>) = composeR
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 9
                    , operator = Node.Node Range.emptyRange ">>"
                    , function = Node.Node Range.emptyRange "composeR"
                    }
                ]
              )
            , ( [ "List" ]
              , [ -- infix right 5 (::) = cons
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 5
                    , operator = Node.Node Range.emptyRange "::"
                    , function = Node.Node Range.emptyRange "cons"
                    }
                ]
              )
            ]
    }


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                Just value ->
                    Just value

                Nothing ->
                    findMap mapper rest
