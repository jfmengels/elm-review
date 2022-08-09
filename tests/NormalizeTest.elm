module NormalizeTest exposing (all)

import Dict
import Elm.Dependency
import Elm.Interface as Interface
import Elm.Parser as Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Expect
import Review.Test
import Review.Test.Dependencies
import Simplify exposing (defaults, ignoreCaseOfForTypes, rule)
import Simplify.Normalize as Normalize
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Normalize"
        [ test "should not report configuration error if all ignored constructors exist" <|
            \() ->
                """module A exposing (..)
value = (1)
"""
                    |> parse
                    |> getValue
                    |> Normalize.normalize { lookupTable = ModuleNameLookupTable.empty }
                    |> Expect.equal (Expression.Integer 1)
        ]


{-| Parse source code into a AST.
-}
parse : String -> File
parse source =
    case Parser.parse source of
        Ok ast ->
            Elm.Processing.process elmProcessContext ast

        Err _ ->
            Debug.todo "Source code given to test contained invalid syntax"


getValue : File -> Expression
getValue file =
    case findValueDeclaration file.declarations of
        Just expression ->
            expression

        Nothing ->
            Debug.todo "Source code did not contain a value declaration"


findValueDeclaration : List (Node Declaration) -> Maybe Expression
findValueDeclaration declarations =
    findMap
        (\node ->
            case Node.value node of
                Declaration.FunctionDeclaration { declaration } ->
                    if Node.value (Node.value declaration).name == "value" then
                        Just (Node.value (Node.value declaration).expression)

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
