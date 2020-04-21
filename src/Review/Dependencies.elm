module Review.Dependencies exposing (elmCore, elmParser, elmUrl)

import Dict
import Elm.Dependency
import Elm.Interface as Interface
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range


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


elmUrl : Elm.Dependency.Dependency
elmUrl =
    { name = "elm/url"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Url", "Parser" ]
              , [ -- infix right 7 (</>) = slash
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Right
                    , precedence = Node.Node Range.emptyRange 7
                    , operator = Node.Node Range.emptyRange "</>"
                    , function = Node.Node Range.emptyRange "slash"
                    }
                , -- infix left  8 (<?>) = questionMark
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 8
                    , operator = Node.Node Range.emptyRange "<?>"
                    , function = Node.Node Range.emptyRange "questionMark"
                    }
                ]
              )
            ]
    }


elmParser : Elm.Dependency.Dependency
elmParser =
    { name = "elm/parser"
    , version = "1.0.0"
    , interfaces =
        Dict.fromList
            [ ( [ "Parser" ]
              , [ -- infix left 5 (|=) = keeper
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 5
                    , operator = Node.Node Range.emptyRange "|="
                    , function = Node.Node Range.emptyRange "keeper"
                    }

                -- infix left 6 (|.) = ignorer
                , Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 6
                    , operator = Node.Node Range.emptyRange "|."
                    , function = Node.Node Range.emptyRange "ignorer"
                    }
                ]
              )
            , ( [ "Parser", "Advanced" ]
              , [ -- infix left 5 (|=) = keeper
                  Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 5
                    , operator = Node.Node Range.emptyRange "|="
                    , function = Node.Node Range.emptyRange "keeper"
                    }

                -- infix left 6 (|.) = ignorer
                , Interface.Operator
                    { direction = Node.Node Range.emptyRange Infix.Left
                    , precedence = Node.Node Range.emptyRange 6
                    , operator = Node.Node Range.emptyRange "|."
                    , function = Node.Node Range.emptyRange "ignorer"
                    }
                ]
              )
            ]
    }
