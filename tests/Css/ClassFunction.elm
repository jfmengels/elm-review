module Css.ClassFunction exposing (CssArgument(..), fromLiteral)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)


type CssArgument
    = Literal String
    | Variable Range
    | MissingArgument Int


fromLiteral : Node Expression -> CssArgument
fromLiteral node =
    case Node.value node of
        Expression.Literal str ->
            Literal str

        _ ->
            Variable (Node.range node)
