module Simplify.Infer exposing
    ( Inferred
    , Resources
    , empty
    , get
    , getBoolean
    , getInt
    , inferForIfCondition
    , isAlwaysBoolean
    )

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Match exposing (Match(..))


type Inferred
    = Inferred (AssocList.Dict Expression Expression)


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred AssocList.empty


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    AssocList.get expr inferred


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] constantTrue inferred )
    , ( falseBranchRange, infer [ condition ] constantFalse inferred )
    ]


infer : List Expression -> Expression -> Inferred -> Inferred
infer nodes expressionValue dict =
    case nodes of
        [] ->
            dict

        node :: rest ->
            case node of
                Expression.FunctionOrValue _ _ ->
                    infer rest expressionValue (injectConstant node expressionValue dict)

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer
                        rest
                        expressionValue
                        (infer [ Node.value expression ] (inverseExpression expressionValue) dict)

                Expression.OperatorApplication "&&" _ left right ->
                    if expressionValue == constantTrue then
                        infer (Node.value left :: Node.value right :: rest) expressionValue dict

                    else
                        infer rest expressionValue dict

                Expression.OperatorApplication "||" _ left right ->
                    if expressionValue == constantFalse then
                        infer (Node.value left :: Node.value right :: rest) expressionValue dict

                    else
                        infer rest expressionValue dict

                Expression.OperatorApplication "==" _ left right ->
                    case Node.value right of
                        Expression.Integer _ ->
                            injectConstant (Node.value left) (Node.value right) dict

                        _ ->
                            infer rest expressionValue dict

                _ ->
                    infer rest expressionValue dict


constantTrue : Expression
constantTrue =
    Expression.FunctionOrValue [ "Basics" ] "True"


constantFalse : Expression
constantFalse =
    Expression.FunctionOrValue [ "Basics" ] "False"


inverseExpression : Expression -> Expression
inverseExpression expr =
    case expr of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            constantFalse

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            constantTrue

        _ ->
            expr


injectConstant : Expression -> Expression -> Inferred -> Inferred
injectConstant expression value (Inferred inferred) =
    inferred
        |> AssocList.foldl
            (\expr v acc ->
                case expr of
                    _ ->
                        AssocList.insert expr v acc
            )
            AssocList.empty
        |> AssocList.insert expression value
        |> Inferred



--


getBoolean : Resources a -> Node Expression -> Match Bool
getBoolean inferMaterial baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node of
                Just [ "Basics" ] ->
                    Determined True

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node of
                Just [ "Basics" ] ->
                    Determined False

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node
                    |> Maybe.andThen (\moduleName -> get (Expression.FunctionOrValue moduleName name) (Tuple.first inferMaterial.inferredConstants))
            of
                Just (Expression.FunctionOrValue [ "Basics" ] "True") ->
                    Determined True

                Just (Expression.FunctionOrValue [ "Basics" ] "False") ->
                    Determined False

                Just _ ->
                    Undetermined

                Nothing ->
                    Undetermined

        _ ->
            Undetermined


isAlwaysBoolean : Resources a -> Node Expression -> Match Bool
isAlwaysBoolean inferMaterial node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookupTable.moduleNameAt inferMaterial.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean inferMaterial boolean

                _ ->
                    Undetermined

        Expression.LambdaExpression { expression } ->
            getBoolean inferMaterial expression

        _ ->
            Undetermined


getInt : Resources a -> Node Expression -> Maybe Int
getInt inferMaterial baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getInt inferMaterial expr)

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node
                    |> Maybe.andThen (\moduleName -> get (Expression.FunctionOrValue moduleName name) (Tuple.first inferMaterial.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing
