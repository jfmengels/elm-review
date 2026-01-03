module Simplify.Evaluate exposing (getBoolean, getInt, getNumber)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize


getBoolean : Normalize.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    case AstHelpers.removeParens baseNode of
        Node referenceRange (Expression.FunctionOrValue _ name) ->
            case ModuleNameLookupTable.moduleNameAt resources.lookupTable referenceRange of
                Just moduleOrigin ->
                    case name of
                        "True" ->
                            case moduleOrigin of
                                [ "Basics" ] ->
                                    determinedTrue

                                _ ->
                                    Undetermined

                        "False" ->
                            case moduleOrigin of
                                [ "Basics" ] ->
                                    determinedFalse

                                _ ->
                                    Undetermined

                        _ ->
                            case
                                Infer.getBoolean (Expression.FunctionOrValue moduleOrigin name)
                                    (Tuple.first resources.inferredConstants)
                            of
                                Just bool ->
                                    Determined bool

                                Nothing ->
                                    Undetermined

                Nothing ->
                    Undetermined

        unparenthesizedNode ->
            case
                Infer.getBoolean
                    (Node.value (Normalize.normalize resources unparenthesizedNode))
                    (Tuple.first resources.inferredConstants)
            of
                Just bool ->
                    Determined bool

                Nothing ->
                    Undetermined


determinedTrue : Match Bool
determinedTrue =
    Determined True


determinedFalse : Match Bool
determinedFalse =
    Determined False


getInt : Infer.Resources a -> Node Expression -> Maybe Int
getInt resources baseNode =
    case AstHelpers.removeParens baseNode of
        Node _ (Expression.Integer n) ->
            Just n

        Node _ (Expression.Hex n) ->
            Just n

        Node _ (Expression.Negation expr) ->
            Maybe.map negate (getInt resources expr)

        Node referenceRange (Expression.FunctionOrValue _ name) ->
            -- note, when this comment was written, getAsExpression never even returned Integer
            case
                ModuleNameLookupTable.moduleNameAt resources.lookupTable referenceRange
                    |> Maybe.andThen (\moduleOrigin -> Infer.getAsExpression (Expression.FunctionOrValue moduleOrigin name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing


getNumber : Infer.Resources a -> Node Expression -> Maybe Float
getNumber resources baseNode =
    let
        unparenthesized : Node Expression
        unparenthesized =
            AstHelpers.removeParens baseNode
    in
    case getInt resources unparenthesized of
        Just int ->
            Just (Basics.toFloat int)

        Nothing ->
            case unparenthesized of
                Node _ (Expression.Floatable float) ->
                    Just float

                Node variableRange (Expression.FunctionOrValue _ name) ->
                    case
                        ModuleNameLookupTable.moduleNameAt resources.lookupTable variableRange
                            |> Maybe.andThen (\moduleOrigin -> Infer.get (Expression.FunctionOrValue moduleOrigin name) (Tuple.first resources.inferredConstants))
                    of
                        Just (Infer.DNumber float) ->
                            Just float

                        _ ->
                            Nothing

                _ ->
                    Nothing
