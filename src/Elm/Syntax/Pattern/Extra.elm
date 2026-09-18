module Elm.Syntax.Pattern.Extra exposing (varNames)

import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.TypeInference.Type exposing (VarName)
import List.ExtraExtra


{-| Collect vars from a pattern
-}
varNames : Pattern -> List VarName
varNames pattern =
    case pattern of
        VarPattern var ->
            [ var ]

        --
        AllPattern ->
            []

        UnitPattern ->
            []

        CharPattern _ ->
            []

        StringPattern _ ->
            []

        IntPattern _ ->
            []

        HexPattern _ ->
            []

        FloatPattern _ ->
            []

        TuplePattern patterns ->
            List.ExtraExtra.fastConcatMap (\(Node.Node _ part) -> varNames part) patterns

        RecordPattern fields ->
            List.map Node.value fields

        UnConsPattern p1 p2 ->
            varNames (Node.value p1) ++ varNames (Node.value p2)

        ListPattern patterns ->
            List.ExtraExtra.fastConcatMap (\(Node.Node _ element) -> varNames element) patterns

        NamedPattern _ patterns ->
            List.ExtraExtra.fastConcatMap (\(Node.Node _ payload) -> varNames payload) patterns

        AsPattern p1 name ->
            Node.value name :: varNames (Node.value p1)

        ParenthesizedPattern p1 ->
            varNames (Node.value p1)
