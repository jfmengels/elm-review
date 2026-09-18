module Elm.Syntax.Pattern.Extra exposing (insertVarNamesIntoSet, varNames)

import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.TypeInference.Type exposing (VarName)
import List.ExtraExtra
import Set exposing (Set)


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


{-| Collect vars from a pattern into a given Set
-}
insertVarNamesIntoSet : Pattern -> Set VarName -> Set VarName
insertVarNamesIntoSet pattern acc =
    case pattern of
        VarPattern var ->
            Set.insert var acc

        AllPattern ->
            acc

        UnitPattern ->
            acc

        CharPattern _ ->
            acc

        StringPattern _ ->
            acc

        IntPattern _ ->
            acc

        HexPattern _ ->
            acc

        FloatPattern _ ->
            acc

        TuplePattern patterns ->
            List.foldl
                (\(Node.Node _ part) accAcrossParts -> insertVarNamesIntoSet part accAcrossParts)
                acc
                patterns

        RecordPattern fields ->
            List.foldl
                (\(Node.Node _ fieldName) accAcrossFields -> Set.insert fieldName accAcrossFields)
                acc
                fields

        UnConsPattern p1 p2 ->
            insertVarNamesIntoSet (Node.value p1) (insertVarNamesIntoSet (Node.value p2) acc)

        ListPattern patterns ->
            List.foldl
                (\(Node.Node _ element) accAcrossElements -> insertVarNamesIntoSet element accAcrossElements)
                acc
                patterns

        NamedPattern _ patterns ->
            List.foldl
                (\(Node.Node _ payload) accAcrossPayloads -> insertVarNamesIntoSet payload accAcrossPayloads)
                acc
                patterns

        AsPattern p1 name ->
            Set.insert (Node.value name) (insertVarNamesIntoSet (Node.value p1) acc)

        ParenthesizedPattern p1 ->
            insertVarNamesIntoSet (Node.value p1) acc
