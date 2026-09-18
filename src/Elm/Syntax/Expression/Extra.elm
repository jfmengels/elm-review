module Elm.Syntax.Expression.Extra exposing (functionName, referencedNames)

import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern.Extra
import Elm.TypeInference.Type exposing (VarName)
import List.ExtraExtra
import Set exposing (Set)


functionName : Function -> String
functionName function =
    function.declaration
        |> Node.value
        |> .name
        |> Node.value


{-| Collects value names (not patterns).
Useful for binding-group SCC logic later.
-}
referencedNames : Expression -> List ( Maybe ModuleName, VarName )
referencedNames expression =
    referencedNamesIn Set.empty expression


{-| Like `referencedNames`, but excludes unqualified names bound by an enclosing
lambda, pattern or nested `let`.

This is important for binding-group logic: a locally shadowed name must not
create a dependency on outer declaration of the same name.

-}
referencedNamesIn : Set VarName -> Expression -> List ( Maybe ModuleName, VarName )
referencedNamesIn bound expression =
    let
        e : Node Expression -> List ( Maybe ModuleName, VarName )
        e node =
            referencedNamesIn bound (Node.value node)

        many : List (Node Expression) -> List ( Maybe ModuleName, VarName )
        many nodes =
            List.ExtraExtra.fastConcatMap e nodes
    in
    case expression of
        FunctionOrValue moduleName varName ->
            if List.isEmpty moduleName && Set.member varName bound then
                []

            else
                [ ( if List.isEmpty moduleName then
                        Nothing

                    else
                        Just moduleName
                  , varName
                  )
                ]

        PrefixOperator operator ->
            [ ( Nothing, operator ) ]

        OperatorApplication operator _ e1 e2 ->
            ( Nothing, operator ) :: e e1 ++ e e2

        Application nodes ->
            many nodes

        IfBlock e1 e2 e3 ->
            many [ e1, e2, e3 ]

        Negation e1 ->
            e e1

        TupledExpression nodes ->
            many nodes

        ParenthesizedExpression e1 ->
            e e1

        LetExpression letBlock ->
            let
                letBound : Set VarName
                letBound =
                    letBlock.declarations
                        |> List.ExtraExtra.fastConcatMap
                            (Node.value
                                >> (\declaration ->
                                        case declaration of
                                            LetFunction fn ->
                                                [ functionName fn ]

                                            LetDestructuring patternNode _ ->
                                                Elm.Syntax.Pattern.Extra.varNames (Node.value patternNode)
                                   )
                            )
                        |> Set.fromList

                nestedBound : Set VarName
                nestedBound =
                    Set.union bound letBound

                declRefs : Node LetDeclaration -> List ( Maybe ModuleName, VarName )
                declRefs declNode =
                    case Node.value declNode of
                        LetFunction fn ->
                            let
                                argumentNames : List String
                                argumentNames =
                                    (Node.value fn.declaration).arguments
                                        |> List.ExtraExtra.fastConcatMap (Node.value >> Elm.Syntax.Pattern.Extra.varNames)
                            in
                            referencedNamesIn (Set.union nestedBound (Set.fromList argumentNames)) (Node.value (Node.value fn.declaration).expression)

                        LetDestructuring _ e1 ->
                            referencedNamesIn nestedBound (Node.value e1)
            in
            List.ExtraExtra.fastConcatMap declRefs letBlock.declarations ++ referencedNamesIn nestedBound (Node.value letBlock.expression)

        CaseExpression caseBlock ->
            e caseBlock.expression
                ++ List.ExtraExtra.fastConcatMap
                    (\( pattern, body ) ->
                        referencedNamesIn
                            (Set.union bound (Set.fromList (Elm.Syntax.Pattern.Extra.varNames (Node.value pattern))))
                            (Node.value body)
                    )
                    caseBlock.cases

        LambdaExpression lambda ->
            let
                argumentNames : List String
                argumentNames =
                    lambda.args
                        |> List.ExtraExtra.fastConcatMap (Node.value >> Elm.Syntax.Pattern.Extra.varNames)
            in
            referencedNamesIn (Set.union bound (Set.fromList argumentNames)) (Node.value lambda.expression)

        RecordExpr setters ->
            setters |> List.ExtraExtra.fastConcatMap (Node.value >> Tuple.second >> e)

        ListExpr nodes ->
            many nodes

        RecordAccess recordNode _ ->
            e recordNode

        RecordAccessFunction _ ->
            []

        RecordUpdateExpression recordVarNode setters ->
            ( Nothing, Node.value recordVarNode )
                :: (setters |> List.ExtraExtra.fastConcatMap (Node.value >> Tuple.second >> e))

        GLSLExpression _ ->
            []

        UnitExpr ->
            []

        Integer _ ->
            []

        Hex _ ->
            []

        Floatable _ ->
            []

        Literal _ ->
            []

        CharLiteral _ ->
            []

        Operator _ ->
            []
