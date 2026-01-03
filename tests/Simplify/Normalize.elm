module Simplify.Normalize exposing (Comparison(..), Resources, areAllTheSameAs, areTheSame, compare, compareWithoutNormalization, isSpecificUnappliedBinaryOperation, normalize, normalizeButKeepRange)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Writer
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer


type alias Resources a =
    Infer.Resources (AstHelpers.ReduceLambdaResources a)


areTheSame : Resources a -> Node Expression -> Node Expression -> Bool
areTheSame resources a b =
    normalize resources a == normalize resources b


areAllTheSameAs : Resources res -> Node Expression -> (a -> Node Expression) -> List a -> Bool
areAllTheSameAs resources first restElementToExpressionNode rest =
    let
        normalizedFirst : Node Expression
        normalizedFirst =
            normalize resources first
    in
    List.all
        (\node ->
            normalize resources (restElementToExpressionNode node)
                == normalizedFirst
        )
        rest


normalize : Resources a -> Node Expression -> Node Expression
normalize resources node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            normalize resources expr

        Expression.Application nodes ->
            case nodes of
                fn :: firstArg :: afterFirstArg ->
                    let
                        normalizedArg1 : Node Expression
                        normalizedArg1 =
                            normalize resources firstArg
                    in
                    toNode
                        (case normalize resources fn of
                            Node _ (Expression.RecordAccessFunction fieldAccess) ->
                                let
                                    recordAccess : Expression
                                    recordAccess =
                                        Expression.RecordAccess normalizedArg1 (toNode (String.dropLeft 1 fieldAccess))
                                in
                                case afterFirstArg of
                                    [] ->
                                        recordAccess

                                    secondArg :: argsAfterSecond ->
                                        Expression.Application
                                            (toNode recordAccess
                                                :: normalize resources secondArg
                                                :: List.map (\arg -> normalize resources arg) argsAfterSecond
                                            )

                            normalizedFn ->
                                Expression.Application
                                    (normalizedFn
                                        :: normalizedArg1
                                        :: List.map (\arg -> normalize resources arg) afterFirstArg
                                    )
                        )

                _ ->
                    node

        Expression.OperatorApplication "<|" _ function extraArgument ->
            addToFunctionCall
                (normalize resources function)
                (normalize resources extraArgument)

        Expression.OperatorApplication "|>" _ extraArgument function ->
            addToFunctionCall
                (normalize resources function)
                (normalize resources extraArgument)

        Expression.OperatorApplication "<<" _ left right ->
            toNode (Expression.OperatorApplication ">>" normalizedInfixDirection (normalize resources right) (normalize resources left))

        Expression.OperatorApplication "::" _ head tail ->
            let
                normalizedHead : Node Expression
                normalizedHead =
                    normalize resources head

                normalizedTail : Node Expression
                normalizedTail =
                    normalize resources tail
            in
            toNode
                (case Node.value normalizedTail of
                    Expression.ListExpr tailElements ->
                        Expression.ListExpr (normalizedHead :: tailElements)

                    _ ->
                        Expression.OperatorApplication "::" normalizedInfixDirection normalizedHead normalizedTail
                )

        Expression.OperatorApplication ">" _ left right ->
            toNode (Expression.OperatorApplication "<" normalizedInfixDirection (normalize resources right) (normalize resources left))

        Expression.OperatorApplication ">=" _ left right ->
            toNode (Expression.OperatorApplication "<=" normalizedInfixDirection (normalize resources right) (normalize resources left))

        Expression.OperatorApplication operator _ left right ->
            createOperation operator (normalize resources left) (normalize resources right)

        Expression.FunctionOrValue rawModuleName string ->
            Expression.FunctionOrValue
                (ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.withDefault rawModuleName
                )
                string
                |> toNodeAndInfer resources

        Expression.IfBlock cond then_ else_ ->
            let
                reverseIfConditionIsNegated : Node Expression -> Node Expression -> Node Expression -> Node Expression
                reverseIfConditionIsNegated condArg thenArg elseArg =
                    case Node.value condArg of
                        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), negatedCondition ] ->
                            reverseIfConditionIsNegated negatedCondition elseArg thenArg

                        _ ->
                            toNode (Expression.IfBlock condArg thenArg elseArg)
            in
            reverseIfConditionIsNegated
                (normalize resources cond)
                (normalize resources then_)
                (normalize resources else_)

        Expression.Negation expr ->
            let
                normalized : Node Expression
                normalized =
                    normalize resources expr
            in
            case Node.value normalized of
                Expression.Integer int ->
                    toNode (Expression.Integer -int)

                Expression.Floatable float ->
                    toNode (Expression.Floatable -float)

                Expression.Negation subExpr ->
                    subExpr

                _ ->
                    toNode (Expression.Negation normalized)

        Expression.TupledExpression nodes ->
            toNode (Expression.TupledExpression (List.map (\part -> normalize resources part) nodes))

        Expression.LetExpression letBlock ->
            toNode
                (Expression.LetExpression
                    { declarations =
                        List.map
                            (\decl ->
                                case Node.value decl of
                                    Expression.LetFunction function ->
                                        let
                                            declaration : Expression.FunctionImplementation
                                            declaration =
                                                Node.value function.declaration
                                        in
                                        toNode
                                            (Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    toNode
                                                        { name = toNode (Node.value declaration.name)
                                                        , arguments = List.map (\param -> normalizePattern resources.lookupTable param) declaration.arguments
                                                        , expression = normalize resources declaration.expression
                                                        }
                                                }
                                            )

                                    Expression.LetDestructuring pattern expr ->
                                        toNode (Expression.LetDestructuring (normalizePattern resources.lookupTable pattern) (normalize resources expr))
                            )
                            letBlock.declarations
                    , expression = normalize resources letBlock.expression
                    }
                )

        Expression.CaseExpression caseBlock ->
            toNode
                (Expression.CaseExpression
                    { cases = List.map (\( pattern, expr ) -> ( normalizePattern resources.lookupTable pattern, normalize resources expr )) caseBlock.cases
                    , expression = normalize resources caseBlock.expression
                    }
                )

        Expression.LambdaExpression lambda ->
            let
                lambdaPatternsNormalized : List (Node Pattern)
                lambdaPatternsNormalized =
                    List.map (\arg -> normalizePattern resources.lookupTable arg) lambda.args
            in
            toNode
                (reduceLambda resources
                    (case normalize resources lambda.expression of
                        Node _ (Expression.LambdaExpression resultLambda) ->
                            { args = lambdaPatternsNormalized ++ resultLambda.args
                            , expression = resultLambda.expression
                            }

                        resultNormalized ->
                            { args = lambdaPatternsNormalized
                            , expression = resultNormalized
                            }
                    )
                )

        Expression.ListExpr elements ->
            toNode (Expression.ListExpr (List.map (\element -> normalize resources element) elements))

        Expression.RecordAccess expr (Node _ field) ->
            toNodeAndInfer resources
                (Expression.RecordAccess (normalize resources expr) (toNode field))

        Expression.RecordExpr fields ->
            fields
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize resources expr ))
                |> Expression.RecordExpr
                |> toNode

        Expression.RecordUpdateExpression (Node _ value) nodes ->
            nodes
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize resources expr ))
                |> Expression.RecordUpdateExpression (toNode value)
                |> toNode

        Expression.Hex int ->
            toNode (Expression.Integer int)

        expr ->
            toNode expr


operatorIsSymmetrical : String -> Bool
operatorIsSymmetrical operator =
    case operator of
        "+" ->
            True

        "*" ->
            True

        "||" ->
            True

        "&&" ->
            True

        "==" ->
            True

        "/=" ->
            True

        _ ->
            False


normalizeButKeepRange : Resources a -> Node Expression -> Node Expression
normalizeButKeepRange checkInfo node =
    Node (Node.range node) (Node.value (normalize checkInfo node))


toNodeAndInfer : Infer.Resources a -> Expression -> Node Expression
toNodeAndInfer resources element =
    toNode
        (case Infer.getAsExpression element (Tuple.first resources.inferredConstants) of
            Just value ->
                value

            Nothing ->
                element
        )


toComparable : Node Expression -> String
toComparable a =
    Elm.Writer.write (Elm.Writer.writeExpression a)


{-| Expects normalized left and right
-}
createOperation : String -> Node Expression -> Node Expression -> Node Expression
createOperation operator left right =
    toNode
        (if
            operatorIsSymmetrical operator
                && (toComparable left > toComparable right)
         then
            Expression.OperatorApplication operator normalizedInfixDirection right left

         else
            Expression.OperatorApplication operator normalizedInfixDirection left right
        )


normalizedInfixDirection : Infix.InfixDirection
normalizedInfixDirection =
    Infix.Non


addToFunctionCall : Node Expression -> Node Expression -> Node Expression
addToFunctionCall functionCall extraArgument =
    case Node.value functionCall of
        Expression.ParenthesizedExpression expr ->
            addToFunctionCall expr extraArgument

        Expression.Application [ Node _ (Expression.PrefixOperator operator), left ] ->
            createOperation operator left extraArgument

        Expression.Application (fnCall :: args) ->
            Expression.Application (fnCall :: (args ++ [ extraArgument ]))
                |> toNode

        Expression.LetExpression { declarations, expression } ->
            Expression.LetExpression { declarations = declarations, expression = addToFunctionCall expression extraArgument }
                |> toNode

        Expression.IfBlock condition ifBranch elseBranch ->
            Expression.IfBlock condition (addToFunctionCall ifBranch extraArgument) (addToFunctionCall elseBranch extraArgument)
                |> toNode

        Expression.CaseExpression { expression, cases } ->
            Expression.CaseExpression { expression = expression, cases = List.map (\( cond, expr ) -> ( cond, addToFunctionCall expr extraArgument )) cases }
                |> toNode

        Expression.RecordAccessFunction fieldAccess ->
            Expression.RecordAccess extraArgument (toNode (String.dropLeft 1 fieldAccess))
                |> toNode

        _ ->
            Expression.Application [ functionCall, extraArgument ]
                |> toNode


{-| Whether a given expression can be called with 2 operands and produces the same result as an operation with a given operator.
Is either a function reducible to the operator in prefix notation `(op)` or a lambda `\a b -> a op b`.
-}
isSpecificUnappliedBinaryOperation : String -> Resources a -> Node Expression -> Bool
isSpecificUnappliedBinaryOperation operator resources expressionNode =
    Node.value (normalize resources expressionNode) == Expression.PrefixOperator operator


reduceLambda : Resources a -> Expression.Lambda -> Expression
reduceLambda resources lambda =
    case Node.value lambda.expression of
        Expression.Application (called :: callArguments) ->
            let
                reduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                reduced =
                    AstHelpers.reduceLambda resources lambda callArguments

                reducedResultCall : Expression
                reducedResultCall =
                    case reduced.callArguments of
                        [] ->
                            Node.value called

                        _ :: _ ->
                            Expression.Application (called :: reduced.callArguments)
            in
            case reduced.lambdaPatterns of
                [] ->
                    reducedResultCall

                _ :: _ ->
                    Expression.LambdaExpression
                        { args = reduced.lambdaPatterns
                        , expression = Node.empty reducedResultCall
                        }

        Expression.OperatorApplication operator _ left right ->
            let
                reduced :
                    { lambdaPatterns : List (Node Pattern)
                    , result : Expression
                    }
                reduced =
                    let
                        leftRightReduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                        leftRightReduced =
                            AstHelpers.reduceLambda resources lambda [ left, right ]
                    in
                    case leftRightReduced.callArguments of
                        [ callArgument ] ->
                            { lambdaPatterns = leftRightReduced.lambdaPatterns
                            , result = Expression.Application [ toNode (Expression.PrefixOperator operator), callArgument ]
                            }

                        [] ->
                            { lambdaPatterns = leftRightReduced.lambdaPatterns
                            , result = Expression.PrefixOperator operator
                            }

                        _ :: _ :: _ ->
                            if operatorIsSymmetrical operator then
                                let
                                    rightLeftReduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                                    rightLeftReduced =
                                        AstHelpers.reduceLambda resources lambda [ right, left ]
                                in
                                case rightLeftReduced.callArguments of
                                    [ callArgument ] ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result = Expression.Application [ toNode (Expression.PrefixOperator operator), callArgument ]
                                        }

                                    [] ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result = Expression.PrefixOperator operator
                                        }

                                    _ :: _ :: _ ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result =
                                            -- same as before
                                            Node.value lambda.expression
                                        }

                            else
                                { lambdaPatterns = leftRightReduced.lambdaPatterns
                                , result =
                                    -- same as before
                                    Node.value lambda.expression
                                }
            in
            case reduced.lambdaPatterns of
                [] ->
                    reduced.result

                _ :: _ ->
                    Expression.LambdaExpression
                        { args = reduced.lambdaPatterns
                        , expression = Node.empty reduced.result
                        }

        _ ->
            Expression.LambdaExpression lambda


normalizePattern : ModuleNameLookupTable -> Node Pattern -> Node Pattern
normalizePattern lookupTable node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            normalizePattern lookupTable pattern

        Pattern.TuplePattern patterns ->
            toNode (Pattern.TuplePattern (List.map (\part -> normalizePattern lookupTable part) patterns))

        Pattern.RecordPattern fields ->
            toNode
                (Pattern.RecordPattern
                    (fields
                        |> List.sortBy (\(Node _ fieldName) -> fieldName)
                        |> List.map (\(Node _ field) -> toNode field)
                    )
                )

        Pattern.UnConsPattern element list ->
            toNode
                (case normalizePattern lookupTable list of
                    Node _ (Pattern.ListPattern elements) ->
                        Pattern.ListPattern (normalizePattern lookupTable element :: elements)

                    normalizedList ->
                        Pattern.UnConsPattern (normalizePattern lookupTable element) normalizedList
                )

        Pattern.ListPattern elements ->
            toNode (Pattern.ListPattern (List.map (\element -> normalizePattern lookupTable element) elements))

        Pattern.NamedPattern qualifiedNameRef values ->
            let
                nameRef : Pattern.QualifiedNameRef
                nameRef =
                    { moduleName =
                        ModuleNameLookupTable.moduleNameFor lookupTable node
                            |> Maybe.withDefault qualifiedNameRef.moduleName
                    , name = qualifiedNameRef.name
                    }
            in
            toNode (Pattern.NamedPattern nameRef (List.map (\value -> normalizePattern lookupTable value) values))

        Pattern.AsPattern pattern (Node _ asName) ->
            toNode (Pattern.AsPattern (normalizePattern lookupTable pattern) (toNode asName))

        Pattern.HexPattern int ->
            toNode (Pattern.IntPattern int)

        pattern ->
            toNode pattern


toNode : a -> Node a
toNode value =
    Node Range.emptyRange value



-- COMPARE


type Comparison
    = ConfirmedEquality
    | ConfirmedInequality
    | Unconfirmed


compare : Resources a -> Node Expression -> Node Expression -> Comparison
compare resources leftNode right =
    compareHelp
        (normalize resources leftNode)
        (normalize resources right)
        True


compareWithoutNormalization : Node Expression -> Node Expression -> Comparison
compareWithoutNormalization leftNode right =
    compareHelp leftNode right True


compareHelp : Node Expression -> Node Expression -> Bool -> Comparison
compareHelp leftNode right canFlip =
    let
        fallback : () -> Comparison
        fallback () =
            if canFlip then
                compareHelp right leftNode False

            else if leftNode == right then
                ConfirmedEquality

            else
                Unconfirmed
    in
    case Node.value leftNode of
        Expression.Integer left ->
            compareNumbers (Basics.toFloat left) right

        Expression.Floatable left ->
            compareNumbers left right

        Expression.Negation left ->
            case Node.value (removeParens right) of
                Expression.Negation rightValue ->
                    compareHelp left rightValue canFlip

                _ ->
                    fallback ()

        Expression.OperatorApplication leftOp _ leftLeft leftRight ->
            if isNumberOperator leftOp then
                case getNumberValue leftNode of
                    Just leftValue ->
                        case getNumberValue right of
                            Just rightValue ->
                                fromEquality (leftValue == rightValue)

                            Nothing ->
                                fallback ()

                    Nothing ->
                        fallback ()

            else
                case Node.value (removeParens right) of
                    Expression.OperatorApplication rightOp _ rightLeft rightRight ->
                        if leftOp == rightOp then
                            compareAll2Help leftLeft rightLeft leftRight rightRight

                        else
                            fallback ()

                    _ ->
                        fallback ()

        Expression.Literal left ->
            case Node.value (removeParens right) of
                Expression.Literal rightValue ->
                    fromEquality (left == rightValue)

                _ ->
                    fallback ()

        Expression.CharLiteral left ->
            case Node.value (removeParens right) of
                Expression.CharLiteral rightValue ->
                    fromEquality (left == rightValue)

                _ ->
                    fallback ()

        Expression.FunctionOrValue moduleNameLeft leftName ->
            case Node.value right of
                Expression.FunctionOrValue moduleNameRight rightName ->
                    if leftName == rightName && moduleNameRight == moduleNameLeft then
                        ConfirmedEquality

                    else
                        fallback ()

                _ ->
                    fallback ()

        Expression.ListExpr leftList ->
            case Node.value (removeParens right) of
                Expression.ListExpr rightList ->
                    compareLists leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.TupledExpression leftList ->
            case Node.value (removeParens right) of
                Expression.TupledExpression rightList ->
                    compareLists leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordExpr leftList ->
            case Node.value (removeParens right) of
                Expression.RecordExpr rightList ->
                    compareRecords leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordUpdateExpression leftBaseValue leftList ->
            case Node.value (removeParens right) of
                Expression.RecordUpdateExpression rightBaseValue rightList ->
                    compareRecords leftList
                        rightList
                        (if Node.value leftBaseValue == Node.value rightBaseValue then
                            ConfirmedEquality

                         else
                            Unconfirmed
                        )

                _ ->
                    fallback ()

        Expression.Application (leftCalled :: leftArg0 :: leftArg1Up) ->
            case Node.value (removeParens right) of
                Expression.Application (rightCalled :: rightArg0 :: rightArg1Up) ->
                    case compareAll2Help leftCalled rightCalled leftArg0 rightArg0 of
                        ConfirmedEquality ->
                            compareAllConfirmedEqualityElseUnconfirmedHelp leftArg1Up rightArg1Up

                        _ ->
                            Unconfirmed

                _ ->
                    fallback ()

        Expression.RecordAccess leftExpr leftName ->
            case Node.value (removeParens right) of
                Expression.RecordAccess rightExpr rightName ->
                    if Node.value leftName == Node.value rightName then
                        compareHelp leftExpr rightExpr canFlip

                    else
                        Unconfirmed

                _ ->
                    fallback ()

        Expression.UnitExpr ->
            ConfirmedEquality

        Expression.IfBlock leftCond leftThen leftElse ->
            case Node.value (removeParens right) of
                Expression.IfBlock rightCond rightThen rightElse ->
                    case compareHelp leftCond rightCond True of
                        ConfirmedEquality ->
                            case compareHelp leftThen rightThen True of
                                ConfirmedInequality ->
                                    case compareHelp leftElse rightElse True of
                                        ConfirmedInequality ->
                                            ConfirmedInequality

                                        _ ->
                                            Unconfirmed

                                ConfirmedEquality ->
                                    case compareHelp leftElse rightElse True of
                                        ConfirmedEquality ->
                                            ConfirmedEquality

                                        _ ->
                                            Unconfirmed

                                Unconfirmed ->
                                    Unconfirmed

                        ConfirmedInequality ->
                            -- notice that we instead compare the then with else branches
                            case compareHelp leftThen rightElse True of
                                ConfirmedInequality ->
                                    case compareHelp leftElse rightThen True of
                                        ConfirmedInequality ->
                                            ConfirmedInequality

                                        _ ->
                                            Unconfirmed

                                ConfirmedEquality ->
                                    case compareHelp leftElse rightThen True of
                                        ConfirmedEquality ->
                                            ConfirmedEquality

                                        _ ->
                                            Unconfirmed

                                Unconfirmed ->
                                    Unconfirmed

                        Unconfirmed ->
                            Unconfirmed

                _ ->
                    fallback ()

        _ ->
            fallback ()


{-| For consistency with `getNumberValue`, this does not
(yet?) include `//`
-}
isNumberOperator : String -> Bool
isNumberOperator operator =
    case operator of
        "+" ->
            True

        "-" ->
            True

        "*" ->
            True

        "/" ->
            True

        _ ->
            False


compareAll2Help : Node Expression -> Node Expression -> Node Expression -> Node Expression -> Comparison
compareAll2Help left0 right0 left1 right1 =
    case compareHelp left0 right0 True of
        ConfirmedInequality ->
            ConfirmedInequality

        ConfirmedEquality ->
            compareHelp left1 right1 True

        Unconfirmed ->
            case compareHelp left1 right1 True of
                ConfirmedInequality ->
                    ConfirmedInequality

                _ ->
                    Unconfirmed


compareLists : List (Node Expression) -> List (Node Expression) -> Comparison -> Comparison
compareLists leftList rightList soFar =
    case leftList of
        [] ->
            case rightList of
                [] ->
                    soFar

                _ :: _ ->
                    ConfirmedInequality

        left :: restOfLeft ->
            case rightList of
                [] ->
                    ConfirmedInequality

                right :: restOfRight ->
                    case compareWithoutNormalization left right of
                        ConfirmedInequality ->
                            ConfirmedInequality

                        ConfirmedEquality ->
                            compareLists restOfLeft restOfRight soFar

                        Unconfirmed ->
                            compareLists restOfLeft restOfRight Unconfirmed


compareAllConfirmedEqualityElseUnconfirmedHelp : List (Node Expression) -> List (Node Expression) -> Comparison
compareAllConfirmedEqualityElseUnconfirmedHelp leftList rightList =
    case leftList of
        [] ->
            case rightList of
                [] ->
                    ConfirmedEquality

                _ :: _ ->
                    Unconfirmed

        left :: restOfLeft ->
            case rightList of
                [] ->
                    Unconfirmed

                right :: restOfRight ->
                    case compareHelp left right True of
                        ConfirmedEquality ->
                            compareAllConfirmedEqualityElseUnconfirmedHelp restOfLeft restOfRight

                        _ ->
                            Unconfirmed


compareNumbers : Float -> Node Expression -> Comparison
compareNumbers leftValue right =
    case getNumberValue right of
        Just rightValue ->
            fromEquality (leftValue == rightValue)

        Nothing ->
            Unconfirmed


getNumberValue : Node Expression -> Maybe Float
getNumberValue node =
    case Node.value node of
        Expression.Integer value ->
            Just (Basics.toFloat value)

        Expression.Hex int ->
            Just (Basics.toFloat int)

        Expression.Floatable float ->
            Just float

        Expression.ParenthesizedExpression expr ->
            getNumberValue expr

        Expression.LetExpression { expression } ->
            getNumberValue expression

        Expression.OperatorApplication "+" _ left right ->
            case getNumberValue left of
                Nothing ->
                    Nothing

                Just leftNumber ->
                    Maybe.map (\rightNumber -> leftNumber + rightNumber)
                        (getNumberValue right)

        Expression.OperatorApplication "-" _ left right ->
            case getNumberValue left of
                Nothing ->
                    Nothing

                Just leftNumber ->
                    Maybe.map (\rightNumber -> leftNumber - rightNumber)
                        (getNumberValue right)

        Expression.OperatorApplication "*" _ left right ->
            case getNumberValue left of
                Nothing ->
                    Nothing

                Just leftNumber ->
                    Maybe.map (\rightNumber -> leftNumber * rightNumber)
                        (getNumberValue right)

        Expression.OperatorApplication "/" _ left right ->
            case getNumberValue left of
                Nothing ->
                    Nothing

                Just leftNumber ->
                    Maybe.map (\rightNumber -> leftNumber / rightNumber)
                        (getNumberValue right)

        Expression.Negation expr ->
            getNumberValue expr
                |> Maybe.map negate

        _ ->
            Nothing


type RecordFieldComparison
    = MissingOtherValue
    | HasBothValues (Node Expression) (Node Expression)


compareRecords : List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareRecords leftList rightList acc =
    let
        leftFields : Dict String (Node Expression)
        leftFields =
            List.foldl
                (\(Node _ ( Node _ fieldName, fieldValue )) soFar ->
                    Dict.insert fieldName fieldValue soFar
                )
                Dict.empty
                leftList

        rightFields : Dict String (Node Expression)
        rightFields =
            List.foldl
                (\(Node _ ( Node _ fieldName, fieldValue )) soFar ->
                    Dict.insert fieldName fieldValue soFar
                )
                Dict.empty
                rightList

        recordFieldComparisons : List RecordFieldComparison
        recordFieldComparisons =
            Dict.merge
                (\key _ -> Dict.insert key MissingOtherValue)
                (\key a b -> Dict.insert key (HasBothValues a b))
                (\key _ -> Dict.insert key MissingOtherValue)
                leftFields
                rightFields
                Dict.empty
                |> Dict.values
    in
    compareRecordFields recordFieldComparisons acc


compareRecordFields : List RecordFieldComparison -> Comparison -> Comparison
compareRecordFields recordFieldComparisons acc =
    case recordFieldComparisons of
        [] ->
            acc

        MissingOtherValue :: rest ->
            compareRecordFields rest Unconfirmed

        (HasBothValues a b) :: rest ->
            case compareHelp a b True of
                ConfirmedInequality ->
                    ConfirmedInequality

                ConfirmedEquality ->
                    compareRecordFields rest acc

                Unconfirmed ->
                    compareRecordFields rest Unconfirmed


fromEquality : Bool -> Comparison
fromEquality bool =
    if bool then
        ConfirmedEquality

    else
        ConfirmedInequality


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node
