module Simplify.Normalize exposing (Comparison(..), areAllTheSame, compare, compareWithoutNormalization, getNumberValue, normalize)

import Dict
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Writer
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.Infer as Infer


areAllTheSame : Infer.Resources a -> Node Expression -> List (Node Expression) -> Bool
areAllTheSame resources first rest =
    let
        normalizedFirst : Node Expression
        normalizedFirst =
            normalize resources first
    in
    List.all (\node -> normalize resources node == normalizedFirst) rest


normalize : Infer.Resources a -> Node Expression -> Node Expression
normalize resources node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            normalize resources expr

        Expression.Application nodes ->
            case nodes of
                fn :: arg1 :: restOrArgs ->
                    let
                        normalizedArg1 : Node Expression
                        normalizedArg1 =
                            normalize resources arg1
                    in
                    case normalize resources fn of
                        Node _ (Expression.RecordAccessFunction fieldAccess) ->
                            let
                                recordAccess : Node Expression
                                recordAccess =
                                    Expression.RecordAccess normalizedArg1 (toNode (String.dropLeft 1 fieldAccess))
                                        |> toNode
                            in
                            if List.isEmpty restOrArgs then
                                recordAccess

                            else
                                (recordAccess :: List.map (normalize resources) restOrArgs)
                                    |> Expression.Application
                                    |> toNode

                        normalizedFn ->
                            (normalizedFn :: normalizedArg1 :: List.map (normalize resources) restOrArgs)
                                |> Expression.Application
                                |> toNode

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
            toNode (Expression.OperatorApplication ">>" Infix.Right (normalize resources right) (normalize resources left))

        Expression.OperatorApplication "::" infixDirection element list ->
            let
                normalizedElement : Node Expression
                normalizedElement =
                    normalize resources element

                normalizedList : Node Expression
                normalizedList =
                    normalize resources list
            in
            case Node.value normalizedList of
                Expression.ListExpr elements ->
                    toNode (Expression.ListExpr (normalizedElement :: elements))

                _ ->
                    toNode (Expression.OperatorApplication "::" infixDirection normalizedElement normalizedList)

        Expression.OperatorApplication ">" infixDirection left right ->
            toNode (Expression.OperatorApplication "<" infixDirection (normalize resources right) (normalize resources left))

        Expression.OperatorApplication ">=" infixDirection left right ->
            toNode (Expression.OperatorApplication "<=" infixDirection (normalize resources right) (normalize resources left))

        Expression.OperatorApplication operator infixDirection l r ->
            let
                left : Node Expression
                left =
                    normalize resources l

                right : Node Expression
                right =
                    normalize resources r
            in
            if List.member operator [ "+", "*", "||", "&&", "==", "/=" ] && toComparable left > toComparable right then
                toNode (Expression.OperatorApplication operator infixDirection right left)

            else
                toNode (Expression.OperatorApplication operator infixDirection left right)

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
            toNode (Expression.TupledExpression (List.map (normalize resources) nodes))

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
                                                        , arguments = List.map (normalizePattern resources.lookupTable) declaration.arguments
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
            toNode
                (Expression.LambdaExpression
                    { args = List.map (normalizePattern resources.lookupTable) lambda.args
                    , expression = normalize resources lambda.expression
                    }
                )

        Expression.ListExpr nodes ->
            toNode (Expression.ListExpr (List.map (normalize resources) nodes))

        Expression.RecordAccess expr (Node _ field) ->
            toNode (Expression.RecordAccess (normalize resources expr) (toNode field))

        Expression.RecordExpr nodes ->
            nodes
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
            Expression.Integer int
                |> toNode

        expr ->
            toNode expr


toNodeAndInfer : Infer.Resources a -> Expression -> Node Expression
toNodeAndInfer resources element =
    case Infer.get element (Tuple.first resources.inferredConstants) of
        Just value ->
            toNode value

        Nothing ->
            toNode element


toComparable : Node Expression -> String
toComparable a =
    Elm.Writer.write (Elm.Writer.writeExpression a)


addToFunctionCall : Node Expression -> Node Expression -> Node Expression
addToFunctionCall functionCall extraArgument =
    case Node.value functionCall of
        Expression.ParenthesizedExpression expr ->
            addToFunctionCall expr extraArgument

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


normalizePattern : ModuleNameLookupTable -> Node Pattern -> Node Pattern
normalizePattern lookupTable node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            toNode (Pattern.TuplePattern (List.map (normalizePattern lookupTable) patterns))

        Pattern.RecordPattern fields ->
            toNode
                (Pattern.RecordPattern
                    (fields
                        |> List.sortBy (\(Node _ fieldName) -> fieldName)
                        |> List.map (\(Node _ field) -> toNode field)
                    )
                )

        Pattern.UnConsPattern element list ->
            case normalizePattern lookupTable list of
                Node _ (Pattern.ListPattern elements) ->
                    toNode (Pattern.ListPattern (normalizePattern lookupTable element :: elements))

                normalizedList ->
                    toNode (Pattern.UnConsPattern (normalizePattern lookupTable element) normalizedList)

        Pattern.ListPattern patterns ->
            toNode (Pattern.ListPattern (List.map (normalizePattern lookupTable) patterns))

        Pattern.NamedPattern qualifiedNameRef patterns ->
            let
                nameRef : Pattern.QualifiedNameRef
                nameRef =
                    { moduleName =
                        ModuleNameLookupTable.moduleNameFor lookupTable node
                            |> Maybe.withDefault qualifiedNameRef.moduleName
                    , name = qualifiedNameRef.name
                    }
            in
            toNode (Pattern.NamedPattern nameRef (List.map (normalizePattern lookupTable) patterns))

        Pattern.AsPattern pattern (Node _ asName) ->
            toNode (Pattern.AsPattern (normalizePattern lookupTable pattern) (toNode asName))

        Pattern.ParenthesizedPattern pattern ->
            normalizePattern lookupTable pattern

        Pattern.HexPattern int ->
            toNode (Pattern.IntPattern int)

        pattern ->
            toNode pattern


toNode : a -> Node a
toNode =
    Node Range.emptyRange



-- COMPARE


type Comparison
    = ConfirmedEquality
    | ConfirmedInequality
    | Unconfirmed


compare : Infer.Resources a -> Node Expression -> Node Expression -> Comparison
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
            if List.member leftOp [ "+", "-", "*", "/" ] then
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
                            compareEqualityOfAll
                                [ leftLeft, leftRight ]
                                [ rightLeft, rightRight ]

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
                    if List.length leftList /= List.length rightList then
                        ConfirmedInequality

                    else
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
                    if Node.value leftBaseValue == Node.value rightBaseValue then
                        compareRecords leftList rightList ConfirmedEquality

                    else
                        compareRecords leftList rightList Unconfirmed

                _ ->
                    fallback ()

        Expression.Application leftArgs ->
            case Node.value (removeParens right) of
                Expression.Application rightArgs ->
                    compareEqualityOfAll leftArgs rightArgs

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
                    compareEqualityOfAll
                        [ leftCond, leftThen, leftElse ]
                        [ rightCond, rightThen, rightElse ]

                _ ->
                    fallback ()

        _ ->
            fallback ()


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
            Maybe.map2 (+)
                (getNumberValue left)
                (getNumberValue right)

        Expression.OperatorApplication "-" _ left right ->
            Maybe.map2 (-)
                (getNumberValue left)
                (getNumberValue right)

        Expression.OperatorApplication "*" _ left right ->
            Maybe.map2 (*)
                (getNumberValue left)
                (getNumberValue right)

        Expression.OperatorApplication "/" _ left right ->
            Maybe.map2 (/)
                (getNumberValue left)
                (getNumberValue right)

        Expression.Negation expr ->
            getNumberValue expr
                |> Maybe.map negate

        _ ->
            Nothing


compareLists : List (Node Expression) -> List (Node Expression) -> Comparison -> Comparison
compareLists leftList rightList acc =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareWithoutNormalization left right of
                ConfirmedEquality ->
                    compareLists restOfLeft restOfRight acc

                ConfirmedInequality ->
                    ConfirmedInequality

                Unconfirmed ->
                    compareLists restOfLeft restOfRight Unconfirmed

        _ ->
            acc


compareEqualityOfAll : List (Node Expression) -> List (Node Expression) -> Comparison
compareEqualityOfAll leftList rightList =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareHelp left right True of
                ConfirmedEquality ->
                    compareEqualityOfAll restOfLeft restOfRight

                ConfirmedInequality ->
                    Unconfirmed

                Unconfirmed ->
                    Unconfirmed

        _ ->
            ConfirmedEquality


type RecordFieldComparison
    = MissingOtherValue
    | HasBothValues (Node Expression) (Node Expression)


compareRecords : List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareRecords leftList rightList acc =
    let
        leftFields : List ( String, Node Expression )
        leftFields =
            List.map (Node.value >> Tuple.mapFirst Node.value) leftList

        rightFields : List ( String, Node Expression )
        rightFields =
            List.map (Node.value >> Tuple.mapFirst Node.value) rightList

        recordFieldComparisons : List RecordFieldComparison
        recordFieldComparisons =
            Dict.merge
                (\key _ -> Dict.insert key MissingOtherValue)
                (\key a b -> Dict.insert key (HasBothValues a b))
                (\key _ -> Dict.insert key MissingOtherValue)
                (Dict.fromList leftFields)
                (Dict.fromList rightFields)
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
