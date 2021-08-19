module Simplify.Normalize exposing (Comparison(..), areAllTheSame, compare, getNumberValue)

import Dict
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


areTheSame : ModuleNameLookupTable -> Node Expression -> Node Expression -> Bool
areTheSame lookupTable left right =
    normalize lookupTable left == normalize lookupTable right


areAllTheSame : ModuleNameLookupTable -> Node Expression -> List (Node Expression) -> Bool
areAllTheSame lookupTable first rest =
    let
        normalizedFirst : Node Expression
        normalizedFirst =
            normalize lookupTable first
    in
    List.all (\node -> normalize lookupTable node == normalizedFirst) rest


normalize : ModuleNameLookupTable -> Node Expression -> Node Expression
normalize lookupTable node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            normalize lookupTable expr

        Expression.Application nodes ->
            toNode (Expression.Application (List.map (normalize lookupTable) nodes))

        Expression.OperatorApplication string infixDirection left right ->
            toNode (Expression.OperatorApplication string infixDirection (normalize lookupTable left) (normalize lookupTable right))

        Expression.FunctionOrValue rawModuleName string ->
            let
                moduleName : ModuleName
                moduleName =
                    ModuleNameLookupTable.moduleNameFor lookupTable node
                        |> Maybe.withDefault rawModuleName
            in
            toNode (Expression.FunctionOrValue moduleName string)

        Expression.IfBlock cond then_ else_ ->
            toNode (Expression.IfBlock (normalize lookupTable cond) (normalize lookupTable then_) (normalize lookupTable else_))

        Expression.Negation expr ->
            toNode (Expression.Negation (normalize lookupTable expr))

        Expression.TupledExpression nodes ->
            toNode (Expression.TupledExpression (List.map (normalize lookupTable) nodes))

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
                                                        , arguments = List.map normalizePattern declaration.arguments
                                                        , expression = normalize lookupTable declaration.expression
                                                        }
                                                }
                                            )

                                    Expression.LetDestructuring pattern expr ->
                                        toNode (Expression.LetDestructuring (normalizePattern pattern) (normalize lookupTable expr))
                            )
                            letBlock.declarations
                    , expression = normalize lookupTable letBlock.expression
                    }
                )

        Expression.CaseExpression caseBlock ->
            toNode
                (Expression.CaseExpression
                    { cases = List.map (\( pattern, expr ) -> ( normalizePattern pattern, normalize lookupTable expr )) caseBlock.cases
                    , expression = toNode <| Node.value caseBlock.expression
                    }
                )

        Expression.LambdaExpression lambda ->
            toNode
                (Expression.LambdaExpression
                    { args = List.map normalizePattern lambda.args
                    , expression = normalize lookupTable lambda.expression
                    }
                )

        Expression.ListExpr nodes ->
            toNode (Expression.ListExpr (List.map (normalize lookupTable) nodes))

        Expression.RecordAccess expr (Node _ field) ->
            toNode (Expression.RecordAccess (normalize lookupTable expr) (toNode field))

        Expression.RecordExpr nodes ->
            nodes
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize lookupTable expr ))
                |> Expression.RecordExpr
                |> toNode

        Expression.RecordUpdateExpression (Node _ value) nodes ->
            nodes
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize lookupTable expr ))
                |> Expression.RecordUpdateExpression (toNode value)
                |> toNode

        expr ->
            toNode expr


normalizePattern : Node Pattern -> Node Pattern
normalizePattern node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            toNode (Pattern.TuplePattern (List.map normalizePattern patterns))

        Pattern.RecordPattern fields ->
            toNode (Pattern.RecordPattern (List.map (\(Node _ field) -> toNode field) fields))

        Pattern.UnConsPattern element list ->
            toNode (Pattern.UnConsPattern (normalizePattern element) (normalizePattern list))

        Pattern.ListPattern patterns ->
            toNode (Pattern.ListPattern (List.map normalizePattern patterns))

        Pattern.NamedPattern qualifiedNameRef patterns ->
            toNode (Pattern.NamedPattern qualifiedNameRef (List.map normalizePattern patterns))

        Pattern.AsPattern pattern (Node _ asName) ->
            toNode (Pattern.AsPattern (normalizePattern pattern) (toNode asName))

        Pattern.ParenthesizedPattern pattern ->
            normalizePattern pattern

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


compare : ModuleNameLookupTable -> Node Expression -> Node Expression -> Comparison
compare lookupTable leftNode right =
    compareHelp lookupTable leftNode right True


compareHelp : ModuleNameLookupTable -> Node Expression -> Node Expression -> Bool -> Comparison
compareHelp lookupTable leftNode right canFlip =
    let
        fallback : Node Expression -> Comparison
        fallback rightNode =
            if canFlip then
                compareHelp lookupTable rightNode leftNode False

            else if areTheSame lookupTable leftNode right then
                ConfirmedEquality

            else
                Unconfirmed
    in
    case Node.value leftNode of
        Expression.ParenthesizedExpression expr ->
            compareHelp lookupTable expr right canFlip

        Expression.Integer left ->
            compareNumbers (Basics.toFloat left) right

        Expression.Floatable left ->
            compareNumbers left right

        Expression.Hex left ->
            compareNumbers (Basics.toFloat left) right

        Expression.Negation left ->
            case getNumberValue left of
                Just leftValue ->
                    compareNumbers -leftValue right

                Nothing ->
                    fallback right

        Expression.OperatorApplication leftOp _ leftLeft leftRight ->
            if List.member leftOp [ "+", "-", "*", "/" ] then
                case getNumberValue leftNode of
                    Just leftValue ->
                        case getNumberValue right of
                            Just rightValue ->
                                fromEquality (leftValue == rightValue)

                            Nothing ->
                                fallback right

                    Nothing ->
                        fallback right

            else
                case Node.value (removeParens right) of
                    Expression.OperatorApplication rightOp _ rightLeft rightRight ->
                        if leftOp == rightOp then
                            compareEqualityOfAll
                                lookupTable
                                [ leftLeft, leftRight ]
                                [ rightLeft, rightRight ]

                        else
                            fallback right

                    _ ->
                        fallback right

        Expression.Literal left ->
            case Node.value (removeParens right) of
                Expression.Literal rightValue ->
                    fromEquality (left == rightValue)

                _ ->
                    fallback right

        Expression.CharLiteral left ->
            case Node.value (removeParens right) of
                Expression.CharLiteral rightValue ->
                    fromEquality (left == rightValue)

                _ ->
                    fallback right

        Expression.FunctionOrValue _ leftName ->
            let
                right_ : Node Expression
                right_ =
                    removeParens right
            in
            case Node.value right_ of
                Expression.FunctionOrValue _ rightName ->
                    if
                        isSameReference
                            lookupTable
                            ( Node.range leftNode, leftName )
                            ( Node.range right_, rightName )
                    then
                        ConfirmedEquality

                    else
                        fallback right_

                _ ->
                    fallback right

        Expression.ListExpr leftList ->
            case Node.value (removeParens right) of
                Expression.ListExpr rightList ->
                    if List.length leftList /= List.length rightList then
                        ConfirmedInequality

                    else
                        compareLists lookupTable leftList rightList ConfirmedEquality

                _ ->
                    fallback right

        Expression.TupledExpression leftList ->
            case Node.value (removeParens right) of
                Expression.TupledExpression rightList ->
                    compareLists lookupTable leftList rightList ConfirmedEquality

                _ ->
                    fallback right

        Expression.RecordExpr leftList ->
            case Node.value (removeParens right) of
                Expression.RecordExpr rightList ->
                    compareRecords lookupTable leftList rightList ConfirmedEquality

                _ ->
                    fallback right

        Expression.RecordUpdateExpression leftBaseValue leftList ->
            case Node.value (removeParens right) of
                Expression.RecordUpdateExpression rightBaseValue rightList ->
                    if Node.value leftBaseValue == Node.value rightBaseValue then
                        compareRecords lookupTable leftList rightList ConfirmedEquality

                    else
                        compareRecords lookupTable leftList rightList Unconfirmed

                _ ->
                    fallback right

        Expression.Application leftArgs ->
            case Node.value (removeParens right) of
                Expression.Application rightArgs ->
                    compareEqualityOfAll lookupTable leftArgs rightArgs

                _ ->
                    fallback right

        Expression.RecordAccess leftExpr leftName ->
            case Node.value (removeParens right) of
                Expression.RecordAccess rightExpr rightName ->
                    if Node.value leftName == Node.value rightName then
                        compareHelp lookupTable leftExpr rightExpr canFlip

                    else
                        Unconfirmed

                _ ->
                    fallback right

        Expression.UnitExpr ->
            ConfirmedEquality

        Expression.IfBlock leftCond leftThen leftElse ->
            case Node.value (removeParens right) of
                Expression.IfBlock rightCond rightThen rightElse ->
                    compareEqualityOfAll
                        lookupTable
                        [ leftCond, leftThen, leftElse ]
                        [ rightCond, rightThen, rightElse ]

                _ ->
                    fallback right

        _ ->
            fallback right


isSameReference : ModuleNameLookupTable -> ( Range, String ) -> ( Range, String ) -> Bool
isSameReference lookupTable ( leftFnRange, leftFnName ) ( rightFnRange, rightFnName ) =
    if leftFnName == rightFnName then
        Maybe.map2 (==)
            (ModuleNameLookupTable.moduleNameAt lookupTable leftFnRange)
            (ModuleNameLookupTable.moduleNameAt lookupTable rightFnRange)
            |> Maybe.withDefault False

    else
        False


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


compareLists : ModuleNameLookupTable -> List (Node Expression) -> List (Node Expression) -> Comparison -> Comparison
compareLists lookupTable leftList rightList acc =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareHelp lookupTable left right True of
                ConfirmedEquality ->
                    compareLists lookupTable restOfLeft restOfRight acc

                ConfirmedInequality ->
                    ConfirmedInequality

                Unconfirmed ->
                    compareLists lookupTable restOfLeft restOfRight Unconfirmed

        _ ->
            acc


compareEqualityOfAll : ModuleNameLookupTable -> List (Node Expression) -> List (Node Expression) -> Comparison
compareEqualityOfAll lookupTable leftList rightList =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareHelp lookupTable left right True of
                ConfirmedEquality ->
                    compareEqualityOfAll lookupTable restOfLeft restOfRight

                ConfirmedInequality ->
                    Unconfirmed

                Unconfirmed ->
                    Unconfirmed

        _ ->
            ConfirmedEquality


type RecordFieldComparison
    = MissingOtherValue
    | HasBothValues (Node Expression) (Node Expression)


compareRecords : ModuleNameLookupTable -> List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareRecords lookupTable leftList rightList acc =
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
    compareRecordFields lookupTable recordFieldComparisons acc


compareRecordFields : ModuleNameLookupTable -> List RecordFieldComparison -> Comparison -> Comparison
compareRecordFields lookupTable recordFieldComparisons acc =
    case recordFieldComparisons of
        [] ->
            acc

        MissingOtherValue :: rest ->
            compareRecordFields lookupTable rest Unconfirmed

        (HasBothValues a b) :: rest ->
            case compare lookupTable a b of
                ConfirmedInequality ->
                    ConfirmedInequality

                ConfirmedEquality ->
                    compareRecordFields lookupTable rest acc

                Unconfirmed ->
                    compareRecordFields lookupTable rest Unconfirmed


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
