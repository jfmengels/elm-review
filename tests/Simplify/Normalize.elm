module Simplify.Normalize exposing (Comparison(..), areAllTheSame, compare, getNumberValue, normalize)

import Dict
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
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

        Expression.OperatorApplication string infixDirection left right ->
            toNode (Expression.OperatorApplication string infixDirection (normalize resources left) (normalize resources right))

        Expression.FunctionOrValue rawModuleName string ->
            case ModuleNameLookupTable.moduleNameFor resources.lookupTable node of
                Just moduleName ->
                    case Infer.get (Expression.FunctionOrValue moduleName string) (Tuple.first resources.inferredConstants) of
                        Just value ->
                            toNode value

                        Nothing ->
                            toNode (Expression.FunctionOrValue moduleName string)

                Nothing ->
                    toNode (Expression.FunctionOrValue rawModuleName string)

        Expression.IfBlock cond then_ else_ ->
            toNode (Expression.IfBlock (normalize resources cond) (normalize resources then_) (normalize resources else_))

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
                                                        , arguments = List.map normalizePattern declaration.arguments
                                                        , expression = normalize resources declaration.expression
                                                        }
                                                }
                                            )

                                    Expression.LetDestructuring pattern expr ->
                                        toNode (Expression.LetDestructuring (normalizePattern pattern) (normalize resources expr))
                            )
                            letBlock.declarations
                    , expression = normalize resources letBlock.expression
                    }
                )

        Expression.CaseExpression caseBlock ->
            toNode
                (Expression.CaseExpression
                    { cases = List.map (\( pattern, expr ) -> ( normalizePattern pattern, normalize resources expr )) caseBlock.cases
                    , expression = normalize resources caseBlock.expression
                    }
                )

        Expression.LambdaExpression lambda ->
            toNode
                (Expression.LambdaExpression
                    { args = List.map normalizePattern lambda.args
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
        resources
        (normalize resources leftNode)
        (normalize resources right)
        True


compareHelp : Infer.Resources a -> Node Expression -> Node Expression -> Bool -> Comparison
compareHelp resources leftNode right canFlip =
    let
        fallback : () -> Comparison
        fallback () =
            if canFlip then
                compareHelp resources right leftNode False

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
            case getNumberValue left of
                Just leftValue ->
                    compareNumbers -leftValue right

                Nothing ->
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
                                resources
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

        Expression.FunctionOrValue _ leftName ->
            case Node.value right of
                Expression.FunctionOrValue _ rightName ->
                    if
                        isSameReference
                            resources.lookupTable
                            ( Node.range leftNode, leftName )
                            ( Node.range right, rightName )
                    then
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
                        compareLists resources leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.TupledExpression leftList ->
            case Node.value (removeParens right) of
                Expression.TupledExpression rightList ->
                    compareLists resources leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordExpr leftList ->
            case Node.value (removeParens right) of
                Expression.RecordExpr rightList ->
                    compareRecords resources leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordUpdateExpression leftBaseValue leftList ->
            case Node.value (removeParens right) of
                Expression.RecordUpdateExpression rightBaseValue rightList ->
                    if Node.value leftBaseValue == Node.value rightBaseValue then
                        compareRecords resources leftList rightList ConfirmedEquality

                    else
                        compareRecords resources leftList rightList Unconfirmed

                _ ->
                    fallback ()

        Expression.Application leftArgs ->
            case Node.value (removeParens right) of
                Expression.Application rightArgs ->
                    compareEqualityOfAll resources leftArgs rightArgs

                _ ->
                    fallback ()

        Expression.RecordAccess leftExpr leftName ->
            case Node.value (removeParens right) of
                Expression.RecordAccess rightExpr rightName ->
                    if Node.value leftName == Node.value rightName then
                        compareHelp resources leftExpr rightExpr canFlip

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
                        resources
                        [ leftCond, leftThen, leftElse ]
                        [ rightCond, rightThen, rightElse ]

                _ ->
                    fallback ()

        _ ->
            fallback ()


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


compareLists : Infer.Resources a -> List (Node Expression) -> List (Node Expression) -> Comparison -> Comparison
compareLists resources leftList rightList acc =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareHelp resources left right True of
                ConfirmedEquality ->
                    compareLists resources restOfLeft restOfRight acc

                ConfirmedInequality ->
                    ConfirmedInequality

                Unconfirmed ->
                    compareLists resources restOfLeft restOfRight Unconfirmed

        _ ->
            acc


compareEqualityOfAll : Infer.Resources a -> List (Node Expression) -> List (Node Expression) -> Comparison
compareEqualityOfAll resources leftList rightList =
    case ( leftList, rightList ) of
        ( left :: restOfLeft, right :: restOfRight ) ->
            case compareHelp resources left right True of
                ConfirmedEquality ->
                    compareEqualityOfAll resources restOfLeft restOfRight

                ConfirmedInequality ->
                    Unconfirmed

                Unconfirmed ->
                    Unconfirmed

        _ ->
            ConfirmedEquality


type RecordFieldComparison
    = MissingOtherValue
    | HasBothValues (Node Expression) (Node Expression)


compareRecords : Infer.Resources a -> List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareRecords resources leftList rightList acc =
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
    compareRecordFields resources recordFieldComparisons acc


compareRecordFields : Infer.Resources a -> List RecordFieldComparison -> Comparison -> Comparison
compareRecordFields resources recordFieldComparisons acc =
    case recordFieldComparisons of
        [] ->
            acc

        MissingOtherValue :: rest ->
            compareRecordFields resources rest Unconfirmed

        (HasBothValues a b) :: rest ->
            case compare resources a b of
                ConfirmedInequality ->
                    ConfirmedInequality

                ConfirmedEquality ->
                    compareRecordFields resources rest acc

                Unconfirmed ->
                    compareRecordFields resources rest Unconfirmed


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
