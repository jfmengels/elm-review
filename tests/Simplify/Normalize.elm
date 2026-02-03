module Simplify.Normalize exposing
    ( Resources, normalizeExpression, normalizeExpressionButKeepRange
    , Comparison(..), areAllTheSameAs, areTheSame, compare, compareExistingNormals
    , getBool, getInt, getNumber, getUnappliedBinaryOperation, isSpecificUnappliedBinaryOperation
    )

{-| Bring expressions to a normal form,
including simple evaluation using [`Simplify.Infer`](Simplify-Infer)

@docs Resources, normalizeExpression, normalizeExpressionButKeepRange


## equality

@docs Comparison, areAllTheSameAs, areTheSame, compare, compareExistingNormals


## parse

@docs getBool, getInt, getNumber, getUnappliedBinaryOperation, isSpecificUnappliedBinaryOperation

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Writer
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer


type alias Resources a =
    Infer.Resources (AstHelpers.ReduceLambdaResources a)


areTheSame : Resources a -> Node Expression -> Node Expression -> Bool
areTheSame resources a b =
    normalizeExpression resources a == normalizeExpression resources b


areAllTheSameAs : Resources res -> Node Expression -> (a -> Node Expression) -> List a -> Bool
areAllTheSameAs resources first restElementToExpressionNode rest =
    let
        normalizedFirst : Expression
        normalizedFirst =
            normalizeExpression resources first
    in
    List.all
        (\node ->
            normalizeExpression resources (restElementToExpressionNode node)
                == normalizedFirst
        )
        rest


normalizeExpressionNode : Resources a -> Node Expression -> Node Expression
normalizeExpressionNode resources node =
    Node.empty (normalizeExpression resources node)


normalizeExpression : Resources a -> Node Expression -> Expression
normalizeExpression resources (Node expressionRange expression) =
    case expression of
        Expression.ParenthesizedExpression inParens ->
            normalizeExpression resources inParens

        Expression.Application nodes ->
            case nodes of
                functionNode :: firstArg :: afterFirstArg ->
                    infer resources
                        (List.foldl
                            (\arg functionNormalSoFar ->
                                addToFunctionCall resources
                                    (Node.empty functionNormalSoFar)
                                    (normalizeExpressionNode resources arg)
                            )
                            (addToFunctionCall resources
                                (normalizeExpressionNode resources functionNode)
                                (normalizeExpressionNode resources firstArg)
                            )
                            afterFirstArg
                        )

                _ ->
                    expression

        Expression.OperatorApplication operator _ left right ->
            createOperation resources operator (normalizeExpressionNode resources left) (normalizeExpressionNode resources right)

        Expression.FunctionOrValue rawModuleName string ->
            Expression.FunctionOrValue
                (ModuleNameLookupTable.moduleNameAt resources.lookupTable expressionRange
                    |> Maybe.withDefault rawModuleName
                )
                string
                |> infer resources

        Expression.IfBlock cond then_ else_ ->
            let
                reverseIfConditionIsNegated : Node Expression -> Node Expression -> Node Expression -> Expression
                reverseIfConditionIsNegated condArg thenArg elseArg =
                    case Node.value condArg of
                        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), negatedCondition ] ->
                            reverseIfConditionIsNegated negatedCondition elseArg thenArg

                        _ ->
                            Expression.IfBlock condArg thenArg elseArg
            in
            reverseIfConditionIsNegated
                (normalizeExpressionNode resources cond)
                (normalizeExpressionNode resources then_)
                (normalizeExpressionNode resources else_)

        Expression.Negation expr ->
            createNegation (normalizeExpression resources expr)

        Expression.TupledExpression nodes ->
            Expression.TupledExpression (List.map (\part -> normalizeExpressionNode resources part) nodes)

        Expression.LetExpression letBlock ->
            Expression.LetExpression
                { declarations =
                    -- possible improvement: sort
                    letBlock.declarations
                        |> List.map
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Expression.LetFunction function ->
                                        let
                                            declaration : Expression.FunctionImplementation
                                            declaration =
                                                Node.value function.declaration
                                        in
                                        Node.empty
                                            (Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    Node.empty
                                                        { name =
                                                            -- possible improvement: assign indices and change the expression scope accordingly
                                                            Node.empty (Node.value declaration.name)
                                                        , arguments = List.map (\param -> normalizePatternNode resources.lookupTable param) declaration.arguments
                                                        , expression = normalizeExpressionNode resources declaration.expression
                                                        }
                                                }
                                            )

                                    Expression.LetDestructuring pattern expr ->
                                        Node.empty (Expression.LetDestructuring (normalizePatternNode resources.lookupTable pattern) (normalizeExpressionNode resources expr))
                            )
                        |> List.sortBy
                            (\(Node _ letDeclaration) ->
                                case letDeclaration of
                                    Expression.LetFunction letVariableDeclaration ->
                                        ( 0, Node.value (Node.value letVariableDeclaration.declaration).name )

                                    Expression.LetDestructuring destructuringPattern _ ->
                                        ( 1, patternToComparable destructuringPattern )
                            )
                , expression = normalizeExpressionNode resources letBlock.expression
                }

        Expression.CaseExpression caseBlock ->
            Expression.CaseExpression
                { cases =
                    caseBlock.cases
                        |> List.map (\( pattern, expr ) -> ( normalizePatternNode resources.lookupTable pattern, normalizeExpressionNode resources expr ))
                        |> List.sortBy (\( pattern, _ ) -> patternToComparable pattern)
                , expression = normalizeExpressionNode resources caseBlock.expression
                }

        Expression.LambdaExpression lambda ->
            let
                lambdaPatternsNormalized : List (Node Pattern)
                lambdaPatternsNormalized =
                    List.map (\arg -> normalizePatternNode resources.lookupTable arg) lambda.args
            in
            reduceLambda resources
                (case normalizeExpression resources lambda.expression of
                    Expression.LambdaExpression resultLambda ->
                        { args = lambdaPatternsNormalized ++ resultLambda.args
                        , expression = resultLambda.expression
                        }

                    resultNormalized ->
                        { args = lambdaPatternsNormalized
                        , expression = Node.empty resultNormalized
                        }
                )

        Expression.ListExpr elements ->
            Expression.ListExpr (List.map (\element -> normalizeExpressionNode resources element) elements)

        Expression.RecordAccess expr (Node _ field) ->
            infer resources
                (Expression.RecordAccess (normalizeExpressionNode resources expr) (Node.empty field))

        Expression.RecordExpr fields ->
            fields
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> Node.empty ( Node.empty fieldName, normalizeExpressionNode resources expr ))
                |> Expression.RecordExpr

        Expression.RecordUpdateExpression (Node _ value) nodes ->
            nodes
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> Node.empty ( Node.empty fieldName, normalizeExpressionNode resources expr ))
                |> Expression.RecordUpdateExpression (Node.empty value)

        Expression.Hex int ->
            Expression.Floatable (Basics.toFloat int)

        Expression.Integer int ->
            Expression.Floatable (Basics.toFloat int)

        expr ->
            expr


normalizeExpressionButKeepRange : Resources a -> Node Expression -> Node Expression
normalizeExpressionButKeepRange checkInfo node =
    Node (Node.range node) (normalizeExpression checkInfo node)


infer : Infer.Resources a -> Expression -> Expression
infer resources element =
    case Infer.getAsExpression element (Tuple.first resources.inferredConstants) of
        Just value ->
            value

        Nothing ->
            element


expressionToComparable : Node Expression -> String
expressionToComparable expressionNode =
    Elm.Writer.write (Elm.Writer.writeExpression expressionNode)


patternToComparable : Node Pattern -> String
patternToComparable patternNode =
    Elm.Writer.write (Elm.Writer.writePattern patternNode)


{-| Expects normalized expression
-}
createNegation : Expression -> Expression
createNegation normalized =
    case normalized of
        Expression.Floatable float ->
            Expression.Floatable -float

        Expression.Negation subExpr ->
            Node.value subExpr

        _ ->
            Expression.Negation (Node.empty normalized)


{-| Expects normalized left and right
-}
createOperation : Infer.Resources a -> String -> Node Expression -> Node Expression -> Expression
createOperation resources operator left right =
    case operator of
        "<|" ->
            infer resources (addToFunctionCall resources left right)

        "|>" ->
            infer resources (addToFunctionCall resources right left)

        "<<" ->
            Expression.OperatorApplication ">>" normalizedInfixDirection right left

        "::" ->
            case Node.value right of
                Expression.ListExpr tailElements ->
                    Expression.ListExpr (left :: tailElements)

                _ ->
                    Expression.OperatorApplication "::" normalizedInfixDirection left right

        "==" ->
            infer resources (createFallbackOperation operator left right)

        "/=" ->
            infer resources (createFallbackOperation operator left right)

        "&&" ->
            infer resources (createFallbackOperation operator left right)

        "||" ->
            infer resources (createFallbackOperation operator left right)

        ">" ->
            infer resources (Expression.OperatorApplication "<" normalizedInfixDirection right left)

        ">=" ->
            infer resources (Expression.OperatorApplication "<=" normalizedInfixDirection right left)

        "<" ->
            infer resources (Expression.OperatorApplication "<" normalizedInfixDirection left right)

        "<=" ->
            infer resources (Expression.OperatorApplication "<=" normalizedInfixDirection left right)

        "+" ->
            createNumberOperation (+) operator left right

        "-" ->
            createNumberOperation (-) operator left right

        "*" ->
            createNumberOperation (*) operator left right

        "/" ->
            createNumberOperation (/) operator left right

        "//" ->
            createNumberOperation
                (\l r ->
                    -- not truncate because that would drop bits above 32
                    Basics.toFloat (Basics.round l // Basics.round r)
                )
                operator
                left
                right

        _ ->
            createFallbackOperation operator left right


createFallbackOperation : String -> Node Expression -> Node Expression -> Expression
createFallbackOperation operator left right =
    if
        operatorIsSymmetrical operator
            && (expressionToComparable left > expressionToComparable right)
    then
        Expression.OperatorApplication operator normalizedInfixDirection right left

    else
        Expression.OperatorApplication operator normalizedInfixDirection left right


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


createNumberOperation : (Float -> Float -> Float) -> String -> Node Expression -> Node Expression -> Expression
createNumberOperation numberOperation operatorSymbol left right =
    case Node.value left of
        Expression.Floatable leftNumber ->
            case Node.value right of
                Expression.Floatable rightNumber ->
                    Expression.Floatable (numberOperation leftNumber rightNumber)

                _ ->
                    createFallbackOperation operatorSymbol left right

        _ ->
            createFallbackOperation operatorSymbol left right


normalizedInfixDirection : Infix.InfixDirection
normalizedInfixDirection =
    Infix.Non


addToFunctionCall : Infer.Resources a -> Node Expression -> Node Expression -> Expression
addToFunctionCall resources functionCall extraArgument =
    case Node.value functionCall of
        Expression.ParenthesizedExpression expr ->
            addToFunctionCall resources expr extraArgument

        Expression.Application [ Node _ (Expression.PrefixOperator operator), left ] ->
            createOperation resources operator left extraArgument

        Expression.Application (fnCall :: args) ->
            Expression.Application (fnCall :: (args ++ [ extraArgument ]))

        Expression.LetExpression { declarations, expression } ->
            Expression.LetExpression
                { declarations = declarations
                , expression = Node.empty (addToFunctionCall resources expression extraArgument)
                }

        Expression.IfBlock condition ifBranch elseBranch ->
            Expression.IfBlock condition
                (Node.empty (addToFunctionCall resources ifBranch extraArgument))
                (Node.empty (addToFunctionCall resources elseBranch extraArgument))

        Expression.CaseExpression { expression, cases } ->
            Expression.CaseExpression
                { expression = expression
                , cases =
                    List.map
                        (\( cond, expr ) ->
                            ( cond, Node.empty (addToFunctionCall resources expr extraArgument) )
                        )
                        cases
                }

        Expression.RecordAccessFunction fieldAccess ->
            infer resources
                (Expression.RecordAccess extraArgument (Node.empty (String.dropLeft 1 fieldAccess)))

        Expression.FunctionOrValue [ "Basics" ] "not" ->
            createNot resources extraArgument

        Expression.FunctionOrValue [ "Basics" ] "negate" ->
            createNegation (Node.value extraArgument)

        Expression.FunctionOrValue [ "List" ] "singleton" ->
            Expression.ListExpr [ extraArgument ]

        _ ->
            Expression.Application [ functionCall, extraArgument ]


{-| Expects normal expression
-}
createNot : Infer.Resources a -> Node Expression -> Expression
createNot resources expressionNormalNodeInNot =
    case Node.value expressionNormalNodeInNot of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            expressionFalse

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            expressionTrue

        Expression.OperatorApplication operator _ left right ->
            case operator of
                "==" ->
                    createOperation resources "/=" left right

                "/=" ->
                    createOperation resources "==" left right

                ">" ->
                    createOperation resources "<=" left right

                ">=" ->
                    createOperation resources "<" left right

                "<=" ->
                    createOperation resources ">" left right

                "<" ->
                    createOperation resources ">=" left right

                "&&" ->
                    createOperation resources
                        "||"
                        (Node.empty (createNot resources left))
                        (Node.empty (createNot resources right))

                "||" ->
                    createOperation resources
                        "&&"
                        (Node.empty (createNot resources left))
                        (Node.empty (createNot resources right))

                _ ->
                    Expression.Application [ expressionNodeBasicsNotFn, expressionNormalNodeInNot ]

        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), Node _ expressionNormalNodeInNotNot ] ->
            expressionNormalNodeInNotNot

        _ ->
            Expression.Application [ expressionNodeBasicsNotFn, expressionNormalNodeInNot ]


expressionNodeBasicsNotFn : Node Expression
expressionNodeBasicsNotFn =
    Node.empty (Expression.FunctionOrValue [ "Basics" ] "not")


expressionTrue : Expression
expressionTrue =
    Expression.FunctionOrValue [ "Basics" ] "True"


expressionFalse : Expression
expressionFalse =
    Expression.FunctionOrValue [ "Basics" ] "False"


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
                            , result = Expression.Application [ Node.empty (Expression.PrefixOperator operator), callArgument ]
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
                                        , result = Expression.Application [ Node.empty (Expression.PrefixOperator operator), callArgument ]
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


normalizePatternNode : ModuleNameLookupTable -> Node Pattern -> Node Pattern
normalizePatternNode lookupTable node =
    Node.empty (normalizePattern lookupTable node)


normalizePattern : ModuleNameLookupTable -> Node Pattern -> Pattern
normalizePattern lookupTable (Node patternRange pattern) =
    case pattern of
        Pattern.ParenthesizedPattern inParens ->
            normalizePattern lookupTable inParens

        Pattern.TuplePattern patterns ->
            Pattern.TuplePattern (List.map (\part -> normalizePatternNode lookupTable part) patterns)

        Pattern.RecordPattern fields ->
            Pattern.RecordPattern
                (fields
                    |> List.sortBy (\(Node _ fieldName) -> fieldName)
                    |> List.map (\(Node _ fieldName) -> Node.empty fieldName)
                )

        Pattern.UnConsPattern element list ->
            case normalizePattern lookupTable list of
                Pattern.ListPattern elements ->
                    Pattern.ListPattern (normalizePatternNode lookupTable element :: elements)

                normalizedList ->
                    Pattern.UnConsPattern (normalizePatternNode lookupTable element) (Node.empty normalizedList)

        Pattern.ListPattern elements ->
            Pattern.ListPattern (List.map (\element -> normalizePatternNode lookupTable element) elements)

        Pattern.NamedPattern qualifiedNameRef values ->
            let
                nameRef : Pattern.QualifiedNameRef
                nameRef =
                    { moduleName =
                        ModuleNameLookupTable.moduleNameAt lookupTable patternRange
                            |> Maybe.withDefault qualifiedNameRef.moduleName
                    , name = qualifiedNameRef.name
                    }
            in
            Pattern.NamedPattern nameRef (List.map (\value -> normalizePatternNode lookupTable value) values)

        Pattern.AsPattern aliasPattern (Node _ asName) ->
            Pattern.AsPattern (normalizePatternNode lookupTable aliasPattern)
                -- possible improvement: assign indices and change the expression scope accordingly
                (Node.empty asName)

        Pattern.HexPattern int ->
            Pattern.IntPattern int

        Pattern.AllPattern ->
            Pattern.AllPattern

        Pattern.UnitPattern ->
            Pattern.UnitPattern

        Pattern.CharPattern _ ->
            pattern

        Pattern.StringPattern _ ->
            pattern

        Pattern.IntPattern _ ->
            pattern

        Pattern.VarPattern _ ->
            -- possible improvement: assign indices and change the expression scope accordingly
            pattern

        -- invalid syntax
        Pattern.FloatPattern _ ->
            pattern



-- COMPARE


type Comparison
    = ConfirmedEquality
    | ConfirmedInequality
    | Unconfirmed


compare : Resources a -> Node Expression -> Node Expression -> Comparison
compare resources leftNode right =
    compareExistingNormals
        (normalizeExpression resources leftNode)
        (normalizeExpression resources right)


compareExistingNormals : Expression -> Expression -> Comparison
compareExistingNormals left right =
    case right of
        Expression.UnitExpr ->
            ConfirmedEquality

        _ ->
            case left of
                Expression.UnitExpr ->
                    ConfirmedEquality

                Expression.Floatable leftNumber ->
                    case right of
                        Expression.Floatable rightNumber ->
                            fromEquality (leftNumber == rightNumber)

                        _ ->
                            Unconfirmed

                Expression.Literal leftString ->
                    case right of
                        Expression.Literal rightString ->
                            fromEquality (leftString == rightString)

                        _ ->
                            Unconfirmed

                Expression.CharLiteral leftChar ->
                    case right of
                        Expression.CharLiteral rightChar ->
                            fromEquality (leftChar == rightChar)

                        _ ->
                            Unconfirmed

                Expression.Negation (Node _ leftInNegation) ->
                    case right of
                        Expression.Negation (Node _ rightInNegation) ->
                            compareExistingNormals leftInNegation rightInNegation

                        _ ->
                            Unconfirmed

                Expression.OperatorApplication leftOp _ (Node _ leftLeft) (Node _ leftRight) ->
                    case right of
                        Expression.OperatorApplication rightOp _ (Node _ rightLeft) (Node _ rightRight) ->
                            if leftOp == rightOp then
                                compareAll2Help leftLeft rightLeft leftRight rightRight

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.FunctionOrValue moduleNameLeft leftName ->
                    case right of
                        Expression.FunctionOrValue moduleNameRight rightName ->
                            if leftName == rightName && moduleNameRight == moduleNameLeft then
                                ConfirmedEquality

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.PrefixOperator leftOperator ->
                    case right of
                        Expression.PrefixOperator rightOperator ->
                            if leftOperator == rightOperator then
                                ConfirmedEquality

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.RecordAccessFunction leftFieldName ->
                    case right of
                        Expression.RecordAccessFunction rightFieldName ->
                            if leftFieldName == rightFieldName then
                                ConfirmedEquality

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.GLSLExpression leftGlsl ->
                    case right of
                        Expression.GLSLExpression rightGlsl ->
                            if leftGlsl == rightGlsl then
                                ConfirmedEquality

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.ListExpr leftList ->
                    case right of
                        Expression.ListExpr rightList ->
                            compareLists leftList rightList ConfirmedEquality

                        _ ->
                            Unconfirmed

                Expression.TupledExpression leftList ->
                    case right of
                        Expression.TupledExpression rightList ->
                            compareLists leftList rightList ConfirmedEquality

                        _ ->
                            Unconfirmed

                Expression.RecordExpr leftFields ->
                    case right of
                        Expression.RecordExpr rightFields ->
                            compareFields leftFields rightFields ConfirmedEquality

                        Expression.RecordUpdateExpression _ rightFields ->
                            compareFields leftFields
                                rightFields
                                -- because if all fields match,
                                -- nothing is left of the original record in the record update
                                ConfirmedEquality

                        _ ->
                            Unconfirmed

                Expression.RecordUpdateExpression (Node _ leftBaseRecordVariableName) leftFields ->
                    case right of
                        Expression.RecordUpdateExpression (Node _ rightBaseRecordVariableName) rightFields ->
                            compareFields leftFields
                                rightFields
                                (if leftBaseRecordVariableName == rightBaseRecordVariableName then
                                    ConfirmedEquality

                                 else
                                    Unconfirmed
                                )

                        Expression.RecordExpr rightFields ->
                            compareFields leftFields
                                rightFields
                                -- because if all fields match,
                                -- nothing is left of the original record in the record update
                                ConfirmedEquality

                        _ ->
                            Unconfirmed

                Expression.Application ((Node _ leftCalled) :: (Node _ leftArg0) :: leftArg1Up) ->
                    case right of
                        Expression.Application ((Node _ rightCalled) :: (Node _ rightArg0) :: rightArg1Up) ->
                            case compareAll2Help leftCalled rightCalled leftArg0 rightArg0 of
                                ConfirmedEquality ->
                                    compareAllConfirmedEqualityElseUnconfirmedHelp leftArg1Up rightArg1Up

                                _ ->
                                    Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.RecordAccess (Node _ leftRecord) (Node _ leftFieldName) ->
                    case right of
                        Expression.RecordAccess (Node _ rightRecord) (Node _ rightFieldName) ->
                            if leftFieldName == rightFieldName then
                                compareExistingNormals leftRecord rightRecord

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.IfBlock (Node _ leftCond) (Node _ leftThen) (Node _ leftElse) ->
                    case right of
                        Expression.IfBlock (Node _ rightCond) (Node _ rightThen) (Node _ rightElse) ->
                            case compareExistingNormals leftCond rightCond of
                                ConfirmedEquality ->
                                    case compareExistingNormals leftThen rightThen of
                                        ConfirmedInequality ->
                                            case compareExistingNormals leftElse rightElse of
                                                ConfirmedInequality ->
                                                    ConfirmedInequality

                                                _ ->
                                                    Unconfirmed

                                        ConfirmedEquality ->
                                            case compareExistingNormals leftElse rightElse of
                                                ConfirmedEquality ->
                                                    ConfirmedEquality

                                                _ ->
                                                    Unconfirmed

                                        Unconfirmed ->
                                            Unconfirmed

                                ConfirmedInequality ->
                                    -- the only way this happens
                                    -- is with Basics.not (which gets normalized away)
                                    -- or literal True/False (which gets reported by Simplify anyway)
                                    Unconfirmed

                                Unconfirmed ->
                                    Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.CaseExpression leftCaseOf ->
                    case right of
                        Expression.CaseExpression rightCaseOf ->
                            case compareCases leftCaseOf.cases rightCaseOf.cases of
                                ConfirmedInequality ->
                                    ConfirmedInequality

                                ConfirmedEquality ->
                                    case compareExistingNormals (Node.value leftCaseOf.expression) (Node.value rightCaseOf.expression) of
                                        ConfirmedEquality ->
                                            ConfirmedEquality

                                        _ ->
                                            Unconfirmed

                                Unconfirmed ->
                                    Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.LambdaExpression leftLambda ->
                    case right of
                        Expression.LambdaExpression rightLambda ->
                            if leftLambda.args == rightLambda.args then
                                compareExistingNormals
                                    (Node.value leftLambda.expression)
                                    (Node.value rightLambda.expression)

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                Expression.LetExpression leftLetIn ->
                    case right of
                        Expression.LetExpression rightLetIn ->
                            if leftLetIn.declarations == rightLetIn.declarations then
                                compareExistingNormals
                                    (Node.value leftLetIn.expression)
                                    (Node.value rightLetIn.expression)

                            else
                                Unconfirmed

                        _ ->
                            Unconfirmed

                -- not normalized
                Expression.Integer leftInt ->
                    compareExistingNormals (Expression.Floatable (Basics.toFloat leftInt)) right

                Expression.Hex leftInt ->
                    compareExistingNormals (Expression.Floatable (Basics.toFloat leftInt)) right

                Expression.ParenthesizedExpression (Node _ leftInParens) ->
                    compareExistingNormals leftInParens right

                -- invalid syntax
                Expression.Application [] ->
                    Unconfirmed

                Expression.Application [ Node _ leftInApplication ] ->
                    compareExistingNormals leftInApplication right

                Expression.Operator _ ->
                    Unconfirmed


compareAll2Help : Expression -> Expression -> Expression -> Expression -> Comparison
compareAll2Help left0 right0 left1 right1 =
    case compareExistingNormals left0 right0 of
        ConfirmedInequality ->
            ConfirmedInequality

        ConfirmedEquality ->
            compareExistingNormals left1 right1

        Unconfirmed ->
            case compareExistingNormals left1 right1 of
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

        (Node _ left) :: restOfLeft ->
            case rightList of
                [] ->
                    ConfirmedInequality

                (Node _ right) :: restOfRight ->
                    case compareExistingNormals left right of
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

        (Node _ left) :: restOfLeft ->
            case rightList of
                [] ->
                    Unconfirmed

                (Node _ right) :: restOfRight ->
                    case compareExistingNormals left right of
                        ConfirmedEquality ->
                            compareAllConfirmedEqualityElseUnconfirmedHelp restOfLeft restOfRight

                        _ ->
                            Unconfirmed


compareFields : List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareFields leftList rightList acc =
    case leftList of
        [] ->
            case rightList of
                [] ->
                    acc

                _ :: _ ->
                    Unconfirmed

        (Node _ ( Node _ leftField0Name, Node _ leftField0Value )) :: leftFields1Up ->
            case rightList of
                [] ->
                    Unconfirmed

                (Node _ ( Node _ rightField0Name, Node _ rightField0Value )) :: rightFields1Up ->
                    case Basics.compare leftField0Name rightField0Name of
                        EQ ->
                            case compareExistingNormals leftField0Value rightField0Value of
                                ConfirmedInequality ->
                                    ConfirmedInequality

                                ConfirmedEquality ->
                                    compareFields leftFields1Up rightFields1Up acc

                                Unconfirmed ->
                                    compareFields leftFields1Up rightFields1Up Unconfirmed

                        LT ->
                            compareFields leftFields1Up rightList Unconfirmed

                        GT ->
                            compareFields leftList rightFields1Up Unconfirmed


compareCases : List Expression.Case -> List Expression.Case -> Comparison
compareCases leftCases rightCases =
    case leftCases of
        [] ->
            case rightCases of
                [] ->
                    ConfirmedEquality

                _ :: _ ->
                    Unconfirmed

        leftCase0 :: leftCase1Up ->
            case rightCases of
                [] ->
                    Unconfirmed

                rightCase0 :: rightCase1Up ->
                    case compareCase leftCase0 rightCase0 of
                        Unconfirmed ->
                            Unconfirmed

                        ConfirmedEquality ->
                            compareCasesWithConfirmedSoFar True leftCase1Up rightCase1Up

                        ConfirmedInequality ->
                            compareCasesWithConfirmedSoFar False leftCase1Up rightCase1Up


compareCase : Expression.Case -> Expression.Case -> Comparison
compareCase ( leftPattern, Node _ leftResult ) ( rightPattern, Node _ rightResult ) =
    if leftPattern == rightPattern then
        compareExistingNormals leftResult rightResult

    else
        Unconfirmed


compareCasesWithConfirmedSoFar : Bool -> List Expression.Case -> List Expression.Case -> Comparison
compareCasesWithConfirmedSoFar confirmedEqualElseConfirmedUnequal leftCases rightCases =
    case leftCases of
        [] ->
            case rightCases of
                [] ->
                    fromEquality confirmedEqualElseConfirmedUnequal

                _ :: _ ->
                    Unconfirmed

        leftCase0 :: leftCase1Up ->
            case rightCases of
                [] ->
                    Unconfirmed

                rightCase0 :: rightCase1Up ->
                    case compareCase leftCase0 rightCase0 of
                        Unconfirmed ->
                            Unconfirmed

                        ConfirmedEquality ->
                            if confirmedEqualElseConfirmedUnequal then
                                compareCasesWithConfirmedSoFar True leftCase1Up rightCase1Up

                            else
                                Unconfirmed

                        ConfirmedInequality ->
                            if confirmedEqualElseConfirmedUnequal then
                                Unconfirmed

                            else
                                compareCasesWithConfirmedSoFar False leftCase1Up rightCase1Up


fromEquality : Bool -> Comparison
fromEquality bool =
    if bool then
        ConfirmedEquality

    else
        ConfirmedInequality


getBool : Resources a -> Node Expression -> Maybe Bool
getBool resources baseNode =
    case normalizeExpression resources baseNode of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            justTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            justFalse

        _ ->
            Nothing


justTrue : Maybe Bool
justTrue =
    Just True


justFalse : Maybe Bool
justFalse =
    Just False


getInt : Resources a -> Node Expression -> Maybe Int
getInt resources expressionNode =
    case normalizeExpression resources expressionNode of
        Expression.Floatable float ->
            let
                asInt : Int
                asInt =
                    Basics.round float
            in
            if Basics.toFloat asInt == float then
                Just asInt

            else
                Nothing

        _ ->
            Nothing


getNumber : Resources a -> Node Expression -> Maybe Float
getNumber resources expressionNode =
    case normalizeExpression resources expressionNode of
        Expression.Floatable float ->
            Just float

        _ ->
            Nothing


{-| Parse a given expression as an operator function
(either a function reducible to the operator in prefix notation `(op)` or a lambda `\a b -> a op b`).
-}
getUnappliedBinaryOperation : Resources a -> Node Expression -> Maybe String
getUnappliedBinaryOperation resources expressionNode =
    case normalizeExpression resources expressionNode of
        Expression.PrefixOperator operator ->
            Just operator

        _ ->
            Nothing


{-| Whether a given expression can be called with 2 operands and produces the same result as an operation with a given operator.
Is either a function reducible to the operator in prefix notation `(op)` or a lambda `\a b -> a op b`.
-}
isSpecificUnappliedBinaryOperation : String -> Resources a -> Node Expression -> Bool
isSpecificUnappliedBinaryOperation operator resources expressionNode =
    normalizeExpression resources expressionNode == Expression.PrefixOperator operator
