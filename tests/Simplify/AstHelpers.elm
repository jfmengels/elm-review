module Simplify.AstHelpers exposing
    ( removeParens, removeParensFromPattern
    , getComposition, getValueOrFunctionOrFunctionCall
    , getSpecificFunctionCall, getSpecificValueOrFunction
    , isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
    , isTupleFirstAccess, isTupleSecondAccess
    , getOrder, getSpecificBool, getBool, getBoolPattern, getUncomputedNumberValue
    , getCollapsedCons, getListLiteral, getListSingleton
    , getTuple
    , boolToString, orderToString, emptyStringAsString
    , moduleNameFromString, qualifiedToString
    , declarationListBindings, letDeclarationListBindings, patternBindings, patternListBindings
    , getTypeExposeIncludingVariants, nameOfExpose
    )

{-|


### remove parens

@docs removeParens, removeParensFromPattern


### value/function/function call/composition

@docs getComposition, getValueOrFunctionOrFunctionCall
@docs getSpecificFunctionCall, getSpecificValueOrFunction


### certain kind

@docs isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
@docs isTupleFirstAccess, isTupleSecondAccess
@docs getOrder, getSpecificBool, getBool, getBoolPattern, getUncomputedNumberValue
@docs getCollapsedCons, getListLiteral, getListSingleton
@docs getTuple


### literal as string

@docs boolToString, orderToString, emptyStringAsString


### qualification

@docs moduleNameFromString, qualifiedToString


### misc

@docs declarationListBindings, letDeclarationListBindings, patternBindings, patternListBindings
@docs getTypeExposeIncludingVariants, nameOfExpose

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)
import Simplify.Infer as Infer
import Simplify.Normalize as Normalize


{-| Keep removing parens from the outside until we have something different from a `ParenthesizedExpression`
-}
removeParens : Node Expression -> Node Expression
removeParens expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression expressionInsideOnePairOfParensNode ->
            removeParens expressionInsideOnePairOfParensNode

        _ ->
            expressionNode


{-| Keep removing parens from the outside until we have something different from a `ParenthesizedPattern`
-}
removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern patternNode =
    case Node.value patternNode of
        Pattern.ParenthesizedPattern patternInsideOnePairOfParensNode ->
            removeParensFromPattern patternInsideOnePairOfParensNode

        _ ->
            patternNode


{-| Parse an expression of type list that contains only a single element.
Could be a call to `List.singleton` or a list literal with one element: `[ a ]`
-}
getListSingleton : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingleton lookupTable expressionNode =
    case expressionNode of
        Node _ (Expression.ListExpr (element :: [])) ->
            Just { element = element }

        Node _ (Expression.ListExpr _) ->
            Nothing

        nonListLiteralNode ->
            case getSpecificFunctionCall ( [ "List" ], "singleton" ) lookupTable nonListLiteralNode of
                Just singletonCall ->
                    case singletonCall.argsAfterFirst of
                        [] ->
                            Just { element = singletonCall.firstArg }

                        _ :: _ ->
                            Nothing

                Nothing ->
                    Nothing


{-| Parses calls and lambdas that are reducible to a call of a function with the given name
-}
getSpecificFunctionCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getSpecificFunctionCall ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFunctionOrFunctionCall expressionNode of
        Just call ->
            case call.args of
                firstArg :: argsAfterFirst ->
                    if
                        (call.fnName /= name)
                            || (ModuleNameLookupTable.moduleNameAt lookupTable call.fnRange /= Just moduleName)
                    then
                        Nothing

                    else
                        Just
                            { nodeRange = call.nodeRange
                            , fnRange = call.fnRange
                            , firstArg = firstArg
                            , argsAfterFirst = argsAfterFirst
                            }

                [] ->
                    Nothing

        Nothing ->
            Nothing


{-| Parse a value or the collapsed function or a lambda fully reduced to a function
-}
getValueOrFunctionOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getValueOrFunctionOrFunctionCall expressionNode =
    case getCollapsedUnreducedValueOrFunctionCall expressionNode of
        Just valueOrCall ->
            Just valueOrCall

        Nothing ->
            case getReducedLambda expressionNode of
                Just reducedLambda ->
                    case ( reducedLambda.lambdaPatterns, reducedLambda.callArguments ) of
                        ( [], args ) ->
                            Just
                                { nodeRange = reducedLambda.nodeRange
                                , fnName = reducedLambda.fnName
                                , fnRange = reducedLambda.fnRange
                                , args = args
                                }

                        ( _ :: _, _ ) ->
                            Nothing

                Nothing ->
                    Nothing


{-| Parses either a value reference with the given name,
a function reference with the given name without arguments
or a lambda that is reducible to a function with the given name without arguments
-}
getSpecificValueOrFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificValueOrFunction ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFunction expressionNode of
        Just normalFn ->
            if
                (normalFn.name /= name)
                    || (ModuleNameLookupTable.moduleNameAt lookupTable normalFn.range /= Just moduleName)
            then
                Nothing

            else
                Just normalFn.range

        Nothing ->
            Nothing


{-| Parses either a value reference, a function reference without arguments or a lambda that is reducible to a function without arguments
-}
getValueOrFunction : Node Expression -> Maybe { name : String, range : Range }
getValueOrFunction expressionNode =
    case removeParens expressionNode of
        Node rangeInParens (Expression.FunctionOrValue _ foundName) ->
            Just { range = rangeInParens, name = foundName }

        nonFunctionOrValueNode ->
            case getReducedLambda nonFunctionOrValueNode of
                Just reducedLambdaToFn ->
                    case ( reducedLambdaToFn.lambdaPatterns, reducedLambdaToFn.callArguments ) of
                        ( [], [] ) ->
                            Just { range = reducedLambdaToFn.fnRange, name = reducedLambdaToFn.fnName }

                        ( _ :: _, _ ) ->
                            Nothing

                        ( _, _ :: _ ) ->
                            Nothing

                Nothing ->
                    Nothing


getCollapsedUnreducedValueOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getCollapsedUnreducedValueOrFunctionCall baseNode =
    let
        step :
            { firstArg : Node Expression, argsAfterFirst : List (Node Expression), fed : Node Expression }
            -> Maybe { nodeRange : Range, fnRange : Range, fnName : String, args : List (Node Expression) }
        step layer =
            Maybe.map
                (\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ (layer.firstArg :: layer.argsAfterFirst)
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall layer.fed)
    in
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ fnName) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , args = []
                }

        Node _ (Expression.Application (fed :: firstArg :: argsAfterFirst)) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = argsAfterFirst
                }

        Node _ (Expression.OperatorApplication "|>" _ firstArg fed) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        Node _ (Expression.OperatorApplication "<|" _ fed firstArg) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        _ ->
            Nothing


{-| Parse a << or >> operation.

  - `earlier` is the operation applied first
  - `later` is the operation applied next, directly after `earlier`, not all that are applied next
  - `parentRange` covers the range between and including `earlier` and `later`

Example: f << g << h

  - `parentRange` only covers `g << h`
  - `h` is `earlier`
  - `f << g` is `later`

-}
getComposition : Node Expression -> Maybe { parentRange : Range, earlier : Node Expression, later : Node Expression }
getComposition expressionNode =
    let
        inParensNode : Node Expression
        inParensNode =
            removeParens expressionNode
    in
    case Node.value inParensNode of
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication "<<" _ _ later_) ->
                            ( later_, { start = (Node.range later_).start, end = (Node.range earlier).end } )

                        endLater ->
                            ( endLater, Node.range inParensNode )
            in
            Just { earlier = earlier, later = later, parentRange = parentRange }

        Expression.OperatorApplication ">>" _ earlier composedLater ->
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication ">>" _ later_ _) ->
                            ( later_, { start = (Node.range earlier).start, end = (Node.range later_).end } )

                        endLater ->
                            ( endLater, Node.range inParensNode )
            in
            Just { earlier = earlier, later = later, parentRange = parentRange }

        _ ->
            Nothing


{-| Whether it's a function that accesses a tuple's first part.
Either a function reducible to `Tuple.first` or `\( first, ... ) -> first`.
-}
isTupleFirstAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleFirstAccess lookupTable expressionNode =
    case getSpecificValueOrFunction ( [ "Tuple" ], "first" ) lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleFirstPatternLambda expressionNode


isTupleFirstPatternLambda : Node Expression -> Bool
isTupleFirstPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ Node _ (Pattern.VarPattern firstVariableName), _ ]) ] ->
                    Node.value lambda.expression == Expression.FunctionOrValue [] firstVariableName

                _ ->
                    False

        _ ->
            False


{-| Whether it's a function that accesses a tuple's second part.
Either a function reducible to `Tuple.second` or `\( ..., second ) -> second`.
-}
isTupleSecondAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleSecondAccess lookupTable expressionNode =
    case getSpecificValueOrFunction ( [ "Tuple" ], "second" ) lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleSecondPatternLambda expressionNode


isTupleSecondPatternLambda : Node Expression -> Bool
isTupleSecondPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ _, Node _ (Pattern.VarPattern firstVariableName) ]) ] ->
                    Node.value lambda.expression == Expression.FunctionOrValue [] firstVariableName

                _ ->
                    False

        _ ->
            False


getUncomputedNumberValue : Node Expression -> Maybe Float
getUncomputedNumberValue expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.Integer n ->
            Just (toFloat n)

        Expression.Hex n ->
            Just (toFloat n)

        Expression.Floatable n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getUncomputedNumberValue expr)

        _ ->
            Nothing


{-| Whether it's a function that returns any given input unchanged.
Either a function reducible to `Basics.identity` or `\a -> a`.
-}
isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseExpressionNode =
    case getSpecificValueOrFunction ( [ "Basics" ], "identity" ) lookupTable baseExpressionNode of
        Just _ ->
            True

        Nothing ->
            case removeParens baseExpressionNode of
                Node _ (Expression.LambdaExpression lambda) ->
                    case lambda.args of
                        arg :: [] ->
                            variableMatchesPattern lambda.expression arg

                        _ ->
                            False

                _ ->
                    False


{-| Parse a function that returns the same for any given input and return the result expression node.
Either a function reducible to `Basics.always x`, `\_ -> x` or even for example `\_ a -> a x` where the result expression node would be `\a -> a x`.
-}
getAlwaysResult : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getAlwaysResult lookupTable expressionNode =
    case getSpecificFunctionCall ( [ "Basics" ], "always" ) lookupTable expressionNode of
        Just alwaysCall ->
            Just alwaysCall.firstArg

        Nothing ->
            getIgnoreFirstLambdaResult expressionNode


getIgnoreFirstLambdaResult : Node Expression -> Maybe (Node Expression)
getIgnoreFirstLambdaResult expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                (Node _ Pattern.AllPattern) :: [] ->
                    Just lambda.expression

                (Node _ Pattern.AllPattern) :: pattern1 :: pattern2Up ->
                    Just
                        (Node (Node.range expressionNode)
                            (Expression.LambdaExpression
                                { args = pattern1 :: pattern2Up
                                , expression = lambda.expression
                                }
                            )
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


getReducedLambda :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            }
getReducedLambda expressionNode =
    -- maybe a version of this is better located in Normalize?
    case getCollapsedLambda expressionNode of
        Just lambda ->
            case getCollapsedUnreducedValueOrFunctionCall lambda.expression of
                Just call ->
                    let
                        ( reducedCallArguments, reducedLambdaPatterns ) =
                            drop2EndingsWhile
                                (\( argument, pattern ) -> variableMatchesPattern argument pattern)
                                ( call.args
                                , lambda.patterns
                                )
                    in
                    Just
                        { nodeRange = Node.range expressionNode
                        , fnName = call.fnName
                        , fnRange = call.fnRange
                        , callArguments = reducedCallArguments
                        , lambdaPatterns = reducedLambdaPatterns
                        }

                Nothing ->
                    Nothing

        _ ->
            Nothing


variableMatchesPattern : Node Expression -> Node Pattern -> Bool
variableMatchesPattern expression pattern =
    case ( removeParensFromPattern pattern, removeParens expression ) of
        ( Node _ (Pattern.VarPattern patternName), Node _ (Expression.FunctionOrValue [] argumentName) ) ->
            patternName == argumentName

        _ ->
            False


{-| Remove elements at the end of both given lists, then repeat for the previous elements until a given test returns False
-}
drop2EndingsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2EndingsWhile shouldDrop ( aList, bList ) =
    let
        ( reducedArgumentsReverse, reducedPatternsReverse ) =
            drop2BeginningsWhile
                shouldDrop
                ( List.reverse aList
                , List.reverse bList
                )
    in
    ( List.reverse reducedArgumentsReverse, List.reverse reducedPatternsReverse )


drop2BeginningsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2BeginningsWhile shouldDrop listPair =
    case listPair of
        ( [], bList ) ->
            ( [], bList )

        ( aList, [] ) ->
            ( aList, [] )

        ( aHead :: aTail, bHead :: bTail ) ->
            if shouldDrop ( aHead, bHead ) then
                drop2BeginningsWhile shouldDrop ( aTail, bTail )

            else
                ( aHead :: aTail, bHead :: bTail )


getCollapsedLambda : Node Expression -> Maybe { patterns : List (Node Pattern), expression : Node Expression }
getCollapsedLambda expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case getCollapsedLambda lambda.expression of
                Nothing ->
                    Just
                        { patterns = lambda.args
                        , expression = lambda.expression
                        }

                Just innerCollapsedLambda ->
                    Just
                        { patterns = lambda.args ++ innerCollapsedLambda.patterns
                        , expression = innerCollapsedLambda.expression
                        }

        _ ->
            Nothing


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    List.foldl
        (\(Node _ pattern) soFar -> Set.union soFar (patternBindings pattern))
        Set.empty
        patterns


{-| Recursively find all bindings in a pattern.
-}
patternBindings : Pattern -> Set String
patternBindings pattern =
    case pattern of
        Pattern.ListPattern patterns ->
            patternListBindings patterns

        Pattern.TuplePattern patterns ->
            patternListBindings patterns

        Pattern.RecordPattern patterns ->
            Set.fromList (List.map Node.value patterns)

        Pattern.NamedPattern _ patterns ->
            patternListBindings patterns

        Pattern.UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (patternBindings tailPattern) (patternBindings headPattern)

        Pattern.VarPattern name ->
            Set.singleton name

        Pattern.AsPattern (Node _ pattern_) (Node _ name) ->
            Set.insert name (patternBindings pattern_)

        Pattern.ParenthesizedPattern (Node _ inParens) ->
            patternBindings inParens

        Pattern.AllPattern ->
            Set.empty

        Pattern.UnitPattern ->
            Set.empty

        Pattern.CharPattern _ ->
            Set.empty

        Pattern.StringPattern _ ->
            Set.empty

        Pattern.IntPattern _ ->
            Set.empty

        Pattern.HexPattern _ ->
            Set.empty

        Pattern.FloatPattern _ ->
            Set.empty


declarationListBindings : List (Node Declaration) -> Set String
declarationListBindings declarationList =
    declarationList
        |> List.map (\(Node _ declaration) -> declarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


declarationBindings : Declaration -> Set String
declarationBindings declaration =
    case declaration of
        Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\(Node _ variant) -> Node.value variant.name)
                |> Set.fromList

        Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (Node.value (Node.value functionDeclaration.declaration).name)

        _ ->
            Set.empty


letDeclarationBindings : Expression.LetDeclaration -> Set String
letDeclarationBindings letDeclaration =
    case letDeclaration of
        Expression.LetFunction fun ->
            Set.singleton
                (fun.declaration |> Node.value |> .name |> Node.value)

        Expression.LetDestructuring (Node _ pattern) _ ->
            patternBindings pattern


letDeclarationListBindings : List (Node Expression.LetDeclaration) -> Set String
letDeclarationListBindings letDeclarationList =
    letDeclarationList
        |> List.map
            (\(Node _ declaration) -> letDeclarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


getListLiteral : Node Expression -> Maybe (List (Node Expression))
getListLiteral expressionNode =
    case Node.value expressionNode of
        Expression.ListExpr list ->
            Just list

        _ ->
            Nothing


getCollapsedCons : Node Expression -> Maybe { consed : List (Node Expression), tail : Node Expression }
getCollapsedCons expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.OperatorApplication "::" _ head tail ->
            let
                tailCollapsed : Maybe { consed : List (Node Expression), tail : Node Expression }
                tailCollapsed =
                    getCollapsedCons tail
            in
            case tailCollapsed of
                Nothing ->
                    Just { consed = [ head ], tail = tail }

                Just tailCollapsedList ->
                    Just { consed = head :: tailCollapsedList.consed, tail = tailCollapsedList.tail }

        _ ->
            Nothing


getBool : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBool lookupTable expressionNode =
    case getSpecificBool True lookupTable expressionNode of
        Just _ ->
            Just True

        Nothing ->
            case getSpecificBool False lookupTable expressionNode of
                Just _ ->
                    Just False

                Nothing ->
                    Nothing


getSpecificBool : Bool -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificBool specificBool lookupTable expressionNode =
    getSpecificValueOrFunction ( [ "Basics" ], boolToString specificBool ) lookupTable expressionNode


getTuple : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getBoolPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBoolPattern lookupTable basePatternNode =
    case removeParensFromPattern basePatternNode of
        Node variantPatternRange (Pattern.NamedPattern variantPattern _) ->
            case variantPattern.name of
                "True" ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable variantPatternRange of
                        Just [ "Basics" ] ->
                            Just True

                        _ ->
                            Nothing

                "False" ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable variantPatternRange of
                        Just [ "Basics" ] ->
                            Just False

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getSpecificOrder : Order -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificOrder specificOrder lookupTable expression =
    getSpecificValueOrFunction ( [ "Basics" ], orderToString specificOrder ) lookupTable expression


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expression =
    case getSpecificOrder LT lookupTable expression of
        Just _ ->
            Just LT

        Nothing ->
            case getSpecificOrder EQ lookupTable expression of
                Just _ ->
                    Just EQ

                Nothing ->
                    case getSpecificOrder GT lookupTable expression of
                        Just _ ->
                            Just GT

                        Nothing ->
                            Nothing


{-| Whether a given expression can be called with 2 operands and produces the same result as an operation with a given operator.
Is either a function reducible to the operator in prefix notation `(op)` or a lambda `\a b -> a op b`.
-}
isSpecificUnappliedBinaryOperation : String -> Infer.Resources a -> Node Expression -> Bool
isSpecificUnappliedBinaryOperation symbol checkInfo expression =
    case expression |> Normalize.normalize checkInfo |> Node.value of
        Expression.PrefixOperator operatorSymbol ->
            operatorSymbol == symbol

        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.VarPattern element) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] argument) ] ->
                            (operatorSymbol == symbol)
                                && (argument == element)

                        -- no simple application
                        _ ->
                            False

                [ Node _ (Pattern.VarPattern element), Node _ (Pattern.VarPattern soFar) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] left), Node _ (Expression.FunctionOrValue [] right) ] ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        Expression.OperatorApplication operatorSymbol _ (Node _ (Expression.FunctionOrValue [] left)) (Node _ (Expression.FunctionOrValue [] right)) ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        _ ->
                            False

                _ ->
                    False

        -- not a known simple operator function
        _ ->
            False


getTypeExposeIncludingVariants : Exposing.TopLevelExpose -> Maybe String
getTypeExposeIncludingVariants expose =
    case expose of
        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose _ ->
            Nothing

        Exposing.TypeExpose variantType ->
            case variantType.open of
                Nothing ->
                    Nothing

                Just _ ->
                    Just variantType.name


nameOfExpose : Exposing.TopLevelExpose -> String
nameOfExpose topLevelExpose =
    case topLevelExpose of
        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.InfixExpose name ->
            name

        Exposing.TypeExpose typeExpose ->
            typeExpose.name



-- STRING


emptyStringAsString : String
emptyStringAsString =
    "\"\""


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


orderToString : Order -> String
orderToString order =
    case order of
        LT ->
            "LT"

        EQ ->
            "EQ"

        GT ->
            "GT"


{-| Put a `ModuleName` and thing name together as a string.
If desired, call in combination with `qualify`
-}
qualifiedToString : ( ModuleName, String ) -> String
qualifiedToString ( moduleName, name ) =
    case moduleName of
        [] ->
            name

        _ :: _ ->
            moduleNameToString moduleName ++ "." ++ name


moduleNameToString : ModuleName -> String
moduleNameToString moduleName =
    String.join "." moduleName


moduleNameFromString : String -> ModuleName
moduleNameFromString string =
    String.split "." string
