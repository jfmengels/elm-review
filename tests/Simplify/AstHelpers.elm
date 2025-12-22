module Simplify.AstHelpers exposing
    ( ReduceLambdaResources
    , removeParens, removeParensFromPattern
    , getValueOrFnOrFnCall
    , getSpecificFnCall, getSpecificUnreducedFnCall, isSpecificUnreducedFnCall, isSpecificValueOrFn, isSpecificValueReference
    , isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
    , isTupleFirstAccess, isTupleSecondAccess
    , getAccessingRecord, getRecordAccessFunction
    , getOrder, getBool, getBoolPattern, getUncomputedNumberValue
    , getCollapsedCons, getListLiteral, isListLiteral, getListSingleton
    , getTuple2, getTuple2Literal
    , boolToString, emptyStringAsString
    , moduleNameFromString, qualifiedModuleName, qualifiedToString, moduleNameToString
    , declarationListBindings, patternBindings, patternListBindings, typeUsesVariable
    , nameOfExpose
    , couldBeValueContainingNaN
    )

{-|


## resources

@docs ReduceLambdaResources


### remove parens

@docs removeParens, removeParensFromPattern


### value/function/function call/composition

@docs getValueOrFnOrFnCall
@docs getSpecificFnCall, getSpecificUnreducedFnCall, isSpecificUnreducedFnCall, isSpecificValueOrFn, isSpecificValueReference


### certain kind

@docs isIdentity, getAlwaysResult, isSpecificUnappliedBinaryOperation
@docs isTupleFirstAccess, isTupleSecondAccess
@docs getAccessingRecord, getRecordAccessFunction
@docs getOrder, getBool, getBoolPattern, getUncomputedNumberValue
@docs getCollapsedCons, getListLiteral, isListLiteral, getListSingleton
@docs getTuple2, getTuple2Literal


### literal as string

@docs boolToString, emptyStringAsString


### qualification

@docs moduleNameFromString, qualifiedModuleName, qualifiedToString, moduleNameToString


### misc

@docs declarationListBindings, patternBindings, patternListBindings, typeUsesVariable
@docs nameOfExpose
@docs couldBeValueContainingNaN

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Fn.Basics
import Fn.List
import Fn.Tuple
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)
import Simplify.CallStyle as CallStyle exposing (FunctionCallStyle)
import Simplify.CoreHelpers exposing (drop2EndingsWhile, list2AreSameLengthAndAll)
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


type alias ReduceLambdaResources context =
    { context
        | lookupTable : ModuleNameLookupTable
        , importCustomTypes :
            Dict
                ModuleName
                (Dict
                    String
                    { variantNames : Set String
                    , allParametersAreUsedInVariants : Bool
                    }
                )
        , moduleCustomTypes :
            Dict
                String
                { variantNames : Set String
                , allParametersAreUsedInVariants : Bool
                }
    }


{-| Parse an expression of type list that contains only a single element.
Could be a call to `List.singleton` or a list literal with one element: `[ a ]`.
Returns the inner element expression
-}
getListSingleton :
    ModuleNameLookupTable
    -> Node Expression
    -> Maybe (Node Expression)
getListSingleton lookupTable expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.ListExpr elements) ->
            case elements of
                [ element ] ->
                    Just element

                _ ->
                    Nothing

        expressionUnparenthesizedNode ->
            Maybe.map .firstArg
                (getSpecificUnreducedFnCall Fn.List.singleton lookupTable expressionUnparenthesizedNode)


{-| Parse a call or a lambda that is reducible to a call of a function with the given name.
If used for parsing fully applied calls, strongly consider `getSpecificUnreducedFnCall`.
-}
getSpecificFnCall :
    ( ModuleName, String )
    -> ReduceLambdaResources context
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
getSpecificFnCall reference context expressionNode =
    Maybe.andThen (\valOrFn -> valueOrFunctionCallToSpecificFnCall reference context.lookupTable valOrFn)
        (getValueOrFnOrFnCall context expressionNode)


{-| A simpler and faster version of `getSpecificFnCall` that skips checking for possible reduced lambdas.
Use when you only care about fully applied calls anyway (which can therefore be reduced), like
when you want to extract the value in `Just` or `List.singleton` calls
or the first and second argument of `Tuple.pair` calls.
Neither of them can ever be found as e.g.
`\x -> Just value x` or `\third -> Tuple.pair first second third`.
Basically whenever you know you have a value (e.g. when checking an argument
of a function you know takes a value at that position, like `List.length` taking a list).

Do _not_ use for functions that can return functions, like `Basics.always`, `Basics.identity`
or `Tuple.first` (basically anything that returns a type variable directly)

-}
getSpecificUnreducedFnCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
getSpecificUnreducedFnCall reference lookupTable expressionNode =
    Maybe.andThen (\valOrFn -> valueOrFunctionCallToSpecificFnCall reference lookupTable valOrFn)
        (getCollapsedUnreducedValueOrFunctionCall expressionNode)


{-| Like `getSpecificUnreducedFnCall` without returning any info
-}
isSpecificUnreducedFnCall : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificUnreducedFnCall ( specificModuleOrigin, specificName ) lookupTable expressionNode =
    case getCollapsedUnreducedValueOrFunctionCall expressionNode of
        Nothing ->
            False

        Just valueOrFunctionCall ->
            Basics.not (List.isEmpty valueOrFunctionCall.args)
                && (valueOrFunctionCall.fnName == specificName)
                && (case ModuleNameLookupTable.moduleNameAt lookupTable valueOrFunctionCall.fnRange of
                        Nothing ->
                            False

                        Just moduleOrigin ->
                            moduleOrigin == specificModuleOrigin
                   )


valueOrFunctionCallToSpecificFnCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    ->
        { nodeRange : Range
        , fnName : String
        , fnRange : Range
        , args : List (Node Expression)
        , callStyle : FunctionCallStyle
        }
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
valueOrFunctionCallToSpecificFnCall ( specificModuleOrigin, specificName ) lookupTable valueOrFunctionCall =
    case valueOrFunctionCall.args of
        firstArg :: argsAfterFirst ->
            if
                (valueOrFunctionCall.fnName == specificName)
                    && (case ModuleNameLookupTable.moduleNameAt lookupTable valueOrFunctionCall.fnRange of
                            Nothing ->
                                False

                            Just moduleOrigin ->
                                moduleOrigin == specificModuleOrigin
                       )
            then
                Just
                    { nodeRange = valueOrFunctionCall.nodeRange
                    , fnRange = valueOrFunctionCall.fnRange
                    , firstArg = firstArg
                    , argsAfterFirst = argsAfterFirst
                    , callStyle = valueOrFunctionCall.callStyle
                    }

            else
                Nothing

        [] ->
            Nothing


{-| Parse a value or the collapsed function or a lambda fully reduced to a function
-}
getValueOrFnOrFnCall :
    ReduceLambdaResources context
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
getValueOrFnOrFnCall lookupTable expressionNode =
    case getCollapsedUnreducedValueOrFunctionCall expressionNode of
        (Just _) as justValueOrCall ->
            justValueOrCall

        Nothing ->
            case getReducedLambda lookupTable expressionNode of
                Just reducedLambda ->
                    case reducedLambda.lambdaPatterns of
                        [] ->
                            Just
                                { nodeRange = reducedLambda.nodeRange
                                , fnName = reducedLambda.fnName
                                , fnRange = reducedLambda.fnRange
                                , callStyle = reducedLambda.callStyle
                                , args = reducedLambda.callArguments
                                }

                        _ :: _ ->
                            Nothing

                Nothing ->
                    Nothing


{-| Check for either a value reference with the given name,
a function reference with the given name without arguments
or a lambda that is reducible to a function with the given name without arguments
-}
isSpecificValueOrFn :
    ( ModuleName, String )
    -> ReduceLambdaResources context
    -> Node Expression
    -> Bool
isSpecificValueOrFn ( specificModuleOrigin, specificName ) context expressionNode =
    case getValueOrFunction context expressionNode of
        Just valueOrFn ->
            (valueOrFn.name == specificName)
                && (case ModuleNameLookupTable.moduleNameAt context.lookupTable valueOrFn.range of
                        Nothing ->
                            False

                        Just moduleOrigin ->
                            moduleOrigin == specificModuleOrigin
                   )

        Nothing ->
            False


{-| Parse either a value reference, a function reference without arguments or a lambda that is reducible to a function without arguments
-}
getValueOrFunction :
    ReduceLambdaResources context
    -> Node Expression
    -> Maybe { name : String, range : Range }
getValueOrFunction lookupTable expressionNode =
    case removeParens expressionNode of
        Node rangeInParens (Expression.FunctionOrValue _ foundName) ->
            Just { range = rangeInParens, name = foundName }

        nonFunctionOrValueNode ->
            case getReducedLambda lookupTable nonFunctionOrValueNode of
                Just reducedLambdaToFn ->
                    if
                        List.isEmpty reducedLambdaToFn.lambdaPatterns
                            && List.isEmpty reducedLambdaToFn.callArguments
                    then
                        Just { range = reducedLambdaToFn.fnRange, name = reducedLambdaToFn.fnName }

                    else
                        Nothing

                Nothing ->
                    Nothing


{-| Specialized, more performant version of `isSpecificValueOrFn`
that only works for variables holding a value that cannot be applied,
like `True`, `Basics.e` or `Nothing`.
-}
isSpecificValueReference :
    ModuleNameLookupTable
    -> ( ModuleName, String )
    -> Node Expression
    -> Bool
isSpecificValueReference lookupTable ( moduleOriginToCheckFor, nameToCheckFor ) baseNode =
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ name) ->
            (name == nameToCheckFor)
                && (case ModuleNameLookupTable.moduleNameAt lookupTable fnRange of
                        Nothing ->
                            False

                        Just moduleOrigin ->
                            moduleOrigin == moduleOriginToCheckFor
                   )

        _ ->
            False


getCollapsedUnreducedValueOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
getCollapsedUnreducedValueOrFunctionCall baseNode =
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ fnName) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , args = []
                , callStyle = CallStyle.Application
                }

        Node _ (Expression.Application (fedNode :: firstArg :: argsAfterFirst)) ->
            Maybe.map
                (\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ (firstArg :: argsAfterFirst)
                    , callStyle = CallStyle.Application
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall fedNode)

        Node _ (Expression.OperatorApplication "|>" _ firstArg fedNode) ->
            Maybe.map
                (\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ [ firstArg ]
                    , callStyle = CallStyle.pipeLeftToRight
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall fedNode)

        Node _ (Expression.OperatorApplication "<|" _ fedNode firstArg) ->
            Maybe.map
                (\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ [ firstArg ]
                    , callStyle = CallStyle.pipeRightToLeft
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall fedNode)

        _ ->
            Nothing


{-| Whether it's a function that accesses a tuple's first part.
Either a function reducible to `Tuple.first` or `\( first, ... ) -> first`.
-}
isTupleFirstAccess : ReduceLambdaResources context -> Node Expression -> Bool
isTupleFirstAccess context expressionNode =
    isSpecificValueOrFn Fn.Tuple.first context expressionNode
        || isTupleFirstPatternLambda expressionNode


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
isTupleSecondAccess : ReduceLambdaResources context -> Node Expression -> Bool
isTupleSecondAccess context expressionNode =
    isSpecificValueOrFn Fn.Tuple.second context expressionNode
        || isTupleSecondPatternLambda expressionNode


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


{-| Parse a record access or call of a record access function.
The resulting `range` refers to the unparenthesized range of the access/function application.
-}
getAccessingRecord : Node Expression -> Maybe { range : Range, record : Node Expression, field : String }
getAccessingRecord expressionNode =
    case removeParens expressionNode of
        Node range (Expression.RecordAccess record (Node _ fieldName)) ->
            Just { field = fieldName, record = record, range = range }

        Node range (Expression.Application (function :: record :: [])) ->
            Maybe.map (\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        Node range (Expression.OperatorApplication "|>" _ record function) ->
            Maybe.map (\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        Node range (Expression.OperatorApplication "<|" _ function record) ->
            Maybe.map (\fieldName -> { field = fieldName, record = record, range = range }) (getRecordAccessFunction function)

        _ ->
            Nothing


{-| Parse a function that accesses a specific field and is therefore equivalent to `.field`.
The resulting String is the field name without the leading dot.
-}
getRecordAccessFunction : Node Expression -> Maybe String
getRecordAccessFunction expressionNode =
    case expressionNode of
        Node _ (Expression.RecordAccessFunction fieldName) ->
            Just (String.replace "." "" fieldName)

        _ ->
            Nothing


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
Either a function reducible to `Basics.identity` or `\a -> a` (or any other lambda that reconstructs the just-destructured value).
-}
isIdentity : ReduceLambdaResources context -> Node Expression -> Bool
isIdentity context baseExpressionNode =
    isSpecificValueOrFn Fn.Basics.identity context baseExpressionNode
        || (case removeParens baseExpressionNode of
                Node _ (Expression.LambdaExpression lambda) ->
                    case lambda.args of
                        arg :: [] ->
                            expressionReconstructsDestructuringPattern context
                                lambda.expression
                                arg

                        _ ->
                            False

                _ ->
                    False
           )


{-| Parse a function that returns the same for any given input and return the result expression node.
Either a function reducible to `Basics.always x`, `\_ -> x` or even for example `\_ a -> a x` where the result expression node would be `\a -> a x`.
-}
getAlwaysResult :
    ReduceLambdaResources context
    -> Node Expression
    -> Maybe (Node Expression)
getAlwaysResult context expressionNode =
    case getSpecificFnCall Fn.Basics.always context expressionNode of
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
    ReduceLambdaResources context
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            , callStyle : FunctionCallStyle
            }
getReducedLambda context expressionNode =
    -- maybe a version of this is better located in Normalize?
    case getCollapsedLambda expressionNode of
        Just lambda ->
            case getCollapsedUnreducedValueOrFunctionCall lambda.expression of
                Just call ->
                    let
                        ( reducedCallArguments, reducedLambdaPatterns ) =
                            drop2EndingsWhile
                                (\argument pattern ->
                                    expressionReconstructsDestructuringPattern context argument pattern
                                )
                                call.args
                                lambda.patterns
                    in
                    Just
                        { nodeRange = Node.range expressionNode
                        , fnName = call.fnName
                        , fnRange = call.fnRange
                        , callArguments = reducedCallArguments
                        , lambdaPatterns = reducedLambdaPatterns
                        , callStyle = call.callStyle
                        }

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Check if the expression is the exact same value that was pattern-matched on.
For example in

    \(( (), _ as d ) as unused) -> ( (), d )

what comes out of this lambda will always be the same value that comes in.

An destructuring pattern is any pattern that exhaustively matches all possible cases.
E.g. any lambda pattern, any let destructuring pattern, any (let) function declaration pattern or the last `case of`
case pattern when matching on a value with infinite cases like an `Int` or a `List`.

-}
expressionReconstructsDestructuringPattern :
    ReduceLambdaResources context
    -> Node Expression
    -> Node Pattern
    -> Bool
expressionReconstructsDestructuringPattern context expressionNode patternNode =
    case Node.value patternNode of
        Pattern.ParenthesizedPattern patternInParens ->
            expressionReconstructsDestructuringPattern context
                expressionNode
                patternInParens

        -- should be covered by ParenthesizedPattern
        Pattern.TuplePattern [ patternInParens ] ->
            expressionReconstructsDestructuringPattern context
                expressionNode
                patternInParens

        Pattern.UnitPattern ->
            isUnit expressionNode

        -- should be covered by UnitPattern
        Pattern.TuplePattern [] ->
            isUnit expressionNode

        Pattern.VarPattern patternName ->
            case removeParens expressionNode of
                Node _ (Expression.FunctionOrValue [] argumentName) ->
                    patternName == argumentName

                _ ->
                    False

        Pattern.AsPattern aliasedPattern (Node _ patternAliasName) ->
            case removeParens expressionNode of
                Node _ (Expression.FunctionOrValue [] expressionReferenceName) ->
                    patternAliasName == expressionReferenceName

                unparenthesizedExpressionNotLocalReference ->
                    let
                        unparenthesizedAliasedPattern : Node Pattern
                        unparenthesizedAliasedPattern =
                            removeParensFromPattern aliasedPattern

                        matchesRecordUpdate : Bool
                        matchesRecordUpdate =
                            -- checking \({x} as r) -> {r|x=x} match
                            case unparenthesizedAliasedPattern of
                                Node _ (Pattern.RecordPattern patternFields) ->
                                    case removeParens expressionNode of
                                        Node _ (Expression.RecordUpdateExpression (Node _ updatedRecordReferenceName) expressionFields) ->
                                            (patternAliasName == updatedRecordReferenceName)
                                                && list2AreSameLengthAndAll
                                                    (\(Node _ ( Node _ expressionFieldName, expressionFieldValueNode )) (Node _ patternFieldName) ->
                                                        (patternFieldName == expressionFieldName)
                                                            && (case removeParens expressionFieldValueNode of
                                                                    Node _ (Expression.FunctionOrValue [] expressionFieldValueReferenceName) ->
                                                                        patternFieldName == expressionFieldValueReferenceName

                                                                    _ ->
                                                                        False
                                                               )
                                                    )
                                                    (expressionFields
                                                        |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                                                    )
                                                    (patternFields
                                                        |> List.sortBy Node.value
                                                    )

                                        _ ->
                                            False

                                _ ->
                                    False
                    in
                    -- || split into if to make use of TCO
                    if matchesRecordUpdate then
                        True

                    else
                        expressionReconstructsDestructuringPattern context
                            unparenthesizedExpressionNotLocalReference
                            unparenthesizedAliasedPattern

        Pattern.TuplePattern [ patternPart0, patternPart1 ] ->
            case getTuple2 context.lookupTable expressionNode of
                Nothing ->
                    False

                Just expressionParts ->
                    -- && split into if to make use of TCO
                    if expressionReconstructsDestructuringPattern context expressionParts.first patternPart0 then
                        expressionReconstructsDestructuringPattern context expressionParts.second patternPart1

                    else
                        False

        Pattern.TuplePattern [ patternPart0, patternPart1, patternPart2 ] ->
            case removeParens expressionNode of
                Node _ (Expression.TupledExpression [ expressionPart0, expressionPart1, expressionPart2 ]) ->
                    -- && split into if to make use of TCO
                    if
                        expressionReconstructsDestructuringPattern context expressionPart0 patternPart0
                            && expressionReconstructsDestructuringPattern context expressionPart1 patternPart1
                    then
                        expressionReconstructsDestructuringPattern context expressionPart2 patternPart2

                    else
                        False

                _ ->
                    False

        Pattern.NamedPattern patternVariantReference valuePatterns ->
            -- one might think \(Variant x y) -> Variant x y
            -- is always a match but that claim actually requires type inference
            -- (or for a more limited version: knowledge about choice type phantom type parameters).
            -- For example:
            --
            --     type IntIsNat isNat = Unsafe Int
            --
            --     unsafeMarkNat : IntIsNat Never -> IntIsNat ()
            --     unsafeMarkNat (Unsafe int) = Unsafe int
            --
            --     unsafeMap : (Int -> Int) -> IntIsNat isNat -> IntIsNat isNat
            --
            --     abs : IntIsNat Never -> IntIsNat ()
            --     abs integer = integer |> unsafeMap Basics.abs |> unsafeMarkNat
            --
            -- Notice how `unsafeMarkNat` has different input and output
            -- type and is _not equivalent to identity_.
            -- This specific example is arguably contrived but it does
            -- occur in real life code!
            case ModuleNameLookupTable.moduleNameAt context.lookupTable (Node.range patternNode) of
                Nothing ->
                    False

                Just patternVariantReferenceModuleOrigin ->
                    let
                        maybeOriginChoiceTypeInfo : Maybe { name : String, variantNames : Set String, allParametersAreUsedInVariants : Bool }
                        maybeOriginChoiceTypeInfo =
                            case patternVariantReferenceModuleOrigin of
                                [] ->
                                    getCustomTypeWithVariant patternVariantReference.name
                                        context.moduleCustomTypes

                                variantReferenceImportedModuleOrigin ->
                                    Maybe.andThen
                                        (\inModule ->
                                            getCustomTypeWithVariant patternVariantReference.name inModule
                                        )
                                        (Dict.get variantReferenceImportedModuleOrigin context.importCustomTypes)

                        reconstructingVariantWillNeverIntroduceNewTypeParameters : Bool
                        reconstructingVariantWillNeverIntroduceNewTypeParameters =
                            case maybeOriginChoiceTypeInfo of
                                Nothing ->
                                    False

                                Just originChoiceTypeInfo ->
                                    originChoiceTypeInfo.allParametersAreUsedInVariants
                    in
                    if reconstructingVariantWillNeverIntroduceNewTypeParameters then
                        case valuePatterns of
                            [] ->
                                isSpecificValueReference context.lookupTable
                                    ( patternVariantReferenceModuleOrigin, patternVariantReference.name )
                                    expressionNode

                            valuePattern0 :: valuePattern1Up ->
                                case
                                    getSpecificUnreducedFnCall
                                        ( patternVariantReferenceModuleOrigin, patternVariantReference.name )
                                        context.lookupTable
                                        expressionNode
                                of
                                    Nothing ->
                                        False

                                    Just expressionVariantCall ->
                                        expressionReconstructsDestructuringPattern context expressionVariantCall.firstArg valuePattern0
                                            && -- must be the same length
                                               -- because e.g. (\(V x y) -> V x)
                                               -- is valid, compiling elm code
                                               -- that is not equivalent to identity
                                               list2AreSameLengthAndAll
                                                (\valueExpression valuePattern ->
                                                    expressionReconstructsDestructuringPattern context valueExpression valuePattern
                                                )
                                                expressionVariantCall.argsAfterFirst
                                                valuePattern1Up

                    else
                        False

        Pattern.RecordPattern _ ->
            -- if we knew the pattern coverers all fields, we could check
            -- that e.g. \\{b,c} -> {b=b,c=c} is a match, requires type inference
            False

        Pattern.AllPattern ->
            -- if we knew it's type was (), unit tuple, unit triple, unit record
            -- or unit variant (without phantom parameters),
            -- we could check if the expression matches that unit structure.
            -- requires type inference
            False

        -- invalid syntax
        Pattern.TuplePattern (_ :: _ :: _ :: _ :: _) ->
            False

        -- all remaining are refutable
        Pattern.CharPattern _ ->
            False

        Pattern.StringPattern _ ->
            False

        Pattern.IntPattern _ ->
            False

        Pattern.HexPattern _ ->
            False

        Pattern.FloatPattern _ ->
            False

        Pattern.UnConsPattern _ _ ->
            False

        Pattern.ListPattern _ ->
            False


getCustomTypeWithVariant :
    String
    -> Dict String { variantNames : Set String, allParametersAreUsedInVariants : Bool }
    -> Maybe { name : String, variantNames : Set String, allParametersAreUsedInVariants : Bool }
getCustomTypeWithVariant variantName customTypes =
    Dict.foldl
        (\customTypeName customTypeInfo soFar ->
            case soFar of
                Just _ ->
                    soFar

                Nothing ->
                    if Set.member variantName customTypeInfo.variantNames then
                        Just
                            { name = customTypeName
                            , variantNames = customTypeInfo.variantNames
                            , allParametersAreUsedInVariants = customTypeInfo.allParametersAreUsedInVariants
                            }

                    else
                        Nothing
        )
        Nothing
        customTypes


isUnit : Node Expression -> Bool
isUnit expressionNode =
    case removeParens expressionNode of
        Node _ Expression.UnitExpr ->
            True

        Node _ (Expression.TupledExpression []) ->
            True

        _ ->
            False


getCollapsedLambda : Node Expression -> Maybe { patterns : List (Node Pattern), expression : Node Expression }
getCollapsedLambda expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            Just
                (case getCollapsedLambda lambda.expression of
                    Nothing ->
                        { patterns = lambda.args
                        , expression = lambda.expression
                        }

                    Just innerCollapsedLambda ->
                        { patterns = lambda.args ++ innerCollapsedLambda.patterns
                        , expression = innerCollapsedLambda.expression
                        }
                )

        _ ->
            Nothing


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    List.foldl
        (\(Node _ pattern) soFar ->
            Set.union (patternBindings pattern) soFar
        )
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

        Pattern.RecordPattern fieldNames ->
            List.foldl
                (\(Node _ fieldName) soFar -> Set.insert fieldName soFar)
                Set.empty
                fieldNames

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
    List.foldl
        (\(Node _ declaration) soFar ->
            Set.union (declarationBindings declaration) soFar
        )
        Set.empty
        declarationList


declarationBindings : Declaration -> Set String
declarationBindings declaration =
    case declaration of
        Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.foldl
                    (\(Node _ variant) soFar ->
                        Set.insert (Node.value variant.name) soFar
                    )
                    Set.empty

        Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (Node.value (Node.value functionDeclaration.declaration).name)

        _ ->
            Set.empty


typeUsesVariable : String -> Node TypeAnnotation -> Bool
typeUsesVariable variableNeedle (Node _ type_) =
    case type_ of
        TypeAnnotation.GenericType variable ->
            variable == variableNeedle

        TypeAnnotation.Typed _ arguments ->
            List.any (\argument -> typeUsesVariable variableNeedle argument)
                arguments

        TypeAnnotation.Unit ->
            False

        TypeAnnotation.Tupled parts ->
            List.any (\part -> typeUsesVariable variableNeedle part)
                parts

        TypeAnnotation.Record fields ->
            List.any
                (\(Node _ ( _, fieldValue )) ->
                    typeUsesVariable variableNeedle fieldValue
                )
                fields

        TypeAnnotation.GenericRecord (Node _ extendedRecordVariable) (Node _ fields) ->
            if extendedRecordVariable == variableNeedle then
                True

            else
                List.any
                    (\(Node _ ( _, fieldValue )) ->
                        typeUsesVariable variableNeedle fieldValue
                    )
                    fields

        TypeAnnotation.FunctionTypeAnnotation input output ->
            if typeUsesVariable variableNeedle input then
                True

            else
                typeUsesVariable variableNeedle output


getListLiteral : Node Expression -> Maybe (List (Node Expression))
getListLiteral expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.ListExpr list) ->
            Just list

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.ListExpr _) ->
            True

        _ ->
            False


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
    if isSpecificValueReference lookupTable Fn.Basics.trueVariant expressionNode then
        Just True

    else if isSpecificValueReference lookupTable Fn.Basics.falseVariant expressionNode then
        Just False

    else
        Nothing


getTuple2Literal : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple2Literal expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getTuple2 : ModuleNameLookupTable -> Node Expression -> Maybe { first : Node Expression, second : Node Expression }
getTuple2 lookupTable expressionNode =
    case removeParens expressionNode of
        Node _ (Expression.TupledExpression (first :: second :: [])) ->
            Just { first = first, second = second }

        _ ->
            case getSpecificUnreducedFnCall Fn.Tuple.pair lookupTable expressionNode of
                Just tuplePairCall ->
                    case tuplePairCall.argsAfterFirst of
                        second :: _ ->
                            Just { first = tuplePairCall.firstArg, second = second }

                        [] ->
                            Nothing

                Nothing ->
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


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expressionNode =
    if isSpecificValueReference lookupTable Fn.Basics.lTVariant expressionNode then
        Just LT

    else if isSpecificValueReference lookupTable Fn.Basics.eQVariant expressionNode then
        Just EQ

    else if isSpecificValueReference lookupTable Fn.Basics.gTVariant expressionNode then
        Just GT

    else
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


{-| Indicates whether this value is potentially NaN,
meaning that it could return `False` when `==` with itself.

This will return `False` for expressions that are known to
only contain literals (e.g. `[ ( 0, { name = "a" ++ "b" } ) ]`)
or functions (e.g. `(++)`).

-}
couldBeValueContainingNaN : Node Expression -> Bool
couldBeValueContainingNaN node =
    couldBeValueContainingNaNHelp [ node ]


couldBeValueContainingNaNHelp : List (Node Expression) -> Bool
couldBeValueContainingNaNHelp nodes =
    case nodes of
        first :: rest ->
            case Node.value first of
                Expression.IfBlock condition thenBranch elseBranch ->
                    couldBeValueContainingNaNHelp (condition :: thenBranch :: elseBranch :: rest)

                Expression.TupledExpression newNodes ->
                    couldBeValueContainingNaNHelp (newNodes ++ rest)

                Expression.ParenthesizedExpression newNode ->
                    couldBeValueContainingNaNHelp (newNode :: rest)

                Expression.ListExpr newNodes ->
                    couldBeValueContainingNaNHelp (newNodes ++ rest)

                Expression.Application _ ->
                    True

                Expression.OperatorApplication operator _ left right ->
                    -- If the operator can lead to a number being returned, then it's possible the expression
                    -- evaluates to NaN.
                    case operator of
                        -- Number operators
                        "/" ->
                            True

                        "//" ->
                            -- Can't result in NaN (even `NaN // NaN` can't seem to result in `NaN`).
                            couldBeValueContainingNaNHelp rest

                        "+" ->
                            couldBeValueContainingNaNHelp (left :: right :: rest)

                        "-" ->
                            couldBeValueContainingNaNHelp (left :: right :: rest)

                        "*" ->
                            couldBeValueContainingNaNHelp (left :: right :: rest)

                        "^" ->
                            couldBeValueContainingNaNHelp (left :: right :: rest)

                        -- Similar to a function application
                        "<|" ->
                            True

                        "|>" ->
                            True

                        -- Operators that return functions
                        "<<" ->
                            couldBeValueContainingNaNHelp rest

                        ">>" ->
                            couldBeValueContainingNaNHelp rest

                        -- Operators that return booleans
                        "||" ->
                            couldBeValueContainingNaNHelp rest

                        "&&" ->
                            couldBeValueContainingNaNHelp rest

                        "==" ->
                            couldBeValueContainingNaNHelp rest

                        "/=" ->
                            couldBeValueContainingNaNHelp rest

                        "<" ->
                            couldBeValueContainingNaNHelp rest

                        ">" ->
                            couldBeValueContainingNaNHelp rest

                        "<=" ->
                            couldBeValueContainingNaNHelp rest

                        ">=" ->
                            couldBeValueContainingNaNHelp rest

                        "++" ->
                            -- Can return either a string or a list potentially containing Nan.
                            -- Further improvement: If we notice this works on strings, then we can return False right away.
                            couldBeValueContainingNaNHelp (left :: right :: rest)

                        _ ->
                            -- There are more operators but they don't deal with numbers
                            couldBeValueContainingNaNHelp rest

                Expression.FunctionOrValue _ _ ->
                    True

                Expression.RecordAccess _ _ ->
                    True

                Expression.RecordUpdateExpression (Node recordRange record) fields ->
                    couldBeValueContainingNaNHelp
                        (Node recordRange (Expression.FunctionOrValue [] record)
                            :: (List.map (\(Node _ ( _, value )) -> value) fields
                                    ++ rest
                               )
                        )

                Expression.LetExpression { expression } ->
                    couldBeValueContainingNaNHelp (expression :: rest)

                Expression.CaseExpression { cases } ->
                    couldBeValueContainingNaNHelp (List.map Tuple.second cases ++ rest)

                Expression.Negation node ->
                    couldBeValueContainingNaNHelp (node :: rest)

                Expression.LambdaExpression _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.RecordExpr fields ->
                    couldBeValueContainingNaNHelp
                        (List.map (\(Node _ ( _, value )) -> value) fields
                            ++ rest
                        )

                Expression.UnitExpr ->
                    couldBeValueContainingNaNHelp rest

                Expression.PrefixOperator _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.Operator _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.Integer _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.Hex _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.Floatable _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.Literal _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.CharLiteral _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.RecordAccessFunction _ ->
                    couldBeValueContainingNaNHelp rest

                Expression.GLSLExpression _ ->
                    couldBeValueContainingNaNHelp rest

        [] ->
            False


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


qualifiedModuleName : ( ModuleName, String ) -> ModuleName
qualifiedModuleName ( moduleName, _ ) =
    moduleName


moduleNameToString : ModuleName -> String
moduleNameToString moduleName =
    String.join "." moduleName


moduleNameFromString : String -> ModuleName
moduleNameFromString string =
    String.split "." string
