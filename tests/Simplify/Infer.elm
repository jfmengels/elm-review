module Simplify.Infer exposing
    ( DeducedValue(..)
    , Fact(..)
    , Inferred(..)
    , Resources
    , deduceNewFacts
    , empty
    , falseExpr
    , fromList
    , get
    , infer
    , inferForIfCondition
    , isBoolean
    , trueExpr
    )

{-| Infers values from `if` conditions.

This is meant to simplify expressions like the following:

```diff
if a then
   -- we know that `a` is True
-  if a && b then
+  if b then
```


### Mechanism

The way that this is done is by collecting "facts" about the conditions we've found. Given the following expression:

    if a && b == 1 then
        1

    else
        2

we can infer that in the `then` branch, the following facts are true:

  - `a && b == 1` is True
  - `a` is True
  - `b == 1` is True
  - `b` equals `1`

and for the `else` branch, that:

  - `a && b == 1` is False
  - `a` is False OR `b == 1` is False (or that `b` does not equal `1`, not sure how we represent this at the moment)

For a condition like `a || b`, we know that in the `then` branch:

  - `a` is True OR `b` is True

and that in the `else` branch:

  - `a || b` is `False`
  - `a` is `False`
  - `b` is `False`

Whenever we get a new fact from a new `if` condition, we then go through all the previously known facts and see if the
new one can simplify some of the old ones to generate new facts.

For instance, if we knew that `a` is True OR `b` is True, and we encounter `if a then`, then we can infer that for the `else` branch `a` is False.
When comparing that to `a` is True OR `b` is True, we can infer that `b` is True.

Every new fact that we uncover from this comparison will also repeat the process of going through the previous list of facts.

Another thing that we do whenever we encounter a new fact os to try and "deduce" a value from it, which we add to a list
of "deduced" values. A few examples:

  - `a` -> `a` is True
  - `a == 1` -> `a` is equal to `1`
  - `a /= 1` -> Can't infer individual values when this is True
  - `a` OR `b` -> Can't infer individual values when this is True

(with the exception that we can infer that the whole expression is `True` or `False`)

Before we do all of this analysis, we normalize the AST, so we have a more predictable AST and don't have to do as many checks.


### Application

This data is then used in `Normalize` to change the AST, so that a reference to `a` whose value we have "deduced" is
replaced by that value. Finally, that data is also used in functions like `Evaluate.getBoolean`.
(Note: This might be a bit redundant but that's a simplification for later on)

Whenever we see a boolean expression, we will look at whether we can simplify it, and report an error when that happens.


### Limits

The current system has a few holes meaning some things we could infer aren't properly handled, and I'd love help with that.
From the top of my mind, I think that `if x /= 1 then (if x == 1 then ...)` (or some variation) does not get simplified when it could.

We are making special exception for numbers for equality, but we could do more: handling `String`, `Char` and probably more.

The system does not currently handle `case` expressions. While handling pattern matching against literals should not be
too hard with the current system, storing "shapes" of the value (the value is a `Just` of something) probably requires
some work.

-}

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type Inferred
    = Inferred
        { facts : List Fact
        , deduced : AssocList.Dict Expression DeducedValue
        }


type DeducedValue
    = DTrue
    | DFalse
    | DNumber Float
    | DString String


type Fact
    = Equals Expression Expression
    | NotEquals Expression Expression
    | Or (List Fact) (List Fact)


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred
        { facts = []
        , deduced = AssocList.empty
        }


fromList : List ( Expression, DeducedValue ) -> Inferred
fromList list =
    Inferred
        { facts = []
        , deduced = AssocList.fromList list
        }


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.map
            (\value ->
                case value of
                    DTrue ->
                        trueExpr

                    DFalse ->
                        falseExpr

                    DNumber float ->
                        Expression.Floatable float

                    DString str ->
                        Expression.Literal str
            )


isBoolean : Expression -> Inferred -> Maybe Bool
isBoolean expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.andThen
            (\value ->
                case value of
                    DTrue ->
                        Just True

                    DFalse ->
                        Just False

                    DNumber _ ->
                        Nothing

                    DString _ ->
                        Nothing
            )


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] True inferred )
    , ( falseBranchRange, infer [ condition ] False inferred )
    ]


trueExpr : Expression
trueExpr =
    Expression.FunctionOrValue [ "Basics" ] "True"


falseExpr : Expression
falseExpr =
    Expression.FunctionOrValue [ "Basics" ] "False"


convertToFact : Expression -> Bool -> List Fact
convertToFact expr shouldBe =
    if shouldBe then
        [ Equals expr trueExpr, NotEquals expr falseExpr ]

    else
        [ Equals expr falseExpr, NotEquals expr trueExpr ]


infer : List Expression -> Bool -> Inferred -> Inferred
infer nodes shouldBe acc =
    List.foldl (inferHelp shouldBe) acc nodes


inferHelp : Bool -> Expression -> Inferred -> Inferred
inferHelp shouldBe node acc =
    let
        dict : Inferred
        dict =
            injectFacts (convertToFact node shouldBe) acc
    in
    case node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
            inferHelp (not shouldBe) (Node.value expression) dict

        Expression.OperatorApplication "&&" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                infer [ left, right ] shouldBe dict

            else
                injectFacts
                    [ Or
                        (convertToFact left False)
                        (convertToFact right False)
                    ]
                    dict

        Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                injectFacts
                    [ Or
                        (convertToFact left True)
                        (convertToFact right True)
                    ]
                    dict

            else
                infer [ left, right ] shouldBe dict

        Expression.OperatorApplication "==" inf left right ->
            dict
                |> (if shouldBe then
                        injectFacts [ NotEquals (Expression.OperatorApplication "/=" inf left right) trueExpr ]

                    else
                        identity
                   )
                |> inferOnEquality left right shouldBe
                |> inferOnEquality right left shouldBe

        Expression.OperatorApplication "/=" inf left right ->
            dict
                |> (if shouldBe then
                        injectFacts [ NotEquals (Expression.OperatorApplication "==" inf left right) trueExpr ]

                    else
                        identity
                   )
                |> inferOnEquality left right (not shouldBe)
                |> inferOnEquality right left (not shouldBe)

        _ ->
            dict


injectFacts : List Fact -> Inferred -> Inferred
injectFacts newFacts (Inferred inferred) =
    case newFacts of
        [] ->
            Inferred inferred

        newFact :: restOfFacts ->
            if List.member newFact inferred.facts then
                injectFacts
                    restOfFacts
                    (Inferred inferred)

            else
                let
                    newFactsToVisit : List Fact
                    newFactsToVisit =
                        deduceNewFacts newFact inferred.facts

                    deducedFromNewFact : Maybe ( Expression, DeducedValue )
                    deducedFromNewFact =
                        case newFact of
                            Equals a b ->
                                equalsFact a b

                            NotEquals a b ->
                                equalsFact a b
                                    |> Maybe.andThen notDeduced

                            Or _ _ ->
                                -- TODO Add "a || b || ..."?
                                Nothing
                in
                injectFacts
                    (newFactsToVisit ++ restOfFacts)
                    (Inferred
                        { facts = newFact :: inferred.facts
                        , deduced =
                            case deducedFromNewFact of
                                Just ( a, b ) ->
                                    AssocList.insert a b inferred.deduced

                                Nothing ->
                                    inferred.deduced
                        }
                    )


deduceNewFacts : Fact -> List Fact -> List Fact
deduceNewFacts newFact facts =
    case newFact of
        Equals factTarget factValue ->
            case expressionToDeduced factValue of
                Just value ->
                    List.concatMap (mergeEqualFacts ( factTarget, value )) facts

                Nothing ->
                    [ Equals factValue factTarget ]

        NotEquals _ _ ->
            []

        Or _ _ ->
            []


equalsFact : Expression -> Expression -> Maybe ( Expression, DeducedValue )
equalsFact a b =
    case expressionToDeduced a of
        Just deducedValue ->
            Just ( b, deducedValue )

        Nothing ->
            case expressionToDeduced b of
                Just deducedValue ->
                    Just ( a, deducedValue )

                Nothing ->
                    Nothing


expressionToDeduced : Expression -> Maybe DeducedValue
expressionToDeduced expression =
    case expression of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            Just DTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Just DFalse

        Expression.Floatable float ->
            Just (DNumber float)

        Expression.Literal string ->
            Just (DString string)

        _ ->
            Nothing


notDeduced : ( a, DeducedValue ) -> Maybe ( a, DeducedValue )
notDeduced ( a, deducedValue ) =
    case deducedValue of
        DTrue ->
            Just ( a, DFalse )

        DFalse ->
            Just ( a, DTrue )

        _ ->
            Nothing


mergeEqualFacts : ( Expression, DeducedValue ) -> Fact -> List Fact
mergeEqualFacts equalFact fact =
    case fact of
        Or left right ->
            List.filterMap (ifSatisfy equalFact)
                (List.map (\cond -> ( cond, right )) left
                    ++ List.map (\cond -> ( cond, left )) right
                )
                |> List.concat

        _ ->
            []


ifSatisfy : ( Expression, DeducedValue ) -> ( Fact, a ) -> Maybe a
ifSatisfy ( target, value ) ( targetFact, otherFact ) =
    case targetFact of
        Equals factTarget factValue ->
            if factTarget == target && areIncompatible value factValue then
                Just otherFact

            else
                Nothing

        NotEquals factTarget factValue ->
            if factTarget == target && areCompatible value factValue then
                Just otherFact

            else
                Nothing

        _ ->
            Nothing


areIncompatible : DeducedValue -> Expression -> Bool
areIncompatible value factValue =
    case ( value, factValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DNumber valueFloat, Expression.Floatable factFloat ) ->
            valueFloat /= factFloat

        ( DString valueString, Expression.Literal factString ) ->
            valueString /= factString

        _ ->
            False


areCompatible : DeducedValue -> Expression -> Bool
areCompatible value factValue =
    case ( value, factValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DNumber valueFloat, Expression.Floatable factFloat ) ->
            valueFloat == factFloat

        ( DString valueString, Expression.Literal factString ) ->
            valueString == factString

        _ ->
            False


inferOnEquality : Node Expression -> Node Expression -> Bool -> Inferred -> Inferred
inferOnEquality (Node _ expr) (Node _ other) shouldBe dict =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

        Expression.Floatable float ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Floatable float) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Floatable float) ]
                    dict

        Expression.Literal str ->
            if shouldBe then
                injectFacts
                    [ Equals other (Expression.Literal str) ]
                    dict

            else
                injectFacts
                    [ NotEquals other (Expression.Literal str) ]
                    dict

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            injectFacts
                [ Equals other
                    (if shouldBe then
                        trueExpr

                     else
                        falseExpr
                    )
                ]
                dict

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            injectFacts
                [ Equals other
                    (if shouldBe then
                        falseExpr

                     else
                        trueExpr
                    )
                ]
                dict

        _ ->
            dict
