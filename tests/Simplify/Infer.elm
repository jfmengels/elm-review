module Simplify.Infer exposing
    ( DeducedValue(..)
    , Fact(..)
    , Inferred(..)
    , Resources
    , deduceNewFacts
    , empty
    , falseExpr
    , fromList
    , getAsExpression
    , infer
    , inferForIfCondition
    , inferredNormalize
    , inferredToDebugString
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

Another thing that we do whenever we encounter a new fact is to try and "deduce" a value from it, which we add to a list
of "deduced" values. A few examples:

  - `a` -> `a` is True
  - `a == 1` -> `a` is equal to `1`
  - `a /= 1` -> Can't infer individual values when this is True
  - `a` OR `b` -> Can't infer individual values when this is True

(with the exception that we can infer that the whole expression is `True` or `False`)

Before we do all of this analysis, we normalize the AST, so we have a more predictable AST and don't have to do as many checks.


### Application

This data is then used in `Normalize` to change the AST, so that a reference to `a` whose value we have "deduced" is
replaced by that value. Finally, as a consequence, that data is also used in functions like `Normalize.getBoolean`.
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
import Elm.Writer
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


getAsExpression : Expression -> Inferred -> Maybe Expression
getAsExpression expr (Inferred inferred) =
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


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer condition True inferred )
    , ( falseBranchRange, infer condition False inferred )
    ]


trueExpr : Expression
trueExpr =
    Expression.FunctionOrValue [ "Basics" ] "True"


falseExpr : Expression
falseExpr =
    Expression.FunctionOrValue [ "Basics" ] "False"


infer : Expression -> Bool -> Inferred -> Inferred
infer node shouldBe acc =
    injectFacts
        (inferHelp shouldBe node [])
        acc


infer2 : Expression -> Expression -> Bool -> List Fact -> List Fact
infer2 a b shouldBe soFar =
    inferHelp shouldBe b (inferHelp shouldBe a soFar)


inferHelp : Bool -> Expression -> List Fact -> List Fact
inferHelp shouldBe node soFar =
    let
        soFarWithCurrentNode : List Fact
        soFarWithCurrentNode =
            if shouldBe then
                Equals node trueExpr :: NotEquals node falseExpr :: soFar

            else
                Equals node falseExpr :: NotEquals node trueExpr :: soFar
    in
    case node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
            inferHelp (not shouldBe) (Node.value expression) soFarWithCurrentNode

        Expression.OperatorApplication "&&" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                infer2 left right True soFarWithCurrentNode

            else
                Or
                    (inferHelp False left [])
                    (inferHelp False right [])
                    :: soFarWithCurrentNode

        Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                Or
                    (inferHelp True left [])
                    (inferHelp True right [])
                    :: soFarWithCurrentNode

            else
                infer2 left right False soFarWithCurrentNode

        Expression.OperatorApplication "==" inf left right ->
            (if shouldBe then
                NotEquals (Expression.OperatorApplication "/=" inf left right) trueExpr
                    :: soFarWithCurrentNode

             else
                soFarWithCurrentNode
            )
                |> inferOnEquality left right shouldBe
                |> inferOnEquality right left shouldBe

        Expression.OperatorApplication "/=" inf left right ->
            (if shouldBe then
                NotEquals (Expression.OperatorApplication "==" inf left right) trueExpr
                    :: soFarWithCurrentNode

             else
                soFarWithCurrentNode
            )
                |> inferOnEquality left right (not shouldBe)
                |> inferOnEquality right left (not shouldBe)

        _ ->
            soFarWithCurrentNode


injectFacts : List Fact -> Inferred -> Inferred
injectFacts newFacts (Inferred inferred) =
    case newFacts of
        [] ->
            Inferred inferred

        newFact :: restOfFacts ->
            if List.member newFact inferred.facts then
                injectFacts restOfFacts (Inferred inferred)

            else
                injectFacts
                    (deduceNewFacts newFact inferred.facts ++ restOfFacts)
                    (inferredAddFact newFact (Inferred inferred))


inferredAddFact : Fact -> Inferred -> Inferred
inferredAddFact newFact (Inferred inferred) =
    Inferred
        { facts = newFact :: inferred.facts
        , deduced =
            case newFact of
                Equals equalsA equalsB ->
                    case equalsFact equalsA equalsB of
                        Just ( toInsertA, toInsertB ) ->
                            AssocList.insert toInsertA toInsertB inferred.deduced

                        Nothing ->
                            inferred.deduced

                NotEquals notEqualsA notEqualsB ->
                    case equalsFact notEqualsA notEqualsB |> Maybe.andThen notDeduced of
                        Just ( toInsertA, toInsertB ) ->
                            AssocList.insert toInsertA toInsertB inferred.deduced

                        Nothing ->
                            inferred.deduced

                Or _ _ ->
                    -- TODO Add "a || b || ..."?
                    inferred.deduced
        }


deduceNewFacts : Fact -> List Fact -> List Fact
deduceNewFacts newFact facts =
    case newFact of
        Equals factTarget factValue ->
            List.concatMap
                (\fact -> factSimplifyForEqual ( factTarget, factValue ) fact)
                facts

        NotEquals factTarget factValue ->
            List.concatMap
                (\fact -> factSimplifyForNotEqual ( factTarget, factValue ) fact)
                facts

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


{-| Trim impossible branches
-}
factSimplifyForEqual : ( Expression, Expression ) -> Fact -> List Fact
factSimplifyForEqual equalFact fact =
    factSimplifyFor
        { isNever = \cond -> isNeverEqualInFact equalFact cond
        }
        fact


{-| Trim impossible branches
-}
factSimplifyForNotEqual : ( Expression, Expression ) -> Fact -> List Fact
factSimplifyForNotEqual notEqualFact fact =
    factSimplifyFor
        { isNever = \cond -> isAlwaysEqualInFact notEqualFact cond
        }
        fact


factSimplifyFor :
    { isNever : Fact -> Bool }
    -> Fact
    -> List Fact
factSimplifyFor factTrue fact =
    case fact of
        Or left right ->
            -- if any branch has a part that is false, that whole branch can never be true
            -- and can be disregarded
            if List.any factTrue.isNever left then
                right

            else if List.any factTrue.isNever right then
                left

            else
                []

        _ ->
            []


isNeverEqualInFact : ( Expression, Expression ) -> Fact -> Bool
isNeverEqualInFact ( target, value ) targetFact =
    case targetFact of
        Equals factTarget factValue ->
            factTarget == target && areNeverEqual value factValue

        NotEquals factTarget factValue ->
            factTarget == target && value == factValue

        Or _ _ ->
            False


isAlwaysEqualInFact : ( Expression, Expression ) -> Fact -> Bool
isAlwaysEqualInFact ( target, value ) targetFact =
    case targetFact of
        Equals factTarget factValue ->
            factTarget == target && value == factValue

        NotEquals factTarget factValue ->
            factTarget == target && areNeverEqual value factValue

        Or _ _ ->
            False


areNeverEqual : Expression -> Expression -> Bool
areNeverEqual value factValue =
    case value of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            case factValue of
                Expression.FunctionOrValue [ "Basics" ] "False" ->
                    True

                _ ->
                    False

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            case factValue of
                Expression.FunctionOrValue [ "Basics" ] "True" ->
                    True

                _ ->
                    False

        Expression.Floatable valueFloat ->
            case factValue of
                Expression.Floatable factFloat ->
                    valueFloat /= factFloat

                _ ->
                    False

        Expression.Literal valueString ->
            case factValue of
                Expression.Literal factString ->
                    valueString /= factString

                _ ->
                    False

        _ ->
            False


inferOnEquality : Node Expression -> Node Expression -> Bool -> List Fact -> List Fact
inferOnEquality (Node _ expr) (Node _ other) shouldBe soFar =
    case expr of
        Expression.Floatable float ->
            if shouldBe then
                Equals other (Expression.Floatable float)
                    :: soFar

            else
                NotEquals other (Expression.Floatable float)
                    :: soFar

        Expression.Literal str ->
            if shouldBe then
                Equals other (Expression.Literal str)
                    :: soFar

            else
                NotEquals other (Expression.Literal str)
                    :: soFar

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            Equals other
                (if shouldBe then
                    trueExpr

                 else
                    falseExpr
                )
                :: soFar

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Equals other
                (if shouldBe then
                    falseExpr

                 else
                    trueExpr
                )
                :: soFar

        _ ->
            soFar



--


{-| For use in tests and Debug.log only
-}
inferredNormalize : Inferred -> Inferred
inferredNormalize (Inferred inferred) =
    Inferred
        { facts =
            inferred.facts
                |> List.sortBy factToDebugString
        , deduced =
            AssocList.toList inferred.deduced
                |> List.sortBy
                    (\( expression, _ ) -> expressionToDebugString expression)
                |> AssocList.fromList
        }


{-| For use in tests and Debug.log only
-}
inferredToDebugString : Inferred -> String
inferredToDebugString (Inferred inferred) =
    "facts: "
        ++ factsToDebugString inferred.facts
        ++ "\ndeduced: "
        ++ (List.map
                (\( from, to ) ->
                    expressionToDebugString from
                        ++ " = "
                        ++ (to |> deducedValueToDebugString)
                )
                (AssocList.toList inferred.deduced)
                |> String.join ", "
           )


deducedValueToDebugString : DeducedValue -> String
deducedValueToDebugString deducedValue =
    case deducedValue of
        DTrue ->
            "✅"

        DFalse ->
            "❌"

        DNumber number ->
            String.fromFloat number

        DString string ->
            string


expressionToDebugString : Expression -> String
expressionToDebugString expression =
    Elm.Writer.write (Elm.Writer.writeExpression (Node.empty expression))
        |> String.replace "Basics.True" "✅"
        |> String.replace "Basics.False" "❌"
        |> String.replace "Basics." ""


factsToDebugString : List Fact -> String
factsToDebugString facts =
    -- when debugging large fact lists,
    -- you may want to insert a filter ignoring
    -- NotEquals left right -> right == trueExpr || right == falseExpr
    case facts of
        [] ->
            "_empty_"

        [ single ] ->
            factToDebugString single

        (_ :: _ :: _) as filteredFacts ->
            "("
                ++ (filteredFacts |> List.map factToDebugString |> String.join " & ")
                ++ ")"


factToDebugString : Fact -> String
factToDebugString fact =
    case fact of
        Equals l r ->
            expressionToDebugString l ++ "=" ++ expressionToDebugString r

        NotEquals l r ->
            expressionToDebugString l ++ "≠" ++ expressionToDebugString r

        Or l r ->
            "(" ++ factsToDebugString l ++ " | " ++ factsToDebugString r ++ ")"
