module CognitiveComplexity exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Json.Encode as Encode
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports functions that have a too high cognitive complexity.

You can configure the threshold above which a function will be reported (`15` in the example configuration below).

    config =
        [ CognitiveComplexity.rule 15
        ]


## What is cognitive complexity?

Cognitive complexity is **not to be confused with "Cyclomatic Complexity"**, which has a different way of measuring the
complexity.

Here's an explanation extracted from [free white paper](https://www.sonarsource.com/resources/white-papers/cognitive-complexity/)
provided by SonarSource, the creators of the concept.

> Cognitive complexity tries to measure how hard it is to understand a function, primarily focusing on the control structures
> that hinder the understanding of a function by reading it from top to bottom in one go, like you would for a novel.

> A Cognitive Complexity score is assessed according to three basic rules:

> 1.  Ignore structures that allow multiple statements to be readably shorthanded into one
> 2.  Increment (add one) for each break in the linear flow of the code
> 3.  Increment when flow-breaking structures are nested

Some small differences may be found between the implementation detailed in the paper and this rule, as the idea was
formulated more on imperative programming languages, and may not be applicable to a pure functional language like Elm.

You can read about how is works in the [complexity breakdown section](#complexity-breakdown) below.


## When (not) to enable this rule

This rule is an experiment. I don't know if this will be more useful or detrimental, and I haven't yet figured out what
the ideal complexity threshold for Elm projects is.

I would for now recommend to use it with a very high threshold to find places in your codebase that need refactoring,
and eventually to enable it in your configuration to make sure no new extremely complex functions appear. As you refactor more
and more of your codebase, you can gradually lower the threshold until you reach a level that you feel happy with.

Please let me know how enabling this rule works out for you! If enforcing doesn't work for you, then you can use this as
an insight rule instead.


## Use as an insight rule

If instead of enforcing a threshold, you wish to have an overview of the complexity for each function, you can run the
rule as an insight rule (using `elm-review --report=json --extract`), which would yield an output like the following:

```json
{
  "Some.Module": {
    "someFunction": 16,
    "someOtherFunction": 0
  },
  "Some.Other.Module": {
    "awesomeFunction": 2
  }
}
```


## Complexity breakdown

Following is a breakdown of how the complexity of a function is computed:

  - If expression: Increases complexity by 1 + nesting, and increases nesting.
    `else if` also increases complexity by 1 + nesting, but doesn't further increase the nesting.
    `else` doesn't increase the complexity.

```js
-- Total: 4
a =
  if b then           -- +1
    if c then         -- +2, including 1 for nesting
      1
    else
      2
  else if d then      -- +1
      3
  else                -- +0
      4
```

  - Case expression: Increases complexity by 1 + nesting, regardless of how many cases are handled, and increases nesting.

```js
-- Total: 3
a =
  case b of -- +1
    A -> 1
    B -> 2
    C ->
      case c of -- +2, including 1 for nesting
        _ -> 3
    D -> 4
```

  - Let functions: Increases nesting.

```js
-- Total: 2
a =
  let
    fn b =   -- increases nesting
      if b then    -- +2, including 1 for nesting
        1
      else
        2

    constant = -- Not a function, no increase
      True
  in
  fn constant
```

  - Anonymous functions: Increases nesting.

```js
-- Total: 2
a things =
  List.map
    (\thing ->        -- increases nesting
      case thing of   -- +2, including 1 for nesting
        Just _ -> 1
        Nothing -> 2
    )
    things
```

  - Logical operator suites: Increase complexity by 1 for every discontinuation of the suite

```elm
a && b -- +1

-- This is still the same logical construction as
-- above, and therefore about as hard to understand
a && b && c && d && e -- +1

-- Total: 3
a && b && c -- +1
  || d -- +1 for breaking the chain of && with a ||
  || not (e || f) -- +1 for breaking the chain
                  -- with a `not` of a binary operation
```

  - Recursive function calls (direct or indirect): Increase complexity by 1 for each different call

```js
-- Total: 2
fun1 n =
  fun2 n    -- +1
  + fun2 n  -- +0, already counted
  + fun1 n  -- +1

-- Total: 1
fun2 n =
  fun1 n    -- +1
```

[The original metric](https://www.sonarsource.com/docs/CognitiveComplexity.pdf) increases the complexity for other
structures that the Elm language doesn't have.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-cognitive-complexity/example --rules CognitiveComplexity
```

The cognitive complexity is set to 15 in the configuration used by the example.

If instead of enforcing a threshold, you wish to have an overview of the complexity for each function, you can run the
rule like this (requires [`jq`](https://stedolan.github.io/jq/)):

```bash
elm-review --template jfmengels/elm-review-cognitive-complexity/example --extract --report=json --rules CognitiveComplexity | jq -r '.extracts.CognitiveComplexity'
```


## Thanks

Thanks to the team at SonarSource for designing the metric and for not restricting its use.
Thanks to G. Ann Campbell for the different talks she made on the subject.

-}
rule : Int -> Rule
rule threshold =
    Rule.newProjectRuleSchema "CognitiveComplexity" initialContext
        |> Rule.withModuleVisitor (moduleVisitor threshold)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor : Int -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor threshold schema =
    schema
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation (finalModuleEvaluation threshold)


type alias ProjectContext =
    Dict String ComplexityDict


type alias ComplexityDict =
    Dict String Int


type alias ModuleContext =
    { nesting : Int
    , operandsToIgnore : List Range
    , elseIfToIgnore : List Range
    , rangesWhereNestingIncreases : List Range
    , increases : List Increase
    , references : Dict String Location
    , functionsToReport : List FunctionToReport
    }


type alias FunctionToReport =
    { functionName : Node String
    , increases : List Increase
    , references : Dict String Location
    }


type alias Increase =
    { line : Location
    , increase : Int
    , nesting : Int
    , kind : IncreaseKind
    }


type IncreaseKind
    = If
    | ElseIf
    | Case
    | Operator String
    | RecursiveCall
    | IndirectRecursiveCall String


initialContext : ProjectContext
initialContext =
    Dict.empty


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (always
            { nesting = 0
            , operandsToIgnore = []
            , elseIfToIgnore = []
            , rangesWhereNestingIncreases = []
            , references = Dict.empty
            , increases = []
            , functionsToReport = []
            }
        )


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleName moduleContext ->
            let
                recursiveCalls : RecursiveCalls
                recursiveCalls =
                    computeRecursiveCalls moduleContext
            in
            List.foldl
                (\functionToReport acc ->
                    let
                        allIncreases : List Increase
                        allIncreases =
                            computeIncreases recursiveCalls functionToReport

                        finalComplexity : Int
                        finalComplexity =
                            List.foldl (\{ increase } complexity -> increase + complexity) 0 allIncreases
                    in
                    Dict.insert (Node.value functionToReport.functionName) finalComplexity acc
                )
                Dict.empty
                moduleContext.functionsToReport
                |> Dict.singleton (String.join "." moduleName)
        )
        |> Rule.withModuleName


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts =
    Dict.union


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionEnterVisitor node context =
    if List.member (Node.range node) context.rangesWhereNestingIncreases then
        ( [], expressionEnterVisitorHelp node { context | nesting = context.nesting + 1 } )

    else
        ( [], expressionEnterVisitorHelp node context )


expressionEnterVisitorHelp : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.IfBlock _ _ else_ ->
            if not (List.member (Node.range node) context.elseIfToIgnore) then
                { context
                    | increases =
                        { line = (Node.range node).start
                        , increase = context.nesting + 1
                        , nesting = context.nesting
                        , kind = If
                        }
                            :: context.increases
                    , nesting = context.nesting + 1
                    , elseIfToIgnore = Node.range else_ :: context.elseIfToIgnore
                }

            else
                -- This if expression is an else if
                -- We want to increase the complexity but keep the same nesting as the parent if
                { context
                    | increases =
                        { line = (Node.range node).start
                        , increase = context.nesting
                        , nesting = context.nesting - 1
                        , kind = ElseIf
                        }
                            :: context.increases
                    , elseIfToIgnore = Node.range else_ :: context.elseIfToIgnore
                }

        Expression.CaseExpression _ ->
            { context
                | increases =
                    { line = (Node.range node).start
                    , increase = context.nesting + 1
                    , nesting = context.nesting
                    , kind = Case
                    }
                        :: context.increases
                , nesting = context.nesting + 1
            }

        Expression.LetExpression { declarations } ->
            { context | rangesWhereNestingIncreases = computeRangesForLetDeclarations declarations ++ context.rangesWhereNestingIncreases }

        Expression.OperatorApplication operator _ left right ->
            if (operator == "&&" || operator == "||") && not (List.member (Node.range node) context.operandsToIgnore) then
                let
                    ( increases, operandsToIgnore ) =
                        incrementAndIgnoreForOperands
                            operator
                            []
                            left
                            right
                in
                { context
                    | increases =
                        { line = (Node.range node).start
                        , increase = 1
                        , nesting = 0
                        , kind = Operator operator
                        }
                            :: increases
                            ++ context.increases
                    , operandsToIgnore = operandsToIgnore ++ context.operandsToIgnore
                }

            else
                context

        Expression.LambdaExpression _ ->
            { context | nesting = context.nesting + 1 }

        Expression.FunctionOrValue [] name ->
            { context
                | references =
                    if Dict.member name context.references then
                        -- The reference already exists, and we want to keep the first reference
                        -- for a better presentation
                        context.references

                    else
                        Dict.insert name (Node.range node).start context.references
            }

        _ ->
            context


computeRangesForLetDeclarations : List (Node Expression.LetDeclaration) -> List Range
computeRangesForLetDeclarations declarations =
    List.filterMap
        (\letDecl ->
            case Node.value letDecl of
                Expression.LetFunction { declaration } ->
                    if List.isEmpty (Node.value declaration).arguments then
                        Nothing

                    else
                        Just (declaration |> Node.value |> .expression |> Node.range)

                Expression.LetDestructuring _ _ ->
                    Nothing
        )
        declarations


incrementAndIgnoreForOperands : String -> List Increase -> Node Expression -> Node Expression -> ( List Increase, List Range )
incrementAndIgnoreForOperands operator increases left right =
    let
        ( leftIncreases, leftIgnore ) =
            incrementAndIgnore operator left

        ( rightIncreases, rightIgnore ) =
            incrementAndIgnore operator right
    in
    ( List.concat [ leftIncreases, rightIncreases, increases ]
    , Node.range left :: Node.range right :: leftIgnore ++ rightIgnore
    )


incrementAndIgnore : String -> Node Expression -> ( List Increase, List Range )
incrementAndIgnore parentOperator node =
    case Node.value node of
        Expression.OperatorApplication operator _ left right ->
            if operator == "&&" || operator == "||" then
                let
                    newOperatorIncrease : List Increase
                    newOperatorIncrease =
                        if operator == parentOperator then
                            []

                        else
                            [ { line = (Node.range node).start
                              , increase = 1
                              , nesting = 0
                              , kind = Operator operator
                              }
                            ]
                in
                incrementAndIgnoreForOperands
                    operator
                    newOperatorIncrease
                    left
                    right

            else
                ( [], [] )

        _ ->
            ( [], [] )


expressionExitVisitor : Node Expression -> ModuleContext -> ( List nothing, ModuleContext )
expressionExitVisitor node context =
    if List.member (Node.range node) context.rangesWhereNestingIncreases then
        ( [], expressionExitVisitorHelp node { context | nesting = context.nesting - 1 } )

    else
        ( [], expressionExitVisitorHelp node context )


expressionExitVisitorHelp : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitorHelp node context =
    case Node.value node of
        Expression.IfBlock _ _ _ ->
            if not (List.member (Node.range node) context.elseIfToIgnore) then
                { context | nesting = context.nesting - 1 }

            else
                context

        Expression.CaseExpression _ ->
            { context | nesting = context.nesting - 1 }

        Expression.LambdaExpression _ ->
            { context | nesting = context.nesting - 1 }

        _ ->
            context


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationExitVisitor node context =
    let
        functionsToReport : List FunctionToReport
        functionsToReport =
            case Node.value node of
                Declaration.FunctionDeclaration function ->
                    { functionName = function.declaration |> Node.value |> .name
                    , increases = context.increases
                    , references = context.references
                    }
                        :: context.functionsToReport

                _ ->
                    context.functionsToReport
    in
    ( []
    , { nesting = 0
      , operandsToIgnore = []
      , elseIfToIgnore = []
      , rangesWhereNestingIncreases = []
      , references = Dict.empty
      , increases = []
      , functionsToReport = functionsToReport
      }
    )


finalModuleEvaluation : Int -> ModuleContext -> List (Rule.Error {})
finalModuleEvaluation threshold context =
    let
        recursiveCalls : RecursiveCalls
        recursiveCalls =
            computeRecursiveCalls context
    in
    List.filterMap
        (\functionToReport ->
            let
                allIncreases : List Increase
                allIncreases =
                    computeIncreases recursiveCalls functionToReport

                finalComplexity : Int
                finalComplexity =
                    List.foldl (\{ increase } acc -> increase + acc) 0 allIncreases
            in
            if finalComplexity > threshold then
                Just
                    (Rule.error
                        { message = Node.value functionToReport.functionName ++ " has a cognitive complexity of " ++ String.fromInt finalComplexity ++ ", higher than the allowed " ++ String.fromInt threshold
                        , details =
                            if List.isEmpty allIncreases then
                                explanation

                            else
                                explanation
                                    ++ [ allIncreases
                                            |> List.sortBy (\{ line } -> ( line.row, line.column ))
                                            |> List.map explain
                                            |> String.join "\n"
                                       ]
                        }
                        (Node.range functionToReport.functionName)
                    )

            else
                Nothing
        )
        context.functionsToReport


computeIncreases : RecursiveCalls -> FunctionToReport -> List Increase
computeIncreases allRecursiveCalls { functionName, increases, references } =
    case Dict.get (Node.value functionName) allRecursiveCalls of
        Just recursiveCalls ->
            Set.foldl
                (\reference acc ->
                    case Dict.get reference references of
                        Just location ->
                            { line = location
                            , increase = 1
                            , nesting = 0
                            , kind =
                                if Node.value functionName == reference then
                                    RecursiveCall

                                else
                                    IndirectRecursiveCall reference
                            }
                                :: acc

                        Nothing ->
                            acc
                )
                increases
                recursiveCalls

        Nothing ->
            increases


computeRecursiveCalls : ModuleContext -> RecursiveCalls
computeRecursiveCalls context =
    let
        potentialRecursiveFunctions : Set String
        potentialRecursiveFunctions =
            List.foldl
                (\fn acc -> Set.insert (Node.value fn.functionName) acc)
                Set.empty
                context.functionsToReport
    in
    context.functionsToReport
        |> List.foldl
            (\{ functionName, references } acc ->
                Dict.insert
                    (Node.value functionName)
                    (Dict.filter (\name _ -> Set.member name potentialRecursiveFunctions) references)
                    acc
            )
            Dict.empty
        |> findRecursiveCalls


explanation : List String
explanation =
    [ "This metric is a heuristic to measure how easy to understand a piece of code is, primarily through increments for breaks in the linear flow and for nesting those breaks."
    , "The most common ways to reduce complexity is to extract sections into functions and to unnest control flow structures. Following is a breakdown of where complexity was found:"
    ]


explain : Increase -> String
explain increase =
    "Line " ++ String.fromInt increase.line.row ++ ": +" ++ String.fromInt increase.increase ++ " for the " ++ kindToString increase.kind ++ mentionNesting increase.nesting


mentionNesting : Int -> String
mentionNesting nesting =
    if nesting == 0 then
        ""

    else
        " (including " ++ String.fromInt nesting ++ " for nesting)"


kindToString : IncreaseKind -> String
kindToString kind =
    case kind of
        If ->
            "if expression"

        ElseIf ->
            "else if expression"

        Case ->
            "case expression"

        Operator operator ->
            "use of `" ++ operator ++ "`"

        RecursiveCall ->
            "recursive call"

        IndirectRecursiveCall fnName ->
            "indirect recursive call to " ++ fnName



-- FINDING RECURSIVE FUNCTIONS
-- Inspired by the algorithm found at https://www.baeldung.com/cs/detecting-recursiveCalls-in-directed-graph


type alias RecursiveCalls =
    Dict String (Set String)


type alias Visited =
    Dict String VisitState


type VisitState
    = InStack
    | Done


findRecursiveCalls : Dict String (Dict String a) -> RecursiveCalls
findRecursiveCalls graph =
    graph
        |> Dict.foldl
            (\vertice _ ( recursiveCalls, visited ) ->
                let
                    res : { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
                    res =
                        processDFSTree
                            graph
                            [ vertice ]
                            (Dict.insert vertice InStack visited)
                in
                ( mergeRecursiveCallsDict res.recursiveCalls recursiveCalls, res.visited )
            )
            ( Dict.empty, Dict.empty )
        |> Tuple.first


mergeRecursiveCallsDict : RecursiveCalls -> RecursiveCalls -> RecursiveCalls
mergeRecursiveCallsDict left right =
    Dict.merge
        (\functionName calls dict -> Dict.insert functionName calls dict)
        (\functionName callsLeft callsRight dict -> Dict.insert functionName (Set.union callsLeft callsRight) dict)
        (\functionName calls dict -> Dict.insert functionName calls dict)
        left
        right
        Dict.empty


processDFSTree : Dict String (Dict String a) -> List String -> Visited -> { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
processDFSTree graph stack visited =
    let
        vertices : List String
        vertices =
            List.head stack
                |> Maybe.andThen (\v -> Dict.get v graph)
                |> Maybe.withDefault Dict.empty
                |> Dict.keys
    in
    List.foldl
        (\vertice acc ->
            case Dict.get vertice visited of
                Just InStack ->
                    { acc | recursiveCalls = insertCycle stack vertice acc.recursiveCalls }

                Just Done ->
                    acc

                Nothing ->
                    let
                        res : { recursiveCalls : RecursiveCalls, visited : Visited, stack : List String }
                        res =
                            processDFSTree
                                graph
                                (vertice :: stack)
                                (Dict.insert vertice InStack visited)
                    in
                    { recursiveCalls = mergeRecursiveCallsDict res.recursiveCalls acc.recursiveCalls, visited = res.visited }
        )
        { recursiveCalls = Dict.empty, visited = visited }
        vertices
        |> (\res ->
                { recursiveCalls = res.recursiveCalls
                , visited =
                    List.head stack
                        |> Maybe.map (\v -> Dict.insert v Done res.visited)
                        |> Maybe.withDefault res.visited
                , stack = List.drop 1 stack
                }
           )


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    Encode.dict identity encodeComplexityDict projectContext


encodeComplexityDict : ComplexityDict -> Encode.Value
encodeComplexityDict dict =
    dict
        |> Dict.toList
        |> List.sortBy (Tuple.second >> negate)
        |> List.map (\( name, complexity ) -> ( name, Encode.int complexity ))
        |> Encode.object


insertCycle : List String -> String -> RecursiveCalls -> RecursiveCalls
insertCycle stack vertice recursiveCalls =
    case stack of
        x :: xs ->
            List.foldl
                (\( functionName, reference ) acc ->
                    Dict.update
                        functionName
                        (Maybe.withDefault Set.empty >> Set.insert reference >> Just)
                        acc
                )
                recursiveCalls
                (toTuples x (takeTop xs ( x, [] ) vertice) [])

        [] ->
            recursiveCalls


toTuples : String -> ( String, List String ) -> List ( String, String ) -> List ( String, String )
toTuples x ( first, xs ) result =
    case xs of
        [] ->
            ( x, first ) :: result

        firstofXs :: restOfXs ->
            toTuples first ( firstofXs, restOfXs ) (( x, first ) :: result)


takeTop : List String -> ( String, List String ) -> String -> ( String, List String )
takeTop stack ( previousValue, previousValues ) stopValue =
    case stack of
        [] ->
            ( previousValue, previousValues )

        x :: xs ->
            if x /= stopValue then
                takeTop xs ( x, previousValue :: previousValues ) stopValue

            else
                ( x, previousValue :: previousValues )
