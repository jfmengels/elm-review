module NoUnoptimizedRecursion exposing
    ( rule
    , Configuration, optOutWithComment, optInWithComment
    )

{-|

@docs rule

Tail-call optimization makes Elm code more performant and helps prevent stack overflows.

Since this optimization is done silently and under specific circumstances, it is unfortunately relatively easy
to not notice when the optimization is not being applied. You can find the [reasons why a function would not be optimized below](#fail).

I wrote a whole [article about tail-call optimization](https://jfmengels.net/tail-call-optimization/). Some of the information
are repeated in this rule's documentation, but it's more complete.


## Configuration

@docs Configuration, optOutWithComment, optInWithComment


## When (not) to enable this rule

This rule is useful for both application maintainers and package authors to detect locations where
performance could be improved and where stack overflows can happen.

You should not enable this rule if you currently do not want to invest your time into thinking about performance.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example --rules NoUnoptimizedRecursion
```

The rule uses `optOutWithComment "IGNORE TCO"` as its configuration.


## Success

This function won't be reported because it is tail-call optimized.

    fun n =
        if condition n then
            fun (n - 1)

        else
            n

This function won't be reported because it has been tagged as ignored.

    -- With opt-out configuration
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        ]

    fun n =
        -- elm-review: IGNORE TCO
        fun n * n

This function won't be reported because it has not been tagged.

    -- With opt-in configuration
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "ENSURE TCO")
        ]

    fun n =
        fun n * n


## Fail

To understand when a function would not get tail-call optimized, it is important to understand when it would be optimized.

The Elm compiler is able to apply tail-call optimization **only** when a recursive call **(1)** is a simple function application and **(2)** is the last operation that the function does in a branch.

**(1)** means that while `recurse n = recurse (n - 1)` would be optimized, `recurse n = recurse <| n - 1` would not. Even though you may consider `<|` and `|>` as syntactic sugar for function calls, the compiler doesn't (at least with regard to TCO).

As for **(2)**, the locations where a recursive call may happen are:

  - branches of an if expression
  - branches of a case expression
  - in the body of a let expression
  - inside simple parentheses

and only if each of the above appeared at the root of the function or in one of the above locations themselves.

The compiler optimizes every recursive call that adheres to the rules above, and simply doesn't optimize the other
branches which would call the function naively and add to the stack frame.
It is therefore possible to have **partially tail-call optimized functions**.

Following is a list of likely situations that will be reported.


### An operation is applied on the result of a function call

The result of this recursive call gets multiplied by `n`, making the recursive call not the last thing to happen in this branch.

    factorial : Int -> Int
    factorial n =
        if n <= 1 then
            1

        else
            factorial (n - 1) * n

Hint: When you need to apply an operation on the result of a recursive call, what you can do is to add an argument holding the result value and apply the operations on it instead.

    factorialHelp : Int -> Int -> Int
    factorialHelp n result =
        if n <= 1 then
            result

        else
            factorialHelp (result * n)

and split the function into the one that will do recursive calls (above) and an "API-facing" function which will set the initial result value (below).

    factorial : Int -> Int
    factorial n =
        factorialHelp n 1


### Calls using the |> or <| operators

Even though you may consider these operators as syntactic sugar for function calls, the compiler doesn't and
the following won't be optimized. The compiler doesn't special-case these functions and considers them as operators just
like `(*)` in the example above.

    fun n =
        if condition n then
            fun <| n - 1

        else
            n
    fun n =
        if condition n then
            (n - 1)
                |> fun

        else
            n

The fix here consists of converting the recursive calls to ones that don't use a pipe operator.


### Calls appearing in || or && conditions

The following won't be optimized.

    isPrefixOf : List a -> List a -> Bool
    isPrefixOf prefix list =
        case ( prefix, list ) of
            ( [], _ ) ->
                True

            ( _ :: _, [] ) ->
                False

            ( p :: ps, x :: xs ) ->
                p == x && isPrefixOf ps xs

The fix here is consists of using if expressions instead.

    isPrefixOf : List a -> List a -> Bool
    isPrefixOf prefix list =
        case ( prefix, list ) of
            ( [], _ ) ->
                True

            ( _ :: _, [] ) ->
                False

            ( p :: ps, x :: xs ) ->
                if p == x then
                    isPrefixOf ps xs

                else
                    False


### Calls from let declarations

Calls from let functions won't be optimized.

    fun n =
        let
            funHelp y =
                fun (y - 1)
        in
        funHelp n

Note that recursive let functions can be optimized if they call themselves, but calling the parent function
will cause the parent to not be optimized.

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports recursive functions that are not [tail-call optimized](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html).
-}
rule : Configuration -> Rule
rule configuration =
    Rule.newModuleRuleSchema "NoUnoptimizedRecursion" initialContext
        |> Rule.withCommentsVisitor (commentsVisitor configuration)
        |> Rule.withDeclarationEnterVisitor (declarationVisitor configuration)
        |> Rule.withExpressionEnterVisitor (expressionEnterVisitor configuration)
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema



-- CONFIGURATION


{-| Configuration for `NoUnoptimizedRecursion`.

Use [`optOutWithComment`](#optOutWithComment) or [`optInWithComment`](#optInWithComment) to configure this rule.

You can use comments to tag functions as to be checked or ignored, depending on the configuration option you chose.
This comment has to appear on the line after the `=` that follows the declaration of your function. Note that this
comment only needs to contain the tag that you're choosing and that it is case-sensitive.
The same will apply for functions defined in a let expression, since they can be tail-call optimized as well.

-}
type Configuration
    = OptOut String
    | OptIn String


{-| Reports recursive functions by default, opt out functions tagged with a comment.

    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        ]

With the configuration above, the following function would not be reported.

    fun n =
        -- elm-review: IGNORE TCO
        if condition n then
            fun n * n

        else
            n

The reasons for allowing to opt-out is because sometimes recursive functions are simply not translatable to
tail-call optimized ones, for instance the ones that need to recurse over multiple elements (`fun left + fun right`).

I recommend to **not** default to ignoring a reported issue, and instead to discuss with your colleagues how to best
solve the error when you encounter it or when you see them ignore an error.

I recommend to use this configuration option as your permanent configuration once you have fixed or opted-out of every function.

-}
optOutWithComment : String -> Configuration
optOutWithComment comment =
    OptOut comment


{-| Reports only the functions tagged with a comment.

    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "ENSURE TCO")
        ]

With the configuration above, the following function would be reported.

    fun n =
        -- ENSURE TCO
        if condition n then
            fun n * n

        else
            n

-}
optInWithComment : String -> Configuration
optInWithComment comment =
    OptIn comment


hasNoArguments : Expression.FunctionImplementation -> Bool
hasNoArguments declaration =
    List.isEmpty declaration.arguments


shouldReportFunction : Configuration -> Context -> Node Expression.FunctionImplementation -> Bool
shouldReportFunction configuration context (Node range declaration) =
    if hasNoArguments declaration then
        False

    else
        let
            foundComment : Bool
            foundComment =
                Set.member (range.start.row + 1) context.comments
        in
        case configuration of
            OptOut _ ->
                not foundComment

            OptIn _ ->
                foundComment



-- CONTEXT


type alias Context =
    { currentFunctionName : String
    , tcoLocations : List Range
    , newScopesForLet : List ( Range, String )
    , parentScopes : List ( Range, Scope )
    , parentNames : Set String
    , comments : Set Int
    , deOptimizationRange : Maybe Range
    , deOptimizationReason : List String
    }


type alias Scope =
    { currentFunctionName : String
    , tcoLocations : List Range
    , newScopes : List ( Range, String )
    }


initialContext : Context
initialContext =
    { currentFunctionName = ""
    , tcoLocations = []
    , newScopesForLet = []
    , parentScopes = []
    , parentNames = Set.empty
    , comments = Set.empty
    , deOptimizationRange = Nothing
    , deOptimizationReason = []
    }



-- VISITORS


commentsVisitor : Configuration -> List (Node String) -> Context -> ( List nothing, Context )
commentsVisitor configuration comments context =
    let
        commentTag : String
        commentTag =
            case configuration of
                OptOut commentTag_ ->
                    commentTag_

                OptIn commentTag_ ->
                    commentTag_
    in
    ( []
    , { context
        | comments =
            List.foldl
                (\(Node range value) acc ->
                    if String.contains commentTag value then
                        Set.insert range.start.row acc

                    else
                        acc
                )
                Set.empty
                comments
      }
    )


declarationVisitor : Configuration -> Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { currentFunctionName =
                    if shouldReportFunction configuration context function.declaration then
                        function.declaration
                            |> Node.value
                            |> .name
                            |> Node.value

                    else
                        ""
              , tcoLocations =
                    [ function.declaration
                        |> Node.value
                        |> .expression
                        |> Node.range
                    ]
              , newScopesForLet = []
              , parentScopes = []
              , parentNames = Set.empty
              , comments = context.comments
              , deOptimizationRange = Nothing
              , deOptimizationReason = []
              }
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Configuration -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor configuration node context =
    let
        newContext : Context
        newContext =
            case context.newScopesForLet of
                [] ->
                    context

                ( range, name ) :: restOfNewScopes ->
                    if range == Node.range node then
                        { currentFunctionName = name
                        , tcoLocations = [ range ]
                        , newScopesForLet = restOfNewScopes
                        , parentScopes =
                            ( range
                            , { currentFunctionName = context.currentFunctionName, tcoLocations = context.tcoLocations, newScopes = restOfNewScopes }
                            )
                                :: context.parentScopes
                        , parentNames = Set.insert context.currentFunctionName context.parentNames
                        , comments = context.comments
                        , deOptimizationRange = context.deOptimizationRange
                        , deOptimizationReason = context.deOptimizationReason
                        }

                    else
                        context
    in
    if isInTcoLocation newContext (Node.range node) then
        ( reportReferencesToParentFunctions node newContext, addAllowedLocation configuration node newContext )

    else
        ( reportRecursiveCallInNonAllowedLocation node newContext, newContext )


reportRecursiveCallInNonAllowedLocation : Node Expression -> Context -> List (Rule.Error {})
reportRecursiveCallInNonAllowedLocation node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            if name == context.currentFunctionName then
                [ error (Node.range node) context.deOptimizationReason ]

            else
                []

        _ ->
            []


reportReferencesToParentFunctions : Node Expression -> Context -> List (Rule.Error {})
reportReferencesToParentFunctions node context =
    case Node.value node of
        Expression.Application ((Node funcRange (Expression.FunctionOrValue [] name)) :: _) ->
            if Set.member name context.parentNames then
                [ error funcRange [ "Among other possible reasons, the recursive call should not appear inside a let declaration." ] ]

            else
                []

        _ ->
            []


error : Range -> List String -> Rule.Error {}
error range additionalDetails =
    Rule.error
        { message = "This function call cannot be tail-call optimized"
        , details =
            additionalDetails
                ++ [ "You can read more about why over at https://package.elm-lang.org/packages/jfmengels/elm-review-performance/latest/NoUnoptimizedRecursion#fail" ]
        }
        range


addAllowedLocation : Configuration -> Node Expression -> Context -> Context
addAllowedLocation configuration node context =
    case Node.value node of
        Expression.Application (function :: _) ->
            { context
                | tcoLocations = Node.range function :: context.tcoLocations
                , deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.IfBlock condition thenBranch elseBranch ->
            { context
                | tcoLocations = Node.range thenBranch :: Node.range elseBranch :: context.tcoLocations
                , deOptimizationRange = Just (Node.range condition)
                , deOptimizationReason = [ "Among other possible reasons, the recursive call should not appear inside an if condition." ]
            }

        Expression.LetExpression { declarations, expression } ->
            addAllowedLocationForLetExpression configuration context declarations expression

        Expression.ParenthesizedExpression expr ->
            {- The following translates to TCO code

               fun x =
                 (fun x)
            -}
            { context | tcoLocations = Node.range expr :: context.tcoLocations }

        Expression.CaseExpression { cases } ->
            let
                tcoLocations : List Range
                tcoLocations =
                    List.foldl (\( _, Node range _ ) acc -> range :: acc) context.tcoLocations cases
            in
            { context
                | tcoLocations = tcoLocations
                , deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, the recursive call should not appear in the pattern to evaluate for a case expression." ]
            }

        Expression.OperatorApplication operator _ _ _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason =
                    List.filterMap identity
                        [ Just "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch."
                        , if operator == "<|" || operator == "|>" then
                            Just ("Removing the usage of `" ++ operator ++ "` may fix the issue here.")

                          else
                            Nothing
                        ]
            }

        Expression.Negation _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are applying operations on the result of recursive call. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.TupledExpression _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are storing the result of recursive call inside a tuple. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.ListExpr _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are storing the result of recursive call inside a list. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.RecordExpr _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are storing the result of recursive call inside a record. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.RecordUpdateExpression _ _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are storing the result of recursive call inside a record. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.RecordAccess _ _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, you are accessing a field on the result of recursive call. The recursive call should be the last thing to happen in this branch." ]
            }

        Expression.LambdaExpression _ ->
            { context
                | deOptimizationRange = Just (Node.range node)
                , deOptimizationReason = [ "Among other possible reasons, the recursive call should not appear inside an anonymous function." ]
            }

        _ ->
            context


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    case context.parentScopes of
        [] ->
            ( [], removeDeOptimizationRangeIfNeeded node context )

        ( headRange, headScope ) :: restOfParentScopes ->
            if headRange == Node.range node then
                ( []
                , removeDeOptimizationRangeIfNeeded node
                    { currentFunctionName = headScope.currentFunctionName
                    , tcoLocations = headScope.tcoLocations
                    , newScopesForLet = headScope.newScopes
                    , parentScopes = restOfParentScopes
                    , parentNames = Set.remove headScope.currentFunctionName context.parentNames
                    , comments = context.comments
                    , deOptimizationRange = context.deOptimizationRange
                    , deOptimizationReason = context.deOptimizationReason
                    }
                )

            else
                ( [], removeDeOptimizationRangeIfNeeded node context )


addAllowedLocationForLetExpression : Configuration -> Context -> List (Node Expression.LetDeclaration) -> Node a -> Context
addAllowedLocationForLetExpression configuration context declarations expression =
    let
        newScopes : List ( Range, String )
        newScopes =
            List.filterMap
                (\decl ->
                    case Node.value decl of
                        Expression.LetFunction function ->
                            let
                                functionDeclaration : Expression.FunctionImplementation
                                functionDeclaration =
                                    Node.value function.declaration
                            in
                            Just
                                ( Node.range functionDeclaration.expression
                                , if shouldReportFunction configuration context function.declaration then
                                    Node.value functionDeclaration.name

                                  else
                                    ""
                                )

                        Expression.LetDestructuring _ _ ->
                            Nothing
                )
                declarations
    in
    { context
        | newScopesForLet = newScopes

        {- The following translates to TCO code

           let
               fun x =
                  fun x
           in
           fun 1
        -}
        , tcoLocations = Node.range expression :: context.tcoLocations
    }


removeDeOptimizationRangeIfNeeded : Node Expression -> Context -> Context
removeDeOptimizationRangeIfNeeded node context =
    if Just (Node.range node) == context.deOptimizationRange then
        { context | deOptimizationRange = Nothing }

    else
        context


isInTcoLocation : Context -> Range -> Bool
isInTcoLocation context range =
    List.member range context.tcoLocations
