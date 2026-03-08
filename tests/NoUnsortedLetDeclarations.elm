module NoUnsortedLetDeclarations exposing
    ( rule
    , RuleConfig, sortLetDeclarations
    , alphabetically, usedInExpressionFirst, usedInExpressionLast, usedInOtherDeclarationsLast, usedInOtherDeclarationsFirst, valuesBeforeFunctions, valuesAfterFunctions
    , glueHelpersBefore, glueHelpersAfter, glueDependenciesBeforeFirstDependent, glueDependenciesAfterFirstDependent, glueDependenciesBeforeLastDependent, glueDependenciesAfterLastDependent
    )

{-|


## Review Rule

@docs rule


## Configuration

@docs RuleConfig, sortLetDeclarations


## Orderings

@docs alphabetically, usedInExpressionFirst, usedInExpressionLast, usedInOtherDeclarationsLast, usedInOtherDeclarationsFirst, valuesBeforeFunctions, valuesAfterFunctions


## Glues

Glues provide a way to "stick" one declaration to another, i.e. to always sort
one declaration alongside another. Note that glues will chain, i.e. if `a` is
glued before `b` and `b` is glued after `c`, then the result will be `c` -> `a`
-> `b` (sorted wherever `c` is sorted to). Glues behave in the following ways:

  - If multiple glues are specified, the first specified will be used.
  - If multiple declarations are glued at the same place, they will be ordered
    by the orderings specified.
  - If glues are not acyclic (i.e. two declarations are glued to each other,
    possibly via intermediates), then all of the involved declarations will not
    be glued and will be sorted normally.

@docs glueHelpersBefore, glueHelpersAfter, glueDependenciesBeforeFirstDependent, glueDependenciesAfterFirstDependent, glueDependenciesBeforeLastDependent, glueDependenciesAfterLastDependent

-}

import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import List.Extra as ListX
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Util exposing (GluedTo(..), allBindingsInPattern, checkSortingWithGlue, countUsesIn, findAllNamesIn, findDependencies, validate)


{-| Reports `let` declarations that are not in the "proper" order.

🔧 Running with `--fix` will automatically sort the declarations.

The proper order of declarations is specified in the rule configuration. See the
[Configuration](#configuration) section below for more information.

    config =
        [ NoUnsortedLetDeclarations.rule
            (NoUnsortedLetDeclarations.sortLetDeclarations
                |> NoUnsortedLetDeclarations.usedInExpressionFirst
                |> NoUnsortedLetDeclarations.alphabetically
            )
        ]


## Fail

    a =
        let
            -- These are used in the expression
            x =
                a

            y =
                b

            -- These are not
            b =
                j

            a =
                i
        in
        x + y

    b =
        let
            -- These are not used in the expression
            a =
                i

            b =
                j

            -- These are
            x =
                a

            y =
                b
        in
        x + y


## Success

    a =
        let
            -- These are used in the expression
            x =
                a

            y =
                b

            -- These are not
            a =
                i

            b =
                j
        in
        x + y


## When (not) to enable this rule

This rule is useful when you want to ensure that your `let` declarations are in
a consistent, predictable order.

This rule is not useful when you want to be able to write `let` declarations in
varying orders throughout your codebase, e.g. if you want to emphasize what
is most important on a case-by-case basis.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template SiriusStarr/elm-review-no-unsorted/example --rules NoUnsortedLetDeclarations
```

-}
rule : RuleConfig r -> Rule
rule (RuleConfig r) =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnsortedLetDeclarations" initialContext
        -- Reverse sort order, as we've been cons-ing them on
        |> Rule.withExpressionEnterVisitor
            (\e c ->
                ( expressionVisitor
                    (RuleConfig
                        { r
                            | glues = List.reverse r.glues
                            , sortBy = List.reverse r.sortBy
                        }
                    )
                    e
                    c
                , c
                )
            )
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


{-| Context for the rule, containing only a source extractor.
-}
type alias Context =
    { extractSource : Range -> String
    }


{-| Create a context with a source extractor.
-}
initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\extractSource () -> { extractSource = extractSource })
        |> Rule.withSourceCodeExtractor


{-| Configuration for this rule. Create a new one with `sortLetDeclarations` and use
orderings to create a hierarchy of sorting.
-}
type RuleConfig r
    = RuleConfig
        { sortBy : List (LetDec -> LetDec -> Order)
        , glues : List Glue
        }


{-| Given a `LetDec` and a list of other `LetDec`s,
-}
type alias Glue =
    ( Int, LetDec ) -> List LetDec -> Maybe GluedTo


{-| A `let` declaration, parsed for full information.
-}
type alias LetDec =
    { range : Range
    , namesBound : Set String
    , usedInExpression : Bool
    , dependentOnBindings : Set String
    , usedInOtherDecs : Bool
    , args : List String
    , glued : Maybe GluedTo
    }


{-| Create a new `RuleConfig`. Use the various orderings to then specify
primary and fallback orderings.
-}
sortLetDeclarations : RuleConfig { noAlphabetical : (), noArgCount : (), noDependency : (), noHelper : (), noUsedInOther : (), noUsedInExpression : () }
sortLetDeclarations =
    RuleConfig { sortBy = [], glues = [] }


{-| Sort declarations alphabetically by the name of their binding. For
destructurings, this will be the name of the actual bindings that are made, in
alphabetical order. For example, the following is sorted alphabetically:

    let
        (Opaque a) =
            i

        ( z, b ) =
            j

        { c, y } =
            k

        d =
            l
    in
    x

-}
alphabetically : RuleConfig { r | noAlphabetical : () } -> RuleConfig r
alphabetically (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 -> compare (Set.toList d1.namesBound) (Set.toList d2.namesBound))
                    :: r.sortBy
        }


{-| Sort declarations with those used in the expression of the `let` block
coming first, then those that aren't. Ties will be broken by the next specified
ordering. For example, the following is sorted by this ordering and then
alphabetically:

    let
        -- These are used in the expression
        x =
            a

        y =
            b

        -- These are not
        a =
            i

        b =
            j
    in
    x + y

-}
usedInExpressionFirst : RuleConfig { r | noUsedInExpression : () } -> RuleConfig r
usedInExpressionFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInExpression, d2.usedInExpression ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in the expression of the `let` block
coming last, with those that aren't coming first. Ties will be broken by the
next specified ordering. For example, the following is sorted by this ordering
and then alphabetically:

    let
        -- These are not used in the expression
        x =
            i

        y =
            j

        -- These are used in the expression
        a =
            x

        b =
            y
    in
    a + b

-}
usedInExpressionLast : RuleConfig { r | noUsedInExpression : () } -> RuleConfig r
usedInExpressionLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInExpression, d2.usedInExpression ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in other declarations coming after those
that are not. Ties will be broken by the next specified ordering. For example,
the following is sorted by this ordering and then alphabetically:

    let
        a =
            x

        b =
            y

        x =
            i

        y =
            j
    in
    0

-}
usedInOtherDeclarationsLast : RuleConfig { r | noUsedInOther : () } -> RuleConfig r
usedInOtherDeclarationsLast (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInOtherDecs, d2.usedInOtherDecs ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations with those used in other declarations coming before those
that are not. Ties will be broken by the next specified ordering. For example,
the following is sorted by this ordering and then alphabetically:

    let
        x =
            i

        y =
            j

        a =
            x

        b =
            y
    in
    0

-}
usedInOtherDeclarationsFirst : RuleConfig { r | noUsedInOther : () } -> RuleConfig r
usedInOtherDeclarationsFirst (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( d1.usedInOtherDecs, d2.usedInOtherDecs ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations that do not have arguments before those that do. Since no
type inference is performed, this does not guarantee that some things that are
functions will not be sorted with values. For example, the following is sorted
by this ordering and then alphabetically:

    let
        -- These do not have arguments
        x =
            a

        y =
            b

        -- These do
        a i =
            i

        b j =
            j
    in
    x + y

-}
valuesBeforeFunctions : RuleConfig { r | noArgCount : () } -> RuleConfig r
valuesBeforeFunctions (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( List.isEmpty d1.args, List.isEmpty d2.args ) of
                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Sort declarations that do not have arguments after those that do. Since no
type inference is performed, this does not guarantee that some things that are
functions will not be sorted with values. For example, the following is sorted
by this ordering and then alphabetically:

    let
        -- These have arguments
        a i =
            i

        b j =
            j

        -- These do not
        x =
            a

        y =
            b
    in
    x + y

-}
valuesAfterFunctions : RuleConfig { r | noArgCount : () } -> RuleConfig r
valuesAfterFunctions (RuleConfig r) =
    RuleConfig
        { r
            | sortBy =
                (\d1 d2 ->
                    case ( List.isEmpty d1.args, List.isEmpty d2.args ) of
                        ( False, True ) ->
                            LT

                        ( True, False ) ->
                            GT

                        _ ->
                            EQ
                )
                    :: r.sortBy
        }


{-| Helpers are declarations that are _not_ used in the expression that are used
in exactly one other declaration. This glue attaches them immediately before the
declaration they are used in.

For example:

    foo input =
        let
            step : Int -> Int -> Int
            step i acc =
                i + acc

            sum : Int
            sum =
                List.foldl step 0 input
        in
        sum + 1

-}
glueHelpersBefore : RuleConfig { r | noHelper : () } -> RuleConfig r
glueHelpersBefore (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be helpers
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate ((==) 1 << Tuple.second)
                            |> Maybe.map (GluedBeforeFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Helpers are declarations that are _not_ used in the expression that are used
in exactly one other declaration. This glue attaches them immediately after the
declaration they are used in.

For example:

    foo input =
        let
            sum : Int
            sum =
                List.foldl step 0 input

            step : Int -> Int -> Int
            step i acc =
                i + acc
        in
        sum + 1

-}
glueHelpersAfter : RuleConfig { r | noHelper : () } -> RuleConfig r
glueHelpersAfter (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be helpers
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate ((==) 1 << Tuple.second)
                            |> Maybe.map (GluedAfterFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are declarations that are _not_ used in the expression that are
used in multiple other declarations. This glue attaches them immediately before
the first declaration they are used in.

For example:

    foo =
        let
            unwrap =
                some func

            a x =
                unwrap x

            b x =
                unwrap x

            c x =
                unwrap x
        in
        bar

-}
glueDependenciesBeforeFirstDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesBeforeFirstDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be dependencies
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedBeforeFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are declarations that are _not_ used in the expression that are
used in multiple other declarations. This glue attaches them immediately after
the first declaration they are used in.

For example:

    foo =
        let
            a x =
                unwrap x

            unwrap =
                some func

            b x =
                unwrap x

            c x =
                unwrap x
        in
        bar

-}
glueDependenciesAfterFirstDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesAfterFirstDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be dependencies
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedAfterFirst << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are declarations that are _not_ used in the expression that are
used in multiple other declarations. This glue attaches them immediately before
the last declaration they are used in.

For example:

    foo =
        let
            a x =
                unwrap x

            b x =
                unwrap x

            unwrap =
                some func

            c x =
                unwrap x
        in
        bar

-}
glueDependenciesBeforeLastDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesBeforeLastDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be dependencies
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedBeforeLast << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Dependencies are declarations that are _not_ used in the expression that are
used in multiple other declarations. This glue attaches them immediately after
the last declaration they are used in.

For example:

    foo =
        let
            a x =
                unwrap x

            b x =
                unwrap x

            c x =
                unwrap x

            unwrap =
                some func
        in
        bar

-}
glueDependenciesAfterLastDependent : RuleConfig { r | noDependency : () } -> RuleConfig r
glueDependenciesAfterLastDependent (RuleConfig r) =
    RuleConfig
        { r
            | glues =
                (\( i, d ) ds ->
                    -- Only decs not used in expression can be dependencies
                    if d.usedInExpression == False then
                        findDependencies ( i, d ) ds
                            |> validate (\( _, numberUsedIn ) -> numberUsedIn > 1)
                            |> Maybe.map (GluedAfterLast << Tuple.first)

                    else
                        Nothing
                )
                    :: r.glues
        }


{-| Visit expressions, checking `let` blocks for sorting.
-}
expressionVisitor : RuleConfig r -> Node Expression -> Context -> List (Error {})
expressionVisitor (RuleConfig { glues, sortBy }) n context =
    case Node.value n of
        LetExpression lb ->
            let
                ( exprsToDecs, exprs ) =
                    List.foldl step ( [], [] ) lb.declarations

                errorRange : Range
                errorRange =
                    let
                        r : Range
                        r =
                            Node.range n
                    in
                    -- Assume that the `let` of a let block is just the first 3 chars
                    { r | end = { row = r.start.row, column = r.start.column + 3 } }

                step :
                    Node LetDeclaration
                    -> ( List (List (Node Expression) -> LetDec), List (Node Expression) )
                    -> ( List (List (Node Expression) -> LetDec), List (Node Expression) )
                step d ( dAcc, eAcc ) =
                    case Node.value d of
                        LetFunction { declaration } ->
                            let
                                { expression, arguments } =
                                    Node.value declaration

                                name : String
                                name =
                                    Node.value (Node.value declaration).name
                            in
                            ( (\es ->
                                { range = Node.range d
                                , namesBound = Set.singleton name
                                , usedInExpression = countUsesIn lb.expression name >= 1
                                , usedInOtherDecs = List.any (\e -> countUsesIn e name >= 1) es
                                , args = List.concatMap allBindingsInPattern arguments
                                , glued = Nothing
                                , dependentOnBindings = findAllNamesIn expression
                                }
                              )
                                :: dAcc
                            , expression :: eAcc
                            )

                        LetDestructuring p expression ->
                            let
                                bs : List String
                                bs =
                                    allBindingsInPattern p
                            in
                            ( (\es ->
                                { range = Node.range d
                                , namesBound = Set.fromList bs
                                , usedInExpression = List.any ((\numUses -> numUses > 0) << countUsesIn lb.expression) bs
                                , usedInOtherDecs = List.any (\e -> List.any ((\numUses -> numUses > 0) << countUsesIn e) bs) es
                                , args = []
                                , glued = Nothing
                                , dependentOnBindings = findAllNamesIn expression
                                }
                              )
                                :: dAcc
                            , expression :: eAcc
                            )

                applyGlues : List LetDec -> Int -> LetDec -> LetDec
                applyGlues ds i d =
                    { d | glued = ListX.findMap (\g -> g ( i, d ) ds) glues }
            in
            ListX.reverseMap (\f -> f exprs) exprsToDecs
                |> (\ds -> List.indexedMap (applyGlues ds) ds)
                |> checkSortingWithGlue context.extractSource "Let declarations" sortBy errorRange

        _ ->
            []
