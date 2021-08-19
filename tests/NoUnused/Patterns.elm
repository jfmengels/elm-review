module NoUnused.Patterns exposing (rule)

{-| Report useless patterns and pattern values that are not used.


# Rule

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer as Writer
import NoUnused.Patterns.NameVisitor as NameVisitor
import NoUnused.RangeDict as RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Report useless patterns and pattern values that are not used.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ NoUnused.Patterns.rule
        ]

This rule looks within let..in blocks and case branches to find any patterns that are unused. It will report any useless patterns as well as any pattern values that are not used.


## Fail

Value `something` is not used:

    case maybe of
        Just something ->
            True

        Nothing ->
            False


## Success

    case maybe of
        Just _ ->
            True

        Nothing ->
            False


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Patterns
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnused.Patterns" initialContext
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : List Scope
    , scopesToCreate : RangeDict (List FoundPattern)
    }


type alias Scope =
    { declared : List FoundPattern
    , used : Set String
    }


type FoundPattern
    = SingleValue
        { name : String
        , range : Range
        , message : String
        , details : List String
        , fix : List Fix
        }
    | RecordPattern
        { fields : List (Node String)
        , recordRange : Range
        }
    | SimplifiablePattern (Rule.Error {})


initialContext : Context
initialContext =
    { scopes = []
    , scopesToCreate = RangeDict.empty
    }



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case RangeDict.get (Node.range node) context.scopesToCreate of
                Just declared ->
                    { context | scopes = { declared = declared, used = Set.empty } :: context.scopes }

                Nothing ->
                    context
    in
    expressionEnterVisitorHelp node newContext


expressionEnterVisitorHelp : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                findPatternsInLetDeclaration : Node Expression.LetDeclaration -> List FoundPattern
                findPatternsInLetDeclaration letDeclaration =
                    case Node.value letDeclaration of
                        Expression.LetFunction _ ->
                            []

                        Expression.LetDestructuring pattern _ ->
                            findPatterns Destructuring pattern
            in
            ( []
            , { context
                | scopes =
                    { declared = List.concatMap findPatternsInLetDeclaration declarations
                    , used = Set.empty
                    }
                        :: context.scopes

                -- Will only be used to remove on exit, we are already adding the declared patterns to the scope above
                , scopesToCreate = RangeDict.insert (Node.range node) [] context.scopesToCreate
              }
            )

        Expression.CaseExpression { cases } ->
            ( []
            , { context
                | scopesToCreate =
                    List.foldl
                        (\( pattern, expr ) scopesToCreate -> RangeDict.insert (Node.range expr) (findPatterns Matching pattern) scopesToCreate)
                        context.scopesToCreate
                        cases
              }
            )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case RangeDict.get (Node.range node) context.scopesToCreate of
        Just _ ->
            report context

        Nothing ->
            ( [], context )


report : Context -> ( List (Rule.Error {}), Context )
report context =
    case context.scopes of
        headScope :: restOfScopes ->
            let
                { singles, records, simplifiablePatterns } =
                    findDeclaredPatterns headScope

                allDeclared : List String
                allDeclared =
                    List.concat
                        [ List.map .name singles
                        , List.concatMap (.fields >> List.map Node.value) records
                        ]

                nonUsedVars : Set String
                nonUsedVars =
                    allDeclared
                        |> Set.fromList
                        |> Set.diff headScope.used

                errors : List (Rule.Error {})
                errors =
                    List.concat
                        [ singleErrors
                        , List.concatMap (recordErrors context) records
                        , simplifiablePatterns
                        ]

                singleErrors : List (Rule.Error {})
                singleErrors =
                    List.filter (\{ name } -> not <| Set.member name headScope.used) singles
                        |> List.map
                            (\pattern ->
                                Rule.errorWithFix
                                    { message = pattern.message
                                    , details = pattern.details
                                    }
                                    pattern.range
                                    pattern.fix
                            )
            in
            ( errors
            , List.foldl
                useValue
                { context | scopes = restOfScopes }
                (Set.toList nonUsedVars)
            )

        _ ->
            ( [], context )


recordErrors : Context -> { fields : List (Node String), recordRange : Range } -> List (Rule.Error {})
recordErrors context { fields, recordRange } =
    if List.isEmpty fields then
        [ Rule.errorWithFix
            { message = "Record pattern is not needed"
            , details = [ "This pattern is redundant and should be replaced with '_'." ]
            }
            recordRange
            [ Fix.replaceRangeBy recordRange "_" ]
        ]

    else
        let
            ( unused, used ) =
                List.partition (isNodeInContext context) fields
        in
        case unused of
            [] ->
                []

            firstNode :: restNodes ->
                let
                    first : String
                    first =
                        Node.value firstNode

                    rest : List String
                    rest =
                        List.map Node.value restNodes

                    ( errorRange, fix ) =
                        case used of
                            [] ->
                                ( recordRange, Fix.replaceRangeBy recordRange "_" )

                            _ ->
                                ( Range.combine (List.map Node.range unused)
                                , Node Range.emptyRange (Pattern.RecordPattern used)
                                    |> Writer.writePattern
                                    |> Writer.write
                                    |> Fix.replaceRangeBy recordRange
                                )
                in
                [ Rule.errorWithFix
                    { message = listToMessage first rest
                    , details = listToDetails first rest
                    }
                    errorRange
                    [ fix ]
                ]


findDeclaredPatterns :
    Scope
    ->
        { singles : List { name : String, range : Range, message : String, details : List String, fix : List Fix }
        , records : List { fields : List (Node String), recordRange : Range }
        , simplifiablePatterns : List (Rule.Error {})
        }
findDeclaredPatterns scope =
    List.foldl
        (\foundPattern acc ->
            case foundPattern of
                SingleValue v ->
                    { acc | singles = v :: acc.singles }

                RecordPattern v ->
                    { acc | records = v :: acc.records }

                SimplifiablePattern simplifiablePatternError ->
                    { acc | simplifiablePatterns = simplifiablePatternError :: acc.simplifiablePatterns }
        )
        { singles = [], records = [], simplifiablePatterns = [] }
        scope.declared


valueVisitor : Node ( ModuleName, String ) -> Context -> ( List (Rule.Error {}), Context )
valueVisitor (Node _ ( moduleName, value )) context =
    case moduleName of
        [] ->
            ( [], useValue value context )

        _ ->
            ( [], context )



--- ON ENTER


findPatterns : PatternUse -> Node Pattern -> List FoundPattern
findPatterns use (Node range pattern) =
    case pattern of
        Pattern.VarPattern name ->
            [ SingleValue
                { name = name
                , message = "Value `" ++ name ++ "` is not used."
                , details = singularReplaceDetails
                , range = range
                , fix = [ Fix.replaceRangeBy range "_" ]
                }
            ]

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            [ SimplifiablePattern
                (Rule.errorWithFix
                    { message = "Tuple pattern is not needed"
                    , details = redundantDetails
                    }
                    range
                    [ Fix.replaceRangeBy range "_" ]
                )
            ]

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            [ SimplifiablePattern
                (Rule.errorWithFix
                    { message = "Tuple pattern is not needed"
                    , details = redundantDetails
                    }
                    range
                    [ Fix.replaceRangeBy range "_" ]
                )
            ]

        Pattern.TuplePattern patterns ->
            List.concatMap (findPatterns use) patterns

        Pattern.RecordPattern fields ->
            [ RecordPattern
                { fields = fields
                , recordRange = range
                }
            ]

        Pattern.UnConsPattern first second ->
            findPatterns use first ++ findPatterns use second

        Pattern.ListPattern patterns ->
            List.concatMap (findPatterns use) patterns

        Pattern.NamedPattern _ patterns ->
            if use == Destructuring && List.all isAllPattern patterns then
                [ SimplifiablePattern
                    (Rule.errorWithFix
                        { message = "Named pattern is not needed"
                        , details = redundantDetails
                        }
                        range
                        [ Fix.replaceRangeBy range "_" ]
                    )
                ]

            else
                List.concatMap (findPatterns use) patterns

        Pattern.AsPattern inner name ->
            findPatternForAsPattern range inner name :: findPatterns use inner

        Pattern.ParenthesizedPattern inner ->
            findPatterns use inner

        _ ->
            []



--- ON EXIT


singularRemoveDetails : List String
singularRemoveDetails =
    [ "You should either use this value somewhere or remove it." ]


singularReplaceDetails : List String
singularReplaceDetails =
    [ "You should either use this value somewhere or replace it with '_'." ]


pluralDetails : List String
pluralDetails =
    [ "You should either use these values somewhere or remove them." ]


redundantDetails : List String
redundantDetails =
    [ "This pattern is redundant and should be replaced with '_'." ]


removeDetails : List String
removeDetails =
    [ "This pattern is redundant and should be removed." ]


andThen :
    (value -> Context -> ( List (Rule.Error {}), Context ))
    -> value
    -> ( List (Rule.Error {}), Context )
    -> ( List (Rule.Error {}), Context )
andThen function value ( errors, context ) =
    let
        ( newErrors, newContext ) =
            function value context
    in
    ( newErrors ++ errors, newContext )


type PatternUse
    = Destructuring
    | Matching


errorsForPatternList : PatternUse -> List (Node Pattern) -> Context -> ( List (Rule.Error {}), Context )
errorsForPatternList use list context =
    List.foldl (andThen (errorsForPattern use)) ( [], context ) list


errorsForPattern : PatternUse -> Node Pattern -> Context -> ( List (Rule.Error {}), Context )
errorsForPattern use (Node range pattern) context =
    case pattern of
        Pattern.AllPattern ->
            ( [], context )

        Pattern.VarPattern value ->
            errorsForValue value range context

        Pattern.RecordPattern values ->
            errorsForRecordValueList range values context

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            errorsForUselessTuple range context

        Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
            errorsForUselessTuple range context

        Pattern.TuplePattern patterns ->
            errorsForPatternList use patterns context

        Pattern.UnConsPattern first second ->
            errorsForPatternList use [ first, second ] context

        Pattern.ListPattern patterns ->
            errorsForPatternList use patterns context

        Pattern.NamedPattern _ patterns ->
            if use == Destructuring && List.all isAllPattern patterns then
                errorsForUselessNamePattern range context

            else
                errorsForPatternList use patterns context

        Pattern.AsPattern inner name ->
            context
                |> errorsForAsPattern range inner name
                |> andThen (errorsForPattern use) inner

        Pattern.ParenthesizedPattern inner ->
            errorsForPattern use inner context

        _ ->
            ( [], context )


errorsForUselessNamePattern : Range -> Context -> ( List (Rule.Error {}), Context )
errorsForUselessNamePattern range context =
    ( [ Rule.errorWithFix
            { message = "Named pattern is not needed"
            , details = redundantDetails
            }
            range
            [ Fix.replaceRangeBy range "_" ]
      ]
    , context
    )


errorsForUselessTuple : Range -> Context -> ( List (Rule.Error {}), Context )
errorsForUselessTuple range context =
    ( [ Rule.errorWithFix
            { message = "Tuple pattern is not needed"
            , details = redundantDetails
            }
            range
            [ Fix.replaceRangeBy range "_" ]
      ]
    , context
    )


errorsForRecordValueList : Range -> List (Node String) -> Context -> ( List (Rule.Error {}), Context )
errorsForRecordValueList recordRange list context =
    let
        ( unused, used ) =
            List.partition (isNodeInContext context) list
    in
    case unused of
        [] ->
            ( [], context )

        firstNode :: restNodes ->
            let
                first : String
                first =
                    Node.value firstNode

                rest : List String
                rest =
                    List.map Node.value restNodes

                ( errorRange, fix ) =
                    case used of
                        [] ->
                            ( recordRange, Fix.replaceRangeBy recordRange "_" )

                        _ ->
                            ( Range.combine (List.map Node.range unused)
                            , Node Range.emptyRange (Pattern.RecordPattern used)
                                |> Writer.writePattern
                                |> Writer.write
                                |> Fix.replaceRangeBy recordRange
                            )
            in
            ( [ Rule.errorWithFix
                    { message = listToMessage first rest
                    , details = listToDetails first rest
                    }
                    errorRange
                    [ fix ]
              ]
            , List.foldl forgetNode context unused
            )


listToMessage : String -> List String -> String
listToMessage first rest =
    case List.reverse rest of
        [] ->
            "Value `" ++ first ++ "` is not used."

        last :: middle ->
            "Values `" ++ String.join "`, `" (first :: middle) ++ "` and `" ++ last ++ "` are not used."


listToDetails : String -> List String -> List String
listToDetails _ rest =
    case rest of
        [] ->
            singularRemoveDetails

        _ ->
            pluralDetails


errorsForAsPattern : Range -> Node Pattern -> Node String -> Context -> ( List (Rule.Error {}), Context )
errorsForAsPattern patternRange inner (Node range name) context =
    if isUnused name context then
        let
            fix : List Fix
            fix =
                [ inner
                    |> Writer.writePattern
                    |> Writer.write
                    |> Fix.replaceRangeBy patternRange
                ]
        in
        ( [ Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used."
                , details = singularRemoveDetails
                }
                range
                fix
          ]
        , useValue name context
        )

    else if isAllPattern inner then
        ( [ Rule.errorWithFix
                { message = "Pattern `_` is not needed"
                , details = removeDetails
                }
                (Node.range inner)
                [ Fix.replaceRangeBy patternRange name ]
          ]
        , useValue name context
        )

    else
        ( [], context )


findPatternForAsPattern : Range -> Node Pattern -> Node String -> FoundPattern
findPatternForAsPattern patternRange inner (Node range name) =
    if isAllPattern inner then
        SimplifiablePattern
            (Rule.errorWithFix
                { message = "Pattern `_` is not needed"
                , details = removeDetails
                }
                (Node.range inner)
                [ Fix.replaceRangeBy patternRange name ]
            )

    else
        let
            fix : List Fix
            fix =
                [ inner
                    |> Writer.writePattern
                    |> Writer.write
                    |> Fix.replaceRangeBy patternRange
                ]
        in
        SingleValue
            { name = name
            , message = "Pattern alias `" ++ name ++ "` is not used."
            , details = singularRemoveDetails
            , range = range
            , fix = fix
            }


isAllPattern : Node Pattern -> Bool
isAllPattern (Node _ pattern) =
    case pattern of
        Pattern.AllPattern ->
            True

        _ ->
            False


forgetNode : Node String -> Context -> Context
forgetNode (Node _ value) context =
    useValue value context


errorsForValue : String -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForValue name range context =
    if isUnused name context then
        ( [ Rule.errorWithFix
                { message = "Value `" ++ name ++ "` is not used."
                , details = singularReplaceDetails
                }
                range
                [ Fix.replaceRangeBy range "_" ]
          ]
        , useValue name context
        )

    else
        ( [], context )


useValue : String -> Context -> Context
useValue name context =
    case context.scopes of
        [] ->
            context

        headScope :: restOfScopes ->
            { context | scopes = { headScope | used = Set.insert name headScope.used } :: restOfScopes }


isNodeInContext : Context -> Node String -> Bool
isNodeInContext context (Node _ value) =
    isUnused value context


isUnused : String -> Context -> Bool
isUnused name context =
    case context.scopes of
        [] ->
            False

        headScope :: _ ->
            not <| Set.member name headScope.used
