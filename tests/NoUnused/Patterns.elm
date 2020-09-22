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
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Report useless patterns and pattern values that are not used.

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


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            ( [], rememberLetDeclarationList declarations context )

        Expression.CaseExpression { cases } ->
            ( [], rememberCaseList cases context )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            errorsForLetDeclarationList declarations context

        Expression.CaseExpression { cases } ->
            errorsForCaseList cases context

        _ ->
            ( [], context )


valueVisitor : Node ( ModuleName, String ) -> Context -> ( List (Rule.Error {}), Context )
valueVisitor (Node _ ( moduleName, value )) context =
    case moduleName of
        [] ->
            ( [], useValue value context )

        _ ->
            ( [], context )



--- ON ENTER


rememberCaseList : List Expression.Case -> Context -> Context
rememberCaseList list context =
    List.foldl rememberCase context list


rememberCase : Expression.Case -> Context -> Context
rememberCase ( pattern, _ ) context =
    rememberPattern pattern context


rememberLetDeclarationList : List (Node Expression.LetDeclaration) -> Context -> Context
rememberLetDeclarationList list context =
    List.foldl rememberLetDeclaration context list


rememberLetDeclaration : Node Expression.LetDeclaration -> Context -> Context
rememberLetDeclaration (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction _ ->
            context

        Expression.LetDestructuring pattern _ ->
            rememberPattern pattern context


rememberPatternList : List (Node Pattern) -> Context -> Context
rememberPatternList list context =
    List.foldl rememberPattern context list


rememberPattern : Node Pattern -> Context -> Context
rememberPattern (Node _ pattern) context =
    case pattern of
        Pattern.AllPattern ->
            context

        Pattern.VarPattern value ->
            rememberValue value context

        Pattern.TuplePattern patterns ->
            rememberPatternList patterns context

        Pattern.RecordPattern values ->
            rememberValueList values context

        Pattern.UnConsPattern first second ->
            context
                |> rememberPattern first
                |> rememberPattern second

        Pattern.ListPattern patterns ->
            rememberPatternList patterns context

        Pattern.NamedPattern _ patterns ->
            rememberPatternList patterns context

        Pattern.AsPattern inner name ->
            context
                |> rememberPattern inner
                |> rememberValue (Node.value name)

        Pattern.ParenthesizedPattern inner ->
            rememberPattern inner context

        _ ->
            context


rememberValueList : List (Node String) -> Context -> Context
rememberValueList list context =
    List.foldl (Node.value >> rememberValue) context list



--- ON EXIT


singularDetails : List String
singularDetails =
    [ "You should either use this value somewhere, or remove it at the location I pointed at." ]


pluralDetails : List String
pluralDetails =
    [ "You should either use these values somewhere, or remove them at the location I pointed at." ]


removeDetails : List String
removeDetails =
    [ "You should remove it at the location I pointed at." ]


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


errorsForCaseList : List Expression.Case -> Context -> ( List (Rule.Error {}), Context )
errorsForCaseList list context =
    List.foldl (andThen errorsForCase) ( [], context ) list


errorsForCase : Expression.Case -> Context -> ( List (Rule.Error {}), Context )
errorsForCase ( pattern, _ ) context =
    errorsForPattern Matching pattern context


errorsForLetDeclarationList : List (Node Expression.LetDeclaration) -> Context -> ( List (Rule.Error {}), Context )
errorsForLetDeclarationList list context =
    List.foldl (andThen errorsForLetDeclaration) ( [], context ) list


errorsForLetDeclaration : Node Expression.LetDeclaration -> Context -> ( List (Rule.Error {}), Context )
errorsForLetDeclaration (Node _ letDeclaration) context =
    case letDeclaration of
        Expression.LetFunction _ ->
            ( [], context )

        Expression.LetDestructuring pattern _ ->
            errorsForPattern Destructuring pattern context


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
            { message = "Named pattern is not needed."
            , details = removeDetails
            }
            range
            [ Fix.replaceRangeBy range "_" ]
      ]
    , context
    )


errorsForUselessTuple : Range -> Context -> ( List (Rule.Error {}), Context )
errorsForUselessTuple range context =
    ( [ Rule.errorWithFix
            { message = "Tuple pattern is not needed."
            , details = removeDetails
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


isNodeInContext : Context -> Node String -> Bool
isNodeInContext context (Node _ value) =
    Set.member value context


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
            singularDetails

        _ ->
            pluralDetails


errorsForAsPattern : Range -> Node Pattern -> Node String -> Context -> ( List (Rule.Error {}), Context )
errorsForAsPattern patternRange inner (Node range name) context =
    if Set.member name context then
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
                , details = singularDetails
                }
                range
                fix
          ]
        , Set.remove name context
        )

    else if isAllPattern inner then
        ( [ Rule.errorWithFix
                { message = "Pattern `_` is not needed."
                , details = removeDetails
                }
                (Node.range inner)
                [ Fix.replaceRangeBy patternRange name ]
          ]
        , Set.remove name context
        )

    else
        ( [], context )


isAllPattern : Node Pattern -> Bool
isAllPattern (Node _ pattern) =
    case pattern of
        Pattern.AllPattern ->
            True

        _ ->
            False


forgetNode : Node String -> Context -> Context
forgetNode (Node _ value) context =
    Set.remove value context



--- CONTEXT


type alias Context =
    Set String


initialContext : Context
initialContext =
    Set.empty


errorsForValue : String -> Range -> Context -> ( List (Rule.Error {}), Context )
errorsForValue value range context =
    if Set.member value context then
        ( [ Rule.errorWithFix
                { message = "Value `" ++ value ++ "` is not used."
                , details = singularDetails
                }
                range
                [ Fix.replaceRangeBy range "_" ]
          ]
        , Set.remove value context
        )

    else
        ( [], context )


rememberValue : String -> Context -> Context
rememberValue value context =
    Set.insert value context


useValue : String -> Context -> Context
useValue value context =
    Set.remove value context
