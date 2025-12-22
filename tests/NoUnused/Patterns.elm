module NoUnused.Patterns exposing (rule)

{-| Report useless patterns and pattern values that are not used.

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
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
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withCaseBranchEnterVisitor caseBranchEnterVisitor
        |> Rule.withCaseBranchExitVisitor caseBranchExitVisitor
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    List Scope


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
    []



-- EXPRESSION ENTER VISITOR


declarationEnterVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            ( findAsPatternsErrors (Node.value declaration).arguments []
            , context
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
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

                asPatternsErrors : Node Expression.LetDeclaration -> List (Rule.Error {})
                asPatternsErrors letDeclaration =
                    case Node.value letDeclaration of
                        Expression.LetFunction { declaration } ->
                            findAsPatternsErrors (Node.value declaration).arguments []

                        Expression.LetDestructuring _ _ ->
                            []
            in
            ( List.concatMap asPatternsErrors declarations
            , { declared = List.concatMap findPatternsInLetDeclaration declarations
              , used = Set.empty
              }
                :: context
            )

        Expression.LambdaExpression { args } ->
            ( findAsPatternsErrors args [], context )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            report context

        _ ->
            ( [], context )


caseBranchEnterVisitor : a -> ( Node Pattern, Node Expression ) -> Context -> ( List nothing, Context )
caseBranchEnterVisitor _ ( pattern, _ ) context =
    ( []
    , { declared = findPatterns Matching pattern
      , used = Set.empty
      }
        :: context
    )


caseBranchExitVisitor : a -> b -> Context -> ( List (Rule.Error {}), Context )
caseBranchExitVisitor _ _ context =
    report context


report : Context -> ( List (Rule.Error {}), Context )
report context =
    case context of
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
            , Set.foldl
                useValue
                restOfScopes
                nonUsedVars
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
                                , Node.empty (Pattern.RecordPattern used)
                                    |> writePattern
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
                    { singles = v :: acc.singles, records = acc.records, simplifiablePatterns = acc.simplifiablePatterns }

                RecordPattern v ->
                    { singles = acc.singles, records = v :: acc.records, simplifiablePatterns = acc.simplifiablePatterns }

                SimplifiablePattern simplifiablePatternError ->
                    { singles = acc.singles, records = acc.records, simplifiablePatterns = simplifiablePatternError :: acc.simplifiablePatterns }
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
findPatterns use pattern =
    findPatternsHelp use [ pattern ] []


findPatternsHelp : PatternUse -> List (Node Pattern) -> List FoundPattern -> List FoundPattern
findPatternsHelp use patterns acc =
    case patterns of
        [] ->
            acc

        (Node range pattern) :: rest ->
            case pattern of
                Pattern.VarPattern name ->
                    let
                        foundPattern : FoundPattern
                        foundPattern =
                            SingleValue
                                { name = name
                                , message = "Value `" ++ name ++ "` is not used"
                                , details = singularReplaceDetails
                                , range = range
                                , fix = [ Fix.replaceRangeBy range "_" ]
                                }
                    in
                    findPatternsHelp use rest (foundPattern :: acc)

                Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
                    let
                        foundPattern : FoundPattern
                        foundPattern =
                            SimplifiablePattern
                                (Rule.errorWithFix
                                    { message = "Tuple pattern is not needed"
                                    , details = redundantDetails
                                    }
                                    range
                                    [ Fix.replaceRangeBy range "_" ]
                                )
                    in
                    findPatternsHelp use rest (foundPattern :: acc)

                Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
                    let
                        foundPattern : FoundPattern
                        foundPattern =
                            SimplifiablePattern
                                (Rule.errorWithFix
                                    { message = "Tuple pattern is not needed"
                                    , details = redundantDetails
                                    }
                                    range
                                    [ Fix.replaceRangeBy range "_" ]
                                )
                    in
                    findPatternsHelp use rest (foundPattern :: acc)

                Pattern.TuplePattern subPatterns ->
                    findPatternsHelp use (subPatterns ++ rest) acc

                Pattern.RecordPattern fields ->
                    let
                        foundPattern : FoundPattern
                        foundPattern =
                            RecordPattern
                                { fields = fields
                                , recordRange = range
                                }
                    in
                    findPatternsHelp use rest (foundPattern :: acc)

                Pattern.UnConsPattern first second ->
                    findPatternsHelp use (first :: second :: rest) acc

                Pattern.ListPattern subPatterns ->
                    findPatternsHelp use (subPatterns ++ rest) acc

                Pattern.NamedPattern _ subPatterns ->
                    if use == Destructuring && List.all isAllPattern subPatterns then
                        let
                            foundPattern : FoundPattern
                            foundPattern =
                                SimplifiablePattern
                                    (Rule.errorWithFix
                                        { message = "Named pattern is not needed"
                                        , details = redundantDetails
                                        }
                                        range
                                        [ Fix.replaceRangeBy range "_" ]
                                    )
                        in
                        findPatternsHelp use rest (foundPattern :: acc)

                    else
                        findPatternsHelp use (subPatterns ++ rest) acc

                Pattern.AsPattern inner name ->
                    findPatternsHelp use (inner :: rest) (findPatternForAsPattern range inner name :: acc)

                Pattern.ParenthesizedPattern inner ->
                    findPatternsHelp use (inner :: rest) acc

                _ ->
                    findPatternsHelp use rest acc



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
                            , Node.empty (Pattern.RecordPattern used)
                                |> writePattern
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
            "Value `" ++ first ++ "` is not used"

        last :: middle ->
            "Values `" ++ String.join "`, `" (first :: middle) ++ "` and `" ++ last ++ "` are not used"


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
                    |> writePattern
                    |> Fix.replaceRangeBy patternRange
                ]
        in
        ( [ Rule.errorWithFix
                { message = "Pattern alias `" ++ name ++ "` is not used"
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


findAsPatternsErrors : List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
findAsPatternsErrors patterns acc =
    case patterns of
        [] ->
            acc

        pattern :: rest ->
            case Node.value pattern of
                Pattern.AsPattern inner name ->
                    let
                        newAcc : List (Rule.Error {})
                        newAcc =
                            case findPatternForAsPattern (Node.range pattern) inner name of
                                SimplifiablePattern error ->
                                    error :: acc

                                SingleValue _ ->
                                    acc

                                RecordPattern _ ->
                                    acc
                    in
                    findAsPatternsErrors (inner :: rest) newAcc

                Pattern.TuplePattern subPatterns ->
                    findAsPatternsErrors (subPatterns ++ rest) acc

                Pattern.UnConsPattern first second ->
                    findAsPatternsErrors (first :: second :: rest) acc

                Pattern.ListPattern subPatterns ->
                    findAsPatternsErrors (subPatterns ++ rest) acc

                Pattern.NamedPattern _ subPatterns ->
                    findAsPatternsErrors (subPatterns ++ rest) acc

                Pattern.ParenthesizedPattern inner ->
                    findAsPatternsErrors (inner :: rest) acc

                _ ->
                    findAsPatternsErrors rest acc


findPatternForAsPattern : Range -> Node Pattern -> Node String -> FoundPattern
findPatternForAsPattern patternRange pattern ((Node range name) as nameNode) =
    case Node.value pattern of
        Pattern.ParenthesizedPattern subPattern ->
            findPatternForAsPattern patternRange subPattern nameNode

        Pattern.AllPattern ->
            SimplifiablePattern
                (Rule.errorWithFix
                    { message = "Pattern `_` is not needed"
                    , details = removeDetails
                    }
                    (Node.range pattern)
                    [ Fix.replaceRangeBy patternRange name ]
                )

        Pattern.VarPattern innerName ->
            SimplifiablePattern
                (Rule.error
                    { message = "Unnecessary duplicate alias `" ++ name ++ "`"
                    , details = [ "This alias is redundant because the value is already named `" ++ innerName ++ "`. I suggest you remove one of them." ]
                    }
                    range
                )

        Pattern.AsPattern _ (Node innerRange innerName) ->
            SimplifiablePattern
                (Rule.error
                    { message = "Unnecessary duplicate alias `" ++ innerName ++ "`"
                    , details = [ "This name is redundant because the value is already aliased as `" ++ name ++ "`. I suggest you remove one of them." ]
                    }
                    innerRange
                )

        _ ->
            let
                fix : List Fix
                fix =
                    [ pattern
                        |> writePattern
                        |> Fix.replaceRangeBy patternRange
                    ]
            in
            SingleValue
                { name = name
                , message = "Pattern alias `" ++ name ++ "` is not used"
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
    case context of
        [] ->
            context

        headScope :: restOfScopes ->
            { headScope | used = Set.insert name headScope.used } :: restOfScopes


isNodeInContext : Context -> Node String -> Bool
isNodeInContext context (Node _ value) =
    isUnused value context


isUnused : String -> Context -> Bool
isUnused name context =
    case context of
        [] ->
            False

        headScope :: _ ->
            not <| Set.member name headScope.used


{-| Write a pattern.
-}
writePattern : Node Pattern -> String
writePattern pattern =
    case Node.value pattern of
        Pattern.AllPattern ->
            "_"

        Pattern.UnitPattern ->
            "()"

        Pattern.CharPattern c ->
            "'" ++ String.fromChar c ++ "'"

        Pattern.StringPattern s ->
            "\"" ++ String.replace "\"" "\\\"" s ++ "\""

        Pattern.HexPattern _ ->
            pattern
                |> Writer.writePattern
                |> Writer.write

        Pattern.IntPattern i ->
            String.fromInt i

        Pattern.FloatPattern f ->
            String.fromFloat f

        Pattern.TuplePattern inner ->
            "( " ++ String.join ", " (List.map writePattern inner) ++ " )"

        Pattern.RecordPattern inner ->
            "{ " ++ String.join ", " (List.map Node.value inner) ++ " }"

        Pattern.UnConsPattern left right ->
            writePattern left ++ " :: " ++ writePattern right

        Pattern.ListPattern inner ->
            "[ " ++ String.join ", " (List.map writePattern inner) ++ " ]"

        Pattern.VarPattern var ->
            var

        Pattern.NamedPattern qnr others ->
            String.join " "
                (String.join "." (qnr.moduleName ++ [ qnr.name ])
                    :: List.map writePattern others
                )

        Pattern.AsPattern innerPattern asName ->
            writePattern innerPattern ++ " as " ++ Node.value asName

        Pattern.ParenthesizedPattern innerPattern ->
            "(" ++ writePattern innerPattern ++ ")"
