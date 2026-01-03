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
    = SingleValue SingleValueData
    | RecordPattern
        { fields : List (Node String)
        , recordRange : Range
        }
    | SimplifiablePattern (Rule.Error {})


type alias SingleValueData =
    { name : String
    , range : Range
    , message : String
    , details : List String
    , fix : List Fix
    }


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
                findPatternsInLetDeclaration : Node Expression.LetDeclaration -> List FoundPattern -> List FoundPattern
                findPatternsInLetDeclaration letDeclaration foundPatterns =
                    case Node.value letDeclaration of
                        Expression.LetFunction _ ->
                            foundPatterns

                        Expression.LetDestructuring pattern _ ->
                            findPatterns Destructuring [ pattern ] foundPatterns

                asPatternsErrors : Node Expression.LetDeclaration -> List (Rule.Error {}) -> List (Rule.Error {})
                asPatternsErrors letDeclaration errors =
                    case Node.value letDeclaration of
                        Expression.LetFunction { declaration } ->
                            findAsPatternsErrors (Node.value declaration).arguments errors

                        Expression.LetDestructuring _ _ ->
                            errors
            in
            ( List.foldl asPatternsErrors [] declarations
            , { declared = List.foldl findPatternsInLetDeclaration [] declarations
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
    , { declared = findPatterns Matching [ pattern ] []
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
        headScope :: previousScope :: restOfScopes ->
            let
                { errors, used } =
                    findDeclaredPatterns context headScope
            in
            ( errors
            , { declared = previousScope.declared, used = Set.union used previousScope.used } :: restOfScopes
            )

        headScope :: [] ->
            ( findDeclaredPatternsForRootScope context headScope, [] )

        _ ->
            ( [], context )


removeFromSet : (a -> comparable) -> List a -> Set comparable -> Set comparable
removeFromSet mapper list initial =
    List.foldl (\a acc -> Set.remove (mapper a) acc) initial list


singleError : SingleValueData -> Rule.Error {}
singleError pattern =
    Rule.errorWithFix
        { message = pattern.message
        , details = pattern.details
        }
        pattern.range
        pattern.fix


recordErrors : Context -> { fields : List (Node String), recordRange : Range } -> Maybe (Rule.Error {})
recordErrors context { fields, recordRange } =
    if List.isEmpty fields then
        Rule.errorWithFix
            { message = "Record pattern is not needed"
            , details = [ "This pattern is redundant and should be replaced with '_'." ]
            }
            recordRange
            [ Fix.replaceRangeBy recordRange "_" ]
            |> Just

    else
        let
            ( unused, used ) =
                List.partition (isNodeInContext context) fields
        in
        case unused of
            [] ->
                Nothing

            (Node _ first) :: restNodes ->
                let
                    ( errorRange, fix ) =
                        case used of
                            [] ->
                                ( recordRange, Fix.replaceRangeBy recordRange "_" )

                            _ ->
                                ( Range.combine (List.map Node.range unused)
                                , Pattern.RecordPattern used
                                    |> writePattern
                                    |> Fix.replaceRangeBy recordRange
                                )
                in
                Rule.errorWithFix
                    { message = listToMessage first restNodes
                    , details = listToDetails restNodes
                    }
                    errorRange
                    [ fix ]
                    |> Just


findDeclaredPatterns : Context -> Scope -> { errors : List (Rule.Error {}), used : Set String }
findDeclaredPatterns context scope =
    List.foldl
        (\foundPattern acc ->
            case foundPattern of
                SingleValue v ->
                    if Set.member v.name acc.used then
                        { errors = acc.errors
                        , used = Set.remove v.name acc.used
                        }

                    else
                        { errors = singleError v :: acc.errors
                        , used = acc.used
                        }

                RecordPattern v ->
                    { errors =
                        case recordErrors context v of
                            Just error ->
                                error :: acc.errors

                            Nothing ->
                                acc.errors
                    , used = removeFromSet Node.value v.fields acc.used
                    }

                SimplifiablePattern simplifiablePatternError ->
                    { errors = simplifiablePatternError :: acc.errors
                    , used = acc.used
                    }
        )
        { errors = []
        , used = scope.used
        }
        scope.declared


{-| Just like `findDeclaredPatterns` but faster when we don't care about the used patterns.
-}
findDeclaredPatternsForRootScope : Context -> Scope -> List (Rule.Error {})
findDeclaredPatternsForRootScope context scope =
    List.foldl
        (\foundPattern errors ->
            case foundPattern of
                SingleValue v ->
                    if Set.member v.name scope.used then
                        errors

                    else
                        singleError v :: errors

                RecordPattern v ->
                    case recordErrors context v of
                        Just error ->
                            error :: errors

                        Nothing ->
                            errors

                SimplifiablePattern simplifiablePatternError ->
                    simplifiablePatternError :: errors
        )
        []
        scope.declared


valueVisitor : Node ( ModuleName, String ) -> Context -> ( List (Rule.Error {}), Context )
valueVisitor (Node _ ( moduleName, value )) context =
    case moduleName of
        [] ->
            ( [], useValue value context )

        _ ->
            ( [], context )



--- ON ENTER


findPatterns : PatternUse -> List (Node Pattern) -> List FoundPattern -> List FoundPattern
findPatterns use patterns acc =
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
                    findPatterns use rest (foundPattern :: acc)

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
                    findPatterns use rest (foundPattern :: acc)

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
                    findPatterns use rest (foundPattern :: acc)

                Pattern.TuplePattern subPatterns ->
                    findPatterns use (subPatterns ++ rest) acc

                Pattern.RecordPattern fields ->
                    let
                        foundPattern : FoundPattern
                        foundPattern =
                            RecordPattern
                                { fields = fields
                                , recordRange = range
                                }
                    in
                    findPatterns use rest (foundPattern :: acc)

                Pattern.UnConsPattern first second ->
                    findPatterns use (first :: second :: rest) acc

                Pattern.ListPattern subPatterns ->
                    findPatterns use (subPatterns ++ rest) acc

                Pattern.NamedPattern _ subPatterns ->
                    if use == Destructuring && List.all (\(Node _ p) -> isAllPattern p) subPatterns then
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
                        findPatterns use rest (foundPattern :: acc)

                    else
                        findPatterns use (subPatterns ++ rest) acc

                Pattern.AsPattern inner name ->
                    findPatterns use (inner :: rest) (findPatternForAsPattern range inner name :: acc)

                Pattern.ParenthesizedPattern inner ->
                    findPatterns use (inner :: rest) acc

                _ ->
                    findPatterns use rest acc



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
            if use == Destructuring && List.all (\(Node _ p) -> isAllPattern p) patterns then
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

        (Node _ first) :: rest ->
            let
                ( errorRange, fix ) =
                    case used of
                        [] ->
                            ( recordRange, Fix.replaceRangeBy recordRange "_" )

                        _ ->
                            ( Range.combine (List.map Node.range unused)
                            , Pattern.RecordPattern used
                                |> writePattern
                                |> Fix.replaceRangeBy recordRange
                            )
            in
            ( [ Rule.errorWithFix
                    { message = listToMessage first rest
                    , details = listToDetails rest
                    }
                    errorRange
                    [ fix ]
              ]
            , List.foldl forgetNode context unused
            )


listToMessage : String -> List (Node String) -> String
listToMessage first rest =
    case List.reverse rest of
        [] ->
            "Value `" ++ first ++ "` is not used"

        (Node _ last) :: middle ->
            "Values `" ++ String.join "`, `" (first :: List.map Node.value middle) ++ "` and `" ++ last ++ "` are not used"


listToDetails : List a -> List String
listToDetails rest =
    case rest of
        [] ->
            singularRemoveDetails

        _ ->
            pluralDetails


errorsForAsPattern : Range -> Node Pattern -> Node String -> Context -> ( List (Rule.Error {}), Context )
errorsForAsPattern patternRange (Node innerRange inner) (Node range name) context =
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
                innerRange
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
findPatternForAsPattern fullPatternRange (Node patternRange pattern) ((Node range name) as nameNode) =
    case pattern of
        Pattern.ParenthesizedPattern subPattern ->
            findPatternForAsPattern fullPatternRange subPattern nameNode

        Pattern.AllPattern ->
            SimplifiablePattern
                (Rule.errorWithFix
                    { message = "Pattern `_` is not needed"
                    , details = removeDetails
                    }
                    patternRange
                    [ Fix.replaceRangeBy fullPatternRange name ]
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
                        |> Fix.replaceRangeBy fullPatternRange
                    ]
            in
            SingleValue
                { name = name
                , message = "Pattern alias `" ++ name ++ "` is not used"
                , details = singularRemoveDetails
                , range = range
                , fix = fix
                }


isAllPattern : Pattern -> Bool
isAllPattern pattern =
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
writePattern : Pattern -> String
writePattern pattern =
    case pattern of
        Pattern.AllPattern ->
            "_"

        Pattern.UnitPattern ->
            "()"

        Pattern.CharPattern c ->
            "'" ++ String.fromChar c ++ "'"

        Pattern.StringPattern s ->
            "\"" ++ String.replace "\"" "\\\"" s ++ "\""

        Pattern.HexPattern hex ->
            "0x" ++ hexToString hex

        Pattern.IntPattern i ->
            String.fromInt i

        Pattern.FloatPattern f ->
            String.fromFloat f

        Pattern.TuplePattern inner ->
            "( " ++ String.join ", " (List.map (\(Node _ p) -> writePattern p) inner) ++ " )"

        Pattern.RecordPattern inner ->
            "{ " ++ String.join ", " (List.map Node.value inner) ++ " }"

        Pattern.UnConsPattern (Node _ left) (Node _ right) ->
            writePattern left ++ " :: " ++ writePattern right

        Pattern.ListPattern inner ->
            "[ " ++ String.join ", " (List.map (\(Node _ p) -> writePattern p) inner) ++ " ]"

        Pattern.VarPattern var ->
            var

        Pattern.NamedPattern qnr others ->
            let
                reference : String
                reference =
                    if List.isEmpty qnr.moduleName then
                        qnr.name

                    else
                        String.join "." qnr.moduleName ++ "." ++ qnr.name
            in
            List.foldl (\(Node _ p) acc -> acc ++ " " ++ writePattern p) reference others

        Pattern.AsPattern (Node _ innerPattern) asName ->
            writePattern innerPattern ++ " as " ++ Node.value asName

        Pattern.ParenthesizedPattern (Node _ innerPattern) ->
            "(" ++ writePattern innerPattern ++ ")"


{-| Convert a decimal integer to a hexdecimal string such as `"abc94f"`.

    Hex.toString 165 == "a5"

-}
hexToString : Int -> String
hexToString num =
    String.fromList <|
        if num < 0 then
            '-' :: unsafePositiveToDigits [] (negate num)

        else
            unsafePositiveToDigits [] num


{-| ONLY EVER CALL THIS WITH POSITIVE INTEGERS!
-}
unsafePositiveToDigits : List Char -> Int -> List Char
unsafePositiveToDigits digits num =
    if num < 16 then
        unsafeToDigit num :: digits

    else
        unsafePositiveToDigits (unsafeToDigit (modBy 16 num) :: digits) (num // 16)


{-| ONLY EVER CALL THIS WITH INTEGERS BETWEEN 0 and 15!
-}
unsafeToDigit : Int -> Char
unsafeToDigit num =
    case num of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        15 ->
            'f'

        _ ->
            -- if this ever gets called with a number over 15, it will never
            -- terminate! If that happens, debug further by uncommenting this:
            --
            -- Debug.todo ("Tried to convert " ++ toString num ++ " to hexadecimal.")
            unsafeToDigit num
