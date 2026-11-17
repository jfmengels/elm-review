module NoUnused.Patterns exposing (rule)

{-| Report useless patterns and pattern values that are not used.

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.Resources.Binding as Binding
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeInternal exposing (ScopeProvider)
import Review.Resources.ScopeOrigin as ScopeOrigin
import Review.Resources.ScopeProvider as ScopeProvider
import Review.Rule as Rule exposing (Rule)


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
    Rule.newModuleRuleSchema "NoUnused.Patterns" ()
        |> Rule.withScopeVisitor (\scopes () -> ( scopeVisitor scopes, () ))
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


scopeVisitor : { s | child : Scope, scopeProvider : ScopeProvider } -> List (Rule.Error {})
scopeVisitor { child, scopeProvider } =
    case Scope.origin child of
        ScopeOrigin.Function (Node _ { declaration }) ->
            findAsPatternsErrors (isUsedPredicate scopeProvider child) (Node.value declaration).arguments []

        ScopeOrigin.CaseBranch ( pattern, _ ) ->
            findPatterns (isUsedPredicate scopeProvider child) Matching [ pattern ] []

        ScopeOrigin.LetBlock (Node _ { declarations }) ->
            let
                findPatternsInLetDeclaration : Node Expression.LetDeclaration -> List (Rule.Error {}) -> List (Rule.Error {})
                findPatternsInLetDeclaration (Node _ letDeclaration) errors =
                    case letDeclaration of
                        Expression.LetFunction _ ->
                            errors

                        Expression.LetDestructuring pattern _ ->
                            findPatterns (isUsedPredicate scopeProvider child) Destructuring [ pattern ] errors
            in
            List.foldl findPatternsInLetDeclaration [] declarations

        ScopeOrigin.Lambda (Node _ { args }) ->
            findAsPatternsErrors (isUsedPredicate scopeProvider child) args []

        _ ->
            []


recordErrors : (String -> Bool) -> { fields : List (Node String), recordRange : Range } -> List (Rule.Error {}) -> List (Rule.Error {})
recordErrors isUsed { fields, recordRange } errors =
    if List.isEmpty fields then
        Rule.errorWithFix
            { message = "Record pattern is not needed"
            , details = [ "This pattern is redundant and should be replaced with '_'." ]
            }
            recordRange
            [ Fix.replaceRangeBy recordRange "_" ]
            :: errors

    else
        let
            ( used, unused ) =
                List.partition (\(Node _ field) -> isUsed field) fields
        in
        case unused of
            [] ->
                errors

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
                    :: errors


isUsedPredicate : ScopeProvider -> Scope -> String -> Bool
isUsedPredicate scopeProvider scope =
    \name ->
        case Scope.valueBinding scope name of
            Just binding ->
                case ScopeProvider.valueReferences scopeProvider (Binding.id binding) of
                    [] ->
                        False

                    _ :: _ ->
                        True

            Nothing ->
                True


findPatterns : (String -> Bool) -> PatternUse -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
findPatterns isUsed use patterns errors =
    case patterns of
        [] ->
            errors

        (Node range pattern) :: rest ->
            case pattern of
                Pattern.VarPattern name ->
                    if isUsed name then
                        findPatterns isUsed use rest errors

                    else
                        let
                            error : Rule.Error {}
                            error =
                                Rule.errorWithFix
                                    { message = "Value `" ++ name ++ "` is not used"
                                    , details = singularReplaceDetails
                                    }
                                    range
                                    [ Fix.replaceRangeBy range "_" ]
                        in
                        findPatterns isUsed use rest (error :: errors)

                Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
                    let
                        error : Rule.Error {}
                        error =
                            Rule.errorWithFix
                                { message = "Tuple pattern is not needed"
                                , details = redundantDetails
                                }
                                range
                                [ Fix.replaceRangeBy range "_" ]
                    in
                    findPatterns isUsed use rest (error :: errors)

                Pattern.TuplePattern [ Node _ Pattern.AllPattern, Node _ Pattern.AllPattern, Node _ Pattern.AllPattern ] ->
                    let
                        error : Rule.Error {}
                        error =
                            Rule.errorWithFix
                                { message = "Tuple pattern is not needed"
                                , details = redundantDetails
                                }
                                range
                                [ Fix.replaceRangeBy range "_" ]
                    in
                    findPatterns isUsed use rest (error :: errors)

                Pattern.TuplePattern subPatterns ->
                    findPatterns isUsed use (subPatterns ++ rest) errors

                Pattern.RecordPattern fields ->
                    findPatterns
                        isUsed
                        use
                        rest
                        (recordErrors isUsed { fields = fields, recordRange = range } errors)

                Pattern.UnConsPattern first second ->
                    findPatterns isUsed use (first :: second :: rest) errors

                Pattern.ListPattern subPatterns ->
                    findPatterns isUsed use (subPatterns ++ rest) errors

                Pattern.NamedPattern _ subPatterns ->
                    if use == Destructuring && List.all (\(Node _ p) -> isAllPattern p) subPatterns then
                        let
                            error : Rule.Error {}
                            error =
                                Rule.errorWithFix
                                    { message = "Named pattern is not needed"
                                    , details = redundantDetails
                                    }
                                    range
                                    [ Fix.replaceRangeBy range "_" ]
                        in
                        findPatterns isUsed use rest (error :: errors)

                    else
                        findPatterns isUsed use (subPatterns ++ rest) errors

                Pattern.AsPattern inner name ->
                    findPatterns
                        isUsed
                        use
                        (inner :: rest)
                        (findPatternForAsPattern
                            { include = True
                            , isUsed = isUsed
                            , fullPatternStart = range.start
                            , patternWithParens = inner
                            , errors = errors
                            }
                            inner
                            name
                        )

                Pattern.ParenthesizedPattern inner ->
                    findPatterns isUsed use (inner :: rest) errors

                _ ->
                    findPatterns isUsed use rest errors


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


type PatternUse
    = Destructuring
    | Matching


listToMessage : String -> List (Node String) -> String
listToMessage first rest =
    -- TODO Check the order of messages
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


findAsPatternsErrors : (String -> Bool) -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
findAsPatternsErrors isUsed patterns acc =
    case patterns of
        [] ->
            acc

        (Node patternRange pattern) :: rest ->
            case pattern of
                Pattern.AsPattern inner name ->
                    findAsPatternsErrors
                        isUsed
                        (inner :: rest)
                        (findPatternForAsPattern
                            { include = False
                            , isUsed = isUsed
                            , fullPatternStart = patternRange.start
                            , patternWithParens = inner
                            , errors = acc
                            }
                            inner
                            name
                        )

                Pattern.TuplePattern subPatterns ->
                    findAsPatternsErrors isUsed (subPatterns ++ rest) acc

                Pattern.UnConsPattern first second ->
                    findAsPatternsErrors isUsed (first :: second :: rest) acc

                Pattern.ListPattern subPatterns ->
                    findAsPatternsErrors isUsed (subPatterns ++ rest) acc

                Pattern.NamedPattern _ subPatterns ->
                    findAsPatternsErrors isUsed (subPatterns ++ rest) acc

                Pattern.ParenthesizedPattern inner ->
                    findAsPatternsErrors isUsed (inner :: rest) acc

                _ ->
                    findAsPatternsErrors isUsed rest acc


findPatternForAsPattern :
    { include : Bool
    , isUsed : String -> Bool
    , fullPatternStart : Location
    , patternWithParens : Node Pattern
    , errors : List (Rule.Error {})
    }
    -> Node Pattern
    -> Node String
    -> List (Rule.Error {})
findPatternForAsPattern params (Node patternRange pattern) ((Node aliasRange aliasName) as nameNode) =
    case pattern of
        Pattern.ParenthesizedPattern subPattern ->
            findPatternForAsPattern params subPattern nameNode

        Pattern.AllPattern ->
            Rule.errorWithFix
                { message = "Pattern `_` is not needed"
                , details = removeDetails
                }
                patternRange
                [ Fix.removeRange { start = params.fullPatternStart, end = aliasRange.start } ]
                :: params.errors

        Pattern.VarPattern innerName ->
            Rule.error
                { message = "Unnecessary duplicate alias `" ++ aliasName ++ "`"
                , details = [ "This alias is redundant because the value is already named `" ++ innerName ++ "`. I suggest you remove one of them." ]
                }
                aliasRange
                :: params.errors

        Pattern.AsPattern _ (Node innerRange innerName) ->
            Rule.error
                { message = "Unnecessary duplicate alias `" ++ innerName ++ "`"
                , details = [ "This name is redundant because the value is already aliased as `" ++ aliasName ++ "`. I suggest you remove one of them." ]
                }
                innerRange
                :: params.errors

        _ ->
            if params.include && not (params.isUsed aliasName) then
                Rule.errorWithFix
                    { message = "Pattern alias `" ++ aliasName ++ "` is not used"
                    , details = singularRemoveDetails
                    }
                    aliasRange
                    [ Fix.removeRange { start = (Node.range params.patternWithParens).start, end = patternRange.start }
                    , Fix.removeRange { start = patternRange.end, end = aliasRange.end }
                    ]
                    :: params.errors

            else
                params.errors


isAllPattern : Pattern -> Bool
isAllPattern pattern =
    case pattern of
        Pattern.AllPattern ->
            True

        _ ->
            False


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

        Pattern.AsPattern (Node _ innerPattern) (Node _ asName) ->
            writePattern innerPattern ++ " as " ++ asName

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
