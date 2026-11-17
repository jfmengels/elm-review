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
            findUnusedAsPatterns (isUsedPredicate scopeProvider child) (Node.value declaration).arguments []

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
            findUnusedAsPatterns (isUsedPredicate scopeProvider child) args []

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
                                , ("{ " ++ String.join ", " (List.map Node.value used) ++ " }")
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


{-| Find and report `... as unused` patterns in function arguments.

We only want to report these because removing anything else is handled by `NoUnused.Parameters`.

-}
findUnusedAsPatterns : (String -> Bool) -> List (Node Pattern) -> List (Rule.Error {}) -> List (Rule.Error {})
findUnusedAsPatterns isUsed patterns acc =
    case patterns of
        [] ->
            acc

        (Node patternRange pattern) :: rest ->
            case pattern of
                Pattern.AsPattern inner name ->
                    findUnusedAsPatterns
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
                    findUnusedAsPatterns isUsed (subPatterns ++ rest) acc

                Pattern.UnConsPattern first second ->
                    findUnusedAsPatterns isUsed (first :: second :: rest) acc

                Pattern.ListPattern subPatterns ->
                    findUnusedAsPatterns isUsed (subPatterns ++ rest) acc

                Pattern.NamedPattern _ subPatterns ->
                    findUnusedAsPatterns isUsed (subPatterns ++ rest) acc

                Pattern.ParenthesizedPattern inner ->
                    findUnusedAsPatterns isUsed (inner :: rest) acc

                _ ->
                    findUnusedAsPatterns isUsed rest acc


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
