module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeOrigin as ScopeOrigin
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports unnecessary or suboptimal trailing underscores in variable names.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoUnnecessaryTrailingUnderscore.rule
        ]

I don't know how widespread this usage is, but I tend to append variable names with `_` to avoid [shadowing conflicts](https://github.com/elm/compiler/blob/9d97114702bf6846cab622a2203f60c2d4ebedf2/hints/shadowing.md), for instance like this:

    viewName name =
      case name of
        Just name_ -> ...
        Nothing -> ...

Obviously when I am able to figure out a better name for one of the two variables, I go for that. In this case, I may rename the argument to `viewName` to `maybeName` for instance.

But I notice that relatively often, the need for having the trailing underscore disappears, because I changed some code and the variable name that required me to add a trailing underscore has also disappeared. When that happens, the trailing underscore becomes a distraction and I find it nicer to rename the variable to not have that underscore.

This rule does not propose a fix for the issues (at least for now). I recommend renaming the variables through your IDE, as that will be a simpler and safer process.
That said, as we'll see in the following sections and examples, these renames may end up in shadowing issues, so please do these renames with a compiler running next to (or `elm-test --watch` for the changes happening in test files) to notice problems early on.


## Fail


### 1. Unneeded trailing underscore

When a variable has a trailing underscore that could be removed without an issue.

    viewName maybeName =
        case maybeName of
            Just name_ ->
                name_


### 2. Top-level declarations containing an underscore

This rule reports top-level declarations containing an underscore, even when removing the underscore would cause a shadowing conflict.
The idea here is that if you have both `viewName` and `viewName_`, someone who looks at the code may have a hard time figuring which function they should use and what the differences are between the two.

    viewName_ name = ...

When `viewName_` is only used inside `viewName`, an alternative name that you could go for is appending `Help`, such as `viewNameHelp`, which I've seen regularly often in several packages and codebases.


### 3. Let declarations on the same level where one has an underscore and one doesn't

    a =
        let
            name =
                "Jeroen"

            name_ =
                "Engels"
        in
        name ++ " " ++ name_

Very similar to the previous point, we report `name_` because this name is too confusing in the sense that readers won't know which one to use when.
In such instances, I believe there are clear benefits from spending a little bit of time figuring out a better name.

Here is another instance where a better name could be given.

    a =
        let
            model =
                { a = 1, b = 2 }

            model_ =
                { model | b = model.b + 1 }
        in
        doSomething model_

In this example, even simple naming schemes like `initialModel` and `modelWithUpdatedB`, or `modelStep1` and `modelStep2` would be an improvement over a trailing underscore. I'm sure you can find even better names for your specific use-case!


### 4. When an underscore is used to avoid a shadowing conflict with a more deeply nested variable

    view model_ =
        case model_ of
            Loaded model ->
                text model.name

In this case, `model_` has a trailing underscore to avoid a conflict with `model` declared in a deeper scope.
I tend to find constructs like these to be an indication that either

  - one or both of those variables have a bad name for what they're represent (`model` could be `loadedModel`?)
  - if they represent the same type, again we have an issue where it's not clear when one should be used over the other


## Success

We don't report errors when there is a reasonable use-case for adding a trailing underscore, or when a variable does not have a trailing underscore.

    viewName name =
        case name of
            Just name_ ->
                name_


## When (not) to enable this rule

This is a pretty personal rule that I'd like to enforce on my own projects.
I have not yet tested it extensively, but I feel like it could bring some value and bring me some sense of satisfaction when I know that the project adheres to this rule.

I feel comfortable enough with asking people making pull requests to the projects I maintain to make changes in order to follow this rule.
But I will probably not enforce this rule in a project where my team is bigger, because this may be too big of a source of frustration for my colleagues, especially if they tend to notice problems in the CI and no way to autofix the issues.

I recommend AGAINST enforcing this rule if you do not agree with the choices I have made, or if you do not have that habit of adding trailing underscores.
If you see some value in it, you may still want to use this rule to detect places where naming could be improved and make improvements to these places, but not end up enforcing it.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-code-style/example --rules NoUnnecessaryTrailingUnderscore
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.withScopeVisitor (\scopes node () -> ( scopeVisitor scopes node, () ))
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Rule.Error {})
declarationVisitor (Node _ node) =
    case node of
        Declaration.FunctionDeclaration function ->
            reportTopLevelFunction (Node.value function.declaration).name

        _ ->
            []


type alias ScopeNames =
    { name : String
    , range : Range
    , signatureRange : Maybe Range
    }


getDeclaredVariableNames : Node Pattern -> List ScopeNames
getDeclaredVariableNames (Node patternRange pattern) =
    case pattern of
        Pattern.VarPattern name ->
            [ { name = name, range = patternRange, signatureRange = Nothing } ]

        Pattern.ParenthesizedPattern subPattern ->
            getDeclaredVariableNames subPattern

        Pattern.AsPattern subPattern (Node nameRange name) ->
            { name = name, range = nameRange, signatureRange = Nothing } :: getDeclaredVariableNames subPattern

        Pattern.TuplePattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.UnConsPattern left right ->
            List.concatMap getDeclaredVariableNames [ left, right ]

        Pattern.ListPattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.RecordPattern _ ->
            []

        _ ->
            []


reservedElmKeywords : Set String
reservedElmKeywords =
    Set.fromList
        [ "if_"
        , "then_"
        , "else_"
        , "case_"
        , "of_"
        , "let_"
        , "in_"
        , "type_"
        , "alias_"
        , "module_"
        , "where_"
        , "import_"
        , "exposing_"
        , "as_"
        , "port_"
        ]


scopeVisitor : { a | scopeProvider : ScopeProvider, child : Scope } -> Node Expression -> List (Rule.Error {})
scopeVisitor { scopeProvider, child } node =
    expressionVisitor scopeProvider child node


expressionVisitor : ScopeProvider -> Scope -> Node Expression -> List (Rule.Error {})
expressionVisitor scopeProvider scope node =
    case Scope.origin scope of
        ScopeOrigin.CaseBranch ( pattern, body ) ->
            report scopeProvider scope [ pattern ] body

        ScopeOrigin.Lambda (Node _ { args, expression }) ->
            report scopeProvider scope args expression

        ScopeOrigin.LetBlock (Node _ let_) ->
            reportErrorsForLet scopeProvider scope node (Scope.valueBindings scope) let_.declarations

        ScopeOrigin.Function (Node _ function) ->
            let
                { arguments, expression } =
                    Node.value function.declaration
            in
            report scopeProvider scope arguments expression

        ScopeOrigin.TopLevel ->
            []


reportErrorsForLet : ScopeProvider -> Scope -> Node Expression -> Dict String a -> List (Node Expression.LetDeclaration) -> List (Rule.Error {})
reportErrorsForLet scopeProvider scope letExpression namesFromLetDeclarations declarations =
    List.foldl
        (\(Node _ node) errors ->
            case node of
                Expression.LetFunction function ->
                    let
                        (Node range name) =
                            function.declaration
                                |> Node.value
                                |> .name
                    in
                    error
                        scopeProvider
                        scope
                        letExpression
                        namesFromLetDeclarations
                        { name = name
                        , range = range
                        , signatureRange = Maybe.map (\(Node _ signature) -> Node.range signature.name) function.signature
                        }
                        errors

                Expression.LetDestructuring pattern _ ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            getDeclaredVariableNames pattern
                    in
                    List.foldl (error scopeProvider scope letExpression namesFromLetDeclarations) errors declaredVariables
        )
        []
        declarations


reportTopLevelFunction : Node String -> List (Rule.Error {})
reportTopLevelFunction (Node nameRange functionName) =
    if
        String.endsWith "_" functionName
            && not (Set.member functionName reservedElmKeywords)
    then
        [ Rule.error
            { message = "Top-level declaration names should not end with an underscore"
            , details =
                [ "A trailing underscore \"_\" is often used to prevent shadowing issues, but top-level declarations should not resolve these issues in that manner."
                ]
            }
            nameRange
        ]

    else
        []


report : ScopeProvider -> Scope -> List (Node Pattern) -> Node Expression -> List (Rule.Error {})
report scopeProvider scope patterns expression =
    let
        declaredVariables : List ScopeNames
        declaredVariables =
            List.concatMap getDeclaredVariableNames patterns
    in
    List.foldl (error scopeProvider scope expression Dict.empty) [] declaredVariables


error : ScopeProvider -> Scope -> Node Expression -> Dict String a -> ScopeNames -> List (Rule.Error {}) -> List (Rule.Error {})
error scopeProvider scope expressionToReplaceThingsIn namesOnTheSameLevel { range, name, signatureRange } errors =
    let
        nameWithoutUnderscore : String
        nameWithoutUnderscore =
            String.dropRight 1 name
    in
    if
        String.endsWith "_" name
            && not (Set.member name reservedElmKeywords)
            && ((Scope.valueBinding scope nameWithoutUnderscore == Nothing) || Dict.member nameWithoutUnderscore namesOnTheSameLevel)
            && (ScopeProvider.importValueBinding scopeProvider nameWithoutUnderscore == Nothing)
    then
        if Dict.member nameWithoutUnderscore namesOnTheSameLevel then
            Rule.error
                { message = name ++ " should not end with an underscore"
                , details =
                    [ "It seems that it has been used to prevent shadowing issues with a variable on the same level, but this is confusing. When should \"" ++ name ++ "\" be used and when should \"" ++ nameWithoutUnderscore ++ "\" be used?"
                    , "Please rename this variable in a way that makes it more explicit when or how each should be used."
                    ]
                }
                range
                :: errors

        else
            Rule.errorWithFix
                (defaultErrorAndMessage name)
                range
                (if Scope.canIntroduceName scope nameWithoutUnderscore then
                    case findUsages name [ expressionToReplaceThingsIn ] [] of
                        Just usages ->
                            List.map removeLastCharacter (addMaybe signatureRange (range :: usages))

                        Nothing ->
                            []

                 else
                    []
                )
                :: errors

    else
        errors


addMaybe : Maybe a -> List a -> List a
addMaybe maybe list =
    case maybe of
        Just element ->
            element :: list

        Nothing ->
            list


findUsages : String -> List (Node Expression) -> List Range -> Maybe (List Range)
findUsages targetName remainingNodes acc =
    case remainingNodes of
        [] ->
            Just acc

        (Node range node) :: rest ->
            case node of
                Expression.FunctionOrValue [] name ->
                    if name == targetName then
                        findUsages targetName rest (range :: acc)

                    else
                        findUsages targetName rest acc

                Expression.RecordUpdateExpression (Node nameRange name) nodes ->
                    if name == targetName then
                        findUsages targetName (mapAppend (\(Node _ ( _, n )) -> n) nodes rest) (nameRange :: acc)

                    else
                        findUsages targetName (mapAppend (\(Node _ ( _, n )) -> n) nodes rest) acc

                Expression.Application nodes ->
                    findUsages targetName (nodes ++ rest) acc

                Expression.OperatorApplication _ _ left right ->
                    findUsages targetName (left :: right :: rest) acc

                Expression.IfBlock cond then_ else_ ->
                    findUsages targetName (cond :: then_ :: else_ :: rest) acc

                Expression.Negation expr ->
                    findUsages targetName (expr :: rest) acc

                Expression.TupledExpression nodes ->
                    findUsages targetName (nodes ++ rest) acc

                Expression.ParenthesizedExpression expr ->
                    findUsages targetName (expr :: rest) acc

                Expression.LetExpression letBlock ->
                    findUsages targetName (letBlock.expression :: mapAppend extractExpression letBlock.declarations rest) acc

                Expression.CaseExpression caseBlock ->
                    findUsages targetName (caseBlock.expression :: mapAppend Tuple.second caseBlock.cases rest) acc

                Expression.LambdaExpression lambda ->
                    findUsages targetName (lambda.expression :: rest) acc

                Expression.RecordExpr nodes ->
                    findUsages targetName (mapAppend (\(Node _ ( _, n )) -> n) nodes rest) acc

                Expression.ListExpr nodes ->
                    findUsages targetName (nodes ++ rest) acc

                Expression.RecordAccess expr _ ->
                    findUsages targetName (expr :: rest) acc

                _ ->
                    findUsages targetName rest acc


extractExpression : Node Expression.LetDeclaration -> Node Expression
extractExpression (Node _ letExpr) =
    case letExpr of
        Expression.LetFunction { declaration } ->
            (Node.value declaration).expression

        Expression.LetDestructuring _ expression ->
            expression


removeLastCharacter : Range -> Fix
removeLastCharacter range =
    Fix.removeRange
        { start = { row = range.end.row, column = range.end.column - 1 }
        , end = range.end
        }


defaultErrorAndMessage : String -> { message : String, details : List String }
defaultErrorAndMessage name =
    { message = name ++ " should not end with an underscore"
    , details =
        [ "It looks like this was used to avoid a shadowing issue, but the variable it would have clashed with is not present in the scope of where this variable was declared anymore. You should rename the variable and remove the underscore."
        , "Note that this may not be a safe change, in that renaming may clash with a value declared deeper in the expression, but I think it's less confusing to have the nested variable have a trailing underscore rather than the variable declared higher up."
        ]
    }


mapAppend : (a -> b) -> List a -> List b -> List b
mapAppend fn list initial =
    List.foldl (\a acc -> fn a :: acc) initial list
