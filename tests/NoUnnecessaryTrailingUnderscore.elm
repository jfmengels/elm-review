module NoUnnecessaryTrailingUnderscore exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports unnecessary or suboptimal trailing underscores in variable names.

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
    Rule.newModuleRuleSchema "NoUnnecessaryTrailingUnderscore" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : Scopes
    , scopesToAdd : Dict RangeLike (Set String)
    }


type alias Scopes =
    ( Set String, List (Set String) )


type alias RangeLike =
    List Int


initialContext : Context
initialContext =
    { scopes = ( Set.empty, [] )
    , scopesToAdd = Dict.empty
    }


declarationListVisitor : List (Node Declaration) -> Context -> ( List (Rule.Error {}), Context )
declarationListVisitor declarations context =
    let
        namesToAdd : Set String
        namesToAdd =
            List.filterMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            function.declaration
                                |> Node.value
                                |> .name
                                |> Node.value
                                |> Just

                        _ ->
                            Nothing
                )
                declarations
                |> Set.fromList

        errors : List (Rule.Error {})
        errors =
            List.filterMap
                (\node ->
                    case Node.value node of
                        Declaration.FunctionDeclaration function ->
                            reportFunction
                                Set.empty
                                context.scopes
                                function
                                { message = "Top-level declaration names should not end with an underscore"
                                , details =
                                    [ "A trailing underscore \"_\" is often used to prevent shadowing issues, but top-level declarations should not resolve these issues in that manner."
                                    ]
                                }

                        _ ->
                            Nothing
                )
                declarations
    in
    ( errors
    , { context | scopes = Tuple.mapFirst (Set.union namesToAdd) context.scopes }
    )


declarationVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                arguments : List (Node Pattern.Pattern)
                arguments =
                    function.declaration
                        |> Node.value
                        |> .arguments

                body : Node Expression
                body =
                    function.declaration
                        |> Node.value
                        |> .expression
            in
            report
                [ ( arguments, body ) ]
                context

        _ ->
            ( [], context )


type alias ScopeNames =
    { name : String
    , range : Range
    , origin : NameOrigin
    }


type NameOrigin
    = FromRecord
    | NotFromRecord


getDeclaredVariableNames : Node Pattern.Pattern -> List ScopeNames
getDeclaredVariableNames pattern =
    case Node.value pattern of
        Pattern.VarPattern name ->
            [ { name = name, range = Node.range pattern, origin = NotFromRecord } ]

        Pattern.ParenthesizedPattern subPattern ->
            getDeclaredVariableNames subPattern

        Pattern.AsPattern subPattern name ->
            { name = Node.value name, range = Node.range name, origin = NotFromRecord } :: getDeclaredVariableNames subPattern

        Pattern.TuplePattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.UnConsPattern left right ->
            List.concatMap getDeclaredVariableNames [ left, right ]

        Pattern.ListPattern patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap getDeclaredVariableNames patterns

        Pattern.RecordPattern fields ->
            List.map (\field -> { name = Node.value field, range = Node.range field, origin = FromRecord }) fields

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
        , "module_"
        , "where_"
        , "import_"
        , "exposing_"
        , "as_"
        , "port_"

        -- Until `elm-format` and `elm-syntax` allow `infix` as an identifier
        , "infix_"
        ]


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    let
        newContext : Context
        newContext =
            case Dict.get (Node.range node |> rangeToRangeLike) context.scopesToAdd of
                Just scopeToAdd ->
                    { context | scopes = addNewScope scopeToAdd context.scopes }

                Nothing ->
                    context
    in
    expressionVisitorHelp node newContext


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    let
        newContext : Context
        newContext =
            if Dict.member (Node.range node |> rangeToRangeLike) context.scopesToAdd then
                { context | scopes = popScope context.scopes }

            else
                context
    in
    ( [], newContext )


expressionVisitorHelp : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitorHelp node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            report
                (List.map (Tuple.mapFirst List.singleton) cases)
                context

        Expression.LambdaExpression { args, expression } ->
            report
                [ ( args, expression ) ]
                context

        Expression.LetExpression { declarations, expression } ->
            let
                namesFromLetDeclarations : Set String
                namesFromLetDeclarations =
                    Set.fromList (getNamesFromLetDeclarations declarations)

                ( errors, newContext ) =
                    report
                        (List.concatMap
                            (\declaration ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        let
                                            functionImplementation : Expression.FunctionImplementation
                                            functionImplementation =
                                                Node.value function.declaration
                                        in
                                        [ ( functionImplementation.arguments
                                          , functionImplementation.expression
                                          )
                                        ]

                                    Expression.LetDestructuring _ _ ->
                                        []
                            )
                            declarations
                        )
                        { context | scopes = addNewScope namesFromLetDeclarations context.scopes }
            in
            ( reportErrorsForLet namesFromLetDeclarations context.scopes declarations ++ errors
            , { newContext
                | scopesToAdd =
                    Dict.insert
                        (rangeToRangeLike (Node.range expression))
                        namesFromLetDeclarations
                        newContext.scopesToAdd
              }
            )

        _ ->
            ( [], context )


getNamesFromLetDeclarations : List (Node Expression.LetDeclaration) -> List String
getNamesFromLetDeclarations declarations =
    List.concatMap
        (\declaration ->
            case Node.value declaration of
                Expression.LetFunction function ->
                    [ function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
                    ]

                Expression.LetDestructuring pattern _ ->
                    List.map .name (getDeclaredVariableNames pattern)
        )
        declarations


reportErrorsForLet : Set String -> Scopes -> List (Node Expression.LetDeclaration) -> List (Rule.Error {})
reportErrorsForLet namesFromLetDeclarations scopes declarations =
    List.concatMap
        (\node ->
            case Node.value node of
                Expression.LetFunction function ->
                    let
                        functionName : String
                        functionName =
                            function.declaration
                                |> Node.value
                                |> .name
                                |> Node.value
                    in
                    case
                        reportFunction
                            namesFromLetDeclarations
                            scopes
                            function
                            (defaultErrorAndMessage functionName)
                    of
                        Just newError ->
                            [ newError ]

                        Nothing ->
                            []

                Expression.LetDestructuring pattern _ ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            getDeclaredVariableNames pattern

                        names : Set String
                        names =
                            declaredVariables
                                |> List.map .name
                                |> Set.fromList
                    in
                    List.filterMap (error (addNewScope names scopes)) declaredVariables
        )
        declarations


reportFunction : Set String -> Scopes -> Expression.Function -> { message : String, details : List String } -> Maybe (Rule.Error {})
reportFunction namesOnTheSameLevel scopes function messageAndDetails =
    let
        functionNameNode : Node String
        functionNameNode =
            function.declaration
                |> Node.value
                |> .name

        functionName : String
        functionName =
            Node.value functionNameNode

        functionNameWithoutUnderscore : String
        functionNameWithoutUnderscore =
            String.dropRight 1 functionName
    in
    if
        String.endsWith "_" functionName
            && not (isDefinedInScope scopes functionNameWithoutUnderscore)
            && not (Set.member functionName reservedElmKeywords)
    then
        if Set.member functionNameWithoutUnderscore namesOnTheSameLevel then
            Just
                (Rule.error
                    { message = functionName ++ " should not end with an underscore"
                    , details =
                        [ "It seems that it has been used to prevent shadowing issues with a variable on the same level, but this is confusing. When should \"" ++ functionName ++ "\" be used and when should \"" ++ functionNameWithoutUnderscore ++ "\" be used?"
                        , "Please rename this variable in a way that makes it more explicit when or how each should be used."
                        ]
                    }
                    (Node.range functionNameNode)
                )

        else
            Just
                (Rule.error
                    messageAndDetails
                    (Node.range functionNameNode)
                )

    else
        Nothing


report : List ( List (Node Pattern.Pattern), Node a ) -> Context -> ( List (Rule.Error {}), Context )
report patternsAndBody context =
    let
        scopesToAdd : List { errors : List (Rule.Error {}), scopesToAdd : ( RangeLike, Set String ) }
        scopesToAdd =
            List.map
                (\( patterns, expression ) ->
                    let
                        declaredVariables : List ScopeNames
                        declaredVariables =
                            List.concatMap getDeclaredVariableNames patterns

                        names : Set String
                        names =
                            declaredVariables
                                |> List.map .name
                                |> Set.fromList
                    in
                    { errors = List.filterMap (error (addNewScope names context.scopes)) declaredVariables
                    , scopesToAdd =
                        ( rangeToRangeLike (Node.range expression)
                        , names
                        )
                    }
                )
                patternsAndBody
    in
    ( List.concatMap .errors scopesToAdd
    , { context
        | scopesToAdd =
            Dict.union
                (scopesToAdd |> List.map .scopesToAdd |> Dict.fromList)
                context.scopesToAdd
      }
    )


addNewScope : Set String -> Scopes -> Scopes
addNewScope set ( head, tail ) =
    ( set, head :: tail )


popScope : Scopes -> Scopes
popScope ( head, tail ) =
    case tail of
        [] ->
            ( head, tail )

        newHead :: newTail ->
            ( newHead, newTail )


error : Scopes -> ScopeNames -> Maybe (Rule.Error {})
error scopes { range, name, origin } =
    if
        String.endsWith "_" name
            && not (isDefinedInScope scopes (String.dropRight 1 name))
            && not (Set.member name reservedElmKeywords)
            && shouldNameBeReported origin
    then
        Just
            (Rule.error
                (defaultErrorAndMessage name)
                range
            )

    else
        Nothing


defaultErrorAndMessage : String -> { message : String, details : List String }
defaultErrorAndMessage name =
    { message = name ++ " should not end with an underscore"
    , details =
        [ "It looks like this was used to avoid a shadowing issue, but the variable it would have clashed with is not present in the scope of where this variable was declared anymore. You should rename the variable and remove the underscore."
        , "Note that this may not be a safe change, in that renaming may clash with a value declared deeper in the expression, but I think it's less confusing to have the nested variable have a trailing underscore rather than the variable declared higher-up."
        ]
    }


shouldNameBeReported : NameOrigin -> Bool
shouldNameBeReported origin =
    case origin of
        FromRecord ->
            False

        NotFromRecord ->
            True


isDefinedInScope : Scopes -> String -> Bool
isDefinedInScope ( top, rest ) name =
    List.any (Set.member name) (top :: rest)


rangeToRangeLike : Range -> RangeLike
rangeToRangeLike range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
