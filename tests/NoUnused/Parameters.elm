module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Report parameters that are not used.

🔧 Running with `--fix` will automatically remove some of the reported errors.

    config =
        [ NoUnused.Parameters.rule
        ]

This rule looks within function arguments, let functions and lambdas to find any values that are unused. It will report any parameters that are not used.


## Fixes for lambdas

We're only offering fixes for lambdas here because we believe unused parameters in functions are a code smell that should be refactored.


## Fail

Value `number` is not used:

    add1 number =
        1

The rule will also report parameters that are only used to be passed again to the containing recursive function:

    last list unused =
        case list of
            [] ->
                Nothing

            [ a ] ->
                Just a

            _ :: rest ->
                last rest unused


## Success

    add1 number =
        number + 1


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Parameters
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnused.Parameters" initialContext
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { scopes : List Scope
    , knownFunctions : Dict String FunctionArgs
    , locationsToIgnoreForUsed : LocationsToIgnore
    }


type alias Scope =
    { functionName : String
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
    }


type alias Declared =
    { name : String
    , range : Range
    , kind : Kind
    , source : Source
    , fix : List Fix
    }


type alias LocationsToIgnore =
    Dict String (List Range)


type alias FunctionArgs =
    Dict Int String


type Kind
    = Parameter
    | Alias
    | TupleWithoutVariables


type Source
    = NamedFunction
    | Lambda


initialContext : Context
initialContext =
    { scopes = []
    , knownFunctions = Dict.empty
    , locationsToIgnoreForUsed = Dict.empty
    }



-- DECLARATION VISITOR


declarationEnterVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                arguments : List (Node Pattern)
                arguments =
                    (Node.value declaration).arguments

                declared : List (List Declared)
                declared =
                    List.map (getParametersFromPatterns NamedFunction) arguments

                functionName : String
                functionName =
                    Node.value declaration |> .name |> Node.value
            in
            ( []
            , { scopes =
                    [ { functionName = functionName
                      , declared = List.concat declared
                      , used = Set.empty
                      , usedRecursively = Set.empty
                      }
                    ]
              , knownFunctions = Dict.singleton functionName (getArgNames declared)
              , locationsToIgnoreForUsed = Dict.empty
              }
            )

        _ ->
            ( [], context )


declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            report context

        _ ->
            ( [], context )


getArgNames : List (List Declared) -> FunctionArgs
getArgNames declared =
    getArgNamesHelp declared 0 Dict.empty


getArgNamesHelp : List (List Declared) -> Int -> FunctionArgs -> FunctionArgs
getArgNamesHelp declared index acc =
    case declared of
        [] ->
            acc

        args :: restOfDeclared ->
            let
                newAcc : Dict Int String
                newAcc =
                    case args of
                        [ arg ] ->
                            Dict.insert index arg.name acc

                        _ ->
                            acc
            in
            getArgNamesHelp restOfDeclared (index + 1) newAcc


getParametersFromPatterns : Source -> Node Pattern -> List Declared
getParametersFromPatterns source node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            getParametersFromPatterns source pattern

        Pattern.VarPattern name ->
            [ { name = name
              , range = Node.range node
              , kind = Parameter
              , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
              , source = source
              }
            ]

        Pattern.AsPattern pattern asName ->
            getParametersFromAsPattern source pattern asName

        Pattern.RecordPattern fields ->
            case fields of
                [ field ] ->
                    [ { name = Node.value field
                      , range = Node.range field
                      , kind = Parameter
                      , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                      , source = source
                      }
                    ]

                _ ->
                    let
                        fieldNames : List String
                        fieldNames =
                            List.map Node.value fields
                    in
                    List.map
                        (\field ->
                            { name = Node.value field
                            , range = Node.range field
                            , kind = Parameter
                            , fix =
                                [ Fix.replaceRangeBy
                                    (Node.range node)
                                    (fieldNames |> List.filter (\f -> f /= Node.value field) |> formatRecord)
                                ]
                            , source = source
                            }
                        )
                        fields

        Pattern.TuplePattern patterns ->
            let
                parametersFromPatterns : List Declared
                parametersFromPatterns =
                    List.concatMap (getParametersFromPatterns source) patterns
            in
            if List.isEmpty parametersFromPatterns && List.all isPatternWildCard patterns then
                [ { name = ""
                  , range = Node.range node
                  , kind = TupleWithoutVariables
                  , fix = [ Fix.replaceRangeBy (Node.range node) "_" ]
                  , source = source
                  }
                ]

            else
                parametersFromPatterns

        Pattern.NamedPattern _ patterns ->
            List.concatMap (getParametersFromPatterns source) patterns

        _ ->
            []


getParametersFromAsPattern : Source -> Node Pattern -> Node String -> List Declared
getParametersFromAsPattern source pattern asName =
    let
        parametersFromPatterns : List Declared
        parametersFromPatterns =
            getParametersFromPatterns source pattern

        asParameter : Declared
        asParameter =
            { name = Node.value asName
            , range = Node.range asName
            , kind = Alias
            , fix = [ Fix.removeRange { start = (Node.range pattern).end, end = (Node.range asName).end } ]
            , source = source
            }
    in
    asParameter :: parametersFromPatterns


isPatternWildCard : Node Pattern -> Bool
isPatternWildCard node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            isPatternWildCard pattern

        Pattern.AllPattern ->
            True

        _ ->
            False


formatRecord : List String -> String
formatRecord fields =
    "{ " ++ String.join ", " fields ++ " }"



-- EXPRESSION ENTER VISITOR


expressionEnterVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitor node context =
    ( [], expressionEnterVisitorHelp node context )


expressionEnterVisitorHelp : Node Expression -> Context -> Context
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            markValueAsUsed (Node.range node) name context

        Expression.RecordUpdateExpression name _ ->
            markValueAsUsed (Node.range name) (Node.value name) context

        Expression.LambdaExpression { args } ->
            { context
                | scopes =
                    { functionName = "dummy lambda"
                    , declared = List.concatMap (getParametersFromPatterns Lambda) args
                    , used = Set.empty
                    , usedRecursively = Set.empty
                    }
                        :: context.scopes
            }

        Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments) ->
            registerFunctionCall fnName 0 arguments context

        Expression.OperatorApplication "|>" _ lastArgument (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            registerFunctionCall fnName (List.length arguments) [ lastArgument ] context

        Expression.OperatorApplication "<|" _ (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) lastArgument ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            registerFunctionCall fnName (List.length arguments) [ lastArgument ] context

        _ ->
            context



-- EXPRESSION EXIT VISITOR


expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionExitVisitor (Node _ node) context =
    case node of
        Expression.LambdaExpression _ ->
            report context

        _ ->
            ( [], context )


letDeclarationEnterVisitor : a -> Node Expression.LetDeclaration -> Context -> ( List nothing, Context )
letDeclarationEnterVisitor _ letDeclaration context =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                ( [], context )

            else
                let
                    functionName : String
                    functionName =
                        Node.value declaration.name

                    declared : List (List Declared)
                    declared =
                        List.map (getParametersFromPatterns NamedFunction) declaration.arguments

                    newScope : Scope
                    newScope =
                        { functionName = functionName
                        , declared = List.concat declared
                        , used = Set.empty
                        , usedRecursively = Set.empty
                        }
                in
                ( []
                , { context
                    | scopes = newScope :: context.scopes
                    , knownFunctions = Dict.insert functionName (getArgNames declared) context.knownFunctions
                  }
                )

        Expression.LetDestructuring _ _ ->
            ( [], context )


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> Context -> ( List (Rule.Error {}), Context )
letDeclarationExitVisitor _ letDeclaration context =
    case Node.value letDeclaration of
        Expression.LetFunction function ->
            let
                declaration : Expression.FunctionImplementation
                declaration =
                    Node.value function.declaration
            in
            if List.isEmpty declaration.arguments then
                ( [], context )

            else
                report context

        Expression.LetDestructuring _ _ ->
            ( [], context )


registerFunctionCall : String -> Int -> List (Node a) -> Context -> Context
registerFunctionCall fnName numberOfIgnoredArguments arguments context =
    case Dict.get fnName context.knownFunctions of
        Just fnArgs ->
            let
                locationsToIgnore : LocationsToIgnore
                locationsToIgnore =
                    ignoreLocations fnArgs numberOfIgnoredArguments arguments 0 context.locationsToIgnoreForUsed
            in
            { context | locationsToIgnoreForUsed = locationsToIgnore }

        Nothing ->
            context


ignoreLocations : FunctionArgs -> Int -> List (Node a) -> Int -> LocationsToIgnore -> LocationsToIgnore
ignoreLocations fnArgs numberOfIgnoredArguments nodes index acc =
    case nodes of
        [] ->
            acc

        (Node range _) :: rest ->
            let
                newAcc : LocationsToIgnore
                newAcc =
                    case Dict.get (numberOfIgnoredArguments + index) fnArgs of
                        Just argName ->
                            case Dict.get argName acc of
                                Just existingLocations ->
                                    Dict.insert argName (range :: existingLocations) acc

                                Nothing ->
                                    Dict.insert argName [ range ] acc

                        Nothing ->
                            acc
            in
            ignoreLocations fnArgs numberOfIgnoredArguments rest (index + 1) newAcc


markValueAsUsed : Range -> String -> Context -> Context
markValueAsUsed range name context =
    case context.scopes of
        [] ->
            context

        headScope :: restOfScopes ->
            let
                newHeadScope : Scope
                newHeadScope =
                    if shouldBeIgnored range name context then
                        { headScope | usedRecursively = Set.insert name headScope.usedRecursively }

                    else
                        { headScope | used = Set.insert name headScope.used }
            in
            { context | scopes = newHeadScope :: restOfScopes }


shouldBeIgnored : Range -> String -> Context -> Bool
shouldBeIgnored range name context =
    case Dict.get name context.locationsToIgnoreForUsed of
        Just ranges ->
            List.any (isRangeIncluded range) ranges

        Nothing ->
            False


isRangeIncluded : Range -> Range -> Bool
isRangeIncluded inner outer =
    (Range.compareLocations inner.start outer.start /= LT)
        && (Range.compareLocations inner.end outer.end /= GT)


markAllAsUsed : Set String -> List Scope -> List Scope
markAllAsUsed names scopes =
    case scopes of
        [] ->
            scopes

        headScope :: restOfScopes ->
            { headScope | used = Set.union names headScope.used } :: restOfScopes


report : Context -> ( List (Rule.Error {}), Context )
report context =
    case context.scopes of
        headScope :: restOfScopes ->
            let
                ( errors, remainingUsed ) =
                    List.foldl
                        (findErrorsAndVariablesNotPartOfScope headScope)
                        ( [], headScope.used )
                        headScope.declared
            in
            ( errors
            , { context
                | scopes = markAllAsUsed remainingUsed restOfScopes
                , knownFunctions = Dict.remove headScope.functionName context.knownFunctions
              }
            )

        [] ->
            ( [], context )


findErrorsAndVariablesNotPartOfScope : Scope -> Declared -> ( List (Rule.Error {}), Set String ) -> ( List (Rule.Error {}), Set String )
findErrorsAndVariablesNotPartOfScope scope declared ( errors_, remainingUsed_ ) =
    if Set.member declared.name scope.usedRecursively then
        -- If variable was used as a recursive argument
        if Set.member declared.name remainingUsed_ then
            -- If variable was used somewhere else as well
            ( errors_, Set.remove declared.name remainingUsed_ )

        else
            -- If variable was used ONLY as a recursive argument
            ( recursiveParameterError scope.functionName declared :: errors_, Set.remove declared.name remainingUsed_ )

    else if Set.member declared.name remainingUsed_ then
        ( errors_, Set.remove declared.name remainingUsed_ )

    else
        ( errorsForValue declared :: errors_, remainingUsed_ )


errorsForValue : Declared -> Rule.Error {}
errorsForValue { name, kind, range, source, fix } =
    Rule.errorWithFix
        (errorMessage kind name)
        range
        (applyFix source fix)


errorMessage : Kind -> String -> { message : String, details : List String }
errorMessage kind name =
    case kind of
        Parameter ->
            { message = "Parameter `" ++ name ++ "` is not used"
            , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
            }

        Alias ->
            { message = "Pattern alias `" ++ name ++ "` is not used"
            , details = [ "You should either use this parameter somewhere, or remove it at the location I pointed at." ]
            }

        TupleWithoutVariables ->
            { message = "Tuple pattern is not needed"
            , details = [ "You should remove this pattern." ]
            }


recursiveParameterError : String -> Declared -> Rule.Error {}
recursiveParameterError functionName { name, range } =
    Rule.error
        { message = "Parameter `" ++ name ++ "` is only used in recursion"
        , details =
            [ "This parameter is only used to be passed as an argument to '" ++ functionName ++ "', but its value is never read or used."
            , "You should either use this parameter somewhere, or remove it at the location I pointed at."
            ]
        }
        range


applyFix : Source -> List Fix -> List Fix
applyFix source fix =
    case source of
        NamedFunction ->
            []

        Lambda ->
            fix
