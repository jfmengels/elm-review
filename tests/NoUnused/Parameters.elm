module NoUnused.Parameters exposing (rule)

{-| Report parameters that are not used.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import NoUnused.RangeDict as RangeDict exposing (RangeDict)
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

Value `something` is not used:

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
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema



--- CONTEXT


type alias Context =
    { scopes : List Scope
    , scopesToCreate : RangeDict ScopeToCreate
    , knownFunctions : Dict String FunctionArgs
    , locationsToIgnoreForUsed : LocationsToIgnore
    }


type alias Scope =
    { functionName : String
    , declared : List Declared
    , used : Set String
    , usedRecursively : Set String
    }


type alias ScopeToCreate =
    { declared : List Declared
    , functionName : String
    , functionArgs : FunctionArgs
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
    | AsWithoutVariables
    | TupleWithoutVariables


type Source
    = NamedFunction
    | Lambda


initialContext : Context
initialContext =
    { scopes = []
    , scopesToCreate = RangeDict.empty
    , knownFunctions = Dict.empty
    , locationsToIgnoreForUsed = Dict.empty
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            let
                arguments : List (Node Pattern)
                arguments =
                    (Node.value declaration).arguments

                declared : List (List Declared)
                declared =
                    List.map (getParametersFromPatterns NamedFunction) arguments
            in
            ( []
            , { scopes = []
              , scopesToCreate =
                    RangeDict.singleton
                        (declaration |> Node.value |> .expression |> Node.range)
                        { declared = List.concat declared
                        , functionName = Node.value declaration |> .name |> Node.value
                        , functionArgs = getArgNames declared
                        }
              , knownFunctions = Dict.empty
              , locationsToIgnoreForUsed = Dict.empty
              }
            )

        _ ->
            ( [], context )


getArgNames : List (List Declared) -> FunctionArgs
getArgNames declared =
    declared
        |> List.indexedMap
            (\index args ->
                case args of
                    [ arg ] ->
                        Just ( index, arg.name )

                    _ ->
                        Nothing
            )
        |> List.filterMap identity
        |> Dict.fromList


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
            if List.isEmpty parametersFromPatterns && isPatternWildCard pattern then
                [ asParameter
                , { name = ""
                  , range = Node.range pattern
                  , kind = AsWithoutVariables
                  , fix = [ Fix.removeRange { start = (Node.range pattern).start, end = (Node.range asName).start } ]
                  , source = source
                  }
                ]

            else
                asParameter :: parametersFromPatterns

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
    let
        newContext : Context
        newContext =
            case RangeDict.get (Node.range node) context.scopesToCreate of
                Just { declared, functionName, functionArgs } ->
                    { context
                        | scopes =
                            { functionName = functionName
                            , declared = declared
                            , used = Set.empty
                            , usedRecursively = Set.singleton "unused"
                            }
                                :: context.scopes
                        , knownFunctions =
                            Dict.insert
                                functionName
                                functionArgs
                                context.knownFunctions
                    }

                Nothing ->
                    context
    in
    expressionEnterVisitorHelp node newContext


expressionEnterVisitorHelp : Node Expression -> Context -> ( List nothing, Context )
expressionEnterVisitorHelp node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            ( [], markValueAsUsed (Node.range node) name context )

        Expression.RecordUpdateExpression name _ ->
            ( [], markValueAsUsed (Node.range name) (Node.value name) context )

        Expression.LetExpression letBlock ->
            let
                declaredWithRange : List ( Range, ScopeToCreate )
                declaredWithRange =
                    List.filterMap
                        (\letDeclaration ->
                            case Node.value letDeclaration of
                                Expression.LetFunction function ->
                                    let
                                        declaration : Expression.FunctionImplementation
                                        declaration =
                                            Node.value function.declaration

                                        declared : List (List Declared)
                                        declared =
                                            List.map (getParametersFromPatterns NamedFunction) declaration.arguments
                                    in
                                    if List.isEmpty declared then
                                        Nothing

                                    else
                                        Just
                                            ( Node.range declaration.expression
                                            , { declared = List.concat declared
                                              , functionName = Node.value declaration.name
                                              , functionArgs = getArgNames declared
                                              }
                                            )

                                Expression.LetDestructuring _ _ ->
                                    Nothing
                        )
                        letBlock.declarations

                scopesToCreate : RangeDict ScopeToCreate
                scopesToCreate =
                    RangeDict.insertAll declaredWithRange context.scopesToCreate
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        Expression.LambdaExpression { args, expression } ->
            let
                scopesToCreate : RangeDict ScopeToCreate
                scopesToCreate =
                    RangeDict.insert
                        (Node.range expression)
                        { declared = List.concatMap (getParametersFromPatterns Lambda) args
                        , functionName = "dummy lambda"
                        , functionArgs = Dict.empty
                        }
                        context.scopesToCreate
            in
            ( [], { context | scopesToCreate = scopesToCreate } )

        Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments) ->
            ( [], registerFunctionCall fnName 0 arguments context )

        Expression.OperatorApplication "|>" _ lastArgument (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            ( [], registerFunctionCall fnName (List.length arguments) [ lastArgument ] context )

        Expression.OperatorApplication "<|" _ (Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [] fnName)) :: arguments))) lastArgument ->
            -- Ignoring "arguments" because they will be visited when the Application node will be visited anyway.
            ( [], registerFunctionCall fnName (List.length arguments) [ lastArgument ] context )

        _ ->
            ( [], context )


registerFunctionCall : String -> Int -> List (Node a) -> Context -> Context
registerFunctionCall fnName numberOfIgnoredArguments arguments context =
    case Dict.get fnName context.knownFunctions of
        Just fnArgs ->
            let
                locationsToIgnore : LocationsToIgnore
                locationsToIgnore =
                    arguments
                        |> List.indexedMap Tuple.pair
                        |> List.filterMap
                            (\( index, arg ) ->
                                Dict.get (numberOfIgnoredArguments + index) fnArgs
                                    |> Maybe.map (\argName -> ( argName, [ Node.range arg ] ))
                            )
                        |> Dict.fromList
            in
            { context
                | locationsToIgnoreForUsed =
                    Dict.merge
                        Dict.insert
                        (\key new old -> Dict.insert key (new ++ old))
                        Dict.insert
                        locationsToIgnore
                        context.locationsToIgnoreForUsed
                        Dict.empty
            }

        Nothing ->
            context


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



-- EXPRESSION EXIT VISITOR


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
                ( errors, remainingUsed ) =
                    List.foldl
                        (\declared ( errors_, remainingUsed_ ) ->
                            if Set.member declared.name headScope.usedRecursively then
                                -- If variable was used as a recursive argument
                                if Set.member declared.name remainingUsed_ then
                                    -- If variable was used somewhere else as well
                                    ( errors_, Set.remove declared.name remainingUsed_ )

                                else
                                    -- If variable was used ONLY as a recursive argument
                                    ( recursiveParameterError headScope.functionName declared :: errors_, Set.remove declared.name remainingUsed_ )

                            else if Set.member declared.name remainingUsed_ then
                                ( errors_, Set.remove declared.name remainingUsed_ )

                            else
                                ( errorsForValue declared :: errors_, remainingUsed_ )
                        )
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

        AsWithoutVariables ->
            { message = "Pattern does not introduce any variables"
            , details = [ "You should remove this pattern." ]
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
