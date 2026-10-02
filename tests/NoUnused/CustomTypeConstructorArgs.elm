module NoUnused.CustomTypeConstructorArgs exposing (rule)

{-|

@docs rule

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.TypeInference.InferError exposing (InferError)
import Elm.TypeInference.Type exposing (Type)
import NoUnused.Parameters.ParameterPath as ParameterPath
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import String.Extra


{-| Reports arguments of custom type constructors that are never used.

🔧 Running with `--fix` will automatically remove most of the reported errors.

    config =
        [ NoUnused.CustomTypeConstructorArgs.rule
        ]

Custom type constructors can contain data that is never extracted out of the constructor.
This rule will warn arguments that are always pattern matched using a wildcard (`_`).

For package projects, custom types whose constructors are exposed as part of the package API are not reported.

Note that this rule **may report false positives** if you compare custom types with the `==` or `/=` operators
(and never destructure the custom type), like when you do `value == Just 0`, or store them in lists for instance with
[`assoc-list`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest).
This rule attempts to detect when the custom type is used in comparisons, but it may still result in false positives.


## Fail

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value _ -> value


## Success

    type CustomType
      = CustomType Used Unused

    case customType of
      CustomType value maybeUsed -> value


## When not to enable this rule?

If you like giving names to all arguments when pattern matching, then this rule will not find many problems.
This rule will work well when enabled along with [`NoUnused.Patterns`](./NoUnused-Patterns).

Also, if you like comparing custom types in the way described above, you might pass on this rule, or want to be very careful when enabling it.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.CustomTypeConstructorArgs
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructorArgs" initialProjectContext
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { dependencyModules : Set ModuleName
    , constructorsPerModule : Dict ModuleName ModuleConstructors
    , unusedArgumentsInPatterns :
        Dict
            ( Int, ConstructorName, ModuleName )
            {- `Just [ ... ]` is the list of unused arguments.
               `Just Nothing` means we have found at least one location where it's used, and we don't want to report it.
            -}
            (Maybe (List { moduleKey : Rule.ModuleKey, args : List Range }))
    , constructorsNotToReport : Set ( ConstructorName, ModuleName )
    , functionCallsWithArguments :
        Dict
            ( ConstructorName, ModuleName )
            (List { moduleKey : Rule.ModuleKey, callSites : List CallSite })
    }


type alias ModuleConstructors =
    { moduleKey : Rule.ModuleKey
    , constructors : Dict ConstructorName { nameRange : Range, args : List Range }
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , getType : Range -> Result.Result InferError Type
    , dependencyModules : Set ModuleName
    , customTypeArgs : List ( TypeName, Dict ConstructorName { nameRange : Range, args : List Range } )
    , unusedArgumentsInPatterns :
        Dict
            ( Int, ConstructorName, ModuleName )
            {- `Just [ ... ]` is the list of unused arguments.
               `Just Nothing` means we have found at least one location where it's used, and we don't want to report it.
            -}
            (Maybe (List Range))
    , constructorsNotToReport : Set ( ConstructorName, ModuleName )

    -- Function calls
    , functionCallsWithArguments : Dict ( ConstructorName, ModuleName ) (List CallSite)
    , locationsToIgnoreFunctionCalls : List Location
    }


type alias CallSite =
    { fnNameEnd : Location
    , arguments : Array (Node Expression)
    }


type TypeName
    = TypeName TypeNameS


type alias TypeNameS =
    String


type alias ConstructorName =
    String


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionVisitor node context ))


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List nothing, ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        dependencyModules : Set ModuleName
        dependencyModules =
            Dict.foldl
                (\_ dep set ->
                    List.foldl (\{ name } set_ -> Set.insert (String.split "." name) set_)
                        set
                        (Dependency.modules dep)
                )
                Set.empty
                dependencies
    in
    ( [], { projectContext | dependencyModules = dependencyModules } )


initialProjectContext : ProjectContext
initialProjectContext =
    { dependencyModules = Set.empty
    , constructorsPerModule = Dict.empty
    , unusedArgumentsInPatterns = Dict.empty
    , constructorsNotToReport = Set.empty
    , functionCallsWithArguments = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable getType projectContext ->
            { lookupTable = lookupTable
            , getType = getType
            , dependencyModules = projectContext.dependencyModules
            , customTypeArgs = []
            , unusedArgumentsInPatterns = Dict.empty
            , constructorsNotToReport = Set.empty
            , functionCallsWithArguments = Dict.empty
            , locationsToIgnoreFunctionCalls = []
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withTypes


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleKey moduleName isModuleExposed { exposesAll, exposed } moduleContext ->
            { dependencyModules = Set.empty
            , constructorsPerModule =
                Dict.singleton
                    moduleName
                    { moduleKey = moduleKey
                    , constructors = getNonPublicConstructors (Maybe.withDefault False isModuleExposed) exposesAll exposed moduleContext
                    }
            , unusedArgumentsInPatterns = Dict.map (\_ args -> Maybe.map (\args_ -> [ { moduleKey = moduleKey, args = args_ } ]) args) moduleContext.unusedArgumentsInPatterns
            , constructorsNotToReport = moduleContext.constructorsNotToReport
            , functionCallsWithArguments = Dict.map (\_ callSites -> [ { moduleKey = moduleKey, callSites = callSites } ]) moduleContext.functionCallsWithArguments
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleName
        |> Rule.withIsModuleExposed
        |> Rule.withExposed


{-| Get all custom types from the module whose constructors are not part of the public API of the package.
If the module is private or the project is an application, then all open custom types are collected.
-}
getNonPublicConstructors : Bool -> Bool -> Dict TypeNameS Bool -> ModuleContext -> Dict ConstructorName { nameRange : Range, args : List Range }
getNonPublicConstructors isModuleExposed exposesAll exposed moduleContext =
    if isModuleExposed then
        if exposesAll then
            Dict.empty

        else
            let
                exposedCustomTypes : Set TypeNameS
                exposedCustomTypes =
                    Dict.foldl
                        (\typeName isOpen set ->
                            if isOpen then
                                Set.insert typeName set

                            else
                                set
                        )
                        Set.empty
                        exposed
            in
            List.foldl
                (\( TypeName typeName, args ) acc ->
                    if Set.member typeName exposedCustomTypes then
                        acc

                    else
                        Dict.union args acc
                )
                Dict.empty
                moduleContext.customTypeArgs

    else
        List.foldl
            (\( _, args ) acc -> Dict.union args acc)
            Dict.empty
            moduleContext.customTypeArgs


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { dependencyModules = previousContext.dependencyModules
    , constructorsPerModule =
        Dict.union
            newContext.constructorsPerModule
            previousContext.constructorsPerModule
    , unusedArgumentsInPatterns =
        Dict.foldl
            (\key value dict ->
                case Dict.get key dict of
                    Just Nothing ->
                        dict

                    Just (Just list) ->
                        Dict.insert key (Maybe.map (\v -> v ++ list) value) dict

                    Nothing ->
                        Dict.insert key value dict
            )
            newContext.unusedArgumentsInPatterns
            previousContext.unusedArgumentsInPatterns
    , constructorsNotToReport = Set.union newContext.constructorsNotToReport previousContext.constructorsNotToReport
    , functionCallsWithArguments = mergeFunctionCallsWithArguments previousContext.functionCallsWithArguments newContext.functionCallsWithArguments
    }


mergeFunctionCallsWithArguments :
    Dict ( ConstructorName, ModuleName ) (List { moduleKey : Rule.ModuleKey, callSites : List CallSite })
    -> Dict ( ConstructorName, ModuleName ) (List { moduleKey : Rule.ModuleKey, callSites : List CallSite })
    -> Dict ( ConstructorName, ModuleName ) (List { moduleKey : Rule.ModuleKey, callSites : List CallSite })
mergeFunctionCallsWithArguments new previous =
    Dict.foldl
        (\key newDict acc ->
            case Dict.get key acc of
                Just previousList ->
                    Dict.insert key (newDict ++ previousList) acc

                Nothing ->
                    Dict.insert key newDict acc
        )
        previous
        new


isNever : ModuleNameLookupTable -> Node TypeAnnotation -> Bool
isNever lookupTable (Node _ node) =
    case node of
        TypeAnnotation.Typed (Node neverRange ( _, "Never" )) [] ->
            case ModuleNameLookupTable.moduleNameAt lookupTable neverRange of
                Just [ "Basics" ] ->
                    True

                _ ->
                    False

        _ ->
            False



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration function ->
            let
                unusedArgumentsInPatterns : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                unusedArgumentsInPatterns =
                    collectCustomTypeArgsInPatterns context (Node.value function.declaration).arguments context.unusedArgumentsInPatterns
            in
            { context
                | unusedArgumentsInPatterns = unusedArgumentsInPatterns
                , locationsToIgnoreFunctionCalls = []
            }

        Declaration.CustomTypeDeclaration typeDeclaration ->
            let
                customTypeConstructors : Dict ConstructorName { nameRange : Range, args : List Range }
                customTypeConstructors =
                    List.foldl
                        (\(Node _ constructor) acc ->
                            Dict.insert
                                (Node.value constructor.name)
                                { nameRange = Node.range constructor.name
                                , args = createArguments context.lookupTable constructor.arguments
                                }
                                acc
                        )
                        Dict.empty
                        typeDeclaration.constructors
            in
            { context
                | customTypeArgs = ( TypeName (Node.value typeDeclaration.name), customTypeConstructors ) :: context.customTypeArgs
            }

        _ ->
            context


createArguments : ModuleNameLookupTable -> List (Node TypeAnnotation) -> List Range
createArguments lookupTable arguments =
    List.foldr
        (\argument acc ->
            if isNever lookupTable argument then
                acc

            else
                Node.range argument :: acc
        )
        []
        arguments



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionVisitor (Node range node) context =
    case node of
        Expression.FunctionOrValue _ name ->
            registerFunctionCallReference name range [] context

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments) ->
            registerFunctionCallReference fnName fnRange arguments context

        Expression.OperatorApplication "|>" _ (Node { start } lastArg) (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) ->
            registerFunctionCallReference
                fnName
                fnRange
                (arguments ++ [ Node { start = start, end = applicationRange.start } lastArg ])
                context

        Expression.OperatorApplication "|>" _ (Node { start } lastArg) (Node fnRange (Expression.FunctionOrValue _ fnName)) ->
            registerFunctionCallReference
                fnName
                fnRange
                [ Node { start = start, end = fnRange.start } lastArg ]
                context

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: arguments))) (Node { end } lastArg) ->
            registerFunctionCallReference
                fnName
                fnRange
                (arguments ++ [ Node { start = applicationRange.end, end = end } lastArg ])
                context

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ fnName)) (Node { end } lastArg) ->
            registerFunctionCallReference
                fnName
                fnRange
                [ Node { start = fnRange.end, end = end } lastArg ]
                context

        Expression.CaseExpression { cases } ->
            let
                unusedArgumentsInPatterns : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                unusedArgumentsInPatterns =
                    collectCustomTypeArgsInPatterns
                        context
                        (List.map Tuple.first cases)
                        context.unusedArgumentsInPatterns
            in
            { context | unusedArgumentsInPatterns = unusedArgumentsInPatterns }

        Expression.LetExpression { declarations } ->
            let
                unusedArgumentsInPatterns : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                unusedArgumentsInPatterns =
                    List.foldl
                        (\(Node _ declaration) acc ->
                            case declaration of
                                Expression.LetDestructuring pattern _ ->
                                    collectCustomTypeArgsInPatterns context [ pattern ] acc

                                Expression.LetFunction function ->
                                    collectCustomTypeArgsInPatterns context (Node.value function.declaration).arguments acc
                        )
                        context.unusedArgumentsInPatterns
                        declarations
            in
            { context | unusedArgumentsInPatterns = unusedArgumentsInPatterns }

        Expression.LambdaExpression { args } ->
            let
                unusedArgumentsInPatterns : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                unusedArgumentsInPatterns =
                    collectCustomTypeArgsInPatterns context args context.unusedArgumentsInPatterns
            in
            { context | unusedArgumentsInPatterns = unusedArgumentsInPatterns }

        Expression.OperatorApplication operator _ left right ->
            if operator == "==" || operator == "/=" then
                { context | constructorsNotToReport = findCustomTypeConstructors context [ left, right ] context.constructorsNotToReport }

            else
                context

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
            if operator == "==" || operator == "/=" then
                { context | constructorsNotToReport = findCustomTypeConstructors context restOfArgs context.constructorsNotToReport }

            else
                context

        _ ->
            context


findCustomTypeConstructors : ModuleContext -> List (Node Expression) -> Set ( String, ModuleName ) -> Set ( String, ModuleName )
findCustomTypeConstructors context nodes acc =
    case nodes of
        [] ->
            acc

        (Node range node) :: restOfNodes ->
            case node of
                Expression.FunctionOrValue rawModuleName functionName ->
                    if String.Extra.isCapitalized functionName then
                        let
                            moduleName : ModuleName
                            moduleName =
                                ModuleNameLookupTable.fullModuleNameAt context.lookupTable range
                                    |> Maybe.withDefault rawModuleName
                        in
                        if Set.member moduleName context.dependencyModules then
                            findCustomTypeConstructors context restOfNodes acc

                        else
                            findCustomTypeConstructors context restOfNodes (Set.insert ( functionName, moduleName ) acc)

                    else
                        findCustomTypeConstructors context restOfNodes acc

                Expression.TupledExpression expressions ->
                    findCustomTypeConstructors context (expressions ++ restOfNodes) acc

                Expression.ParenthesizedExpression expression ->
                    findCustomTypeConstructors context (expression :: restOfNodes) acc

                Expression.Application (((Node _ (Expression.FunctionOrValue _ functionName)) as first) :: expressions) ->
                    if String.Extra.isCapitalized functionName then
                        findCustomTypeConstructors context (first :: (expressions ++ restOfNodes)) acc

                    else
                        findCustomTypeConstructors context restOfNodes acc

                Expression.OperatorApplication _ _ left right ->
                    findCustomTypeConstructors context (left :: right :: restOfNodes) acc

                Expression.Negation expression ->
                    findCustomTypeConstructors context (expression :: restOfNodes) acc

                Expression.ListExpr expressions ->
                    findCustomTypeConstructors context (expressions ++ restOfNodes) acc

                _ ->
                    findCustomTypeConstructors context restOfNodes acc


collectCustomTypeArgsInPatterns :
    ModuleContext
    -> List (Node Pattern)
    -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
    -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
collectCustomTypeArgsInPatterns context nodes acc =
    case nodes of
        [] ->
            acc

        (Node range pattern) :: restOfNodes ->
            case pattern of
                Pattern.NamedPattern ref args ->
                    let
                        newAcc : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                        newAcc =
                            case ModuleNameLookupTable.fullModuleNameAt context.lookupTable range of
                                Just moduleName ->
                                    if Set.member moduleName context.dependencyModules then
                                        acc

                                    else
                                        let
                                            endPositionOfName : Location
                                            endPositionOfName =
                                                { row = range.end.row
                                                , column = range.start.column + String.length (String.join "." (ref.name :: ref.moduleName))
                                                }
                                        in
                                        getUnusedConstructorFields moduleName ref.name 0 args endPositionOfName acc

                                Nothing ->
                                    acc
                    in
                    collectCustomTypeArgsInPatterns context (args ++ restOfNodes) newAcc

                Pattern.TuplePattern patterns ->
                    collectCustomTypeArgsInPatterns context (patterns ++ restOfNodes) acc

                Pattern.ListPattern patterns ->
                    collectCustomTypeArgsInPatterns context (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    collectCustomTypeArgsInPatterns context (left :: right :: restOfNodes) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectCustomTypeArgsInPatterns context (subPattern :: restOfNodes) acc

                Pattern.AsPattern subPattern _ ->
                    collectCustomTypeArgsInPatterns context (subPattern :: restOfNodes) acc

                _ ->
                    collectCustomTypeArgsInPatterns context restOfNodes acc


getUnusedConstructorFields : ModuleName -> ConstructorName -> Int -> List (Node Pattern) -> Location -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range)) -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
getUnusedConstructorFields moduleName constructorName index arguments previousEnd acc =
    case arguments of
        [] ->
            acc

        arg :: restOfArgs ->
            let
                key : ( Int, ConstructorName, ModuleName )
                key =
                    ( index, constructorName, moduleName )

                newAcc : Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
                newAcc =
                    case Dict.get key acc of
                        Just Nothing ->
                            -- We have previously found pattern matches for this constructor field
                            -- and some of them were *not* unused. We will continue to not report this field.
                            acc

                        Just (Just list) ->
                            addWildcardPosition key previousEnd arg list acc

                        Nothing ->
                            addWildcardPosition key previousEnd arg [] acc
            in
            getUnusedConstructorFields
                moduleName
                constructorName
                (index + 1)
                restOfArgs
                (Node.range arg).end
                newAcc


addWildcardPosition :
    ( Int, ConstructorName, ModuleName )
    -> Location
    -> Node Pattern
    -> List Range
    -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
    -> Dict ( Int, ConstructorName, ModuleName ) (Maybe (List Range))
addWildcardPosition key previousEnd arg list acc =
    if isWildcard arg then
        Dict.insert key (Just ({ start = previousEnd, end = (Node.range arg).end } :: list)) acc

    else
        -- This constructor field is *not* unused, we therefore insert `Nothing` to disable the rule reporting it.
        Dict.insert key Nothing acc


isWildcard : Node Pattern -> Bool
isWildcard (Node _ node) =
    case node of
        Pattern.AllPattern ->
            True

        Pattern.ParenthesizedPattern pattern ->
            isWildcard pattern

        _ ->
            False


registerFunctionCallReference : ConstructorName -> Range -> List (Node Expression) -> ModuleContext -> ModuleContext
registerFunctionCallReference fnName fnRange arguments context =
    if String.Extra.isCapitalized fnName && not (List.member fnRange.start context.locationsToIgnoreFunctionCalls) then
        case ModuleNameLookupTable.fullModuleNameAt context.lookupTable fnRange of
            Just moduleName ->
                if Set.member moduleName context.dependencyModules then
                    context

                else
                    let
                        functionCallsWithArguments : Dict ( ConstructorName, ModuleName ) (List CallSite)
                        functionCallsWithArguments =
                            insertInDictList
                                ( fnName, moduleName )
                                { fnNameEnd = fnRange.end, arguments = Array.fromList arguments }
                                context.functionCallsWithArguments
                    in
                    { context
                        | functionCallsWithArguments = functionCallsWithArguments
                        , locationsToIgnoreFunctionCalls = fnRange.start :: context.locationsToIgnoreFunctionCalls
                    }

            Nothing ->
                context

    else
        context



-- FINAL EVALUATION


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation context =
    Dict.foldl (finalEvaluationForSingleModule context) [] context.constructorsPerModule


finalEvaluationForSingleModule : ProjectContext -> ModuleName -> ModuleConstructors -> List (Error { useErrorForModule : () }) -> List (Error { useErrorForModule : () })
finalEvaluationForSingleModule context moduleName { moduleKey, constructors } previousErrors =
    Dict.foldl
        (\constructorName { nameRange, args } acc ->
            let
                key : ( ConstructorName, ModuleName )
                key =
                    ( constructorName, moduleName )
            in
            if Set.member key context.constructorsNotToReport then
                acc

            else
                errorsForUnusedArguments
                    context
                    moduleKey
                    moduleName
                    constructorName
                    0
                    nameRange
                    args
                    acc
        )
        previousErrors
        constructors


errorsForUnusedArguments :
    ProjectContext
    -> Rule.ModuleKey
    -> ModuleName
    -> ConstructorName
    -> Int
    -> Range
    -> List Range
    -> List (Error anywhere)
    -> List (Error anywhere)
errorsForUnusedArguments context moduleKey moduleName constructorName index previousRange argRanges acc =
    case argRanges of
        [] ->
            acc

        range :: rest ->
            let
                createError : List { moduleKey : Rule.ModuleKey, args : List Range } -> Error scope
                createError unusedArgumentsInPattern =
                    let
                        callSitesPerFile : List { moduleKey : Rule.ModuleKey, callSites : List CallSite }
                        callSitesPerFile =
                            Dict.get ( constructorName, moduleName ) context.functionCallsWithArguments
                                |> Maybe.withDefault []
                    in
                    error
                        moduleKey
                        constructorName
                        index
                        previousRange
                        range
                        callSitesPerFile
                        unusedArgumentsInPattern

                newAcc : List (Error anywhere)
                newAcc =
                    case Dict.get ( index, constructorName, moduleName ) context.unusedArgumentsInPatterns of
                        Just Nothing ->
                            acc

                        Just (Just unusedArgumentsInPattern) ->
                            createError unusedArgumentsInPattern :: acc

                        Nothing ->
                            createError [] :: acc
            in
            errorsForUnusedArguments
                context
                moduleKey
                moduleName
                constructorName
                (index + 1)
                range
                rest
                newAcc


error :
    Rule.ModuleKey
    -> String
    -> Int
    -> Range
    -> Range
    -> List { moduleKey : Rule.ModuleKey, callSites : List CallSite }
    -> List { moduleKey : Rule.ModuleKey, args : List Range }
    -> Error scope
error moduleKey constructorName index previousRange range callSitesPerFile patterns =
    let
        fixes : List Rule.FixV2
        fixes =
            case applyFixesAcrossModules index callSitesPerFile [] of
                Just callSiteFixes ->
                    Rule.editModule
                        moduleKey
                        [ Fix.removeRange { start = previousRange.end, end = range.end }
                        ]
                        :: List.map
                            (\pattern ->
                                Rule.editModule pattern.moduleKey (List.map Fix.removeRange pattern.args)
                            )
                            patterns
                        ++ callSiteFixes

                Nothing ->
                    []
    in
    Rule.errorForModule moduleKey
        { message = "The " ++ toOrdinal (index + 1) ++ " field of " ++ constructorName ++ " is never used"
        , details =
            [ "This field is never extracted and therefore never used. You should either use it somewhere, or remove it at the location I pointed at."
            ]
        }
        range
        |> Rule.withFixesV2 fixes


applyFixesAcrossModules :
    Int
    -> List { moduleKey : Rule.ModuleKey, callSites : List CallSite }
    -> List Rule.FixV2
    -> Maybe (List Rule.FixV2)
applyFixesAcrossModules index callSitesPerFile fixesSoFar =
    case callSitesPerFile of
        [] ->
            Just fixesSoFar

        { moduleKey, callSites } :: rest ->
            case addArgumentToRemove index [] callSites [] of
                Nothing ->
                    Nothing

                Just rangesToRemove ->
                    applyFixesAcrossModules
                        index
                        rest
                        (Rule.editModule moduleKey (List.map Fix.removeRange rangesToRemove) :: fixesSoFar)


addArgumentToRemove : Int -> List ParameterPath.Nesting -> List CallSite -> List Range -> Maybe (List Range)
addArgumentToRemove position nesting callSites acc =
    case callSites of
        [] ->
            Just acc

        callSite :: rest ->
            case Array.get position callSite.arguments of
                Just ((Node range _) as node) ->
                    case ParameterPath.fixCall (prettyRemovalRange range position callSite) node nesting acc of
                        Just edits ->
                            addArgumentToRemove position nesting rest edits

                        Nothing ->
                            Nothing

                Nothing ->
                    -- If an argument at that location could not be found, then we can't autofix the issue.
                    Nothing


prettyRemovalRange : Range -> Int -> CallSite -> Range
prettyRemovalRange range position callSite =
    let
        previousEnd : Location
        previousEnd =
            case Array.get (position - 1) callSite.arguments of
                Just (Node { end } _) ->
                    end

                Nothing ->
                    callSite.fnNameEnd
    in
    -- If the call was made with |>, then the constructed range will be negative.
    -- Therefore in that case, simply remove `range` which corresponds to `arg |> `
    case compare previousEnd.row range.end.row of
        LT ->
            { start = previousEnd, end = range.end }

        EQ ->
            if previousEnd.column <= range.end.column then
                { start = previousEnd, end = range.end }

            else
                range

        GT ->
            range


toOrdinal : Int -> String
toOrdinal n =
    let
        lastDigit : Int
        lastDigit =
            Basics.modBy 10 n

        suffix : String
        suffix =
            if lastDigit == 1 then
                "st"

            else if lastDigit == 2 then
                "nd"

            else
                "th"
    in
    String.fromInt n ++ suffix ++ ""


insertInDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
insertInDictList key value dict =
    let
        previous : List value
        previous =
            Dict.get key dict
                |> Maybe.withDefault []
    in
    Dict.insert key (value :: previous) dict
