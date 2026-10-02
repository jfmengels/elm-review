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
import Elm.TypeInference.Type as Type exposing (Type)
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
    , customTypesNotToReport : Set ( TypeName, ModuleName )
    , constructorsNotToReport : Set ( ConstructorName, ModuleName )
    , functionCallsWithArguments :
        Dict
            ( ConstructorName, ModuleName )
            (List { moduleKey : Rule.ModuleKey, callSites : List CallSite })
    }


type alias ModuleConstructors =
    { moduleKey : Rule.ModuleKey
    , constructors : Dict ( TypeName, ConstructorName ) { nameRange : Range, args : List Range }
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , getType : Range -> Result.Result InferError Type
    , dependencyModules : Set ModuleName
    , customTypeArgs : Dict ( TypeName, ConstructorName ) { nameRange : Range, args : List Range }
    , unusedArgumentsInPatterns :
        Dict
            ( Int, ConstructorName, ModuleName )
            {- `Just [ ... ]` is the list of unused arguments.
               `Just Nothing` means we have found at least one location where it's used, and we don't want to report it.
            -}
            (Maybe (List Range))
    , customTypesNotToReport : Set ( TypeName, ModuleName )
    , constructorsNotToReport : Set ( ConstructorName, ModuleName )

    -- Function calls
    , functionCallsWithArguments : Dict ( ConstructorName, ModuleName ) (List CallSite)
    , locationsToIgnoreFunctionCalls : List Location
    }


type alias CallSite =
    { fnNameEnd : Location
    , arguments : Array (Node Expression)
    }


type alias TypeName =
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
    , customTypesNotToReport = Set.empty
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
            , customTypeArgs = Dict.empty
            , unusedArgumentsInPatterns = Dict.empty
            , customTypesNotToReport = Set.empty
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
            , customTypesNotToReport = moduleContext.customTypesNotToReport
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
getNonPublicConstructors : Bool -> Bool -> Dict TypeName Bool -> ModuleContext -> Dict ( TypeName, ConstructorName ) { nameRange : Range, args : List Range }
getNonPublicConstructors isModuleExposed exposesAll exposed moduleContext =
    if isModuleExposed then
        if exposesAll then
            Dict.empty

        else
            let
                exposedCustomTypes : Set TypeName
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
            Dict.filter
                (\( _, typeName ) _ ->
                    not (Set.member typeName exposedCustomTypes)
                )
                moduleContext.customTypeArgs

    else
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
    , customTypesNotToReport = Set.union newContext.customTypesNotToReport previousContext.customTypesNotToReport
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
                customTypeConstructors : Dict ( TypeName, ConstructorName ) { nameRange : Range, args : List Range }
                customTypeConstructors =
                    List.foldl
                        (\(Node _ constructor) acc ->
                            Dict.insert
                                ( Node.value constructor.name, Node.value typeDeclaration.name )
                                { nameRange = Node.range constructor.name
                                , args = createArguments context.lookupTable constructor.arguments
                                }
                                acc
                        )
                        context.customTypeArgs
                        typeDeclaration.constructors
            in
            { context | customTypeArgs = customTypeConstructors }

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
                let
                    ( typeVars, customTypesNotToReport ) =
                        avoidReportingCustomTypes
                            (List.filterMap (Node.range >> context.getType >> Result.toMaybe) [ left, right ])
                            Set.empty
                            context.customTypesNotToReport
                in
                { context | customTypesNotToReport = customTypesNotToReport }

            else
                context

        Expression.Application ((Node _ (Expression.PrefixOperator operator)) :: restOfArgs) ->
            if operator == "==" || operator == "/=" then
                { context | constructorsNotToReport = findCustomTypeConstructors context restOfArgs context.constructorsNotToReport }

            else
                context

        _ ->
            context


compareTypes :
    ModuleContext
    -> List ( Node Expression, Node Expression )
    -> { constructorsNotToReport : Set ( ConstructorName, ModuleName ), customTypesNotToReport : Set ( TypeName, ModuleName ) }
    -> { constructorsNotToReport : Set ( ConstructorName, ModuleName ), customTypesNotToReport : Set ( TypeName, ModuleName ) }
compareTypes context list acc =
    case list of
        [] ->
            acc

        ( (Node leftRange left) as nodeL, (Node rightRange right) as nodeR ) :: rest ->
            case ( left, right ) of
                -- Expanding
                ( Expression.ParenthesizedExpression expr, _ ) ->
                    compareTypes context (( expr, nodeR ) :: rest) acc

                ( _, Expression.ParenthesizedExpression expr ) ->
                    compareTypes context (( nodeL, expr ) :: rest) acc

                ( Expression.RecordAccess expr _, _ ) ->
                    -- TODO expand field
                    compareTypes context (( expr, nodeR ) :: rest) acc

                ( _, Expression.RecordAccess expr _ ) ->
                    -- TODO expand field
                    compareTypes context (( nodeL, expr ) :: rest) acc

                ( Expression.LetExpression { expression }, _ ) ->
                    compareTypes context (( expression, nodeR ) :: rest) acc

                ( _, Expression.LetExpression { expression } ) ->
                    compareTypes context (( nodeL, expression ) :: rest) acc

                ( Expression.CaseExpression _, _ ) ->
                    -- TODO Compare
                    -- TODO Can we simplify this further?
                    compareTypes context rest acc

                ( _, Expression.CaseExpression _ ) ->
                    -- TODO Compare
                    -- TODO Can we simplify this further?
                    compareTypes context rest acc

                -- Associating elements for more detailed comparisons
                ( Expression.TupledExpression listL, Expression.TupledExpression listR ) ->
                    compareTypes context
                        (List.map2 Tuple.pair listL listR ++ rest)
                        acc

                ( Expression.ListExpr listL, Expression.ListExpr listR ) ->
                    if areSameSize listL listR then
                        compareTypes context
                            (List.map2 Tuple.pair listL listR ++ rest)
                            acc

                    else
                        -- If one list is longer, then the equality is simply False
                        -- and whatever constructors we use is irrelevant is contained
                        compareTypes context rest acc

                ( Expression.IfBlock _ then_ else_, _ ) ->
                    compareTypes context (( nodeL, then_ ) :: ( nodeL, else_ ) :: rest) acc

                ( _, Expression.IfBlock _ then_ else_ ) ->
                    compareTypes context (( then_, nodeR ) :: ( else_, nodeR ) :: rest) acc

                ( Expression.RecordExpr listL, Expression.RecordExpr listR ) ->
                    let
                        ( newRest, newAcc ) =
                            prepareRecordsForTypeComparison context ( Nothing, listL ) ( Nothing, listR ) rest acc
                    in
                    compareTypes context newRest newAcc

                ( Expression.RecordUpdateExpression updateL listL, Expression.RecordExpr listR ) ->
                    let
                        ( newRest, newAcc ) =
                            prepareRecordsForTypeComparison context ( Just updateL, listL ) ( Nothing, listR ) rest acc
                    in
                    compareTypes context newRest newAcc

                ( Expression.RecordExpr listL, Expression.RecordUpdateExpression updateR listR ) ->
                    let
                        ( newRest, newAcc ) =
                            prepareRecordsForTypeComparison context ( Nothing, listL ) ( Just updateR, listR ) rest acc
                    in
                    compareTypes context newRest newAcc

                ( Expression.RecordUpdateExpression updateL listL, Expression.RecordUpdateExpression updateR listR ) ->
                    let
                        ( newRest, newAcc ) =
                            prepareRecordsForTypeComparison context ( Just updateL, listL ) ( Just updateR, listR ) rest acc
                    in
                    compareTypes context newRest newAcc

                -- Types where we know there is no fields involved
                ( Expression.UnitExpr, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.UnitExpr ) ->
                    compareTypes context rest acc

                ( Expression.Integer _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Integer _ ) ->
                    compareTypes context rest acc

                ( Expression.Hex _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Hex _ ) ->
                    compareTypes context rest acc

                ( Expression.Floatable _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Floatable _ ) ->
                    compareTypes context rest acc

                ( Expression.Negation _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Negation _ ) ->
                    compareTypes context rest acc

                ( Expression.Literal _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Literal _ ) ->
                    compareTypes context rest acc

                ( Expression.CharLiteral _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.CharLiteral _ ) ->
                    compareTypes context rest acc

                ( Expression.GLSLExpression _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.GLSLExpression _ ) ->
                    compareTypes context rest acc

                ( Expression.Operator _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.Operator _ ) ->
                    compareTypes context rest acc

                ( Expression.RecordAccessFunction _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.RecordAccessFunction _ ) ->
                    compareTypes context rest acc

                ( Expression.LambdaExpression _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.LambdaExpression _ ) ->
                    compareTypes context rest acc

                ( Expression.PrefixOperator _, _ ) ->
                    compareTypes context rest acc

                ( _, Expression.PrefixOperator _ ) ->
                    compareTypes context rest acc

                -- The rest is other impossible combinations, such as `( ListExpr _, RecordExpr _ )`
                -- Or non-reducible values, such as `FunctionOrValue` or `Application`
                _ ->
                    -- TODO
                    let
                        ( typeVars, customTypesNotToReport ) =
                            avoidReportingCustomTypes
                                [ context.getType leftRange |> Result.withDefault Type.Unit
                                , context.getType rightRange |> Result.withDefault Type.Unit
                                ]
                                Set.empty
                                acc.customTypesNotToReport
                    in
                    compareTypes
                        context
                        rest
                        { constructorsNotToReport = acc.constructorsNotToReport, customTypesNotToReport = customTypesNotToReport }


prepareRecordsForTypeComparison :
    ModuleContext
    -> ( Maybe (Node String), List (Node Expression.RecordSetter) )
    -> ( Maybe (Node String), List (Node Expression.RecordSetter) )
    -> List ( Node Expression, Node Expression )
    -> { constructorsNotToReport : Set ( ConstructorName, ModuleName ), customTypesNotToReport : Set ( TypeName, ModuleName ) }
    ->
        ( List ( Node Expression, Node Expression )
        , { constructorsNotToReport : Set ( ConstructorName, ModuleName ), customTypesNotToReport : Set ( TypeName, ModuleName ) }
        )
prepareRecordsForTypeComparison context ( updateVarL, listL ) ( updateVarR, listR ) rest acc =
    let
        leftFieldDict : Dict String (Node Expression)
        leftFieldDict =
            List.foldl
                (\(Node _ ( Node _ field, valueL )) dict -> Dict.insert field valueL dict)
                Dict.empty
                listL

        fieldDiffResult : { expressions : List (Node Expression), rest : List ( Node Expression, Node Expression ), leftFieldDict : Dict String (Node Expression) }
        fieldDiffResult =
            List.foldl
                (\(Node _ ( Node _ field, valueR )) subAcc ->
                    case Dict.get field subAcc.leftFieldDict of
                        Just valueL ->
                            { expressions = subAcc.expressions
                            , rest = ( valueL, valueR ) :: subAcc.rest
                            , leftFieldDict = Dict.remove field subAcc.leftFieldDict
                            }

                        Nothing ->
                            { expressions = valueR :: subAcc.expressions
                            , rest = subAcc.rest
                            , leftFieldDict = subAcc.leftFieldDict
                            }
                )
                { expressions = [], rest = rest, leftFieldDict = leftFieldDict }
                listR
    in
    ( fieldDiffResult.rest
    , { constructorsNotToReport =
            findCustomTypeConstructors
                context
                (Dict.values fieldDiffResult.leftFieldDict ++ fieldDiffResult.expressions)
                -- TODO Add all fields from the updateVar, except the ones available in the record update expression
                acc.constructorsNotToReport
      , customTypesNotToReport = acc.customTypesNotToReport
      }
    )


avoidReportingCustomTypes : List Type -> Set String -> Set ( TypeName, ModuleName ) -> ( Set String, Set ( TypeName, ModuleName ) )
avoidReportingCustomTypes types typeVars acc =
    case types of
        [] ->
            ( typeVars, acc )

        type_ :: rest ->
            case type_ of
                Type.Named { package, moduleName, name, arguments } ->
                    let
                        newAcc =
                            if package == "" then
                                -- TODO Handle args
                                Set.insert ( name, moduleName ) acc

                            else
                                -- TODO Handle args
                                acc
                    in
                    avoidReportingCustomTypes rest typeVars newAcc

                Type.TypeVar var ->
                    avoidReportingCustomTypes rest (Set.insert var typeVars) acc

                Type.List subType ->
                    avoidReportingCustomTypes
                        (subType :: rest)
                        typeVars
                        acc

                Type.Tuple2 a b ->
                    avoidReportingCustomTypes
                        (a :: b :: rest)
                        typeVars
                        acc

                Type.Tuple3 a b c ->
                    avoidReportingCustomTypes
                        (a :: b :: c :: rest)
                        typeVars
                        acc

                Type.Record { fields } ->
                    avoidReportingCustomTypes
                        (Dict.foldl (\_ fieldType list -> fieldType :: list) rest fields)
                        typeVars
                        acc

                Type.ExtensibleRecord { extensionTypevar, fields } ->
                    avoidReportingCustomTypes
                        (Dict.foldl (\_ fieldType list -> fieldType :: list) rest fields)
                        (Set.insert extensionTypevar typeVars)
                        acc

                _ ->
                    avoidReportingCustomTypes rest typeVars acc


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
        (\( constructorName, typeName ) { nameRange, args } acc ->
            if
                Set.member ( typeName, moduleName ) context.customTypesNotToReport
                    || Set.member ( constructorName, moduleName ) context.constructorsNotToReport
            then
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


areSameSize : List a -> List b -> Bool
areSameSize left right =
    case left of
        [] ->
            List.isEmpty right

        _ :: restL ->
            case right of
                [] ->
                    False

                _ :: restR ->
                    areSameSize restL restR
