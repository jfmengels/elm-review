module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Report variables or types that are declared or imported but never used.

    config =
        [ NoUnused.Variables.rule
        ]


## Fail

    module A exposing (a)

    a n =
        n + 1

    b =
        a 2


## Success

    module A exposing (a)

    a n =
        n + 1


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Variables
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnused.Variables" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , inTheDeclarationOf : Maybe String
    , exposesEverything : Bool
    , constructorNameToTypeName : Dict String String
    , declaredModules : List DeclaredModule
    , usedModules : Set ( ModuleName, ModuleName )
    }


type alias DeclaredModule =
    { moduleName : ModuleName
    , alias : Maybe String
    , typeName : String
    , variableType : VariableType
    , under : Range
    , rangeToRemove : Range
    }


type alias Scope =
    { declared : Dict String VariableInfo
    , used : Set String
    }


type alias VariableInfo =
    { variableType : VariableType
    , typeName : String
    , under : Range
    , rangeToRemove : Range
    }


type VariableType
    = TopLevelVariable
    | LetVariable
    | ImportedModule
    | ImportedItem ImportType
    | ModuleAlias { originalNameOfTheImport : String, exposesSomething : Bool }
    | Type
    | Port
    | Operator


type LetBlockContext
    = HasMultipleDeclarations
    | HasNoOtherDeclarations Range


type ImportType
    = ImportedVariable
    | ImportedType
    | ImportedOperator


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , scopes = NonemptyList.fromElement emptyScope
            , inTheDeclarationOf = Nothing
            , exposesEverything = False
            , constructorNameToTypeName = Dict.empty
            , declaredModules = []
            , usedModules = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


emptyScope : Scope
emptyScope =
    { declared = Dict.empty
    , used = Set.empty
    }


error : (String -> Bool) -> { a | variableType : VariableType, typeName : String, under : Range, rangeToRemove : Range } -> String -> Error {}
error willConflictWithOtherModule variableInfo name =
    Rule.errorWithFix
        { message = variableInfo.typeName ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableInfo.variableType
        , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
        }
        variableInfo.under
        (fix willConflictWithOtherModule variableInfo)


variableTypeWarning : VariableType -> String
variableTypeWarning value =
    case value of
        TopLevelVariable ->
            ""

        LetVariable ->
            ""

        ImportedModule ->
            ""

        ImportedItem _ ->
            ""

        ModuleAlias _ ->
            ""

        Type ->
            ""

        Port ->
            " (Warning: Removing this port may break your application if it is used in the JS code)"

        Operator ->
            ""


fix : (String -> Bool) -> { a | variableType : VariableType, rangeToRemove : Range } -> List Fix
fix willConflictWithOtherModule { variableType, rangeToRemove } =
    let
        shouldOfferFix : Bool
        shouldOfferFix =
            case variableType of
                TopLevelVariable ->
                    True

                LetVariable ->
                    True

                ImportedModule ->
                    True

                ImportedItem _ ->
                    True

                ModuleAlias { originalNameOfTheImport, exposesSomething } ->
                    not exposesSomething
                        || not (willConflictWithOtherModule originalNameOfTheImport)

                Type ->
                    True

                Port ->
                    False

                Operator ->
                    False
    in
    if shouldOfferFix then
        [ Fix.removeRange rangeToRemove ]

    else
        []


moduleDefinitionVisitor : Node Module -> Context -> ( List nothing, Context )
moduleDefinitionVisitor (Node _ moduleNode) context =
    case Module.exposingList moduleNode of
        Exposing.All _ ->
            ( [], { context | exposesEverything = True } )

        Exposing.Explicit list ->
            let
                names : List String
                names =
                    List.map
                        (\(Node _ node) ->
                            case node of
                                Exposing.FunctionExpose name ->
                                    name

                                Exposing.TypeOrAliasExpose name ->
                                    name

                                Exposing.TypeExpose { name } ->
                                    name

                                Exposing.InfixExpose name ->
                                    name
                        )
                        list
            in
            ( [], markAllAsUsed names context )


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor ((Node _ import_) as node) context =
    let
        errors : List (Error {})
        errors =
            case import_.moduleAlias of
                Just moduleAlias ->
                    if Node.value moduleAlias == Node.value import_.moduleName then
                        [ Rule.errorWithFix
                            { message = "Module `" ++ String.join "." (Node.value moduleAlias) ++ "` is aliased as itself"
                            , details = [ "The alias is the same as the module name, and brings no useful value" ]
                            }
                            (Node.range moduleAlias)
                            [ Fix.removeRange <| moduleAliasRange node (Node.range moduleAlias) ]
                        ]

                    else
                        []

                Nothing ->
                    []
    in
    case import_.exposingList of
        Nothing ->
            ( errors, registerModuleNameOrAlias node context )

        Just declaredImports ->
            ( errors
            , List.foldl
                (\( name, variableInfo ) context_ -> register variableInfo name context_)
                (case import_.moduleAlias of
                    Just moduleAlias ->
                        registerModuleAlias node moduleAlias context

                    Nothing ->
                        -- TODO?
                        context
                )
                (collectFromExposing declaredImports)
            )


registerModuleNameOrAlias : Node Import -> Context -> Context
registerModuleNameOrAlias ((Node range { moduleAlias, moduleName }) as node) context =
    case moduleAlias of
        Just moduleAlias_ ->
            registerModuleAlias node moduleAlias_ context

        Nothing ->
            registerModule
                { moduleName = Node.value moduleName
                , alias = Nothing
                , typeName = "Imported module"
                , variableType = ImportedModule
                , under = Node.range moduleName
                , rangeToRemove = untilStartOfNextLine range
                }
                context


registerModuleAlias : Node Import -> Node ModuleName -> Context -> Context
registerModuleAlias ((Node range { exposingList, moduleName }) as node) moduleAlias context =
    registerModule
        { moduleName = Node.value moduleName
        , alias = Just (getModuleName (Node.value moduleAlias))
        , variableType =
            ModuleAlias
                { originalNameOfTheImport = getModuleName <| Node.value moduleName
                , exposesSomething = exposingList /= Nothing
                }
        , typeName = "Module alias"
        , under = Node.range moduleAlias
        , rangeToRemove =
            case exposingList of
                Nothing ->
                    untilStartOfNextLine range

                Just _ ->
                    moduleAliasRange node (Node.range moduleAlias)
        }
        context


moduleAliasRange : Node Import -> Range -> Range
moduleAliasRange (Node _ { moduleName }) range =
    { range | start = (Node.range moduleName).end }


expressionEnterVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionEnterVisitor (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            ( [], markAsUsed name context )

        Expression.FunctionOrValue moduleName _ ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just realModuleName ->
                    ( [], markModuleAsUsed ( realModuleName, moduleName ) context )

                Nothing ->
                    ( [], context )

        Expression.OperatorApplication name _ _ _ ->
            ( [], markAsUsed name context )

        Expression.PrefixOperator name ->
            ( [], markAsUsed name context )

        Expression.LetExpression { declarations, expression } ->
            let
                letBlockContext : LetBlockContext
                letBlockContext =
                    if List.length declarations == 1 then
                        HasNoOtherDeclarations <| rangeUpUntil range (Node.range expression |> .start)

                    else
                        HasMultipleDeclarations
            in
            List.foldl
                (\declaration (( errors, foldContext ) as unchangedResult) ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            let
                                namesUsedInArgumentPatterns : { types : List String, modules : List ( ModuleName, ModuleName ) }
                                namesUsedInArgumentPatterns =
                                    function.declaration
                                        |> Node.value
                                        |> .arguments
                                        |> List.map (getUsedVariablesFromPattern context.lookupTable)
                                        |> foldUsedTypesAndModules
                            in
                            ( errors
                            , foldContext
                                |> registerFunction letBlockContext function
                                |> markUsedTypesAndModules namesUsedInArgumentPatterns
                            )

                        Expression.LetDestructuring pattern _ ->
                            case isAllPattern pattern of
                                Just wildCardRange ->
                                    ( Rule.errorWithFix
                                        { message = "Value assigned to `_` is unused"
                                        , details =
                                            [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                                            ]
                                        }
                                        wildCardRange
                                        [ case letBlockContext of
                                            HasMultipleDeclarations ->
                                                Fix.removeRange (Node.range declaration)

                                            HasNoOtherDeclarations letDeclarationsRange ->
                                                -- If there are no other declarations in the let in block,
                                                -- we also need to remove the `let in` keywords.
                                                Fix.removeRange letDeclarationsRange
                                        ]
                                        :: errors
                                    , foldContext
                                    )

                                Nothing ->
                                    unchangedResult
                )
                ( [], { context | scopes = NonemptyList.cons emptyScope context.scopes } )
                declarations

        Expression.LambdaExpression { args } ->
            let
                namesUsedInArgumentPatterns : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInArgumentPatterns =
                    args
                        |> List.map (getUsedVariablesFromPattern context.lookupTable)
                        |> foldUsedTypesAndModules
            in
            ( [], markUsedTypesAndModules namesUsedInArgumentPatterns context )

        _ ->
            ( [], context )


isAllPattern : Node Pattern -> Maybe Range
isAllPattern node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            isAllPattern pattern

        Pattern.AllPattern ->
            Just (Node.range node)

        _ ->
            Nothing


expressionExitVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.RecordUpdateExpression expr _ ->
            ( [], markAsUsed (Node.value expr) context )

        Expression.CaseExpression { cases } ->
            let
                usedVariables : { types : List String, modules : List ( ModuleName, ModuleName ) }
                usedVariables =
                    cases
                        |> List.map
                            (\( patternNode, _ ) ->
                                getUsedVariablesFromPattern context.lookupTable patternNode
                            )
                        |> foldUsedTypesAndModules
            in
            ( []
            , markUsedTypesAndModules usedVariables context
            )

        Expression.LetExpression _ ->
            let
                ( errors, remainingUsed ) =
                    makeReport (NonemptyList.head context.scopes)

                contextWithPoppedScope : Context
                contextWithPoppedScope =
                    { context | scopes = NonemptyList.pop context.scopes }
            in
            ( errors
            , markAllAsUsed remainingUsed contextWithPoppedScope
            )

        _ ->
            ( [], context )


getUsedVariablesFromPattern : ModuleNameLookupTable -> Node Pattern -> { types : List String, modules : List ( ModuleName, ModuleName ) }
getUsedVariablesFromPattern lookupTable patternNode =
    { types = getUsedTypesFromPattern patternNode
    , modules = getUsedModulesFromPattern lookupTable patternNode
    }


getUsedTypesFromPattern : Node Pattern -> List String
getUsedTypesFromPattern patternNode =
    case Node.value patternNode of
        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []

        Pattern.TuplePattern patterns ->
            List.concatMap getUsedTypesFromPattern patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap getUsedTypesFromPattern [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap getUsedTypesFromPattern patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            case qualifiedNameRef.moduleName of
                [] ->
                    qualifiedNameRef.name :: List.concatMap getUsedTypesFromPattern patterns

                _ ->
                    List.concatMap getUsedTypesFromPattern patterns

        Pattern.AsPattern pattern _ ->
            getUsedTypesFromPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedTypesFromPattern pattern


getUsedModulesFromPattern : ModuleNameLookupTable -> Node Pattern -> List ( ModuleName, ModuleName )
getUsedModulesFromPattern lookupTable patternNode =
    case Node.value patternNode of
        Pattern.AllPattern ->
            []

        Pattern.UnitPattern ->
            []

        Pattern.CharPattern _ ->
            []

        Pattern.StringPattern _ ->
            []

        Pattern.IntPattern _ ->
            []

        Pattern.HexPattern _ ->
            []

        Pattern.FloatPattern _ ->
            []

        Pattern.TuplePattern patterns ->
            List.concatMap (getUsedModulesFromPattern lookupTable) patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap (getUsedModulesFromPattern lookupTable) [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap (getUsedModulesFromPattern lookupTable) patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            case qualifiedNameRef.moduleName of
                [] ->
                    List.concatMap (getUsedModulesFromPattern lookupTable) patterns

                moduleName ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable patternNode of
                        Just realModuleName ->
                            ( realModuleName, moduleName ) :: List.concatMap (getUsedModulesFromPattern lookupTable) patterns

                        Nothing ->
                            List.concatMap (getUsedModulesFromPattern lookupTable) patterns

        Pattern.AsPattern pattern _ ->
            getUsedModulesFromPattern lookupTable pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedModulesFromPattern lookupTable pattern


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionImplementation : FunctionImplementation
                functionImplementation =
                    Node.value function.declaration

                namesUsedInSignature : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInSignature =
                    function.signature
                        |> Maybe.map (Node.value >> .typeAnnotation >> collectNamesFromTypeAnnotation context.lookupTable)
                        |> Maybe.withDefault { types = [], modules = [] }

                namesUsedInArgumentPatterns : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInArgumentPatterns =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.map (getUsedVariablesFromPattern context.lookupTable)
                        |> foldUsedTypesAndModules

                newContext : Context
                newContext =
                    { context | inTheDeclarationOf = Just <| Node.value functionImplementation.name }
                        |> register
                            { variableType = TopLevelVariable
                            , typeName = "Top-level variable"
                            , under = Node.range functionImplementation.name
                            , rangeToRemove = rangeToRemoveForNodeWithDocumentation node function.documentation
                            }
                            (Node.value functionImplementation.name)
                        |> markUsedTypesAndModules namesUsedInSignature
                        |> markUsedTypesAndModules namesUsedInArgumentPatterns
            in
            ( [], newContext )

        Declaration.CustomTypeDeclaration { name, documentation, constructors } ->
            let
                variablesFromConstructorArguments : { types : List String, modules : List ( ModuleName, ModuleName ) }
                variablesFromConstructorArguments =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.map (collectNamesFromTypeAnnotation context.lookupTable)
                        |> foldUsedTypesAndModules

                typeName : String
                typeName =
                    Node.value name

                constructorsForType : Dict String String
                constructorsForType =
                    constructors
                        |> List.map (Node.value >> .name >> Node.value)
                        |> List.map (\constructorName -> ( constructorName, typeName ))
                        |> Dict.fromList
            in
            ( []
            , { context | constructorNameToTypeName = Dict.union constructorsForType context.constructorNameToTypeName }
                |> register
                    { variableType = Type
                    , typeName = "Type"
                    , under = Node.range name
                    , rangeToRemove = rangeToRemoveForNodeWithDocumentation node documentation
                    }
                    (Node.value name)
                |> markUsedTypesAndModules variablesFromConstructorArguments
            )

        Declaration.AliasDeclaration { name, typeAnnotation, documentation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation context.lookupTable typeAnnotation
            in
            ( []
            , context
                |> register
                    { variableType = Type
                    , typeName = "Type"
                    , under = Node.range name
                    , rangeToRemove = rangeToRemoveForNodeWithDocumentation node documentation
                    }
                    (Node.value name)
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
            )

        Declaration.PortDeclaration { name, typeAnnotation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation context.lookupTable typeAnnotation
            in
            ( []
            , context
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
                |> register
                    { variableType = Port
                    , typeName = "Port"
                    , under = Node.range name
                    , rangeToRemove = Node.range node
                    }
                    (Node.value name)
            )

        Declaration.InfixDeclaration { operator, function } ->
            ( []
            , context
                |> markAsUsed (Node.value function)
                |> register
                    { variableType = Operator
                    , typeName = "Declared operator"
                    , under = Node.range operator
                    , rangeToRemove = Node.range node
                    }
                    (Node.value operator)
            )

        Declaration.Destructuring _ _ ->
            ( [], context )


foldUsedTypesAndModules : List { types : List String, modules : List ( ModuleName, ModuleName ) } -> { types : List String, modules : List ( ModuleName, ModuleName ) }
foldUsedTypesAndModules =
    List.foldl (\a b -> { types = a.types ++ b.types, modules = a.modules ++ b.modules }) { types = [], modules = [] }


markUsedTypesAndModules : { types : List String, modules : List ( ModuleName, ModuleName ) } -> Context -> Context
markUsedTypesAndModules { types, modules } context =
    context
        |> markAllAsUsed types
        |> markAllModulesAsUsed modules


rangeToRemoveForNodeWithDocumentation : Node Declaration -> Maybe (Node a) -> Range
rangeToRemoveForNodeWithDocumentation (Node nodeRange _) documentation =
    case documentation of
        Nothing ->
            untilStartOfNextLine nodeRange

        Just (Node documentationRange _) ->
            untilStartOfNextLine
                { start = documentationRange.start
                , end = nodeRange.end
                }


finalEvaluation : Context -> List (Error {})
finalEvaluation context =
    -- TODO report imports even when exposing everything
    if context.exposesEverything then
        []

    else
        let
            rootScope : Scope
            rootScope =
                NonemptyList.head context.scopes

            namesOfCustomTypesUsedByCallingAConstructor : Set String
            namesOfCustomTypesUsedByCallingAConstructor =
                context.constructorNameToTypeName
                    |> Dict.filter (\usedName _ -> Set.member usedName rootScope.used)
                    |> Dict.values
                    |> Set.fromList

            newRootScope : Scope
            newRootScope =
                { rootScope | used = Set.union namesOfCustomTypesUsedByCallingAConstructor rootScope.used }

            moduleNamesInUse : Set String
            moduleNamesInUse =
                context.declaredModules
                    |> List.map (\{ alias, moduleName } -> Maybe.withDefault (getModuleName moduleName) alias)
                    |> Set.fromList

            moduleErrors : List (Error {})
            moduleErrors =
                context.declaredModules
                    |> List.filter
                        (\variableInfo ->
                            not
                                (case variableInfo.alias of
                                    Just alias ->
                                        Set.member ( variableInfo.moduleName, [ alias ] ) context.usedModules

                                    Nothing ->
                                        Set.member ( variableInfo.moduleName, variableInfo.moduleName ) context.usedModules
                                )
                        )
                    |> List.map
                        (\variableInfo ->
                            error
                                (\moduleName -> Set.member moduleName moduleNamesInUse)
                                variableInfo
                                (case variableInfo.alias of
                                    Just alias ->
                                        alias

                                    Nothing ->
                                        getModuleName variableInfo.moduleName
                                )
                        )
        in
        List.concat
            [ newRootScope
                |> makeReport
                |> Tuple.first
            , moduleErrors
            ]


registerFunction : LetBlockContext -> Function -> Context -> Context
registerFunction letBlockContext function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        namesUsedInSignature : { types : List String, modules : List ( ModuleName, ModuleName ) }
        namesUsedInSignature =
            case Maybe.map Node.value function.signature of
                Just signature ->
                    collectNamesFromTypeAnnotation context.lookupTable signature.typeAnnotation

                Nothing ->
                    { types = [], modules = [] }

        functionRange : Range
        functionRange =
            case function.signature of
                Just signature ->
                    mergeRanges
                        (Node.range function.declaration)
                        (Node.range signature)

                Nothing ->
                    Node.range function.declaration
    in
    context
        |> register
            { variableType = LetVariable
            , typeName = "`let in` variable"
            , under = Node.range declaration.name
            , rangeToRemove =
                case letBlockContext of
                    HasMultipleDeclarations ->
                        functionRange

                    HasNoOtherDeclarations letDeclarationsRange ->
                        -- If there are no other declarations in the let in block,
                        -- we also need to remove the `let in` keywords.
                        letDeclarationsRange
            }
            (Node.value declaration.name)
        |> markUsedTypesAndModules namesUsedInSignature


collectFromExposing : Node Exposing -> List ( String, VariableInfo )
collectFromExposing exposingNode =
    case Node.value exposingNode of
        Exposing.All _ ->
            []

        Exposing.Explicit list ->
            let
                listWithPreviousRange : List (Maybe Range)
                listWithPreviousRange =
                    Nothing
                        :: (list
                                |> List.map (Node.range >> Just)
                                |> List.take (List.length list - 1)
                           )

                listWithNextRange : List Range
                listWithNextRange =
                    (list
                        |> List.map Node.range
                        |> List.drop 1
                    )
                        ++ [ { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } ]
            in
            list
                |> List.map3 (\prev next current -> ( prev, current, next )) listWithPreviousRange listWithNextRange
                |> List.indexedMap
                    (\index ( maybePreviousRange, Node range value, nextRange ) ->
                        let
                            rangeToRemove : Range
                            rangeToRemove =
                                if List.length list == 1 then
                                    Node.range exposingNode

                                else if index == 0 then
                                    { range | end = nextRange.start }

                                else
                                    case maybePreviousRange of
                                        Nothing ->
                                            range

                                        Just previousRange ->
                                            { range | start = previousRange.end }
                        in
                        case value of
                            Exposing.FunctionExpose name ->
                                Just
                                    ( name
                                    , { variableType = ImportedItem ImportedVariable
                                      , typeName = "Imported variable"
                                      , under = untilEndOfVariable name range
                                      , rangeToRemove = rangeToRemove
                                      }
                                    )

                            Exposing.InfixExpose name ->
                                Just
                                    ( name
                                    , { variableType = ImportedItem ImportedOperator
                                      , typeName = "Imported operator"
                                      , under = untilEndOfVariable name range
                                      , rangeToRemove = rangeToRemove
                                      }
                                    )

                            Exposing.TypeOrAliasExpose name ->
                                -- TODO Detect whether it is a custom type or type alias
                                Just
                                    ( name
                                    , { variableType = ImportedItem ImportedType
                                      , typeName = "Imported type"
                                      , under = untilEndOfVariable name range
                                      , rangeToRemove = rangeToRemove
                                      }
                                    )

                            Exposing.TypeExpose { name, open } ->
                                case open of
                                    Just _ ->
                                        -- TODO Change this behavior once we know the contents of the open range, using dependencies or the interfaces of the other modules
                                        Nothing

                                    Nothing ->
                                        -- TODO Check that this case has tests, it doesn't look like it
                                        Just
                                            ( name
                                            , { variableType = ImportedItem ImportedType
                                              , typeName = "Imported type"
                                              , under = range
                                              , rangeToRemove = rangeToRemove
                                              }
                                            )
                    )
                |> List.filterMap identity


untilEndOfVariable : String -> Range -> Range
untilEndOfVariable name range =
    if range.start.row == range.end.row then
        range

    else
        { range | end = { row = range.start.row, column = range.start.column + String.length name } }


collectNamesFromTypeAnnotation : ModuleNameLookupTable -> Node TypeAnnotation -> { types : List String, modules : List ( ModuleName, ModuleName ) }
collectNamesFromTypeAnnotation lookupTable node =
    { types = collectTypesFromTypeAnnotation node
    , modules = collectModuleNamesFromTypeAnnotation lookupTable node
    }


collectTypesFromTypeAnnotation : Node TypeAnnotation -> List String
collectTypesFromTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectTypesFromTypeAnnotation a ++ collectTypesFromTypeAnnotation b

        TypeAnnotation.Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            [ str ]

                        _ ->
                            []
            in
            name ++ List.concatMap collectTypesFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        TypeAnnotation.GenericRecord _ list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        TypeAnnotation.Tupled list ->
            List.concatMap collectTypesFromTypeAnnotation list

        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Unit ->
            []


collectModuleNamesFromTypeAnnotation : ModuleNameLookupTable -> Node TypeAnnotation -> List ( ModuleName, ModuleName )
collectModuleNamesFromTypeAnnotation lookupTable node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectModuleNamesFromTypeAnnotation lookupTable a ++ collectModuleNamesFromTypeAnnotation lookupTable b

        TypeAnnotation.Typed nameNode params ->
            case ( Tuple.first (Node.value nameNode), ModuleNameLookupTable.moduleNameFor lookupTable nameNode ) of
                ( usedModuleNameFirst :: usedModuleNameRest, Just realModuleName ) ->
                    ( realModuleName, usedModuleNameFirst :: usedModuleNameRest ) :: List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable) params

                _ ->
                    List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable) params

        TypeAnnotation.Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable)

        TypeAnnotation.GenericRecord _ list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable)

        TypeAnnotation.Tupled list ->
            List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable) list

        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Unit ->
            []


register : VariableInfo -> String -> Context -> Context
register variableInfo name context =
    case variableInfo.variableType of
        TopLevelVariable ->
            -- The main function is "exposed" by default
            -- TODO Don't do this for libraries
            if name == "main" then
                context

            else
                registerVariable variableInfo name context

        LetVariable ->
            registerVariable variableInfo name context

        ImportedModule ->
            -- Does not call this function for this case
            context

        ModuleAlias _ ->
            -- Does not call this function for this case
            context

        ImportedItem _ ->
            registerVariable variableInfo name context

        Type ->
            registerVariable variableInfo name context

        Port ->
            registerVariable variableInfo name context

        Operator ->
            registerVariable variableInfo name context


registerModule : DeclaredModule -> Context -> Context
registerModule declaredModule context =
    { context | declaredModules = declaredModule :: context.declaredModules }


registerVariable : VariableInfo -> String -> Context -> Context
registerVariable variableInfo name context =
    let
        scopes : Nonempty Scope
        scopes =
            NonemptyList.mapHead
                (\scope ->
                    { scope | declared = Dict.insert name variableInfo scope.declared }
                )
                context.scopes
    in
    { context | scopes = scopes }


markAllAsUsed : List String -> Context -> Context
markAllAsUsed names context =
    List.foldl markAsUsed context names


markAsUsed : String -> Context -> Context
markAsUsed name context =
    if context.inTheDeclarationOf == Just name then
        context

    else
        let
            scopes : Nonempty Scope
            scopes =
                NonemptyList.mapHead
                    (\scope ->
                        { scope | used = Set.insert name scope.used }
                    )
                    context.scopes
        in
        { context | scopes = scopes }


markAllModulesAsUsed : List ( ModuleName, ModuleName ) -> Context -> Context
markAllModulesAsUsed names context =
    { context | usedModules = Set.union (Set.fromList names) context.usedModules }


markModuleAsUsed : ( ModuleName, ModuleName ) -> Context -> Context
markModuleAsUsed name context =
    { context | usedModules = Set.insert name context.usedModules }


getModuleName : List String -> String
getModuleName name =
    String.join "." name


makeReport : Scope -> ( List (Error {}), List String )
makeReport { declared, used } =
    let
        nonUsedVars : List String
        nonUsedVars =
            Set.diff used (Set.fromList <| Dict.keys declared)
                |> Set.toList

        errors : List (Error {})
        errors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, variableInfo ) -> error (always False) variableInfo key)
    in
    ( errors, nonUsedVars )



-- RANGE MANIPULATION


{-| Include everything until the line after the end.
-}
untilStartOfNextLine : Range -> Range
untilStartOfNextLine range =
    if range.end.column == 1 then
        range

    else
        { range | end = { row = range.end.row + 1, column = 1 } }


{-| Create a new range that starts at the start of the range that starts first,
and ends at the end of the range that starts last. If the two ranges are distinct
and there is code in between, that code will be included in the resulting range.

    range : Range
    range =
        Fix.mergeRanges
            (Node.range node1)
            (Node.range node2)

-}
mergeRanges : Range -> Range -> Range
mergeRanges a b =
    let
        start : { row : Int, column : Int }
        start =
            case comparePosition a.start b.start of
                LT ->
                    a.start

                EQ ->
                    a.start

                GT ->
                    b.start

        end : { row : Int, column : Int }
        end =
            case comparePosition a.end b.end of
                LT ->
                    b.end

                EQ ->
                    b.end

                GT ->
                    a.end
    in
    { start = start, end = end }


{-| Make a range stop at a position. If the position is not inside the range,
then the range won't change.

    range : Range
    range =
        rangeUpUntil
            (Node.range node)
            (node |> Node.value |> .typeAnnotation |> Node.range |> .start)

-}
rangeUpUntil : Range -> { row : Int, column : Int } -> Range
rangeUpUntil range position =
    let
        positionAsInt_ : Int
        positionAsInt_ =
            positionAsInt position
    in
    if positionAsInt range.start <= positionAsInt_ && positionAsInt range.end >= positionAsInt_ then
        { range | end = position }

    else
        range


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long. Then, as long as ranges don't overlap,
    -- this should work fine.
    row * 1000000 + column


comparePosition : { row : Int, column : Int } -> { row : Int, column : Int } -> Order
comparePosition a b =
    let
        order : Order
        order =
            compare a.row b.row
    in
    case order of
        EQ ->
            compare a.column b.column

        _ ->
            order
