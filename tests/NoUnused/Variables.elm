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
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import Review.Fix as Fix exposing (Fix)
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
    Rule.newModuleRuleSchema "NoUnused.Variables" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : Nonempty Scope
    , inTheDeclarationOf : Maybe String
    , exposesEverything : Bool
    , constructorNameToTypeName : Dict String String
    , declaredModules : Dict String VariableInfo
    , usedModules : Set String
    }


type alias Scope =
    { declared : Dict String VariableInfo
    , used : Set String
    }


type alias VariableInfo =
    { variableType : VariableType
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


type LetBlockContext
    = HasMultipleDeclarations
    | HasNoOtherDeclarations Range


type ImportType
    = ImportedVariable
    | ImportedType
    | ImportedOperator


initialContext : Context
initialContext =
    { scopes = NonemptyList.fromElement emptyScope
    , inTheDeclarationOf = Nothing
    , exposesEverything = False
    , constructorNameToTypeName = Dict.empty
    , declaredModules = Dict.empty
    , usedModules = Set.empty
    }


emptyScope : Scope
emptyScope =
    { declared = Dict.empty
    , used = Set.empty
    }


error : Dict String VariableInfo -> VariableInfo -> String -> Error {}
error declaredModules variableInfo name =
    Rule.errorWithFix
        { message = variableTypeToString variableInfo.variableType ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableInfo.variableType
        , details = [ "You should either use this value somewhere, or remove it at the location I pointed at." ]
        }
        variableInfo.under
        (fix declaredModules variableInfo)


variableTypeToString : VariableType -> String
variableTypeToString variableType =
    case variableType of
        TopLevelVariable ->
            "Top-level variable"

        LetVariable ->
            "`let in` variable"

        ImportedModule ->
            "Imported module"

        ImportedItem ImportedVariable ->
            "Imported variable"

        ImportedItem ImportedType ->
            "Imported type"

        ImportedItem ImportedOperator ->
            "Imported operator"

        ModuleAlias _ ->
            "Module alias"

        Type ->
            "Type"

        Port ->
            "Port"


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


fix : Dict String VariableInfo -> VariableInfo -> List Fix
fix declaredModules { variableType, rangeToRemove } =
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
                        || not (Dict.member originalNameOfTheImport declaredModules)

                Type ->
                    True

                Port ->
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
                    List.filterMap
                        (\(Node _ node) ->
                            case node of
                                Exposing.FunctionExpose name ->
                                    Just name

                                Exposing.TypeOrAliasExpose name ->
                                    Just name

                                Exposing.TypeExpose { name } ->
                                    Just name

                                Exposing.InfixExpose name ->
                                    -- Just name
                                    Nothing
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
                (registerModuleAlias node context)
                (collectFromExposing declaredImports)
            )


registerModuleNameOrAlias : Node Import -> Context -> Context
registerModuleNameOrAlias ((Node range { moduleAlias, moduleName }) as node) context =
    case moduleAlias of
        Just _ ->
            registerModuleAlias node context

        Nothing ->
            register
                { variableType = ImportedModule
                , under = Node.range moduleName
                , rangeToRemove = untilStartOfNextLine range
                }
                (getModuleName <| Node.value moduleName)
                context


registerModuleAlias : Node Import -> Context -> Context
registerModuleAlias ((Node range { exposingList, moduleAlias, moduleName }) as node) context =
    case moduleAlias of
        Just moduleAlias_ ->
            register
                { variableType =
                    ModuleAlias
                        { originalNameOfTheImport = getModuleName <| Node.value moduleName
                        , exposesSomething = exposingList /= Nothing
                        }
                , under = Node.range moduleAlias_
                , rangeToRemove =
                    case exposingList of
                        Nothing ->
                            untilStartOfNextLine range

                        Just _ ->
                            moduleAliasRange node (Node.range moduleAlias_)
                }
                (getModuleName <| Node.value moduleAlias_)
                context

        Nothing ->
            context


moduleAliasRange : Node Import -> Range -> Range
moduleAliasRange (Node _ { moduleName }) range =
    { range | start = (Node.range moduleName).end }


expressionEnterVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionEnterVisitor (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            ( [], markAsUsed name context )

        Expression.FunctionOrValue moduleName name ->
            ( [], markModuleAsUsed (getModuleName moduleName) context )

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

                newContext : Context
                newContext =
                    List.foldl
                        (\declaration context_ ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    let
                                        namesUsedInArgumentPatterns : { types : List String, modules : List String }
                                        namesUsedInArgumentPatterns =
                                            function.declaration
                                                |> Node.value
                                                |> .arguments
                                                |> List.map getUsedVariablesFromPattern
                                                |> foldUsedTypesAndModules
                                    in
                                    context_
                                        |> registerFunction letBlockContext function
                                        |> markUsedTypesAndModules namesUsedInArgumentPatterns

                                Expression.LetDestructuring _ _ ->
                                    context_
                        )
                        { context | scopes = NonemptyList.cons emptyScope context.scopes }
                        declarations
            in
            ( [], newContext )

        Expression.LambdaExpression { args } ->
            let
                namesUsedInArgumentPatterns : { types : List String, modules : List String }
                namesUsedInArgumentPatterns =
                    args
                        |> List.map getUsedVariablesFromPattern
                        |> foldUsedTypesAndModules
            in
            ( [], markUsedTypesAndModules namesUsedInArgumentPatterns context )

        _ ->
            ( [], context )


expressionExitVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionExitVisitor node context =
    case Node.value node of
        Expression.RecordUpdateExpression expr _ ->
            ( [], markAsUsed (Node.value expr) context )

        Expression.CaseExpression { cases } ->
            let
                usedVariables : { types : List String, modules : List String }
                usedVariables =
                    cases
                        |> List.map
                            (\( patternNode, _ ) ->
                                getUsedVariablesFromPattern patternNode
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


getUsedVariablesFromPattern : Node Pattern -> { types : List String, modules : List String }
getUsedVariablesFromPattern patternNode =
    { types = getUsedTypesFromPattern patternNode
    , modules = getUsedModulesFromPattern patternNode
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


getUsedModulesFromPattern : Node Pattern -> List String
getUsedModulesFromPattern patternNode =
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
            List.concatMap getUsedModulesFromPattern patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap getUsedModulesFromPattern [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap getUsedModulesFromPattern patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            case qualifiedNameRef.moduleName of
                [] ->
                    List.concatMap getUsedModulesFromPattern patterns

                moduleName ->
                    getModuleName moduleName :: List.concatMap getUsedModulesFromPattern patterns

        Pattern.AsPattern pattern _ ->
            getUsedModulesFromPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedModulesFromPattern pattern


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionImplementation : FunctionImplementation
                functionImplementation =
                    Node.value function.declaration

                namesUsedInSignature : { types : List String, modules : List String }
                namesUsedInSignature =
                    function.signature
                        |> Maybe.map (Node.value >> .typeAnnotation >> collectNamesFromTypeAnnotation)
                        |> Maybe.withDefault { types = [], modules = [] }

                namesUsedInArgumentPatterns : { types : List String, modules : List String }
                namesUsedInArgumentPatterns =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.map getUsedVariablesFromPattern
                        |> foldUsedTypesAndModules

                newContext : Context
                newContext =
                    { context | inTheDeclarationOf = Just <| Node.value functionImplementation.name }
                        |> register
                            { variableType = TopLevelVariable
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
                variablesFromConstructorArguments : { types : List String, modules : List String }
                variablesFromConstructorArguments =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.map collectNamesFromTypeAnnotation
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
                    , under = Node.range name
                    , rangeToRemove = rangeToRemoveForNodeWithDocumentation node documentation
                    }
                    (Node.value name)
                |> markUsedTypesAndModules variablesFromConstructorArguments
            )

        Declaration.AliasDeclaration { name, typeAnnotation, documentation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List String }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation typeAnnotation
            in
            ( []
            , context
                |> register
                    { variableType = Type
                    , under = Node.range name
                    , rangeToRemove = rangeToRemoveForNodeWithDocumentation node documentation
                    }
                    (Node.value name)
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
            )

        Declaration.PortDeclaration { name, typeAnnotation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List String }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation typeAnnotation
            in
            ( []
            , context
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
                |> register
                    { variableType = Port
                    , under = Node.range name
                    , rangeToRemove = Node.range node
                    }
                    (Node.value name)
            )

        Declaration.InfixDeclaration _ ->
            ( [], context )

        Declaration.Destructuring _ _ ->
            ( [], context )


foldUsedTypesAndModules : List { types : List String, modules : List String } -> { types : List String, modules : List String }
foldUsedTypesAndModules =
    List.foldl (\a b -> { types = a.types ++ b.types, modules = a.modules ++ b.modules }) { types = [], modules = [] }


markUsedTypesAndModules : { types : List String, modules : List String } -> Context -> Context
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

            moduleErrors : List (Error {})
            moduleErrors =
                context.declaredModules
                    |> Dict.filter (\key _ -> not <| Set.member key context.usedModules)
                    |> Dict.toList
                    |> List.map (\( key, variableInfo ) -> error context.declaredModules variableInfo key)
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

        namesUsedInSignature : { types : List String, modules : List String }
        namesUsedInSignature =
            case Maybe.map Node.value function.signature of
                Just signature ->
                    collectNamesFromTypeAnnotation signature.typeAnnotation

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
                                      , under = untilEndOfVariable name range
                                      , rangeToRemove = rangeToRemove
                                      }
                                    )

                            Exposing.InfixExpose name ->
                                Just
                                    ( name
                                    , { variableType = ImportedItem ImportedOperator
                                      , under = untilEndOfVariable name range
                                      , rangeToRemove = rangeToRemove
                                      }
                                    )

                            Exposing.TypeOrAliasExpose name ->
                                -- TODO Detect whether it is a custom type or type alias
                                Just
                                    ( name
                                    , { variableType = ImportedItem ImportedType
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
                                        Just
                                            ( name
                                            , { variableType = ImportedItem ImportedType
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


collectNamesFromTypeAnnotation : Node TypeAnnotation -> { types : List String, modules : List String }
collectNamesFromTypeAnnotation node =
    { types = collectTypesFromTypeAnnotation node
    , modules = collectModuleNamesFromTypeAnnotation node
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

        TypeAnnotation.GenericRecord name list ->
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


collectModuleNamesFromTypeAnnotation : Node TypeAnnotation -> List String
collectModuleNamesFromTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectModuleNamesFromTypeAnnotation a ++ collectModuleNamesFromTypeAnnotation b

        TypeAnnotation.Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], _ ) ->
                            []

                        ( moduleName, _ ) ->
                            [ getModuleName moduleName ]
            in
            name ++ List.concatMap collectModuleNamesFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        TypeAnnotation.GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        TypeAnnotation.Tupled list ->
            List.concatMap collectModuleNamesFromTypeAnnotation list

        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Unit ->
            []


register : VariableInfo -> String -> Context -> Context
register variableInfo name context =
    case variableInfo.variableType of
        TopLevelVariable ->
            -- The main function is "exposed" by default
            if name == "main" then
                context

            else
                registerVariable variableInfo name context

        LetVariable ->
            registerVariable variableInfo name context

        ImportedModule ->
            registerModule variableInfo name context

        ImportedItem _ ->
            registerVariable variableInfo name context

        ModuleAlias _ ->
            registerModule variableInfo name context

        Type ->
            registerVariable variableInfo name context

        Port ->
            registerVariable variableInfo name context


registerModule : VariableInfo -> String -> Context -> Context
registerModule variableInfo name context =
    { context | declaredModules = Dict.insert name variableInfo context.declaredModules }


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


markAllModulesAsUsed : List String -> Context -> Context
markAllModulesAsUsed names context =
    { context | usedModules = Set.union (Set.fromList names) context.usedModules }


markModuleAsUsed : String -> Context -> Context
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
                |> List.map (\( key, variableInfo ) -> error Dict.empty variableInfo key)
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
