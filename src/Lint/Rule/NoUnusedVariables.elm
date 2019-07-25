module Lint.Rule.NoUnusedVariables exposing (rule)

{-| Forbid variables or types that are declared or imported but never used.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Lint.Rule as Rule exposing (Direction, Error, Rule)
import List.Nonempty as Nonempty exposing (Nonempty)
import Set exposing (Set)


{-| Forbid variables or types that are declared or imported but never used.

    config =
        [ NoUnusedVariables.rule
        ]


## Fail

    -- module A exposing (a)
    a n =
        n + 1

    b =
        a 2


## Success

    -- module A exposing (a)
    a n =
        n + 1

-}
rule : Rule
rule =
    Rule.newSchema "NoUnusedVariables"
        |> Rule.withInitialContext initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withFinalEvaluation finalEvaluation
        |> Rule.fromSchema


type alias Context =
    { scopes : Nonempty Scope
    , exposesEverything : Bool
    , constructorNameToTypeName : Dict String String
    , declaredModules : Dict String ( VariableType, Range )
    , usedModules : Set String
    }


type alias Scope =
    { declared : Dict String ( VariableType, Range )
    , used : Set String
    }


type VariableType
    = Variable
    | ImportedModule
    | ImportedVariable
    | ImportedType
    | ImportedOperator
    | ModuleAlias
    | Type
    | Port


initialContext : Context
initialContext =
    { scopes = Nonempty.fromElement emptyScope
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


error : VariableType -> Range -> String -> Error
error variableType range_ name =
    Rule.error
        (variableTypeToString variableType ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableType)
        range_


variableTypeToString : VariableType -> String
variableTypeToString value =
    case value of
        Variable ->
            "Variable"

        ImportedModule ->
            "Imported module"

        ImportedVariable ->
            "Imported variable"

        ImportedType ->
            "Imported type"

        ImportedOperator ->
            "Imported operator"

        ModuleAlias ->
            "Module alias"

        Type ->
            "Type"

        Port ->
            "Port"


variableTypeWarning : VariableType -> String
variableTypeWarning value =
    case value of
        Variable ->
            ""

        ImportedModule ->
            ""

        ImportedVariable ->
            ""

        ImportedType ->
            ""

        ImportedOperator ->
            ""

        ModuleAlias ->
            ""

        Type ->
            ""

        Port ->
            " (Warning: Removing this port may break your application if it is used in the JS code)"


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        All _ ->
            ( [], { context | exposesEverything = True } )

        Explicit list ->
            let
                names =
                    List.filterMap
                        (\node ->
                            case Node.value node of
                                FunctionExpose name ->
                                    Just name

                                TypeOrAliasExpose name ->
                                    Just name

                                TypeExpose { name } ->
                                    Just name

                                InfixExpose name ->
                                    -- Just name
                                    Nothing
                        )
                        list
            in
            ( [], markAllAsUsed names context )


importVisitor : Node Import -> Context -> ( List Error, Context )
importVisitor node context =
    let
        exposed : Maybe Exposing
        exposed =
            node
                |> Node.value
                |> .exposingList
                |> Maybe.map Node.value
    in
    case exposed of
        Nothing ->
            let
                ( variableType, moduleName ) =
                    case Node.value node |> .moduleAlias of
                        Just moduleAlias ->
                            ( ModuleAlias, moduleAlias )

                        Nothing ->
                            ( ImportedModule, Node.value node |> .moduleName )
            in
            ( []
            , register
                variableType
                (Node.range moduleName)
                (Node.value moduleName |> getModuleName)
                context
            )

        Just declaredImports ->
            let
                contextWithoutImports : Context
                contextWithoutImports =
                    case Node.value node |> .moduleAlias of
                        Just moduleAlias ->
                            register
                                ModuleAlias
                                (Node.range moduleAlias)
                                (Node.value moduleAlias |> getModuleName)
                                context

                        Nothing ->
                            context
            in
            ( []
            , List.foldl
                (\( variableType, range, name ) context_ -> register variableType range name context_)
                contextWithoutImports
                (collectFromExposing declaredImports)
            )


expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, FunctionOrValue [] name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, FunctionOrValue moduleName name ) ->
            ( [], markModuleAsUsed (getModuleName moduleName) context )

        ( Rule.OnEnter, OperatorApplication name _ _ _ ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, PrefixOperator name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, LetExpression { declarations } ) ->
            let
                newContext : Context
                newContext =
                    List.foldl
                        (\declaration context_ ->
                            case Node.value declaration of
                                LetFunction function ->
                                    registerFunction function context_

                                LetDestructuring pattern _ ->
                                    context_
                        )
                        { context | scopes = Nonempty.cons emptyScope context.scopes }
                        declarations
            in
            ( [], newContext )

        ( Rule.OnExit, RecordUpdateExpression expr _ ) ->
            ( [], markAsUsed (Node.value expr) context )

        ( Rule.OnExit, CaseExpression { cases } ) ->
            let
                usedVariables : { types : List String, modules : List String }
                usedVariables =
                    cases
                        |> List.map
                            (\( patternNode, expressionNode ) ->
                                getUsedVariablesFromPattern patternNode
                            )
                        |> foldUsedTypesAndModules
            in
            ( []
            , markUsedTypesAndModules usedVariables context
            )

        ( Rule.OnExit, LetExpression _ ) ->
            let
                ( errors, remainingUsed ) =
                    makeReport (Nonempty.head context.scopes)

                contextWithPoppedScope =
                    { context | scopes = Nonempty.pop context.scopes }
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
            let
                usedVariable : List String
                usedVariable =
                    case qualifiedNameRef.moduleName of
                        [] ->
                            [ qualifiedNameRef.name ]

                        moduleName ->
                            []
            in
            usedVariable ++ List.concatMap getUsedTypesFromPattern patterns

        Pattern.AsPattern pattern alias_ ->
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
            let
                usedVariable : List String
                usedVariable =
                    case qualifiedNameRef.moduleName of
                        [] ->
                            []

                        moduleName ->
                            [ getModuleName moduleName ]
            in
            usedVariable ++ List.concatMap getUsedModulesFromPattern patterns

        Pattern.AsPattern pattern alias_ ->
            getUsedModulesFromPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedModulesFromPattern pattern


declarationVisitor : Node Declaration -> Direction -> Context -> ( List Error, Context )
declarationVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, FunctionDeclaration function ) ->
            let
                functionImplementation : FunctionImplementation
                functionImplementation =
                    Node.value function.declaration

                namesUsedInSignature : { types : List String, modules : List String }
                namesUsedInSignature =
                    function.signature
                        |> Maybe.map (Node.value >> .typeAnnotation >> collectNamesFromTypeAnnotation)
                        |> Maybe.withDefault { types = [], modules = [] }

                newContext : Context
                newContext =
                    context
                        |> register Variable (Node.range functionImplementation.name) (Node.value functionImplementation.name)
                        |> markUsedTypesAndModules namesUsedInSignature
            in
            ( [], newContext )

        ( Rule.OnEnter, CustomTypeDeclaration { name, constructors } ) ->
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
                |> register Type (Node.range name) (Node.value name)
                |> markUsedTypesAndModules variablesFromConstructorArguments
            )

        ( Rule.OnEnter, AliasDeclaration { name, typeAnnotation } ) ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List String }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation typeAnnotation
            in
            ( []
            , context
                |> register Type (Node.range name) (Node.value name)
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
            )

        ( Rule.OnEnter, PortDeclaration { name, typeAnnotation } ) ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List String }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation typeAnnotation
            in
            ( []
            , context
                |> markUsedTypesAndModules namesUsedInTypeAnnotation
                |> register Port (Node.range name) (Node.value name)
            )

        ( Rule.OnEnter, InfixDeclaration _ ) ->
            ( [], context )

        ( Rule.OnEnter, Destructuring _ _ ) ->
            ( [], context )

        ( Rule.OnExit, _ ) ->
            ( [], context )


foldUsedTypesAndModules : List { types : List String, modules : List String } -> { types : List String, modules : List String }
foldUsedTypesAndModules =
    List.foldl (\a b -> { types = a.types ++ b.types, modules = a.modules ++ b.modules }) { types = [], modules = [] }


markUsedTypesAndModules : { types : List String, modules : List String } -> Context -> Context
markUsedTypesAndModules { types, modules } context =
    context
        |> markAllAsUsed types
        |> markAllModulesAsUsed modules


finalEvaluation : Context -> List Error
finalEvaluation context =
    if context.exposesEverything then
        []

    else
        let
            rootScope : Scope
            rootScope =
                Nonempty.head context.scopes

            namesOfCustomTypesUsedByCallingAConstructor : Set String
            namesOfCustomTypesUsedByCallingAConstructor =
                context.constructorNameToTypeName
                    |> Dict.filter (\usedName _ -> Set.member usedName rootScope.used)
                    |> Dict.values
                    |> Set.fromList

            newRootScope : Scope
            newRootScope =
                { rootScope | used = Set.union namesOfCustomTypesUsedByCallingAConstructor rootScope.used }

            moduleErrors : List Error
            moduleErrors =
                context.declaredModules
                    |> Dict.filter (\key _ -> not <| Set.member key context.usedModules)
                    |> Dict.toList
                    |> List.map (\( key, ( variableType, range ) ) -> error variableType range key)
        in
        List.concat
            [ newRootScope
                |> makeReport
                |> Tuple.first
            , moduleErrors
            ]


registerFunction : Function -> Context -> Context
registerFunction function context =
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
    in
    context
        |> register Variable (Node.range declaration.name) (Node.value declaration.name)
        |> markUsedTypesAndModules namesUsedInSignature


collectFromExposing : Exposing -> List ( VariableType, Range, String )
collectFromExposing exposing_ =
    case exposing_ of
        All _ ->
            []

        Explicit list ->
            List.filterMap
                (\node ->
                    case Node.value node of
                        FunctionExpose name ->
                            Just ( ImportedVariable, Node.range node, name )

                        InfixExpose name ->
                            Just ( ImportedOperator, Node.range node, name )

                        TypeOrAliasExpose name ->
                            Just ( ImportedType, Node.range node, name )

                        TypeExpose { name, open } ->
                            case open of
                                Just openRange ->
                                    Nothing

                                Nothing ->
                                    Just ( ImportedType, Node.range node, name )
                )
                list


collectNamesFromTypeAnnotation : Node TypeAnnotation -> { types : List String, modules : List String }
collectNamesFromTypeAnnotation node =
    { types = collectTypesFromTypeAnnotation node
    , modules = collectModuleNamesFromTypeAnnotation node
    }


collectTypesFromTypeAnnotation : Node TypeAnnotation -> List String
collectTypesFromTypeAnnotation node =
    case Node.value node of
        FunctionTypeAnnotation a b ->
            collectTypesFromTypeAnnotation a ++ collectTypesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            [ str ]

                        ( moduleName, _ ) ->
                            []
            in
            name ++ List.concatMap collectTypesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectTypesFromTypeAnnotation

        Tupled list ->
            List.concatMap collectTypesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []


collectModuleNamesFromTypeAnnotation : Node TypeAnnotation -> List String
collectModuleNamesFromTypeAnnotation node =
    case Node.value node of
        FunctionTypeAnnotation a b ->
            collectModuleNamesFromTypeAnnotation a ++ collectModuleNamesFromTypeAnnotation b

        Typed nameNode params ->
            let
                name : List String
                name =
                    case Node.value nameNode of
                        ( [], str ) ->
                            []

                        ( moduleName, _ ) ->
                            [ getModuleName moduleName ]
            in
            name ++ List.concatMap collectModuleNamesFromTypeAnnotation params

        Record list ->
            list
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        GenericRecord name list ->
            list
                |> Node.value
                |> List.map (Node.value >> Tuple.second)
                |> List.concatMap collectModuleNamesFromTypeAnnotation

        Tupled list ->
            List.concatMap collectModuleNamesFromTypeAnnotation list

        GenericType _ ->
            []

        Unit ->
            []


register : VariableType -> Range -> String -> Context -> Context
register variableType range name context =
    if variableType == ImportedModule || variableType == ModuleAlias then
        { context | declaredModules = Dict.insert name ( variableType, range ) context.declaredModules }

    else
        let
            scopes : Nonempty Scope
            scopes =
                mapNonemptyHead
                    (\scope ->
                        { scope | declared = Dict.insert name ( variableType, range ) scope.declared }
                    )
                    context.scopes
        in
        { context | scopes = scopes }


markAllAsUsed : List String -> Context -> Context
markAllAsUsed names context =
    List.foldl markAsUsed context names


markAsUsed : String -> Context -> Context
markAsUsed name context =
    let
        scopes : Nonempty Scope
        scopes =
            mapNonemptyHead
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


makeReport : Scope -> ( List Error, List String )
makeReport { declared, used } =
    let
        nonUsedVars : List String
        nonUsedVars =
            Set.diff used (Set.fromList <| Dict.keys declared)
                |> Set.toList

        errors : List Error
        errors =
            Dict.filter (\key _ -> not <| Set.member key used) declared
                |> Dict.toList
                |> List.map (\( key, ( variableType, range ) ) -> error variableType range key)
    in
    ( errors, nonUsedVars )


mapNonemptyHead : (a -> a) -> Nonempty a -> Nonempty a
mapNonemptyHead fn nonempty =
    let
        newHead : a
        newHead =
            fn (Nonempty.head nonempty)
    in
    Nonempty.replaceHead newHead nonempty
