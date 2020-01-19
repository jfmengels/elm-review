module Review.Rule.NoUnusedVariables exposing (rule)

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
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import NonemptyList as Nonempty exposing (Nonempty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Direction, Error, Rule)
import Set exposing (Set)


{-| Forbid variables or types that are declared or imported but never used.

    config =
        [ NoUnusedVariables.rule
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

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoUnusedVariables" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withFinalEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopes : Nonempty Scope
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
    | ModuleAlias
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


error : VariableInfo -> String -> Error
error { variableType, under, rangeToRemove } name =
    Rule.error
        { message = variableTypeToString variableType ++ " `" ++ name ++ "` is not used" ++ variableTypeWarning variableType
        , details = [ "Since it is not being used, I recommend removing it. It should make the code clearer to read for other people." ]
        }
        under
        |> Rule.withFixes [ Fix.removeRange rangeToRemove ]


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

        ModuleAlias ->
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

        ModuleAlias ->
            ""

        Type ->
            ""

        Port ->
            " (Warning: Removing this port may break your application if it is used in the JS code)"


moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
moduleDefinitionVisitor (Node _ moduleNode) context =
    case Module.exposingList moduleNode of
        All _ ->
            ( [], { context | exposesEverything = True } )

        Explicit list ->
            let
                names =
                    List.filterMap
                        (\(Node _ node) ->
                            case node of
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
importVisitor ((Node range { exposingList, moduleAlias, moduleName }) as importNode) context =
    case exposingList of
        Nothing ->
            let
                ( variableType, Node nameNodeRange nameNodeValue, rangeToRemove ) =
                    case moduleAlias of
                        Just moduleAlias_ ->
                            ( ModuleAlias, moduleAlias_, moduleAliasRange importNode (Node.range moduleAlias_) )

                        Nothing ->
                            ( ImportedModule, moduleName, range )
            in
            ( []
            , register
                { variableType = variableType, under = nameNodeRange, rangeToRemove = rangeToRemove }
                (getModuleName nameNodeValue)
                context
            )

        Just declaredImports ->
            let
                contextWithoutImports : Context
                contextWithoutImports =
                    case moduleAlias of
                        Just (Node moduleAliasRange_ value) ->
                            register
                                { variableType = ModuleAlias
                                , under = moduleAliasRange_
                                , rangeToRemove = moduleAliasRange importNode moduleAliasRange_
                                }
                                (getModuleName value)
                                context

                        Nothing ->
                            context
            in
            ( []
            , List.foldl
                (\( name, variableInfo ) context_ -> register variableInfo name context_)
                contextWithoutImports
                (collectFromExposing declaredImports)
            )


moduleAliasRange : Node Import -> Range -> Range
moduleAliasRange (Node _ { moduleName }) range =
    { range | start = (Node.range moduleName).end }


expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
expressionVisitor (Node range value) direction context =
    case ( direction, value ) of
        ( Rule.OnEnter, FunctionOrValue [] name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, FunctionOrValue moduleName name ) ->
            ( [], markModuleAsUsed (getModuleName moduleName) context )

        ( Rule.OnEnter, OperatorApplication name _ _ _ ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, PrefixOperator name ) ->
            ( [], markAsUsed name context )

        ( Rule.OnEnter, LetExpression { declarations, expression } ) ->
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
                                LetFunction function ->
                                    registerFunction letBlockContext function context_

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
                        |> register
                            { variableType = TopLevelVariable
                            , under = Node.range functionImplementation.name
                            , rangeToRemove = Node.range node
                            }
                            (Node.value functionImplementation.name)
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
                |> register
                    { variableType = Type
                    , under = Node.range name
                    , rangeToRemove = Node.range node
                    }
                    (Node.value name)
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
                |> register
                    { variableType = Type
                    , under = Node.range name
                    , rangeToRemove = Node.range node
                    }
                    (Node.value name)
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
                |> register
                    { variableType = Port
                    , under = Node.range name
                    , rangeToRemove = Node.range node
                    }
                    (Node.value name)
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
                    |> List.map (\( key, variableInfo ) -> error variableInfo key)
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
        All _ ->
            []

        Explicit list ->
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
                            FunctionExpose name ->
                                Just ( name, { variableType = ImportedItem ImportedVariable, under = range, rangeToRemove = rangeToRemove } )

                            InfixExpose name ->
                                Just ( name, { variableType = ImportedItem ImportedOperator, under = range, rangeToRemove = rangeToRemove } )

                            TypeOrAliasExpose name ->
                                Just ( name, { variableType = ImportedItem ImportedType, under = range, rangeToRemove = rangeToRemove } )

                            TypeExpose { name, open } ->
                                case open of
                                    Just openRange ->
                                        Nothing

                                    Nothing ->
                                        Just ( name, { variableType = ImportedItem ImportedType, under = range, rangeToRemove = rangeToRemove } )
                    )
                |> List.filterMap identity


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


register : VariableInfo -> String -> Context -> Context
register variableInfo name context =
    case variableInfo.variableType of
        TopLevelVariable ->
            registerVariable variableInfo name context

        LetVariable ->
            registerVariable variableInfo name context

        ImportedModule ->
            registerModule variableInfo name context

        ImportedItem _ ->
            registerVariable variableInfo name context

        ModuleAlias ->
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
            Nonempty.mapHead
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
    let
        scopes : Nonempty Scope
        scopes =
            Nonempty.mapHead
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
                |> List.map (\( key, variableInfo ) -> error variableInfo key)
    in
    ( errors, nonUsedVars )



-- RANGE MANIPULATION


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
        Fix.rangeUpUntil
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
