module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.


# Rule

@docs rule

-}

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.RangeDict as RangeDict exposing (RangeDict)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
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
    Rule.newProjectRuleSchema "NoUnused.Variables" initialContext
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { isApplication : Bool
    , customTypes : Dict ModuleName (Dict String (List String))
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , inTheDeclarationOf : List String
    , declarations : RangeDict String
    , exposesEverything : Bool
    , isApplication : Bool
    , constructorNameToTypeName : Dict String String
    , declaredModules : List DeclaredModule
    , exposingAllModules : List ModuleThatExposesEverything
    , usedModules : Set ( ModuleName, ModuleName )
    , unusedImportedCustomTypes : Dict String ImportedCustomType
    , importedCustomTypeLookup : Dict String String
    , localCustomTypes : Dict String CustomTypeData
    , customTypes : Dict ModuleName (Dict String (List String))
    }


type alias DeclaredModule =
    { moduleName : ModuleName
    , alias : Maybe String
    , typeName : String
    , variableType : DeclaredModuleType
    , under : Range
    , rangeToRemove : Range
    }


type alias CustomTypeData =
    { under : Range
    , rangeToRemove : Range
    , variants : List String
    }


type alias ModuleThatExposesEverything =
    { name : ModuleName
    , alias : Maybe String
    , moduleNameRange : Range
    , exposingRange : Range
    , importRange : Range
    , wasUsedImplicitly : Bool
    , wasUsedWithModuleName : Bool
    }


type DeclaredModuleType
    = ImportedModule
    | ModuleAlias { originalNameOfTheImport : String, exposesSomething : Bool }


type alias Scope =
    { declared : Dict String VariableInfo
    , used : Dict ModuleName (Set String)
    }


type alias VariableInfo =
    { typeName : String
    , under : Range
    , rangeToRemove : Maybe Range
    , warning : String
    }


type alias ImportedCustomType =
    { typeName : String
    , under : Range
    , rangeToRemove : Range
    , openRange : Range
    }


type LetBlockContext
    = HasMultipleDeclarations
    | HasNoOtherDeclarations Range


initialContext : ProjectContext
initialContext =
    { isApplication = True
    , customTypes = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable { isApplication, customTypes } ->
            { lookupTable = lookupTable
            , scopes = NonemptyList.fromElement emptyScope
            , inTheDeclarationOf = []
            , declarations = Dict.empty
            , exposesEverything = False
            , isApplication = isApplication
            , constructorNameToTypeName = Dict.empty
            , declaredModules = []
            , exposingAllModules = []
            , usedModules = Set.empty
            , unusedImportedCustomTypes = Dict.empty
            , importedCustomTypeLookup = Dict.empty
            , localCustomTypes = Dict.empty
            , customTypes = customTypes
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes =
                moduleContext.localCustomTypes
                    |> Dict.map (\_ customType -> customType.variants)
                    |> Dict.singleton (Rule.moduleNameFromMetadata metadata)

            -- Will be ignored in foldProjectContexts
            , isApplication = True
            }
        )
        |> Rule.withMetadata


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newProjectContext previousProjectContext =
    { isApplication = previousProjectContext.isApplication
    , customTypes = Dict.union newProjectContext.customTypes previousProjectContext.customTypes
    }


emptyScope : Scope
emptyScope =
    { declared = Dict.empty
    , used = Dict.empty
    }


error : { typeName : String, under : Range, rangeToRemove : Maybe Range, warning : String } -> String -> Error {}
error variableInfo name =
    Rule.errorWithFix
        { message = variableInfo.typeName ++ " `" ++ name ++ "` is not used" ++ variableInfo.warning
        , details = details
        }
        variableInfo.under
        (case variableInfo.rangeToRemove of
            Just rangeToRemove ->
                [ Fix.removeRange rangeToRemove ]

            Nothing ->
                []
        )


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case Maybe.map .project maybeElmJson of
        Just (Elm.Project.Application _) ->
            ( [], { projectContext | isApplication = True } )

        Just (Elm.Project.Package _) ->
            ( [], { projectContext | isApplication = False } )

        Nothing ->
            -- Sensible default, because now `main` won't be reported.
            ( [], { projectContext | isApplication = True } )



-- DEPENDENCIES VISITOR


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List (Error nothing), ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        customTypes : Dict ModuleName (Dict String (List String))
        customTypes =
            dependencies
                |> Dict.values
                |> List.concatMap Dependency.modules
                |> List.map
                    (\module_ ->
                        ( String.split "." module_.name
                        , module_.unions
                            |> List.map (\{ name, tags } -> ( name, List.map Tuple.first tags ))
                            |> Dict.fromList
                        )
                    )
                |> Dict.fromList
    in
    ( [], { projectContext | customTypes = customTypes } )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
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


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor ((Node importRange import_) as node) context =
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
            let
                customTypesFromModule : Dict String (List String)
                customTypesFromModule =
                    context.customTypes
                        |> Dict.get (Node.value import_.moduleName)
                        |> Maybe.withDefault Dict.empty

                contextWithAlias : ModuleContext
                contextWithAlias =
                    case import_.moduleAlias of
                        Just moduleAlias ->
                            registerModuleAlias node moduleAlias context

                        Nothing ->
                            context
            in
            ( errors
            , case Node.value declaredImports of
                Exposing.All _ ->
                    { contextWithAlias
                        | exposingAllModules =
                            { name = Node.value import_.moduleName
                            , alias = Maybe.map (Node.value >> String.join ".") import_.moduleAlias
                            , moduleNameRange = Node.range import_.moduleName
                            , exposingRange = Node.range declaredImports
                            , importRange = importRange
                            , wasUsedImplicitly = False
                            , wasUsedWithModuleName = False
                            }
                                :: context.exposingAllModules
                    }

                Exposing.Explicit list ->
                    List.foldl
                        (registerExposedElements customTypesFromModule)
                        contextWithAlias
                        (collectExplicitlyExposedElements (Node.range declaredImports) list)
            )


registerExposedElements : Dict String (List String) -> ExposedElement -> ModuleContext -> ModuleContext
registerExposedElements customTypesFromModule importedElement context_ =
    case importedElement of
        CustomType name variableInfo ->
            case Dict.get name customTypesFromModule of
                Just constructorNames ->
                    { context_
                        | unusedImportedCustomTypes = Dict.insert name variableInfo context_.unusedImportedCustomTypes
                        , importedCustomTypeLookup =
                            Dict.union
                                (constructorNames
                                    |> List.map (\constructorName -> ( constructorName, name ))
                                    |> Dict.fromList
                                )
                                context_.importedCustomTypeLookup
                    }

                Nothing ->
                    context_

        TypeOrValue name variableInfo ->
            registerVariable variableInfo name context_


collectExplicitlyExposedElements : Range -> List (Node Exposing.TopLevelExpose) -> List ExposedElement
collectExplicitlyExposedElements exposingNodeRange list =
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
                            exposingNodeRange

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
                        TypeOrValue
                            name
                            { typeName = "Imported variable"
                            , under = untilEndOfVariable name range
                            , rangeToRemove = Just rangeToRemove
                            , warning = ""
                            }
                            |> Just

                    Exposing.InfixExpose name ->
                        TypeOrValue
                            name
                            { typeName = "Imported operator"
                            , under = untilEndOfVariable name range
                            , rangeToRemove = Just rangeToRemove
                            , warning = ""
                            }
                            |> Just

                    Exposing.TypeOrAliasExpose name ->
                        TypeOrValue
                            name
                            { typeName = "Imported type"
                            , under = untilEndOfVariable name range
                            , rangeToRemove = Just rangeToRemove
                            , warning = ""
                            }
                            |> Just

                    Exposing.TypeExpose { name, open } ->
                        case open of
                            Just openRange ->
                                CustomType
                                    name
                                    { typeName = "Imported type"
                                    , under = range
                                    , rangeToRemove = rangeToRemove
                                    , openRange = openRange
                                    }
                                    |> Just

                            Nothing ->
                                -- Can't happen with `elm-syntax`. If open is Nothing, then this we'll have a
                                -- `Exposing.TypeOrAliasExpose`, not a `Exposing.TypeExpose`.
                                Nothing
            )
        |> List.filterMap identity


registerModuleNameOrAlias : Node Import -> ModuleContext -> ModuleContext
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


registerModuleAlias : Node Import -> Node ModuleName -> ModuleContext -> ModuleContext
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


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    let
        newContext : ModuleContext
        newContext =
            case RangeDict.get (Node.range node) context.declarations of
                Just functionName ->
                    { context | inTheDeclarationOf = functionName :: context.inTheDeclarationOf }

                Nothing ->
                    context
    in
    expressionEnterVisitorHelp node newContext


expressionEnterVisitorHelp : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitorHelp (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            case Dict.get name context.constructorNameToTypeName of
                Just typeName ->
                    ( [], markAsUsed typeName context )

                Nothing ->
                    case Dict.get name context.importedCustomTypeLookup of
                        Just customTypeName ->
                            ( [], { context | unusedImportedCustomTypes = Dict.remove customTypeName context.unusedImportedCustomTypes } )

                        Nothing ->
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                                Just realModuleName ->
                                    ( []
                                    , context
                                        |> markAsUsed name
                                        |> markModuleAsUsed ( realModuleName, [] )
                                    )

                                Nothing ->
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
                                        |> List.map (getUsedVariablesFromPattern context)
                                        |> foldUsedTypesAndModules

                                markAsInTheDeclarationOf : a -> { b | declarations : RangeDict a } -> { b | declarations : RangeDict a }
                                markAsInTheDeclarationOf name ctx =
                                    { ctx
                                        | declarations =
                                            RangeDict.insert
                                                (function.declaration |> Node.value |> .expression |> Node.range)
                                                name
                                                ctx.declarations
                                    }
                            in
                            ( errors
                            , foldContext
                                |> registerFunction letBlockContext function
                                |> markUsedTypesAndModules namesUsedInArgumentPatterns
                                |> markAsInTheDeclarationOf (function.declaration |> Node.value |> .name |> Node.value)
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
                        |> List.map (getUsedVariablesFromPattern context)
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


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    let
        newContext : ModuleContext
        newContext =
            if RangeDict.member (Node.range node) context.declarations then
                { context | inTheDeclarationOf = List.drop 1 context.inTheDeclarationOf }

            else
                context
    in
    expressionExitVisitorHelp node newContext


expressionExitVisitorHelp : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitorHelp node context =
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
                                getUsedVariablesFromPattern context patternNode
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

                contextWithPoppedScope : ModuleContext
                contextWithPoppedScope =
                    { context | scopes = NonemptyList.pop context.scopes }
            in
            ( errors
            , markAllAsUsed remainingUsed contextWithPoppedScope
            )

        _ ->
            ( [], context )


getUsedVariablesFromPattern : ModuleContext -> Node Pattern -> { types : List String, modules : List ( ModuleName, ModuleName ) }
getUsedVariablesFromPattern context patternNode =
    { types = getUsedTypesFromPattern context.constructorNameToTypeName patternNode
    , modules = getUsedModulesFromPattern context.lookupTable patternNode
    }


getUsedTypesFromPattern : Dict String String -> Node Pattern -> List String
getUsedTypesFromPattern constructorNameToTypeName patternNode =
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
            List.concatMap (getUsedTypesFromPattern constructorNameToTypeName) patterns

        Pattern.RecordPattern _ ->
            []

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.concatMap (getUsedTypesFromPattern constructorNameToTypeName) [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.concatMap (getUsedTypesFromPattern constructorNameToTypeName) patterns

        Pattern.VarPattern _ ->
            []

        Pattern.NamedPattern qualifiedNameRef patterns ->
            case qualifiedNameRef.moduleName of
                [] ->
                    (Dict.get qualifiedNameRef.name constructorNameToTypeName |> Maybe.withDefault qualifiedNameRef.name)
                        :: List.concatMap (getUsedTypesFromPattern constructorNameToTypeName) patterns

                _ ->
                    List.concatMap (getUsedTypesFromPattern constructorNameToTypeName) patterns

        Pattern.AsPattern pattern _ ->
            getUsedTypesFromPattern constructorNameToTypeName pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedTypesFromPattern constructorNameToTypeName pattern


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
            case ModuleNameLookupTable.moduleNameFor lookupTable patternNode of
                Just realModuleName ->
                    ( realModuleName, qualifiedNameRef.moduleName ) :: List.concatMap (getUsedModulesFromPattern lookupTable) patterns

                Nothing ->
                    List.concatMap (getUsedModulesFromPattern lookupTable) patterns

        Pattern.AsPattern pattern _ ->
            getUsedModulesFromPattern lookupTable pattern

        Pattern.ParenthesizedPattern pattern ->
            getUsedModulesFromPattern lookupTable pattern



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor nodes context =
    List.foldl
        (\node ( errors, ctx ) ->
            case Node.value node of
                Declaration.CustomTypeDeclaration { name, constructors, documentation } ->
                    let
                        typeName : String
                        typeName =
                            Node.value name

                        constructorsForType : Dict String String
                        constructorsForType =
                            constructors
                                |> List.map (Node.value >> .name >> Node.value)
                                |> List.map (\constructorName -> ( constructorName, typeName ))
                                |> Dict.fromList

                        customType : CustomTypeData
                        customType =
                            { under = Node.range name
                            , rangeToRemove = rangeToRemoveForNodeWithDocumentation node documentation
                            , variants = List.map (Node.value >> .name >> Node.value) constructors
                            }
                    in
                    ( errors
                    , { ctx
                        | localCustomTypes =
                            Dict.insert
                                (Node.value name)
                                customType
                                ctx.localCustomTypes
                        , constructorNameToTypeName = Dict.union constructorsForType ctx.constructorNameToTypeName
                      }
                    )

                Declaration.AliasDeclaration { name, documentation } ->
                    ( []
                    , registerVariable
                        { typeName = "Type"
                        , under = Node.range name
                        , rangeToRemove = Just (rangeToRemoveForNodeWithDocumentation node documentation)
                        , warning = ""
                        }
                        (Node.value name)
                        { context | importedCustomTypeLookup = Dict.remove (Node.value name) context.importedCustomTypeLookup }
                    )

                _ ->
                    ( errors, ctx )
        )
        ( [], context )
        nodes



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
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
                        |> List.map (getUsedVariablesFromPattern context)
                        |> foldUsedTypesAndModules

                newContextWhereFunctionIsRegistered : ModuleContext
                newContextWhereFunctionIsRegistered =
                    if
                        context.exposesEverything
                            -- The main function is "exposed" by default for applications
                            || (context.isApplication && Node.value functionImplementation.name == "main")
                    then
                        context

                    else
                        registerVariable
                            { typeName = "Top-level variable"
                            , under = Node.range functionImplementation.name
                            , rangeToRemove = Just (rangeToRemoveForNodeWithDocumentation node function.documentation)
                            , warning = ""
                            }
                            (Node.value functionImplementation.name)
                            context

                newContext : ModuleContext
                newContext =
                    { newContextWhereFunctionIsRegistered | inTheDeclarationOf = [ Node.value functionImplementation.name ], declarations = Dict.empty }
                        |> markUsedTypesAndModules namesUsedInSignature
                        |> markUsedTypesAndModules namesUsedInArgumentPatterns
            in
            ( [], newContext )

        Declaration.CustomTypeDeclaration { name, constructors } ->
            let
                { types, modules } =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.map (collectNamesFromTypeAnnotation context.lookupTable)
                        |> foldUsedTypesAndModules
            in
            ( []
            , markUsedTypesAndModules
                { types = List.filter ((/=) (Node.value name)) types
                , modules = modules
                }
                context
            )

        Declaration.AliasDeclaration { name, typeAnnotation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation context.lookupTable typeAnnotation
            in
            ( []
            , markUsedTypesAndModules namesUsedInTypeAnnotation context
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
                |> registerVariable
                    { typeName = "Port"
                    , under = Node.range name
                    , rangeToRemove = Nothing
                    , warning = " (Warning: Removing this port may break your application if it is used in the JS code)"
                    }
                    (Node.value name)
            )

        Declaration.InfixDeclaration { operator, function } ->
            ( []
            , context
                |> markAsUsed (Node.value function)
                |> registerVariable
                    { typeName = "Declared operator"
                    , under = Node.range operator
                    , rangeToRemove = Just (Node.range node)
                    , warning = ""
                    }
                    (Node.value operator)
            )

        Declaration.Destructuring _ _ ->
            ( [], context )


foldUsedTypesAndModules : List { types : List String, modules : List ( ModuleName, ModuleName ) } -> { types : List String, modules : List ( ModuleName, ModuleName ) }
foldUsedTypesAndModules =
    List.foldl (\a b -> { types = a.types ++ b.types, modules = a.modules ++ b.modules }) { types = [], modules = [] }


markUsedTypesAndModules : { types : List String, modules : List ( ModuleName, ModuleName ) } -> ModuleContext -> ModuleContext
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


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    let
        rootScope : Scope
        rootScope =
            NonemptyList.head context.scopes

        namesOfCustomTypesUsedByCallingAConstructor : Set String
        namesOfCustomTypesUsedByCallingAConstructor =
            context.constructorNameToTypeName
                |> Dict.filter (\usedName _ -> Set.member usedName (Dict.get [] rootScope.used |> Maybe.withDefault Set.empty))
                |> Dict.values
                |> Set.fromList

        newRootScope : Scope
        newRootScope =
            { rootScope
                | used =
                    Dict.update []
                        (\set ->
                            set
                                |> Maybe.withDefault Set.empty
                                |> Set.union namesOfCustomTypesUsedByCallingAConstructor
                                |> Just
                        )
                        rootScope.used
            }

        moduleNamesInUse : Set String
        moduleNamesInUse =
            context.declaredModules
                |> List.map (\{ alias, moduleName } -> Maybe.withDefault (getModuleName moduleName) alias)
                |> Set.fromList

        usedLocally : Set String
        usedLocally =
            Dict.get [] rootScope.used |> Maybe.withDefault Set.empty

        importedTypeErrors : List (Error {})
        importedTypeErrors =
            context.unusedImportedCustomTypes
                |> Dict.toList
                |> List.map
                    (\( name, { under, rangeToRemove, openRange } ) ->
                        if Set.member name usedLocally && not (Dict.member name context.localCustomTypes) then
                            Rule.errorWithFix
                                { message = "Imported constructors for `" ++ name ++ "` are not used"
                                , details = details
                                }
                                under
                                -- If the constructors are not used but the type itself is, then only remove the `(..)`
                                [ Fix.removeRange openRange ]

                        else
                            Rule.errorWithFix
                                { message = "Imported type `" ++ name ++ "` is not used"
                                , details = details
                                }
                                under
                                [ Fix.removeRange rangeToRemove ]
                    )

        moduleThatExposeEverythingErrors : List ( Maybe (Error {}), Maybe ( ModuleName, ModuleName ) )
        moduleThatExposeEverythingErrors =
            List.map
                (\({ importRange, exposingRange } as module_) ->
                    if not module_.wasUsedImplicitly then
                        if module_.wasUsedWithModuleName then
                            ( Just
                                (Rule.errorWithFix
                                    { message = "No imported elements from `" ++ String.join "." module_.name ++ "` are used"
                                    , details = details
                                    }
                                    exposingRange
                                    [ Fix.removeRange exposingRange ]
                                )
                            , Nothing
                            )

                        else
                            ( Just
                                (Rule.errorWithFix
                                    { message = "Imported module `" ++ String.join "." module_.name ++ "` is not used"
                                    , details = details
                                    }
                                    module_.moduleNameRange
                                    [ Fix.removeRange { importRange | end = { row = importRange.end.row + 1, column = 1 } } ]
                                )
                            , Maybe.map (\alias -> ( module_.name, [ alias ] )) module_.alias
                            )

                    else
                        ( Nothing, Nothing )
                )
                context.exposingAllModules

        usedModules : Set ( ModuleName, ModuleName )
        usedModules =
            Set.union
                (Set.fromList (List.filterMap Tuple.second moduleThatExposeEverythingErrors))
                context.usedModules

        moduleErrors : List (Error {})
        moduleErrors =
            context.declaredModules
                |> List.filter
                    (\variableInfo ->
                        not
                            (case variableInfo.alias of
                                Just alias ->
                                    Set.member ( variableInfo.moduleName, [ alias ] ) usedModules

                                Nothing ->
                                    Set.member ( variableInfo.moduleName, variableInfo.moduleName ) usedModules
                            )
                    )
                |> List.map
                    (\variableInfo ->
                        let
                            name : String
                            name =
                                case variableInfo.alias of
                                    Just alias ->
                                        alias

                                    Nothing ->
                                        getModuleName variableInfo.moduleName

                            fix : List Fix
                            fix =
                                case variableInfo.variableType of
                                    ImportedModule ->
                                        [ Fix.removeRange variableInfo.rangeToRemove ]

                                    ModuleAlias { originalNameOfTheImport, exposesSomething } ->
                                        if not exposesSomething || not (Set.member originalNameOfTheImport moduleNamesInUse) then
                                            [ Fix.removeRange variableInfo.rangeToRemove ]

                                        else
                                            []
                        in
                        Rule.errorWithFix
                            { message = variableInfo.typeName ++ " `" ++ name ++ "` is not used"
                            , details = details
                            }
                            variableInfo.under
                            fix
                    )

        customTypeErrors : List (Error {})
        customTypeErrors =
            context.localCustomTypes
                |> Dict.toList
                |> List.filter (\( name, _ ) -> not <| Set.member name usedLocally)
                |> List.map
                    (\( name, customType ) ->
                        Rule.errorWithFix
                            { message = "Type `" ++ name ++ "` is not used"
                            , details = details
                            }
                            customType.under
                            [ Fix.removeRange customType.rangeToRemove ]
                    )
    in
    List.concat
        [ newRootScope
            |> makeReport
            |> Tuple.first
        , importedTypeErrors
        , moduleErrors
        , List.filterMap Tuple.first moduleThatExposeEverythingErrors
        , customTypeErrors
        ]


registerFunction : LetBlockContext -> Function -> ModuleContext -> ModuleContext
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
        |> registerVariable
            { typeName = "`let in` variable"
            , under = Node.range declaration.name
            , rangeToRemove =
                case letBlockContext of
                    HasMultipleDeclarations ->
                        Just functionRange

                    HasNoOtherDeclarations letDeclarationsRange ->
                        -- If there are no other declarations in the let in block,
                        -- we also need to remove the `let in` keywords.
                        Just letDeclarationsRange
            , warning = ""
            }
            (Node.value declaration.name)
        |> markUsedTypesAndModules namesUsedInSignature


type ExposedElement
    = CustomType String ImportedCustomType
    | TypeOrValue String VariableInfo


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
            case ModuleNameLookupTable.moduleNameFor lookupTable nameNode of
                Just realModuleName ->
                    ( realModuleName, Tuple.first (Node.value nameNode) ) :: List.concatMap (collectModuleNamesFromTypeAnnotation lookupTable) params

                Nothing ->
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


registerModule : DeclaredModule -> ModuleContext -> ModuleContext
registerModule declaredModule context =
    { context | declaredModules = declaredModule :: context.declaredModules }


registerVariable : VariableInfo -> String -> ModuleContext -> ModuleContext
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


markAllAsUsed : List String -> ModuleContext -> ModuleContext
markAllAsUsed names context =
    List.foldl markAsUsed context names


markAsUsed : String -> ModuleContext -> ModuleContext
markAsUsed name context =
    case ( Dict.get name <| context.importedCustomTypeLookup, Dict.get name context.constructorNameToTypeName ) of
        ( Just customTypeName, Nothing ) ->
            { context | unusedImportedCustomTypes = Dict.remove customTypeName context.unusedImportedCustomTypes }

        _ ->
            if List.member name context.inTheDeclarationOf then
                context

            else
                let
                    scopes : Nonempty Scope
                    scopes =
                        NonemptyList.mapHead
                            (\scope ->
                                { scope
                                    | used =
                                        Dict.update []
                                            (\set ->
                                                set
                                                    |> Maybe.withDefault Set.empty
                                                    |> Set.insert name
                                                    |> Just
                                            )
                                            scope.used
                                }
                            )
                            context.scopes
                in
                { context | scopes = scopes }


markAllModulesAsUsed : List ( ModuleName, ModuleName ) -> ModuleContext -> ModuleContext
markAllModulesAsUsed names context =
    List.foldl markModuleAsUsed context names


markModuleAsUsed : ( ModuleName, ModuleName ) -> ModuleContext -> ModuleContext
markModuleAsUsed (( realModuleName, aliasName ) as realAndAliasModuleNames) context =
    { context
        | usedModules = Set.insert realAndAliasModuleNames context.usedModules
        , exposingAllModules =
            List.map
                (\module_ ->
                    if module_.name == realModuleName then
                        if module_.name == aliasName || Just (String.join "." aliasName) == module_.alias then
                            { module_ | wasUsedWithModuleName = True }

                        else if aliasName == [] then
                            { module_ | wasUsedImplicitly = True }

                        else
                            module_

                    else
                        module_
                )
                context.exposingAllModules
    }


getModuleName : List String -> String
getModuleName name =
    String.join "." name


makeReport : Scope -> ( List (Error {}), List String )
makeReport { declared, used } =
    let
        usedLocally : Set String
        usedLocally =
            Dict.get [] used |> Maybe.withDefault Set.empty

        nonUsedVars : List String
        nonUsedVars =
            Dict.keys declared
                |> Set.fromList
                |> Set.diff usedLocally
                |> Set.toList

        errors : List (Error {})
        errors =
            Dict.filter (\key _ -> not <| Set.member key usedLocally) declared
                |> Dict.toList
                |> List.map (\( key, variableInfo ) -> error variableInfo key)
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
