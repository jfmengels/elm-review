module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.

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
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Report variables or types that are declared or imported but never used.

🔧 Running with `--fix` will automatically remove all the reported errors.

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
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.withCaseBranchEnterVisitor caseBranchEnterVisitor
        |> Rule.withCaseBranchExitVisitor caseBranchExitVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ProjectContext =
    { isApplication : Bool
    , customTypes : Dict ModuleName (Dict String (List String))
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , scopes : Nonempty Scope
    , inTheDeclarationOf : List String
    , exposesEverything : Bool
    , isApplication : Bool
    , constructorNameToTypeName : Dict String String
    , declaredModules : List DeclaredModule
    , exposingAllModules : List ModuleThatExposesEverything
    , usedModules : Set ( ModuleName, ModuleName )
    , unusedImportedCustomTypes : Dict String ImportedCustomType
    , importedCustomTypeLookup : Dict String String
    , localTypes : Dict String TypeData
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


type alias TypeData =
    { under : Range
    , kind : TypeKind
    , rangeToRemove : Range
    , variants : List String
    }


type TypeKind
    = CustomTypeKind
    | TypeAliasKind


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
    , namesToIgnore : Set String
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
            , exposesEverything = False
            , isApplication = isApplication
            , constructorNameToTypeName = Dict.empty
            , declaredModules = []
            , exposingAllModules = []
            , usedModules = Set.empty
            , unusedImportedCustomTypes = Dict.empty
            , importedCustomTypeLookup = Dict.empty
            , localTypes = Dict.empty
            , customTypes = customTypes
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes =
                moduleContext.localTypes
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
    , namesToIgnore = Set.empty
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
                    List.map getExposingName list
            in
            ( [], markAllAsUsed names context )


getExposingName : Node Exposing.TopLevelExpose -> String
getExposingName node =
    case Node.value node of
        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.TypeExpose { name } ->
            name

        Exposing.InfixExpose name ->
            name


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
                    if Dict.member (Node.value import_.moduleName) context.customTypes then
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

                    else
                        contextWithAlias

                Exposing.Explicit list ->
                    let
                        customTypesFromModule : Dict String (List String)
                        customTypesFromModule =
                            context.customTypes
                                |> Dict.get (Node.value import_.moduleName)
                                |> Maybe.withDefault Dict.empty
                    in
                    List.foldl
                        (registerExposedElements customTypesFromModule)
                        contextWithAlias
                        (collectExplicitlyExposedElements (Node.range declaredImports) list)
            )


registerExposedElements : Dict String (List String) -> ExposedElement -> ModuleContext -> ModuleContext
registerExposedElements customTypesFromModule importedElement context =
    case importedElement of
        CustomType name variableInfo ->
            case Dict.get name customTypesFromModule of
                Just constructorNames ->
                    { context
                        | unusedImportedCustomTypes = Dict.insert name variableInfo context.unusedImportedCustomTypes
                        , importedCustomTypeLookup =
                            Dict.union
                                (constructorNames
                                    |> List.map (\constructorName -> ( constructorName, name ))
                                    |> Dict.fromList
                                )
                                context.importedCustomTypeLookup
                    }

                Nothing ->
                    context

        TypeOrValue name variableInfo ->
            registerVariable variableInfo name context


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
expressionEnterVisitor (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            case Dict.get name context.constructorNameToTypeName of
                Just typeName ->
                    ( [], markValueAsUsed typeName context )

                Nothing ->
                    case Dict.get name context.importedCustomTypeLookup of
                        Just customTypeName ->
                            ( [], { context | unusedImportedCustomTypes = Dict.remove customTypeName context.unusedImportedCustomTypes } )

                        Nothing ->
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                                Just realModuleName ->
                                    ( []
                                    , context
                                        |> markValueAsUsed name
                                        |> markModuleAsUsed ( realModuleName, [] )
                                    )

                                Nothing ->
                                    ( [], markValueAsUsed name context )

        Expression.FunctionOrValue moduleName _ ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just realModuleName ->
                    ( [], markModuleAsUsed ( realModuleName, moduleName ) context )

                Nothing ->
                    ( [], context )

        Expression.OperatorApplication name _ _ _ ->
            ( [], markValueAsUsed name context )

        Expression.PrefixOperator name ->
            ( [], markValueAsUsed name context )

        Expression.RecordUpdateExpression expr _ ->
            ( [], markValueAsUsed (Node.value expr) context )

        Expression.LambdaExpression { args } ->
            let
                namesUsedInArgumentPatterns : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInArgumentPatterns =
                    args
                        |> List.map (getUsedVariablesFromPattern context)
                        |> foldUsedTypesAndModules
            in
            ( []
            , List.foldl markValueAsUsed context namesUsedInArgumentPatterns.types
                |> markAllModulesAsUsed namesUsedInArgumentPatterns.modules
                |> registerParameters args
            )

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
            , List.foldl
                markValueAsUsed
                context
                usedVariables.types
                |> markAllModulesAsUsed usedVariables.modules
            )

        _ ->
            ( [], context )


letDeclarationEnterVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Error {}), ModuleContext )
letDeclarationEnterVisitor (Node range { declarations, expression }) declaration context =
    let
        letBlockContext : LetBlockContext
        letBlockContext =
            if List.length declarations == 1 then
                HasNoOtherDeclarations <| rangeUpUntil range (Node.range expression |> .start)

            else
                HasMultipleDeclarations
    in
    case Node.value declaration of
        Expression.LetFunction function ->
            let
                functionDeclaration : FunctionImplementation
                functionDeclaration =
                    Node.value function.declaration

                namesUsedInArgumentPatterns : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInArgumentPatterns =
                    functionDeclaration.arguments
                        |> List.map (getUsedVariablesFromPattern context)
                        |> foldUsedTypesAndModules

                namesToIgnore : Set String
                namesToIgnore =
                    List.concatMap getDeclaredParametersFromPattern functionDeclaration.arguments
                        |> Set.fromList

                newContext : ModuleContext
                newContext =
                    { context | inTheDeclarationOf = Node.value functionDeclaration.name :: context.inTheDeclarationOf }
                        |> markAllAsUsed namesUsedInArgumentPatterns.types
                        |> markAllModulesAsUsed namesUsedInArgumentPatterns.modules
                        |> registerFunction letBlockContext function
            in
            ( []
            , { newContext
                | scopes = NonemptyList.cons { declared = Dict.empty, used = Dict.empty, namesToIgnore = namesToIgnore } newContext.scopes
              }
            )

        Expression.LetDestructuring pattern _ ->
            case removeParens pattern of
                Node wildCardRange Pattern.AllPattern ->
                    ( [ Rule.errorWithFix
                            { message = "Value assigned to `_` is unused"
                            , details =
                                [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                                ]
                            }
                            wildCardRange
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext (Node.range declaration)) ]
                      ]
                    , context
                    )

                Node unitPattern Pattern.UnitPattern ->
                    ( [ Rule.errorWithFix
                            { message = "Unit value is unused"
                            , details =
                                [ "This value has no data, which makes the value unusable. You should remove it at the location I pointed at."
                                ]
                            }
                            unitPattern
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext (Node.range declaration)) ]
                      ]
                    , context
                    )

                _ ->
                    let
                        namesUsedInPattern : { types : List String, modules : List ( ModuleName, ModuleName ) }
                        namesUsedInPattern =
                            getUsedVariablesFromPattern context pattern
                    in
                    ( if not (introducesVariable pattern) then
                        [ Rule.errorWithFix
                            { message = "Pattern doesn't introduce any variables"
                            , details =
                                [ "This value has been computed but isn't assigned to any variable, which makes the value unusable. You should remove it at the location I pointed at." ]
                            }
                            (Node.range pattern)
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext (Node.range declaration)) ]
                        ]

                      else
                        []
                    , List.foldl markValueAsUsed context namesUsedInPattern.types
                        |> markAllModulesAsUsed namesUsedInPattern.modules
                    )


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Error {}), ModuleContext )
letDeclarationExitVisitor _ declaration context =
    case Node.value declaration of
        Expression.LetFunction _ ->
            makeReport { context | inTheDeclarationOf = List.drop 1 context.inTheDeclarationOf }

        Expression.LetDestructuring _ _ ->
            ( [], context )


caseBranchEnterVisitor : a -> ( Node Pattern, b ) -> ModuleContext -> ( List nothing, ModuleContext )
caseBranchEnterVisitor _ ( pattern, _ ) context =
    ( []
    , { context
        | scopes =
            NonemptyList.cons
                { declared = Dict.empty
                , used = Dict.empty
                , namesToIgnore = Set.fromList (getDeclaredParametersFromPattern pattern)
                }
                context.scopes
      }
    )


caseBranchExitVisitor : a -> ( Node Pattern, b ) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
caseBranchExitVisitor _ _ context =
    makeReport context


letDeclarationToRemoveRange : LetBlockContext -> Range -> Range
letDeclarationToRemoveRange letBlockContext range =
    case letBlockContext of
        HasMultipleDeclarations ->
            range

        HasNoOtherDeclarations letDeclarationsRange ->
            -- If there are no other declarations in the let in block,
            -- we also need to remove the `let in` keywords.
            letDeclarationsRange


removeParens : Node Pattern -> Node Pattern
removeParens node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            removeParens pattern

        _ ->
            node


getUsedVariablesFromPattern : ModuleContext -> Node Pattern -> { types : List String, modules : List ( ModuleName, ModuleName ) }
getUsedVariablesFromPattern context patternNode =
    { types = getUsedTypesFromPattern context.constructorNameToTypeName patternNode
    , modules = getUsedModulesFromPattern context.lookupTable patternNode
    }


getDeclaredParametersFromPattern : Node Pattern -> List String
getDeclaredParametersFromPattern node =
    getDeclaredParametersFromPatternHelp [ node ] []


getDeclaredParametersFromPatternHelp : List (Node Pattern) -> List String -> List String
getDeclaredParametersFromPatternHelp nodes acc =
    case nodes of
        (Node _ node) :: tail ->
            case node of
                Pattern.ParenthesizedPattern pattern ->
                    getDeclaredParametersFromPatternHelp (pattern :: tail) acc

                Pattern.VarPattern name ->
                    getDeclaredParametersFromPatternHelp tail (name :: acc)

                Pattern.AsPattern pattern (Node _ asName) ->
                    getDeclaredParametersFromPatternHelp (pattern :: tail) (asName :: acc)

                Pattern.RecordPattern fields ->
                    getDeclaredParametersFromPatternHelp
                        tail
                        (List.append (List.map Node.value fields) acc)

                Pattern.TuplePattern patterns ->
                    getDeclaredParametersFromPatternHelp (patterns ++ tail) acc

                Pattern.NamedPattern _ patterns ->
                    getDeclaredParametersFromPatternHelp (patterns ++ tail) acc

                Pattern.UnConsPattern left right ->
                    getDeclaredParametersFromPatternHelp (left :: right :: tail) acc

                Pattern.ListPattern patterns ->
                    getDeclaredParametersFromPatternHelp (patterns ++ tail) acc

                _ ->
                    getDeclaredParametersFromPatternHelp tail acc

        [] ->
            acc


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


introducesVariable : Node Pattern -> Bool
introducesVariable patternNode =
    case Node.value patternNode of
        Pattern.VarPattern _ ->
            True

        Pattern.AsPattern _ _ ->
            True

        Pattern.RecordPattern fields ->
            not (List.isEmpty fields)

        Pattern.TuplePattern patterns ->
            List.any introducesVariable patterns

        Pattern.UnConsPattern pattern1 pattern2 ->
            List.any introducesVariable [ pattern1, pattern2 ]

        Pattern.ListPattern patterns ->
            List.any introducesVariable patterns

        Pattern.NamedPattern _ patterns ->
            List.any introducesVariable patterns

        Pattern.ParenthesizedPattern pattern ->
            introducesVariable pattern

        _ ->
            False



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor nodes context =
    ( []
    , List.foldl registerTypes context nodes
    )


registerTypes : Node Declaration -> ModuleContext -> ModuleContext
registerTypes node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration customType ->
            registerCustomType (Node.range node) customType context

        Declaration.AliasDeclaration typeAliasDeclaration ->
            registerTypeAlias (Node.range node) typeAliasDeclaration context

        _ ->
            context


registerCustomType : Range -> Elm.Syntax.Type.Type -> ModuleContext -> ModuleContext
registerCustomType range { name, constructors } context =
    let
        typeName : String
        typeName =
            Node.value name

        constructorNames : List String
        constructorNames =
            List.map (Node.value >> .name >> Node.value) constructors

        constructorsForType : Dict String String
        constructorsForType =
            constructorNames
                |> List.map (\constructorName -> ( constructorName, typeName ))
                |> Dict.fromList

        customType : TypeData
        customType =
            { kind = CustomTypeKind
            , under = Node.range name
            , rangeToRemove = untilStartOfNextLine range
            , variants = constructorNames
            }
    in
    { context
        | localTypes =
            Dict.insert
                (Node.value name)
                customType
                context.localTypes
        , constructorNameToTypeName = Dict.union constructorsForType context.constructorNameToTypeName
    }


registerTypeAlias : Range -> TypeAlias -> ModuleContext -> ModuleContext
registerTypeAlias range { name, typeAnnotation } context =
    let
        newContext : ModuleContext
        newContext =
            case Node.value typeAnnotation of
                TypeAnnotation.Record _ ->
                    { context | importedCustomTypeLookup = Dict.remove (Node.value name) context.importedCustomTypeLookup }

                _ ->
                    context
    in
    case Node.value typeAnnotation of
        TypeAnnotation.Record _ ->
            if context.exposesEverything then
                newContext

            else
                registerVariable
                    { typeName = "Type"
                    , under = Node.range name
                    , rangeToRemove = Just (untilStartOfNextLine range)
                    , warning = ""
                    }
                    (Node.value name)
                    newContext

        _ ->
            let
                typeAlias : TypeData
                typeAlias =
                    { kind = TypeAliasKind
                    , under = Node.range name
                    , rangeToRemove = untilStartOfNextLine range
                    , variants =
                        case Node.value typeAnnotation of
                            TypeAnnotation.Record _ ->
                                [ Node.value name ]

                            _ ->
                                []
                    }

                localCustomTypes : Dict String TypeData
                localCustomTypes =
                    Dict.insert
                        (Node.value name)
                        typeAlias
                        newContext.localTypes
            in
            { newContext | localTypes = localCustomTypes }



-- DECLARATION ENTER VISITOR


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                functionImplementation : FunctionImplementation
                functionImplementation =
                    Node.value function.declaration

                functionName : String
                functionName =
                    Node.value functionImplementation.name

                namesUsedInSignature : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInSignature =
                    case function.signature of
                        Just signature ->
                            signature |> Node.value |> .typeAnnotation |> collectNamesFromTypeAnnotation context.lookupTable

                        Nothing ->
                            { types = [], modules = [] }

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
                            || (context.isApplication && functionName == "main")
                    then
                        context

                    else
                        registerVariable
                            { typeName = "Top-level variable"
                            , under = Node.range functionImplementation.name
                            , rangeToRemove = Just (untilStartOfNextLine (Node.range node))
                            , warning = ""
                            }
                            functionName
                            context

                newContext : ModuleContext
                newContext =
                    { newContextWhereFunctionIsRegistered
                        | inTheDeclarationOf = [ functionName ]
                        , scopes = NonemptyList.cons emptyScope newContextWhereFunctionIsRegistered.scopes
                    }
                        |> registerParameters functionImplementation.arguments
                        |> (\ctx -> List.foldl markValueAsUsed ctx namesUsedInArgumentPatterns.types)
                        |> markAllAsUsed namesUsedInSignature.types
                        |> markAllModulesAsUsed namesUsedInSignature.modules
                        |> markAllModulesAsUsed namesUsedInArgumentPatterns.modules

                shadowingImportError : List (Error {})
                shadowingImportError =
                    case Dict.get functionName (NonemptyList.head context.scopes).declared of
                        Just existingVariable ->
                            if existingVariable.typeName == "Imported variable" then
                                [ error existingVariable functionName ]

                            else
                                []

                        _ ->
                            []
            in
            ( shadowingImportError, newContext )

        Declaration.CustomTypeDeclaration { name, constructors } ->
            let
                { types, modules } =
                    constructors
                        |> List.concatMap (Node.value >> .arguments)
                        |> List.map (collectNamesFromTypeAnnotation context.lookupTable)
                        |> foldUsedTypesAndModules
            in
            ( []
            , types
                |> List.filter ((/=) (Node.value name))
                |> List.foldl markAsUsed context
                |> markAllModulesAsUsed modules
            )

        Declaration.AliasDeclaration { typeAnnotation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation context.lookupTable typeAnnotation
            in
            ( []
            , List.foldl markAsUsed context namesUsedInTypeAnnotation.types
                |> markAllModulesAsUsed namesUsedInTypeAnnotation.modules
            )

        Declaration.PortDeclaration { name, typeAnnotation } ->
            let
                namesUsedInTypeAnnotation : { types : List String, modules : List ( ModuleName, ModuleName ) }
                namesUsedInTypeAnnotation =
                    collectNamesFromTypeAnnotation context.lookupTable typeAnnotation

                contextWithUsedElements : ModuleContext
                contextWithUsedElements =
                    List.foldl markAsUsed context namesUsedInTypeAnnotation.types
                        |> markAllModulesAsUsed namesUsedInTypeAnnotation.modules
            in
            ( []
            , if context.exposesEverything then
                contextWithUsedElements

              else
                registerVariable
                    { typeName = "Port"
                    , under = Node.range name
                    , rangeToRemove = Nothing
                    , warning = " (Warning: Removing this port may break your application if it is used in the JS code)"
                    }
                    (Node.value name)
                    contextWithUsedElements
            )

        Declaration.InfixDeclaration { operator, function } ->
            ( []
            , context
                |> markValueAsUsed (Node.value function)
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



-- DECLARATION EXIT VISITOR


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            makeReport context

        _ ->
            ( [], context )



-- FINAL EVALUATION


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
                        if Set.member name usedLocally && not (Dict.member name context.localTypes) then
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
            if context.exposesEverything then
                []

            else
                context.localTypes
                    |> Dict.toList
                    |> List.filter (\( name, _ ) -> not <| Set.member name usedLocally)
                    |> List.map errorForLocalType
    in
    List.concat
        [ makeReportHelp newRootScope
            |> Tuple.first
        , importedTypeErrors
        , moduleErrors
        , List.filterMap Tuple.first moduleThatExposeEverythingErrors
        , customTypeErrors
        ]


errorForLocalType : ( String, TypeData ) -> Error {}
errorForLocalType ( name, type_ ) =
    let
        kind : String
        kind =
            case type_.kind of
                CustomTypeKind ->
                    "Type"

                TypeAliasKind ->
                    "Type alias"
    in
    Rule.errorWithFix
        { message = kind ++ " `" ++ name ++ "` is not used"
        , details = details
        }
        type_.under
        [ Fix.removeRange type_.rangeToRemove ]


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
    in
    List.foldl markAsUsed context namesUsedInSignature.types
        |> markAllModulesAsUsed namesUsedInSignature.modules
        |> registerVariable
            { typeName = "`let in` variable"
            , under = Node.range declaration.name
            , rangeToRemove = Just (letDeclarationToRemoveRange letBlockContext (Node.range function.declaration))
            , warning = ""
            }
            (Node.value declaration.name)


registerParameters : List (Node Pattern) -> ModuleContext -> ModuleContext
registerParameters patterns context =
    { context
        | scopes =
            NonemptyList.mapHead
                (setNamesToIgnoreFromPattern patterns)
                context.scopes
    }


setNamesToIgnoreFromPattern : List (Node Pattern) -> { a | namesToIgnore : Set String } -> { a | namesToIgnore : Set String }
setNamesToIgnoreFromPattern patterns scope =
    let
        namesToIgnore : Set String
        namesToIgnore =
            List.concatMap getDeclaredParametersFromPattern patterns
                |> Set.fromList
    in
    { scope | namesToIgnore = namesToIgnore }


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


markValueAsUsed : String -> ModuleContext -> ModuleContext
markValueAsUsed name context =
    if Dict.member name context.constructorNameToTypeName then
        markAsUsed name context

    else
        case Dict.get name context.importedCustomTypeLookup of
            Just customTypeName ->
                { context | unusedImportedCustomTypes = Dict.remove customTypeName context.unusedImportedCustomTypes }

            _ ->
                markAsUsed name context


markAsUsed : String -> ModuleContext -> ModuleContext
markAsUsed name context =
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


makeReport : ModuleContext -> ( List (Error {}), ModuleContext )
makeReport context =
    let
        ( errors, remainingUsed ) =
            makeReportHelp (NonemptyList.head context.scopes)

        contextWithPoppedScope : ModuleContext
        contextWithPoppedScope =
            { context | scopes = NonemptyList.pop context.scopes }
    in
    ( errors
    , markAllAsUsed remainingUsed contextWithPoppedScope
    )


makeReportHelp : Scope -> ( List (Error {}), List String )
makeReportHelp { declared, used, namesToIgnore } =
    let
        usedLocally : Set String
        usedLocally =
            Dict.get [] used |> Maybe.withDefault Set.empty

        nonUsedVars : List String
        nonUsedVars =
            Dict.keys declared
                |> Set.fromList
                |> Set.diff usedLocally
                |> (\set -> Set.diff set namesToIgnore)
                |> Set.toList

        errors : List (Error {})
        errors =
            Dict.filter (\key _ -> not (Set.member key usedLocally)) declared
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
