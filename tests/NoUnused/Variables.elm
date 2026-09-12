module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
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
import List.Extra
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.Variables.ElmPrelude as ElmPrelude
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

    module A exposing (a, b)

    import UnusedImport

    a n =
        n + 1

    b =
        let
            unused =
                some thing

            _ =
                someOther thing
        in
        2

    c =
        a 2


## Success

    module A exposing (a, b)

    a n =
        n + 1

    b =
        2


## Exception

To avoid resorting to weird workarounds that are sometimes used in internal interactive examples, the rule won't report
values assigned to `_` if a direct call to `Debug.log` is assigned to it.

    a value =
        let
            _ =
                Debug.log "value" value
        in
        value + 1

If you enable the [`NoDebug.Log`](https://package.elm-lang.org/packages/jfmengels/elm-review-debug/latest/NoDebug-Log) rule
from the [`jfmengels/elm-review-debug`](https://package.elm-lang.org/packages/jfmengels/elm-review-debug/latest/) package,
and configure it to ignore the locations where it's acceptable, then the combination of both rules will make sure to
clean up code like the above in all the other locations.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-unused/example --rules NoUnused.Variables
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.Variables" initialContext
        |> Rule.withElmJsonProjectVisitor (\project context -> ( [], elmJsonVisitor project context ))
        |> Rule.withDirectDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor (\module_ context -> ( [], moduleDefinitionVisitor module_ context ))
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor nodes context ))
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionEnterVisitor node context ))
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
        |> Rule.withCaseBranchEnterVisitor (\_ casePattern context -> ( [], caseBranchEnterVisitor casePattern context ))
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
    , moduleNameInUse : ModuleName
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
    , exposingRangeToRemove : Range
    , importRange : Range
    , wasUsedImplicitly : Bool
    , wasUsedWithModuleName : Bool
    }


type DeclaredModuleType
    = ImportedModule
    | ModuleAlias { alias : String, originalNameOfTheImport : String, exposesSomething : Bool }


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
        (\moduleName moduleContext ->
            { customTypes =
                moduleContext.localTypes
                    |> Dict.map (\_ customType -> customType.variants)
                    |> Dict.singleton moduleName

            -- Will be ignored in foldProjectContexts
            , isApplication = True
            }
        )
        |> Rule.withModuleName


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


elmJsonVisitor : Maybe { a | project : Elm.Project.Project } -> ProjectContext -> ProjectContext
elmJsonVisitor maybeElmJson projectContext =
    case Maybe.map .project maybeElmJson of
        Just (Elm.Project.Application _) ->
            { projectContext | isApplication = True }

        Just (Elm.Project.Package _) ->
            { projectContext | isApplication = False }

        Nothing ->
            -- Sensible default, because now `main` won't be reported.
            { projectContext | isApplication = True }



-- DEPENDENCIES VISITOR


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List (Error nothing), ProjectContext )
dependenciesVisitor dependencies projectContext =
    let
        customTypes : Dict ModuleName (Dict String (List String))
        customTypes =
            Dict.foldl
                (\_ dep acc ->
                    List.foldl
                        (\module_ subAcc ->
                            Dict.insert
                                (String.split "." module_.name)
                                (unionsToDict module_.unions)
                                subAcc
                        )
                        acc
                        (Dependency.modules dep)
                )
                Dict.empty
                dependencies
    in
    ( [], { projectContext | customTypes = customTypes } )


unionsToDict : List Elm.Docs.Union -> Dict String (List String)
unionsToDict unions =
    List.foldl
        (\{ name, tags } acc -> Dict.insert name (List.map Tuple.first tags) acc)
        Dict.empty
        unions



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ModuleContext
moduleDefinitionVisitor (Node _ moduleNode) context =
    case Module.exposingList moduleNode of
        Exposing.All _ ->
            { context | exposesEverything = True }

        Exposing.Explicit list ->
            List.foldl
                (\element ctx -> markAsUsed (getExposingName element) ctx)
                context
                list


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
importVisitor ((Node _ import_) as node) context =
    let
        moduleName : ModuleName
        moduleName =
            Node.value import_.moduleName

        errors : List (Error {})
        errors =
            case import_.moduleAlias of
                Just (Node moduleAliasRange moduleAlias) ->
                    if moduleAlias == moduleName then
                        [ Rule.errorWithFix
                            { message = "Module `" ++ String.join "." moduleAlias ++ "` is aliased as itself"
                            , details = [ "The alias is the same as the module name, and brings no useful value" ]
                            }
                            moduleAliasRange
                            [ Fix.removeRange <| endOfModuleNameUntilModuleAliasEnd node moduleAliasRange ]
                        ]

                    else
                        []

                Nothing ->
                    []

        contextWithAlias : ModuleContext
        contextWithAlias =
            case import_.moduleAlias of
                Just moduleAlias ->
                    registerModuleAlias node moduleAlias context

                Nothing ->
                    context

        ( exposingErrors, newContext ) =
            if moduleName == [ "Basics" ] then
                ( reportBasicsImport node, contextWithAlias )

            else
                reportImport node contextWithAlias
    in
    ( exposingErrors ++ errors, newContext )


reportImport : Node { moduleName : Node ModuleName, moduleAlias : Maybe (Node ModuleName), exposingList : Maybe (Node Exposing.Exposing) } -> ModuleContext -> ( List (Error {}), ModuleContext )
reportImport ((Node _ import_) as node) context =
    let
        moduleName : ModuleName
        moduleName =
            Node.value import_.moduleName

        preludeModule : Maybe { exposes : List String, alias : Maybe (List String) }
        preludeModule =
            Dict.get moduleName ElmPrelude.elmPrelude
    in
    case import_.exposingList of
        Nothing ->
            ( case preludeModule of
                Nothing ->
                    []

                Just { alias } ->
                    let
                        preludeModuleReference : ModuleName
                        preludeModuleReference =
                            Maybe.withDefault moduleName alias

                        importedModuleReference : ModuleName
                        importedModuleReference =
                            Maybe.withDefault moduleName (Maybe.map Node.value import_.moduleAlias)
                    in
                    if preludeModuleReference == importedModuleReference then
                        [ importPreludeModuleError node ]

                    else
                        []
            , registerModuleNameOrAlias node context
            )

        Just (Node exposingRange (Exposing.All _)) ->
            ( [], collectExplicitExposingAll context exposingRange node )

        Just (Node exposingRange (Exposing.Explicit list)) ->
            collectExplicitImports
                context
                node
                exposingRange
                (preludeModule
                    |> Maybe.map .exposes
                    |> Maybe.withDefault []
                )
                list


reportBasicsImport : Node Import -> List (Error {})
reportBasicsImport ((Node _ import_) as node) =
    case import_.moduleAlias of
        Nothing ->
            [ importPreludeModuleError node ]

        Just (Node aliasRange _) ->
            case Maybe.map Node.range import_.exposingList of
                Just exposingRange ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary import to elements from `Basics`"
                        , details = [ "These are already imported by default in all Elm modules, you can therefore safely remove them." ]
                        }
                        (Node.range import_.moduleName)
                        [ Fix.removeRange { start = aliasRange.end, end = exposingRange.end } ]
                    ]

                Nothing ->
                    []


importPreludeModuleError : Node Import -> Error {}
importPreludeModuleError (Node importRange import_) =
    Rule.errorWithFix
        { message = "Unnecessary import to implicitly imported `" ++ String.join "." (Node.value import_.moduleName) ++ "`"
        , details = [ "This module is already imported by default in all Elm modules, you can therefore safely remove it." ]
        }
        (Node.range import_.moduleName)
        [ Fix.removeRange (untilStartOfNextLine importRange) ]


collectExplicitExposingAll : ModuleContext -> Range -> Node Import -> ModuleContext
collectExplicitExposingAll context exposingRange (Node importRange import_) =
    let
        (Node moduleNameRange moduleName) =
            import_.moduleName
    in
    if Dict.member moduleName context.customTypes then
        { context
            | exposingAllModules =
                { name = moduleName
                , alias = Maybe.map (Node.value >> String.join ".") import_.moduleAlias
                , moduleNameRange = moduleNameRange
                , exposingRange = exposingRange
                , exposingRangeToRemove = exposingRange
                , importRange = importRange
                , wasUsedImplicitly = False
                , wasUsedWithModuleName = False
                }
                    :: context.exposingAllModules
        }

    else
        context


collectExplicitImports : ModuleContext -> Node Import -> Range -> List String -> List (Node TopLevelExpose) -> ( List (Error {}), ModuleContext )
collectExplicitImports context (Node _ import_) exposingRange exposedFromPrelude list =
    let
        customTypesFromModule : Dict String (List String)
        customTypesFromModule =
            context.customTypes
                |> Dict.get (Node.value import_.moduleName)
                |> Maybe.withDefault Dict.empty
    in
    collectExplicitlyExposedElements
        (handleExposedElements (NonemptyList.head context.scopes).declared customTypesFromModule exposedFromPrelude)
        exposingRange
        list
        ( [], context )


handleExposedElements : Dict String VariableInfo -> Dict String (List String) -> List String -> ExposedElement -> ( List (Rule.Error {}), ModuleContext ) -> ( List (Rule.Error {}), ModuleContext )
handleExposedElements declared customTypesFromModule exposedFromPrelude =
    \importedElement ( errors, context ) ->
        let
            newErrors : List (Rule.Error {})
            newErrors =
                if List.member importedElement.name exposedFromPrelude then
                    importPreludeElementError importedElement :: errors

                else
                    case Dict.get importedElement.name declared of
                        Just variableInfo ->
                            error variableInfo importedElement.name :: errors

                        Nothing ->
                            errors
        in
        ( newErrors, registerExposedElements customTypesFromModule importedElement context )


importPreludeElementError : ExposedElement -> Error {}
importPreludeElementError importedElement =
    let
        { under, rangeToRemove } =
            case importedElement.kind of
                CustomType type_ ->
                    { under = type_.under
                    , rangeToRemove = Just type_.rangeToRemove
                    }

                TypeOrValue type_ ->
                    { under = type_.under
                    , rangeToRemove = type_.rangeToRemove
                    }
    in
    Rule.errorWithFix
        { message = "Unnecessary import to implicitly imported `" ++ importedElement.name ++ "`"
        , details = [ "This element is already imported by default in all Elm modules, you can therefore safely remove it." ]
        }
        under
        (case rangeToRemove of
            Just range ->
                [ Fix.removeRange range ]

            Nothing ->
                []
        )


registerExposedElements : Dict String (List String) -> ExposedElement -> ModuleContext -> ModuleContext
registerExposedElements customTypesFromModule { name, kind } context =
    case kind of
        CustomType variableInfo ->
            case Dict.get name customTypesFromModule of
                Just constructorNames ->
                    { context
                        | unusedImportedCustomTypes = Dict.insert name variableInfo context.unusedImportedCustomTypes
                        , importedCustomTypeLookup =
                            List.foldl
                                (\constructorName acc -> Dict.insert constructorName name acc)
                                context.importedCustomTypeLookup
                                constructorNames
                    }

                Nothing ->
                    context

        TypeOrValue variableInfo ->
            registerVariable variableInfo name context


collectExplicitlyExposedElements : (ExposedElement -> a -> a) -> Range -> List (Node Exposing.TopLevelExpose) -> a -> a
collectExplicitlyExposedElements fold exposingNodeRange list acc =
    collectExplicitlyExposedElementsHelp fold exposingNodeRange list Nothing acc


collectExplicitlyExposedElementsHelp : (ExposedElement -> a -> a) -> Range -> List (Node Exposing.TopLevelExpose) -> Maybe Range -> a -> a
collectExplicitlyExposedElementsHelp fold exposingNodeRange list maybePreviousRange acc =
    case list of
        [] ->
            acc

        (Node range value) :: rest ->
            let
                rangeToRemove : Range -> Range
                rangeToRemove r =
                    case maybePreviousRange of
                        Nothing ->
                            case List.head rest of
                                Just (Node nextRange _) ->
                                    { r | end = nextRange.start }

                                Nothing ->
                                    exposingNodeRange

                        Just previousRange ->
                            { r | start = previousRange.end }

                newAcc : a
                newAcc =
                    case topLevelExposeToExposedElement rangeToRemove (Node range value) of
                        Just v ->
                            fold v acc

                        Nothing ->
                            acc
            in
            collectExplicitlyExposedElementsHelp fold exposingNodeRange rest (Just range) newAcc


topLevelExposeToExposedElement : (Range -> Range) -> Node Exposing.TopLevelExpose -> Maybe ExposedElement
topLevelExposeToExposedElement rangeToRemove (Node range value) =
    case value of
        Exposing.FunctionExpose name ->
            { name = name
            , kind =
                TypeOrValue
                    { typeName = "Imported variable"
                    , under = untilEndOfVariable name range
                    , rangeToRemove = Just (rangeToRemove range)
                    , warning = ""
                    }
            }
                |> Just

        Exposing.InfixExpose name ->
            { name = name
            , kind =
                TypeOrValue
                    { typeName = "Imported operator"
                    , under = untilEndOfVariable name range
                    , rangeToRemove = Just (rangeToRemove range)
                    , warning = ""
                    }
            }
                |> Just

        Exposing.TypeOrAliasExpose name ->
            { name = name
            , kind =
                TypeOrValue
                    { typeName = "Imported type"
                    , under = untilEndOfVariable name range
                    , rangeToRemove = Just (rangeToRemove range)
                    , warning = ""
                    }
            }
                |> Just

        Exposing.TypeExpose { name, open } ->
            case open of
                Just openRange ->
                    { name = name
                    , kind =
                        CustomType
                            { typeName = "Imported type"
                            , under = range
                            , rangeToRemove = rangeToRemove range
                            , openRange = openRange
                            }
                    }
                        |> Just

                Nothing ->
                    -- Can't happen with `elm-syntax`. If open is Nothing, then this we'll have a
                    -- `Exposing.TypeOrAliasExpose`, not a `Exposing.TypeExpose`.
                    Nothing


registerModuleNameOrAlias : Node Import -> ModuleContext -> ModuleContext
registerModuleNameOrAlias ((Node range { moduleAlias, moduleName }) as node) context =
    case moduleAlias of
        Just moduleAlias_ ->
            registerModuleAlias node moduleAlias_ context

        Nothing ->
            registerModule
                { moduleName = Node.value moduleName
                , moduleNameInUse = Node.value moduleName
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
        , moduleNameInUse = Node.value moduleAlias
        , variableType =
            ModuleAlias
                { alias = getModuleName (Node.value moduleAlias)
                , originalNameOfTheImport = getModuleName <| Node.value moduleName
                , exposesSomething = exposingList /= Nothing
                }
        , typeName = "Module alias"
        , under = Node.range moduleAlias
        , rangeToRemove =
            case exposingList of
                Nothing ->
                    untilStartOfNextLine range

                Just _ ->
                    endOfModuleNameUntilModuleAliasEnd node (Node.range moduleAlias)
        }
        context


endOfModuleNameUntilModuleAliasEnd : Node Import -> Range -> Range
endOfModuleNameUntilModuleAliasEnd (Node _ { moduleName }) range =
    { range | start = (Node.range moduleName).end }


expressionEnterVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitor (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            case Dict.get name context.constructorNameToTypeName of
                Just typeName ->
                    markValueAsUsed typeName context

                Nothing ->
                    case Dict.get name context.importedCustomTypeLookup of
                        Just customTypeName ->
                            { context | unusedImportedCustomTypes = Dict.remove customTypeName context.unusedImportedCustomTypes }

                        Nothing ->
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                                Just realModuleName ->
                                    context
                                        |> markValueAsUsed name
                                        |> markModuleAsUsed ( realModuleName, [] )

                                Nothing ->
                                    markValueAsUsed name context

        Expression.FunctionOrValue moduleName _ ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just realModuleName ->
                    markModuleAsUsed ( realModuleName, moduleName ) context

                Nothing ->
                    context

        Expression.OperatorApplication name _ _ _ ->
            markValueAsUsed name context

        Expression.PrefixOperator name ->
            markValueAsUsed name context

        Expression.RecordUpdateExpression expr _ ->
            markValueAsUsed (Node.value expr) context

        Expression.LambdaExpression { args } ->
            markValuesFromPatternsAsUsed args
                { context | scopes = NonemptyList.cons (scopeWithPatternsToIgnore args) context.scopes }

        Expression.CaseExpression { cases } ->
            markValuesFromPatternsAsUsed
                (List.map (\( patternNode, _ ) -> patternNode) cases)
                context

        Expression.LetExpression letBlock ->
            let
                namesToIgnore : Set String
                namesToIgnore =
                    List.foldl
                        (\declaration acc ->
                            case Node.value declaration of
                                Expression.LetFunction _ ->
                                    acc

                                Expression.LetDestructuring pattern _ ->
                                    getDeclaredNamesFromPattern [ pattern ] acc
                        )
                        Set.empty
                        letBlock.declarations
            in
            { context
                | scopes = NonemptyList.cons { declared = Dict.empty, used = Dict.empty, namesToIgnore = namesToIgnore } context.scopes
            }

        _ ->
            context


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            makeReport context

        Expression.LambdaExpression _ ->
            makeReport context

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

                namesToIgnore : Set String
                namesToIgnore =
                    getDeclaredNamesFromPattern functionDeclaration.arguments Set.empty

                newContext : ModuleContext
                newContext =
                    { context | inTheDeclarationOf = Node.value functionDeclaration.name :: context.inTheDeclarationOf }
                        |> markValuesFromPatternsAsUsed functionDeclaration.arguments
                        |> registerFunction letBlockContext function (Node.range declaration)
            in
            ( []
            , { newContext
                | scopes = NonemptyList.cons { declared = Dict.empty, used = Dict.empty, namesToIgnore = namesToIgnore } newContext.scopes
              }
            )

        Expression.LetDestructuring pattern value ->
            case removeParensFromPattern pattern of
                Node wildCardRange Pattern.AllPattern ->
                    if isDebugLog context.lookupTable value then
                        ( [], context )

                    else
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
                    ( if introducesVariable [ pattern ] then
                        []

                      else
                        [ Rule.errorWithFix
                            { message = "Pattern doesn't introduce any variables"
                            , details =
                                [ "This value has been computed but isn't assigned to any variable, which makes the value unusable. You should remove it at the location I pointed at." ]
                            }
                            (Node.range pattern)
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext (Node.range declaration)) ]
                        ]
                    , markValuesFromPatternsAsUsed [ pattern ] context
                    )


isDebugLog : ModuleNameLookupTable -> Node Expression -> Bool
isDebugLog lookupTable node =
    case Node.value node of
        Expression.Application [ functionWithParens, _, _ ] ->
            let
                function : Node Expression
                function =
                    removeParensFromExpression functionWithParens
            in
            case Node.value function of
                Expression.FunctionOrValue _ "log" ->
                    ModuleNameLookupTable.moduleNameFor lookupTable function == Just [ "Debug" ]

                _ ->
                    False

        Expression.OperatorApplication "|>" _ _ pipeFunction ->
            case Node.value (removeParensFromExpression pipeFunction) of
                Expression.Application [ functionWithParens, _ ] ->
                    let
                        function : Node Expression
                        function =
                            removeParensFromExpression functionWithParens
                    in
                    case Node.value function of
                        Expression.FunctionOrValue _ "log" ->
                            ModuleNameLookupTable.moduleNameFor lookupTable function == Just [ "Debug" ]

                        _ ->
                            False

                _ ->
                    False

        Expression.OperatorApplication "<|" _ pipeFunction _ ->
            case Node.value (removeParensFromExpression pipeFunction) of
                Expression.Application [ functionWithParens, _ ] ->
                    let
                        function : Node Expression
                        function =
                            removeParensFromExpression functionWithParens
                    in
                    case Node.value function of
                        Expression.FunctionOrValue _ "log" ->
                            ModuleNameLookupTable.moduleNameFor lookupTable function == Just [ "Debug" ]

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


letDeclarationExitVisitor : a -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Error {}), ModuleContext )
letDeclarationExitVisitor _ declaration context =
    case Node.value declaration of
        Expression.LetFunction _ ->
            makeReport { context | inTheDeclarationOf = List.drop 1 context.inTheDeclarationOf }

        Expression.LetDestructuring _ _ ->
            ( [], context )


caseBranchEnterVisitor : ( Node Pattern, b ) -> ModuleContext -> ModuleContext
caseBranchEnterVisitor ( pattern, _ ) context =
    { context
        | scopes =
            NonemptyList.cons
                { declared = Dict.empty
                , used = Dict.empty
                , namesToIgnore = getDeclaredNamesFromPattern [ pattern ] Set.empty
                }
                context.scopes
    }


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


removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            removeParensFromPattern pattern

        _ ->
            node


removeParensFromExpression : Node Expression -> Node Expression
removeParensFromExpression node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParensFromExpression expr

        _ ->
            node


getDeclaredNamesFromPattern : List (Node Pattern) -> Set String -> Set String
getDeclaredNamesFromPattern nodes acc =
    case nodes of
        (Node _ node) :: restOfNodes ->
            case node of
                Pattern.ParenthesizedPattern pattern ->
                    getDeclaredNamesFromPattern (pattern :: restOfNodes) acc

                Pattern.VarPattern name ->
                    getDeclaredNamesFromPattern restOfNodes (Set.insert name acc)

                Pattern.AsPattern pattern (Node _ asName) ->
                    getDeclaredNamesFromPattern (pattern :: restOfNodes) (Set.insert asName acc)

                Pattern.RecordPattern fields ->
                    getDeclaredNamesFromPattern
                        restOfNodes
                        (List.foldl (\field subAcc -> Set.insert (Node.value field) subAcc) acc fields)

                Pattern.TuplePattern patterns ->
                    getDeclaredNamesFromPattern (patterns ++ restOfNodes) acc

                Pattern.NamedPattern _ patterns ->
                    getDeclaredNamesFromPattern (patterns ++ restOfNodes) acc

                Pattern.UnConsPattern left right ->
                    getDeclaredNamesFromPattern (left :: right :: restOfNodes) acc

                Pattern.ListPattern patterns ->
                    getDeclaredNamesFromPattern (patterns ++ restOfNodes) acc

                _ ->
                    getDeclaredNamesFromPattern restOfNodes acc

        [] ->
            acc


markValuesFromPatternsAsUsed : List (Node Pattern) -> ModuleContext -> ModuleContext
markValuesFromPatternsAsUsed nodes context =
    case nodes of
        [] ->
            context

        node :: restOfNodes ->
            case Node.value node of
                Pattern.TuplePattern patterns ->
                    markValuesFromPatternsAsUsed (patterns ++ restOfNodes) context

                Pattern.UnConsPattern left right ->
                    markValuesFromPatternsAsUsed (left :: right :: restOfNodes) context

                Pattern.ListPattern patterns ->
                    markValuesFromPatternsAsUsed (patterns ++ restOfNodes) context

                Pattern.NamedPattern qualifiedNameRef patterns ->
                    let
                        contextAfterTypeUsage : ModuleContext
                        contextAfterTypeUsage =
                            case qualifiedNameRef.moduleName of
                                [] ->
                                    let
                                        name : String
                                        name =
                                            Dict.get qualifiedNameRef.name context.constructorNameToTypeName
                                                |> Maybe.withDefault qualifiedNameRef.name
                                    in
                                    markValueAsUsed name context

                                _ ->
                                    context

                        contextAfterModuleUsage : ModuleContext
                        contextAfterModuleUsage =
                            case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                                Just realModuleName ->
                                    markModuleAsUsed ( realModuleName, qualifiedNameRef.moduleName ) contextAfterTypeUsage

                                Nothing ->
                                    contextAfterTypeUsage
                    in
                    markValuesFromPatternsAsUsed
                        (patterns ++ restOfNodes)
                        contextAfterModuleUsage

                Pattern.AsPattern pattern _ ->
                    markValuesFromPatternsAsUsed (pattern :: restOfNodes) context

                Pattern.ParenthesizedPattern pattern ->
                    markValuesFromPatternsAsUsed (pattern :: restOfNodes) context

                _ ->
                    markValuesFromPatternsAsUsed restOfNodes context


introducesVariable : List (Node Pattern) -> Bool
introducesVariable nodes =
    case nodes of
        [] ->
            False

        patternNode :: restOfNodes ->
            case Node.value patternNode of
                Pattern.VarPattern _ ->
                    True

                Pattern.AsPattern _ _ ->
                    True

                Pattern.RecordPattern fields ->
                    if List.isEmpty fields then
                        introducesVariable restOfNodes

                    else
                        True

                Pattern.TuplePattern patterns ->
                    introducesVariable (patterns ++ restOfNodes)

                Pattern.UnConsPattern left right ->
                    introducesVariable (left :: right :: restOfNodes)

                Pattern.ListPattern patterns ->
                    introducesVariable (patterns ++ restOfNodes)

                Pattern.NamedPattern _ patterns ->
                    introducesVariable (patterns ++ restOfNodes)

                Pattern.ParenthesizedPattern pattern ->
                    introducesVariable (pattern :: restOfNodes)

                _ ->
                    introducesVariable restOfNodes



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor nodes context =
    List.foldl registerTypes context nodes


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

        constructorNameToTypeName : Dict String String
        constructorNameToTypeName =
            List.foldl (\constructorName acc -> Dict.insert constructorName typeName acc)
                context.constructorNameToTypeName
                constructorNames

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
        , constructorNameToTypeName = constructorNameToTypeName
    }


registerTypeAlias : Range -> TypeAlias -> ModuleContext -> ModuleContext
registerTypeAlias range { name, typeAnnotation } context =
    case Node.value typeAnnotation of
        TypeAnnotation.Record _ ->
            let
                newContext : ModuleContext
                newContext =
                    { context | importedCustomTypeLookup = Dict.remove (Node.value name) context.importedCustomTypeLookup }
            in
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
                    , variants = []
                    }

                localTypes : Dict String TypeData
                localTypes =
                    Dict.insert
                        (Node.value name)
                        typeAlias
                        context.localTypes
            in
            { context | localTypes = localTypes }



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

                typeAnnotation : List (Node TypeAnnotation)
                typeAnnotation =
                    case function.signature of
                        Just signature ->
                            [ (Node.value signature).typeAnnotation ]

                        Nothing ->
                            []

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
                        , scopes = NonemptyList.cons (scopeWithPatternsToIgnore functionImplementation.arguments) newContextWhereFunctionIsRegistered.scopes
                    }
                        |> markValuesFromPatternsAsUsed (Node.value function.declaration).arguments
                        |> collectNamesFromTypeAnnotation Nothing typeAnnotation

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
                arguments : List (Node TypeAnnotation)
                arguments =
                    List.concatMap (Node.value >> .arguments) constructors
            in
            ( []
            , collectNamesFromTypeAnnotation (Just (Node.value name)) arguments context
            )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( []
            , collectNamesFromTypeAnnotation Nothing [ typeAnnotation ] context
            )

        Declaration.PortDeclaration { name, typeAnnotation } ->
            let
                contextWithUsedElements : ModuleContext
                contextWithUsedElements =
                    collectNamesFromTypeAnnotation Nothing [ typeAnnotation ] context
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
                |> List.map (\mod -> getModuleName mod.moduleNameInUse)
                |> Set.fromList

        usedLocally : Set String
        usedLocally =
            Dict.get [] rootScope.used |> Maybe.withDefault Set.empty

        addImportedTypeErrors : List (Error {}) -> List (Error {})
        addImportedTypeErrors acc =
            List.Extra.dictToListMap
                (\name { under, rangeToRemove, openRange } ->
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
                context.unusedImportedCustomTypes
                acc

        moduleThatExposeEverythingErrors : List ( Maybe (Error {}), Maybe ( ModuleName, ModuleName ) )
        moduleThatExposeEverythingErrors =
            List.map
                (\({ importRange, exposingRange, exposingRangeToRemove } as module_) ->
                    if not module_.wasUsedImplicitly then
                        if module_.wasUsedWithModuleName then
                            ( Just
                                (Rule.errorWithFix
                                    { message = "No imported elements from `" ++ String.join "." module_.name ++ "` are used"
                                    , details = details
                                    }
                                    exposingRange
                                    [ Fix.removeRange exposingRangeToRemove ]
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
            List.Extra.insertAllJusts moduleThatExposeEverythingErrors context.usedModules

        addModuleErrors : List (Error {}) -> List (Error {})
        addModuleErrors acc =
            List.Extra.listFilterThenMapInto
                (\variableInfo ->
                    let
                        moduleReference : ( ModuleName, ModuleName )
                        moduleReference =
                            ( variableInfo.moduleName, variableInfo.moduleNameInUse )
                    in
                    not (Set.member moduleReference usedModules)
                )
                (\variableInfo ->
                    let
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
                        { message = variableInfo.typeName ++ " `" ++ String.join "." variableInfo.moduleNameInUse ++ "` is not used"
                        , details = details
                        }
                        variableInfo.under
                        fix
                )
                context.declaredModules
                acc

        addCustomTypeErrors : List (Error {}) -> List (Error {})
        addCustomTypeErrors acc =
            if context.exposesEverything then
                acc

            else
                List.Extra.dictToListFilterAndMap
                    (\name -> not <| Set.member name usedLocally)
                    errorForLocalType
                    context.localTypes
                    acc
    in
    List.filterMap Tuple.first moduleThatExposeEverythingErrors
        |> List.append (makeReportHelp newRootScope |> Tuple.first)
        |> addImportedTypeErrors
        |> addCustomTypeErrors
        |> addModuleErrors


errorForLocalType : String -> TypeData -> Error {}
errorForLocalType name type_ =
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


registerFunction : LetBlockContext -> Function -> Range -> ModuleContext -> ModuleContext
registerFunction letBlockContext function functionDeclarationRange context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        typeAnnotations : List (Node TypeAnnotation)
        typeAnnotations =
            case Maybe.map Node.value function.signature of
                Just signature ->
                    [ signature.typeAnnotation ]

                Nothing ->
                    []
    in
    context
        |> collectNamesFromTypeAnnotation Nothing typeAnnotations
        |> registerVariable
            { typeName = "`let in` variable"
            , under = Node.range declaration.name
            , rangeToRemove = Just (letDeclarationToRemoveRange letBlockContext functionDeclarationRange)
            , warning = ""
            }
            (Node.value declaration.name)


scopeWithPatternsToIgnore : List (Node Pattern) -> Scope
scopeWithPatternsToIgnore patterns =
    { declared = Dict.empty
    , used = Dict.empty
    , namesToIgnore = getDeclaredNamesFromPattern patterns Set.empty
    }


type alias ExposedElement =
    { name : String
    , kind : ExposedElementKind
    }


type ExposedElementKind
    = CustomType ImportedCustomType
    | TypeOrValue VariableInfo


untilEndOfVariable : String -> Range -> Range
untilEndOfVariable name range =
    if range.start.row == range.end.row then
        range

    else
        { range | end = { row = range.start.row, column = range.start.column + String.length name } }


collectNamesFromTypeAnnotation : Maybe String -> List (Node TypeAnnotation) -> ModuleContext -> ModuleContext
collectNamesFromTypeAnnotation exception nodes context =
    case nodes of
        [] ->
            context

        node :: restOfNodes ->
            case Node.value node of
                TypeAnnotation.FunctionTypeAnnotation left right ->
                    collectNamesFromTypeAnnotation exception (left :: right :: restOfNodes) context

                TypeAnnotation.Typed (Node typeRange ( rawModuleName, typeName )) params ->
                    let
                        contextAfterTypeUsage : ModuleContext
                        contextAfterTypeUsage =
                            if Just typeName /= exception then
                                case rawModuleName of
                                    [] ->
                                        markAsUsed typeName context

                                    _ ->
                                        context

                            else
                                context

                        contextAfterModuleUsage : ModuleContext
                        contextAfterModuleUsage =
                            case ModuleNameLookupTable.moduleNameAt context.lookupTable typeRange of
                                Just realModuleName ->
                                    markModuleAsUsed ( realModuleName, rawModuleName ) contextAfterTypeUsage

                                Nothing ->
                                    contextAfterTypeUsage
                    in
                    collectNamesFromTypeAnnotation
                        exception
                        (params ++ restOfNodes)
                        contextAfterModuleUsage

                TypeAnnotation.Record fields ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectNamesFromTypeAnnotation exception (subNodes ++ restOfNodes) context

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    let
                        subNodes : List (Node TypeAnnotation)
                        subNodes =
                            List.map (\(Node _ ( _, value )) -> value) fields
                    in
                    collectNamesFromTypeAnnotation exception (subNodes ++ restOfNodes) context

                TypeAnnotation.Tupled list ->
                    collectNamesFromTypeAnnotation exception (list ++ restOfNodes) context

                _ ->
                    collectNamesFromTypeAnnotation exception restOfNodes context


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

                        else if List.isEmpty aliasName then
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
            Dict.foldl
                (\key variableInfo acc ->
                    if not (Set.member key usedLocally) then
                        error variableInfo key :: acc

                    else
                        acc
                )
                []
                declared
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
