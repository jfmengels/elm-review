module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import List.Extra
import NoUnused.NonemptyList as NonemptyList exposing (Nonempty)
import NoUnused.Variables.ElmPrelude as ElmPrelude
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Resources.Binding as Binding exposing (Binding)
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.Reference as Reference exposing (Reference)
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
import Review.Resources.Target as Target
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
    Rule.newProjectRuleSchema "NoUnused.Variables" ()
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor (\node context -> ( importVisitor node context, context ))
        |> Rule.withDeclarationListVisitor (\nodes context -> ( [], declarationListVisitor nodes context ))
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.withDeclarationExitVisitor declarationExitVisitor
        |> Rule.withExpressionEnterVisitor (\node context -> ( [], expressionEnterVisitor node context ))
        |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
        |> Rule.withCaseBranchEnterVisitor (\_ casePattern context -> ( [], caseBranchEnterVisitor casePattern context ))
        |> Rule.withFinalModuleEvaluation finalEvaluation


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , scopeProvider : ScopeProvider
    , scopes : Nonempty Scope
    , inTheDeclarationOf : List String
    , exposed : Dict String Bool
    , isApplication : Bool
    , constructorNameToTypeName : Dict ModuleName (Dict String String)
    , moduleNamesInUse : Set String
    , localTypes : Dict String TypeData
    , moduleDocs : Dict ModuleName Elm.Docs.Module
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
    { rangeToRemove : Range
    , openRange : Range
    }


type LetBlockContext
    = HasMultipleDeclarations
    | HasNoOtherDeclarations Range


fromProjectToModule : Rule.ContextCreator () ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable scopeProvider { exposed } isModuleExposed ast moduleDocs constructorToType () ->
            { lookupTable = lookupTable
            , scopeProvider = scopeProvider
            , scopes = NonemptyList.fromElement (scopeWithUsedExports exposed)
            , inTheDeclarationOf = []
            , exposed = exposed
            , isApplication = isModuleExposed == Nothing
            , constructorNameToTypeName = constructorToType
            , moduleNamesInUse = collectModuleNamesInUse ast.imports Set.empty
            , localTypes = Dict.empty
            , moduleDocs = moduleDocs
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withScopes
        |> Rule.withExposed
        |> Rule.withIsModuleExposed
        |> Rule.withFullAst
        |> Rule.withImportedModuleDocs { implicitImports = False, dependencies = True }
        |> Rule.withConstructorToType { implicitImports = False, dependencies = True }


collectModuleNamesInUse : List (Node Import) -> Set String -> Set String
collectModuleNamesInUse nodes set =
    case nodes of
        [] ->
            set

        (Node _ import_) :: rest ->
            let
                (Node _ effectiveName) =
                    case import_.moduleAlias of
                        Just moduleAlias ->
                            moduleAlias

                        Nothing ->
                            import_.moduleName
            in
            collectModuleNamesInUse rest (Set.insert (String.join "." effectiveName) set)


fromModuleToProject : Rule.ContextCreator ModuleContext ()
fromModuleToProject =
    Rule.initContextCreator (\_ -> ())


foldProjectContexts : () -> () -> ()
foldProjectContexts _ previousProjectContext =
    previousProjectContext


scopeWithUsedExports : Dict String Bool -> Scope
scopeWithUsedExports exposed =
    { declared = Dict.empty
    , used = Dict.singleton [] (Dict.foldl (\key _ set -> Set.insert key set) Set.empty exposed)
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


importVisitor : Node Import -> ModuleContext -> List (Error {})
importVisitor ((Node _ import_) as node) context =
    let
        (Node moduleNameRange moduleName) =
            import_.moduleName

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
                            [ Fix.removeRange { start = moduleNameRange.end, end = moduleAliasRange.end } ]
                        ]

                    else
                        []

                Nothing ->
                    []
    in
    if moduleName == [ "Basics" ] then
        reportBasicsImport node errors

    else
        reportImport node context errors


reportImport : Node { moduleName : Node ModuleName, moduleAlias : Maybe (Node ModuleName), exposingList : Maybe (Node Exposing.Exposing) } -> ModuleContext -> List (Error {}) -> List (Error {})
reportImport ((Node _ import_) as node) context errors =
    let
        (Node _ moduleName) =
            import_.moduleName
    in
    case import_.exposingList of
        Nothing ->
            case Dict.get moduleName ElmPrelude.elmPreludeAliases of
                Nothing ->
                    case import_.moduleAlias of
                        Just _ ->
                            reportUnusedModuleAlias node import_.moduleAlias context errors

                        Nothing ->
                            reportUnusedModule node context errors

                Just preludeModuleReference ->
                    let
                        importedModuleReference : ModuleName
                        importedModuleReference =
                            Maybe.withDefault moduleName (Maybe.map Node.value import_.moduleAlias)
                    in
                    if preludeModuleReference == importedModuleReference then
                        importPreludeModuleError node :: errors

                    else
                        errors

        Just (Node exposingRange (Exposing.All _)) ->
            explicitExposingAllErrors context exposingRange node errors

        Just (Node exposingRange (Exposing.Explicit list)) ->
            errors
                |> collectExplicitlyExposedElements
                    context
                    moduleName
                    (Dict.get moduleName ElmPrelude.elmPreludeExposing
                        |> Maybe.withDefault []
                    )
                    exposingRange
                    list
                    Nothing
                |> reportUnusedModuleAlias node import_.moduleAlias context


reportBasicsImport : Node Import -> List (Error {}) -> List (Error {})
reportBasicsImport ((Node _ import_) as node) errors =
    case import_.moduleAlias of
        Nothing ->
            importPreludeModuleError node :: errors

        Just (Node aliasRange _) ->
            case Maybe.map Node.range import_.exposingList of
                Just exposingRange ->
                    Rule.errorWithFix
                        { message = "Unnecessary import to elements from `Basics`"
                        , details = [ "These are already imported by default in all Elm modules, you can therefore safely remove them." ]
                        }
                        (Node.range import_.moduleName)
                        [ Fix.removeRange { start = aliasRange.end, end = exposingRange.end } ]
                        :: errors

                Nothing ->
                    errors


importPreludeModuleError : Node Import -> Error {}
importPreludeModuleError (Node importRange import_) =
    let
        (Node moduleNameRange moduleName) =
            import_.moduleName
    in
    Rule.errorWithFix
        { message = "Unnecessary import to implicitly imported `" ++ String.join "." moduleName ++ "`"
        , details = [ "This module is already imported by default in all Elm modules, you can therefore safely remove it." ]
        }
        moduleNameRange
        [ Fix.removeRange (untilStartOfNextLine importRange) ]


explicitExposingAllErrors : ModuleContext -> Range -> Node Import -> List (Rule.Error {}) -> List (Rule.Error {})
explicitExposingAllErrors context exposingRange ((Node _ import_) as node) errors =
    let
        (Node _ moduleName) =
            import_.moduleName
    in
    case Dict.get moduleName context.moduleDocs of
        Nothing ->
            -- We somehow don't know the contents of the module, so we presume the `exposing (..)` is useful
            -- to avoid reporting a false positive.
            errors

        Just _ ->
            case ScopeProvider.moduleReferences context.scopeProvider moduleName of
                [] ->
                    -- The entire import is unused
                    unusedImportError node :: errors

                refs ->
                    if List.any (\ref -> List.isEmpty (Reference.qualifier ref)) refs then
                        -- Something from the `exposing (..)` was used.
                        -- We still want to report a unused module alias if there is one
                        reportUnusedModuleAlias node import_.moduleAlias context errors

                    else
                        -- There are references to the module, but they're all through qualified imports (`A.b`)
                        -- so the module name (or its alias) is used, but the `exposing (..)` is unused.
                        Rule.errorWithFix
                            { message = "No imported elements from `" ++ String.join "." moduleName ++ "` are used"
                            , details = details
                            }
                            exposingRange
                            [ Fix.removeRange exposingRange ]
                            :: errors


reportPreludeElement : List String -> String -> Range -> Range -> List (Error {}) -> List (Error {})
reportPreludeElement exposedFromPrelude name under rangeToRemove errors =
    if List.member name exposedFromPrelude then
        importPreludeElementError name under rangeToRemove :: errors

    else
        errors


importPreludeElementError : String -> Range -> Range -> Error {}
importPreludeElementError name under rangeToRemove =
    Rule.errorWithFix
        { message = "Unnecessary import to implicitly imported `" ++ name ++ "`"
        , details = [ "This element is already imported by default in all Elm modules, you can therefore safely remove it." ]
        }
        under
        [ Fix.removeRange rangeToRemove ]


reportImportedCustomType :
    ModuleContext
    -> ModuleName
    -> String
    -> Range
    -> ImportedCustomType
    -> List (Error {})
    -> List (Error {})
reportImportedCustomType context moduleName name under variableInfo errors =
    case
        Dict.get moduleName context.moduleDocs
            |> Maybe.andThen (\docs -> List.Extra.find (\union -> union.name == name) docs.unions)
    of
        Just { tags } ->
            let
                areConstructorsUsed : Bool
                areConstructorsUsed =
                    -- TODO Only if it's done in an unqualified manner
                    List.any (\( tagName, _ ) -> isImportedValueReferenced context.scopeProvider tagName) tags
            in
            if areConstructorsUsed then
                errors

            else if isImportedTypeReferenced context.scopeProvider name then
                Rule.errorWithFix
                    { message = "Imported constructors for `" ++ name ++ "` are not used"
                    , details = details
                    }
                    under
                    -- If the constructors are not used but the type itself is, then only remove the `(..)`
                    [ Fix.removeRange variableInfo.openRange ]
                    :: errors

            else
                Rule.errorWithFix
                    { message = "Imported type `" ++ name ++ "` is not used"
                    , details = details
                    }
                    under
                    [ Fix.removeRange variableInfo.rangeToRemove ]
                    :: errors

        Nothing ->
            errors


reportImportedTypeOrValue : ModuleContext -> String -> VariableInfo -> List (Error {}) -> List (Error {})
reportImportedTypeOrValue context name variableInfo errors =
    if
        isImportedValueReferenced context.scopeProvider name
            || isImportedTypeReferenced context.scopeProvider name
    then
        errors

    else
        error variableInfo name :: errors


collectExplicitlyExposedElements :
    ModuleContext
    -> ModuleName
    -> List String
    -> Range
    -> List (Node Exposing.TopLevelExpose)
    -> Maybe Range
    -> List (Rule.Error {})
    -> List (Rule.Error {})
collectExplicitlyExposedElements context moduleName exposedFromPrelude exposingNodeRange list maybePreviousRange errors =
    case list of
        [] ->
            errors

        (Node range value) :: rest ->
            let
                rangeToRemove : Range
                rangeToRemove =
                    case maybePreviousRange of
                        Nothing ->
                            case rest of
                                (Node nextRange _) :: _ ->
                                    { range | end = nextRange.start }

                                [] ->
                                    exposingNodeRange

                        Just previousRange ->
                            { range | start = previousRange.end }

                newAcc : List (Error {})
                newAcc =
                    case value of
                        Exposing.FunctionExpose name ->
                            let
                                under : Range
                                under =
                                    untilEndOfVariable name range
                            in
                            errors
                                |> reportPreludeElement exposedFromPrelude name under rangeToRemove
                                |> reportImportedTypeOrValue context
                                    name
                                    { typeName = "Imported variable"
                                    , under = under
                                    , rangeToRemove = Just rangeToRemove
                                    , warning = ""
                                    }

                        Exposing.InfixExpose name ->
                            let
                                under : Range
                                under =
                                    untilEndOfVariable name range
                            in
                            errors
                                |> reportPreludeElement exposedFromPrelude name under rangeToRemove
                                |> reportImportedTypeOrValue context
                                    name
                                    { typeName = "Imported operator"
                                    , under = under
                                    , rangeToRemove = Just rangeToRemove
                                    , warning = ""
                                    }

                        Exposing.TypeOrAliasExpose name ->
                            let
                                under : Range
                                under =
                                    untilEndOfVariable name range
                            in
                            errors
                                |> reportPreludeElement exposedFromPrelude name under rangeToRemove
                                |> reportImportedTypeOrValue context
                                    name
                                    { typeName = "Imported type"
                                    , under = under
                                    , rangeToRemove = Just rangeToRemove
                                    , warning = ""
                                    }

                        Exposing.TypeExpose { name, open } ->
                            case open of
                                Just openRange ->
                                    errors
                                        |> reportPreludeElement exposedFromPrelude name range rangeToRemove
                                        |> reportImportedCustomType context moduleName name range { rangeToRemove = rangeToRemove, openRange = openRange }

                                Nothing ->
                                    -- Can't happen with `elm-syntax`. If open is Nothing, then this we'll have a
                                    -- `Exposing.TypeOrAliasExpose`, not a `Exposing.TypeExpose`.
                                    errors
            in
            collectExplicitlyExposedElements context moduleName exposedFromPrelude exposingNodeRange rest (Just range) newAcc


isImportedValueReferenced : ScopeProvider -> String -> Bool
isImportedValueReferenced scopeProvider name =
    case ScopeProvider.importValueBinding scopeProvider name of
        Just importBinding ->
            not <| List.isEmpty (ScopeProvider.valueReferences scopeProvider (ImportBinding.id importBinding))

        Nothing ->
            False


isImportedTypeReferenced : ScopeProvider -> String -> Bool
isImportedTypeReferenced scopeProvider name =
    case ScopeProvider.importTypeBinding scopeProvider name of
        Just importBinding ->
            not <| List.isEmpty (ScopeProvider.typeReferences scopeProvider (ImportBinding.id importBinding))

        Nothing ->
            False


targets : ModuleName -> Reference -> Bool
targets moduleName ref =
    case Reference.target ref of
        Target.ImportBinding_ importBinding ->
            ImportBinding.moduleName importBinding == moduleName

        Target.QualifiedReference r ->
            r.moduleName == moduleName

        Target.LocalBinding _ ->
            False


reportUnusedModuleAlias : Node Import -> Maybe (Node ModuleName) -> ModuleContext -> List (Rule.Error {}) -> List (Rule.Error {})
reportUnusedModuleAlias (Node range import_) maybeModuleAlias context errors =
    case maybeModuleAlias of
        Just (Node aliasRange alias) ->
            let
                (Node moduleNameRange moduleName) =
                    import_.moduleName
            in
            if
                ScopeProvider.qualifiedReferences context.scopeProvider alias
                    |> List.any (targets moduleName)
            then
                errors

            else
                Rule.errorWithFix
                    { message = "Module alias `" ++ getModuleName alias ++ "` is not used"
                    , details = details
                    }
                    aliasRange
                    (case import_.exposingList of
                        Nothing ->
                            [ Fix.removeRange (untilStartOfNextLine range) ]

                        Just _ ->
                            -- Don't remove module alias if there are other imports aliased to the original name
                            if Set.member (getModuleName moduleName) context.moduleNamesInUse then
                                []

                            else
                                [ Fix.removeRange { start = moduleNameRange.end, end = aliasRange.end } ]
                    )
                    :: errors

        Nothing ->
            errors


expressionEnterVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionEnterVisitor (Node range value) context =
    case value of
        Expression.FunctionOrValue [] name ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable range of
                Just [] ->
                    case Dict.get [] context.constructorNameToTypeName |> Maybe.andThen (Dict.get name) of
                        Just typeName ->
                            markValueAsUsed [] typeName context

                        Nothing ->
                            markValueAsUsed [] name context

                Just realModuleName ->
                    context
                        |> markValueAsUsed realModuleName name

                Nothing ->
                    context

        Expression.OperatorApplication name _ _ _ ->
            markAsUsed name context

        Expression.PrefixOperator name ->
            markAsUsed name context

        Expression.RecordUpdateExpression (Node _ expr) _ ->
            markAsUsed expr context

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


letDeclarationEnterVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> ModuleContext -> ( List (Error {}), ModuleContext )
letDeclarationEnterVisitor (Node range { declarations, expression }) (Node declarationRange declaration) context =
    let
        letBlockContext : LetBlockContext
        letBlockContext =
            case declarations of
                [ _ ] ->
                    HasNoOtherDeclarations <| rangeUpUntil range (Node.range expression).start

                _ ->
                    HasMultipleDeclarations
    in
    case declaration of
        Expression.LetFunction function ->
            let
                (Node _ functionDeclaration) =
                    function.declaration

                namesToIgnore : Set String
                namesToIgnore =
                    getDeclaredNamesFromPattern functionDeclaration.arguments Set.empty

                typeAnnotations : List (Node TypeAnnotation)
                typeAnnotations =
                    case function.signature of
                        Just (Node _ signature) ->
                            [ signature.typeAnnotation ]

                        Nothing ->
                            []

                newContext : ModuleContext
                newContext =
                    { context | inTheDeclarationOf = Node.value functionDeclaration.name :: context.inTheDeclarationOf }
                        |> collectNamesFromTypeAnnotation Nothing typeAnnotations
                        |> markValuesFromPatternsAsUsed functionDeclaration.arguments
            in
            ( reportUnusedLetFunction letBlockContext function range newContext
            , { newContext
                | scopes = NonemptyList.cons { declared = Dict.empty, used = Dict.empty, namesToIgnore = namesToIgnore } newContext.scopes
              }
            )

        Expression.LetDestructuring pattern value ->
            case removeParensFromPattern pattern of
                Node wildCardRange Pattern.AllPattern ->
                    if isDebugLogCall context.lookupTable value then
                        ( [], context )

                    else
                        ( [ Rule.errorWithFix
                                { message = "Value assigned to `_` is unused"
                                , details =
                                    [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                                    ]
                                }
                                wildCardRange
                                [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext declarationRange) ]
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
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext declarationRange) ]
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
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext declarationRange) ]
                        ]
                    , markValuesFromPatternsAsUsed [ pattern ] context
                    )


isDebugLogCall : ModuleNameLookupTable -> Node Expression -> Bool
isDebugLogCall lookupTable (Node _ node) =
    case node of
        Expression.Application [ function, _, _ ] ->
            isDebugLog lookupTable function

        Expression.OperatorApplication "|>" _ _ pipeFunction ->
            case removeParensFromExpression pipeFunction of
                Node _ (Expression.Application [ functionWithParens, _ ]) ->
                    isDebugLog lookupTable functionWithParens

                _ ->
                    False

        Expression.OperatorApplication "<|" _ pipeFunction _ ->
            case removeParensFromExpression pipeFunction of
                Node _ (Expression.Application [ functionWithParens, _ ]) ->
                    isDebugLog lookupTable functionWithParens

                _ ->
                    False

        _ ->
            False


isDebugLog : ModuleNameLookupTable -> Node Expression -> Bool
isDebugLog lookupTable node =
    case removeParensFromExpression node of
        Node functionRange (Expression.FunctionOrValue _ "log") ->
            ModuleNameLookupTable.moduleNameAt lookupTable functionRange == Just [ "Debug" ]

        _ ->
            False


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
    case node of
        Node _ (Pattern.ParenthesizedPattern pattern) ->
            removeParensFromPattern pattern

        _ ->
            node


removeParensFromExpression : Node Expression -> Node Expression
removeParensFromExpression node =
    case node of
        Node _ (Expression.ParenthesizedExpression expr) ->
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
                    case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                        Just realModuleName ->
                            context
                                |> markLocalConstructorAsUsed realModuleName qualifiedNameRef
                                |> markValuesFromPatternsAsUsed (patterns ++ restOfNodes)

                        Nothing ->
                            context

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

        (Node _ pattern) :: restOfNodes ->
            case pattern of
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

                Pattern.ParenthesizedPattern subPattern ->
                    introducesVariable (subPattern :: restOfNodes)

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
        constructorNames : List String
        constructorNames =
            List.map (Node.value >> .name >> Node.value) constructors

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
    }


registerTypeAlias : Range -> TypeAlias -> ModuleContext -> ModuleContext
registerTypeAlias range { name, typeAnnotation } context =
    case Node.value typeAnnotation of
        TypeAnnotation.Record _ ->
            if Dict.member (Node.value name) context.exposed then
                context

            else
                registerVariable
                    { typeName = "Type"
                    , under = Node.range name
                    , rangeToRemove = Just (untilStartOfNextLine range)
                    , warning = ""
                    }
                    (Node.value name)
                    context

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
declarationEnterVisitor (Node nodeRange expr) context =
    case expr of
        Declaration.FunctionDeclaration function ->
            let
                functionImplementation : FunctionImplementation
                functionImplementation =
                    Node.value function.declaration

                functionName : String
                functionName =
                    Node.value functionImplementation.name
            in
            case Scope.valueBinding (ScopeProvider.topLevel context.scopeProvider) functionName of
                Nothing ->
                    ( [], context )

                Just binding ->
                    let
                        typeAnnotation : List (Node TypeAnnotation)
                        typeAnnotation =
                            case function.signature of
                                Just signature ->
                                    [ (Node.value signature).typeAnnotation ]

                                Nothing ->
                                    []

                        newContext : ModuleContext
                        newContext =
                            { context
                                | inTheDeclarationOf = [ functionName ]
                                , scopes = NonemptyList.cons (scopeWithPatternsToIgnore functionImplementation.arguments) context.scopes
                            }
                                |> markValuesFromPatternsAsUsed (Node.value function.declaration).arguments
                                |> collectNamesFromTypeAnnotation Nothing typeAnnotation
                    in
                    ( if
                        Dict.member functionName context.exposed
                            -- The main function is "exposed" by default for applications
                            || (context.isApplication && functionName == "main")
                            || not
                                (List.all
                                    (referenceIsUsedFromWithinDefinition (Node.range functionImplementation.expression))
                                    (ScopeProvider.valueReferences context.scopeProvider (Binding.id binding))
                                )
                      then
                        []

                      else
                        [ error
                            { typeName = "Top-level variable"
                            , under = Node.range functionImplementation.name
                            , rangeToRemove = Just (untilStartOfNextLine nodeRange)
                            , warning = ""
                            }
                            (Binding.name binding)
                        ]
                    , newContext
                    )

        Declaration.CustomTypeDeclaration { name, constructors } ->
            let
                arguments : List (Node TypeAnnotation)
                arguments =
                    List.concatMap (\(Node _ c) -> c.arguments) constructors
            in
            ( []
            , collectNamesFromTypeAnnotation (Just (Node.value name)) arguments context
            )

        Declaration.AliasDeclaration { typeAnnotation } ->
            ( []
            , collectNamesFromTypeAnnotation Nothing [ typeAnnotation ] context
            )

        Declaration.PortDeclaration signature ->
            let
                (Node under name) =
                    signature.name
            in
            case Scope.valueBinding (ScopeProvider.topLevel context.scopeProvider) name of
                Nothing ->
                    ( [], context )

                Just binding ->
                    ( if
                        Dict.member name context.exposed
                            || not (List.isEmpty (ScopeProvider.valueReferences context.scopeProvider (Binding.id binding)))
                      then
                        []

                      else
                        [ Rule.error
                            { message = "Port `" ++ name ++ "` is not used (Warning: Removing this port may break your application if it is used in the JS code)"
                            , details = details
                            }
                            under
                        ]
                    , collectNamesFromTypeAnnotation Nothing [ signature.typeAnnotation ] context
                    )

        Declaration.InfixDeclaration { operator, function } ->
            let
                (Node under name) =
                    operator
            in
            ( []
            , context
                |> markAsUsed (Node.value function)
                |> registerVariable
                    { typeName = "Declared operator"
                    , under = under
                    , rangeToRemove = Just nodeRange
                    , warning = ""
                    }
                    name
            )

        Declaration.Destructuring _ _ ->
            ( [], context )



-- DECLARATION EXIT VISITOR


declarationExitVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            let
                scope : Scope
                scope =
                    NonemptyList.head context.scopes

                remainingUsed =
                    Dict.keys scope.declared
                        |> Set.fromList
                        |> Set.diff (usedLocally scope)
                        |> (\set -> Set.diff set scope.namesToIgnore)
                        |> Set.toList

                contextWithPoppedScope : ModuleContext
                contextWithPoppedScope =
                    { context | scopes = NonemptyList.pop context.scopes }
            in
            ( []
            , markAllAsUsed remainingUsed contextWithPoppedScope
            )

        _ ->
            ( [], context )



-- FINAL EVALUATION


finalEvaluation : ModuleContext -> List (Error {})
finalEvaluation context =
    let
        rootScope : Scope
        rootScope =
            NonemptyList.head context.scopes

        usedLocally_ : Set String
        usedLocally_ =
            usedLocally rootScope

        addCustomTypeErrors : List (Error {}) -> List (Error {})
        addCustomTypeErrors acc =
            List.Extra.dictToListFilterAndMap
                (\name -> not <| Set.member name usedLocally_)
                errorForLocalType
                context.localTypes
                acc
    in
    Dict.foldl
        (\key variableInfo acc ->
            if not (Set.member key usedLocally_) then
                error variableInfo key :: acc

            else
                acc
        )
        []
        rootScope.declared
        |> addCustomTypeErrors


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


reportUnusedLetFunction : LetBlockContext -> Function -> Range -> ModuleContext -> List (Rule.Error {})
reportUnusedLetFunction letBlockContext function letDeclarationRange context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        scope : Scope.Scope
        scope =
            (Node.value function.declaration).expression
                |> ScopeProvider.scopeFor context.scopeProvider
    in
    case Scope.valueBinding scope (Node.value declaration.name) of
        Just binding ->
            reportUnusedLetBinding
                context
                letBlockContext
                letDeclarationRange
                (Node.value function.declaration).expression
                (Node.range declaration.name)
                binding

        Nothing ->
            []


reportUnusedLetBinding :
    ModuleContext
    -> LetBlockContext
    -> Range
    -> Node Expression
    -> Range
    -> Binding
    -> List (Error {})
reportUnusedLetBinding context letBlockContext letDeclarationRange (Node expressionRange _) declarationName binding =
    if
        List.all
            (referenceIsUsedFromWithinDefinition expressionRange)
            (ScopeProvider.valueReferences context.scopeProvider (Binding.id binding))
    then
        [ error
            { typeName = "`let in` variable"
            , under = declarationName
            , rangeToRemove = Just (letDeclarationToRemoveRange letBlockContext letDeclarationRange)
            , warning = ""
            }
            (Binding.name binding)
        ]

    else
        []


referenceIsUsedFromWithinDefinition : Range -> Reference -> Bool
referenceIsUsedFromWithinDefinition definitionRange reference =
    isRangeIncludedIn (Reference.range reference) definitionRange


isRangeIncludedIn : Range -> Range -> Bool
isRangeIncludedIn inner outer =
    case compareLocation inner.start outer.start of
        LT ->
            False

        _ ->
            case compareLocation inner.end outer.end of
                GT ->
                    False

                _ ->
                    True


compareLocation : Location -> Location -> Order
compareLocation a b =
    case Basics.compare a.row b.row of
        EQ ->
            Basics.compare a.column b.column

        ltOrGt ->
            ltOrGt


scopeWithPatternsToIgnore : List (Node Pattern) -> Scope
scopeWithPatternsToIgnore patterns =
    { declared = Dict.empty
    , used = Dict.empty
    , namesToIgnore = getDeclaredNamesFromPattern patterns Set.empty
    }


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

                TypeAnnotation.Typed (Node _ ( rawModuleName, typeName )) params ->
                    let
                        newContext : ModuleContext
                        newContext =
                            if Just typeName /= exception then
                                case rawModuleName of
                                    [] ->
                                        markAsUsed typeName context

                                    _ ->
                                        context

                            else
                                context
                    in
                    collectNamesFromTypeAnnotation
                        exception
                        (params ++ restOfNodes)
                        newContext

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


reportUnusedModule : Node Import -> ModuleContext -> List (Rule.Error {}) -> List (Rule.Error {})
reportUnusedModule ((Node _ import_) as node) context errors =
    let
        (Node _ moduleName) =
            import_.moduleName
    in
    case ScopeProvider.qualifiedReferences context.scopeProvider moduleName of
        _ :: _ ->
            errors

        [] ->
            unusedImportError node :: errors


unusedImportError : Node Import -> Error {}
unusedImportError (Node importRange import_) =
    let
        (Node moduleNameRange moduleName) =
            import_.moduleName
    in
    Rule.errorWithFix
        { message = "Imported module `" ++ String.join "." moduleName ++ "` is not used"
        , details = details
        }
        moduleNameRange
        [ Fix.removeRange (untilStartOfNextLine importRange) ]


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


markValueAsUsed : ModuleName -> String -> ModuleContext -> ModuleContext
markValueAsUsed moduleName name context =
    if List.isEmpty moduleName then
        markAsUsed name context

    else
        case
            Dict.get moduleName context.constructorNameToTypeName
                |> Maybe.andThen (Dict.get name)
        of
            Just _ ->
                context

            Nothing ->
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


markLocalConstructorAsUsed : ModuleName -> Pattern.QualifiedNameRef -> ModuleContext -> ModuleContext
markLocalConstructorAsUsed realModuleName qualifiedNameRef context =
    case qualifiedNameRef.moduleName of
        [] ->
            case
                Dict.get [] context.constructorNameToTypeName
                    |> Maybe.andThen (Dict.get qualifiedNameRef.name)
            of
                Just typeName ->
                    markValueAsUsed realModuleName typeName context

                Nothing ->
                    markValueAsUsed realModuleName qualifiedNameRef.name context

        _ ->
            context


getModuleName : List String -> String
getModuleName name =
    String.join "." name


usedLocally : Scope -> Set String
usedLocally { used } =
    Dict.get [] used
        |> Maybe.withDefault Set.empty



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
