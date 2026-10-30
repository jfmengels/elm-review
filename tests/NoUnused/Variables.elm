module NoUnused.Variables exposing (rule)

{-| Report variables or types that are declared or imported but never used inside of a module.

@docs rule

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import List.Extra
import NoUnused.Variables.ElmPrelude as ElmPrelude
import Review.Fix as Fix
import Review.Resources.Binding as Binding exposing (Binding)
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.Reference as Reference exposing (Reference)
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeInternal
import Review.Resources.ScopeProvider as ScopeProvider exposing (ScopeProvider)
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
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnused.Variables" fromProjectToModule
        |> Rule.withImportVisitor (\node context -> ( importVisitor node context, context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( declarationEnterVisitor node context, context ))
        |> Rule.withLetDeclarationEnterVisitor (\letBlock letDecl context -> ( letDeclarationEnterVisitor letBlock letDecl context, context ))
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { scopeProvider : ScopeProvider
    , exposed : Dict String Bool
    , isApplication : Bool
    , moduleNamesInUse : Set String
    , moduleDocs : Dict ModuleName Elm.Docs.Module
    }


type alias ImportedCustomType =
    { rangeToRemove : Range
    , openRange : Range
    }


type LetBlockContext
    = HasMultipleDeclarations
    | HasNoOtherDeclarations Range


fromProjectToModule : Rule.ContextCreator () Context
fromProjectToModule =
    Rule.initContextCreator
        (\scopeProvider { exposed } isModuleExposed ast moduleDocs () ->
            { scopeProvider = scopeProvider
            , exposed = exposed
            , isApplication = isModuleExposed == Nothing
            , moduleNamesInUse = collectModuleNamesInUse ast.imports Set.empty
            , moduleDocs = moduleDocs
            }
        )
        |> Rule.withScopes
        |> Rule.withExposed
        |> Rule.withIsModuleExposed
        |> Rule.withFullAst
        |> Rule.withImportedModuleDocs { implicitImports = False, dependencies = True }


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


type alias VariableInfo =
    { typeName : String
    , under : Range
    , rangeToRemove : Range
    }


error : VariableInfo -> String -> Error {}
error variableInfo name =
    Rule.errorWithFix
        { message = variableInfo.typeName ++ " `" ++ name ++ "` is not used"
        , details = details
        }
        variableInfo.under
        [ Fix.removeRange variableInfo.rangeToRemove ]


details : List String
details =
    [ "You should either use this value somewhere, or remove it at the location I pointed at."
    ]


importVisitor : Node Import -> Context -> List (Error {})
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


reportImport : Node { moduleName : Node ModuleName, moduleAlias : Maybe (Node ModuleName), exposingList : Maybe (Node Exposing.Exposing) } -> Context -> List (Error {}) -> List (Error {})
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


explicitExposingAllErrors : Context -> Range -> Node Import -> List (Rule.Error {}) -> List (Rule.Error {})
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
    Context
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


reportImportedTypeOrValue : Context -> String -> VariableInfo -> List (Error {}) -> List (Error {})
reportImportedTypeOrValue context name variableInfo errors =
    if
        isImportedValueReferenced context.scopeProvider name
            || isImportedTypeReferenced context.scopeProvider name
    then
        errors

    else
        error variableInfo name :: errors


collectExplicitlyExposedElements :
    Context
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
                                    , rangeToRemove = rangeToRemove
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
                                    , rangeToRemove = rangeToRemove
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
                                    , rangeToRemove = rangeToRemove
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
        Review.Resources.ScopeInternal.ImportBinding_ importBinding ->
            ImportBinding.moduleName importBinding == moduleName

        Review.Resources.ScopeInternal.QualifiedReference r ->
            r.moduleName == moduleName

        Review.Resources.ScopeInternal.LocalBinding _ ->
            False


reportUnusedModuleAlias : Node Import -> Maybe (Node ModuleName) -> Context -> List (Rule.Error {}) -> List (Rule.Error {})
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


letDeclarationEnterVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> Context -> List (Error {})
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
            reportUnusedLetFunction letBlockContext function declarationRange context

        Expression.LetDestructuring pattern value ->
            case removeParensFromPattern pattern of
                Node wildCardRange Pattern.AllPattern ->
                    if isDebugLogCall context.scopeProvider value then
                        []

                    else
                        [ Rule.errorWithFix
                            { message = "Value assigned to `_` is unused"
                            , details =
                                [ "This value has been assigned to a wildcard, which makes the value unusable. You should remove it at the location I pointed at."
                                ]
                            }
                            wildCardRange
                            [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext declarationRange) ]
                        ]

                Node unitPattern Pattern.UnitPattern ->
                    [ Rule.errorWithFix
                        { message = "Unit value is unused"
                        , details =
                            [ "This value has no data, which makes the value unusable. You should remove it at the location I pointed at."
                            ]
                        }
                        unitPattern
                        [ Fix.removeRange (letDeclarationToRemoveRange letBlockContext declarationRange) ]
                    ]

                _ ->
                    if introducesVariable [ pattern ] then
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


isDebugLogCall : ScopeProvider -> Node Expression -> Bool
isDebugLogCall scopeProvider (Node _ node) =
    case node of
        Expression.Application [ function, _, _ ] ->
            isDebugLog scopeProvider function

        Expression.OperatorApplication "|>" _ _ pipeFunction ->
            case removeParensFromExpression pipeFunction of
                Node _ (Expression.Application [ functionWithParens, _ ]) ->
                    isDebugLog scopeProvider functionWithParens

                _ ->
                    False

        Expression.OperatorApplication "<|" _ pipeFunction _ ->
            case removeParensFromExpression pipeFunction of
                Node _ (Expression.Application [ functionWithParens, _ ]) ->
                    isDebugLog scopeProvider functionWithParens

                _ ->
                    False

        _ ->
            False


isDebugLog : ScopeProvider -> Node Expression -> Bool
isDebugLog scopeProvider node =
    case removeParensFromExpression node of
        Node functionRange (Expression.FunctionOrValue moduleName "log") ->
            case ScopeProvider.moduleNameForValue scopeProvider functionRange moduleName "log" of
                [ "Debug" ] ->
                    True

                _ ->
                    False

        _ ->
            False


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


reportCustomType :
    Context
    -> Type.Type
    -> Range
    -> List (Error {})
reportCustomType context type_ nodeRange =
    let
        (Node nameRange name) =
            type_.name
    in
    case ScopeProvider.typeBinding context.scopeProvider name of
        Nothing ->
            []

        Just binding ->
            let
                isConstructorReferenced : Node { a | name : Node String } -> Bool
                isConstructorReferenced (Node _ constructor) =
                    case Scope.getBinding (ScopeProvider.topLevel context.scopeProvider) (Node.value constructor.name) of
                        Nothing ->
                            False

                        Just constructorBinding ->
                            not (List.isEmpty (ScopeProvider.valueReferences context.scopeProvider (Binding.id constructorBinding)))
            in
            if
                Dict.member name context.exposed
                    || not
                        (List.all
                            (referenceIsUsedFromWithinDefinition nodeRange)
                            (ScopeProvider.typeReferences context.scopeProvider (Binding.id binding))
                        )
                    || List.any isConstructorReferenced type_.constructors
            then
                []

            else
                [ Rule.errorWithFix
                    { message = "Type `" ++ name ++ "` is not used"
                    , details = details
                    }
                    nameRange
                    [ Fix.removeRange (untilStartOfNextLine nodeRange) ]
                ]


registerTypeAlias : Context -> TypeAlias -> Range -> List (Rule.Error {})
registerTypeAlias context alias nodeRange =
    let
        (Node nameRange name) =
            alias.name
    in
    case ScopeProvider.typeBinding context.scopeProvider name of
        Nothing ->
            []

        Just binding ->
            if
                Dict.member name context.exposed
                    || not (List.isEmpty (ScopeProvider.typeReferences context.scopeProvider (Binding.id binding)))
                    || not (List.isEmpty (ScopeProvider.valueReferences context.scopeProvider (Binding.id binding)))
            then
                []

            else
                [ Rule.errorWithFix
                    { message = "Type alias `" ++ name ++ "` is not used"
                    , details = details
                    }
                    nameRange
                    [ Fix.removeRange (untilStartOfNextLine nodeRange) ]
                ]



-- DECLARATION ENTER VISITOR


declarationEnterVisitor : Node Declaration -> Context -> List (Error {})
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
            case Scope.getBinding (ScopeProvider.topLevel context.scopeProvider) functionName of
                Nothing ->
                    []

                Just binding ->
                    if
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
                            , rangeToRemove = untilStartOfNextLine nodeRange
                            }
                            (Binding.name binding)
                        ]

        Declaration.CustomTypeDeclaration type_ ->
            reportCustomType context type_ nodeRange

        Declaration.AliasDeclaration typeAliasDeclaration ->
            registerTypeAlias context typeAliasDeclaration nodeRange

        Declaration.PortDeclaration signature ->
            let
                (Node under name) =
                    signature.name
            in
            case Scope.getBinding (ScopeProvider.topLevel context.scopeProvider) name of
                Nothing ->
                    []

                Just binding ->
                    if
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

        Declaration.InfixDeclaration { operator } ->
            let
                (Node under name) =
                    operator
            in
            case Scope.getBinding (ScopeProvider.topLevel context.scopeProvider) name of
                Nothing ->
                    []

                Just binding ->
                    if
                        Dict.member name context.exposed
                            || not (List.isEmpty (ScopeProvider.valueReferences context.scopeProvider (Binding.id binding)))
                    then
                        []

                    else
                        [ error
                            { typeName = "Declared operator"
                            , under = under
                            , rangeToRemove = nodeRange
                            }
                            name
                        ]

        Declaration.Destructuring _ _ ->
            []


reportUnusedLetFunction : LetBlockContext -> Function -> Range -> Context -> List (Rule.Error {})
reportUnusedLetFunction letBlockContext function letDeclarationRange context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        scope : Scope
        scope =
            (Node.value function.declaration).expression
                |> ScopeProvider.for context.scopeProvider
    in
    case Scope.getBinding scope (Node.value declaration.name) of
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
    Context
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
            , rangeToRemove = letDeclarationToRemoveRange letBlockContext letDeclarationRange
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


untilEndOfVariable : String -> Range -> Range
untilEndOfVariable name range =
    if range.start.row == range.end.row then
        range

    else
        { range | end = { row = range.start.row, column = range.start.column + String.length name } }


reportUnusedModule : Node Import -> Context -> List (Rule.Error {}) -> List (Rule.Error {})
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


getModuleName : List String -> String
getModuleName name =
    String.join "." name



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
