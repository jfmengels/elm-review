module Review.Resources.ScopeProvider exposing
    ( ScopeProvider
    , topLevel, parent, scopeAt, scopeFor
    , moduleNameForType, moduleNameForValue, fullModuleNameForType, fullModuleNameForValue
    , importValueBinding, importTypeBinding
    , typeBinding
    , Reference, typeReferences, valueReferences, moduleReferences, qualifiedReferences
    )

{-| REPLACEME

@docs ScopeProvider
@docs topLevel, parent, scopeAt, scopeFor
@docs moduleNameForType, moduleNameForValue, fullModuleNameForType, fullModuleNameForValue
@docs importValueBinding, importTypeBinding
@docs typeBinding
@docs Reference, typeReferences, valueReferences, moduleReferences, qualifiedReferences

-}

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Review.Resources.BindingId as BindingId
import Review.Resources.ImportBinding exposing (ImportBinding)
import Review.Resources.Reference as Reference
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeInternal as Internal exposing (Binding, ScopeInfo)


type alias ScopeProvider =
    Internal.ScopeProvider


type alias Reference =
    Reference.Reference


topLevel : ScopeProvider -> Scope
topLevel (Internal.ScopeProvider scopeProvider) =
    Internal.Scope scopeProvider.topLevel


scopeAt : ScopeProvider -> Range -> Scope
scopeAt (Internal.ScopeProvider scopeProvider) range =
    scopeAtHelp
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


scopeAtHelp : Range -> ScopeInfo -> List Scope -> Scope
scopeAtHelp range bestMatch children =
    case children of
        [] ->
            Internal.Scope bestMatch

        (Internal.Scope scope) :: rest ->
            case Range.compareLocations range.start scope.range.start of
                LT ->
                    scopeAtHelp range bestMatch rest

                _ ->
                    case Range.compareLocations range.end scope.range.end of
                        GT ->
                            Internal.Scope bestMatch

                        _ ->
                            scopeAtHelp range scope scope.children


scopeFor : ScopeProvider -> Node a -> Scope
scopeFor (Internal.ScopeProvider scopeProvider) (Node range _) =
    scopeAtHelp
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


parent : ScopeProvider -> Scope -> Scope
parent (Internal.ScopeProvider scopeProvider) ((Internal.Scope scope) as untouched) =
    case
        Maybe.andThen
            (\parentId -> Dict.get parentId scopeProvider.scopes)
            scope.parentId
    of
        Just parentScope ->
            Internal.Scope parentScope

        Nothing ->
            untouched


typeBinding : ScopeProvider -> String -> Maybe Binding
typeBinding (Internal.ScopeProvider scopeProvider) name =
    Dict.get name scopeProvider.topLevel.typeBindings


importValueBinding : ScopeProvider -> String -> Maybe ImportBinding
importValueBinding (Internal.ScopeProvider scopeProvider) name =
    Dict.get name scopeProvider.topLevel.importValueBindings


importTypeBinding : ScopeProvider -> String -> Maybe ImportBinding
importTypeBinding (Internal.ScopeProvider scopeProvider) name =
    Dict.get name scopeProvider.topLevel.importTypeBindings


{-| All references to a value.
-}
valueReferences : ScopeProvider -> BindingId.Id -> List Reference
valueReferences (Internal.ScopeProvider scopeProvider) (BindingId.Id id) =
    Dict.get id scopeProvider.valueReferences
        |> Maybe.withDefault []


{-| All references to a type.
-}
typeReferences : Internal.ScopeProvider -> BindingId.Id -> List Reference
typeReferences (Internal.ScopeProvider scopeProvider) (BindingId.Id id) =
    Dict.get id scopeProvider.typeReferences
        |> Maybe.withDefault []


{-| All references to elements from a module through the use of
a qualified import with the given name, e.g.:

    import A
    import Some.Import as Alias

    list : List A.Type
    list =
        [ Alias.value ]

-}
qualifiedReferences : ScopeProvider -> ModuleName -> List Reference
qualifiedReferences (Internal.ScopeProvider scopeProvider) moduleName =
    Dict.get moduleName scopeProvider.qualifiedReferences
        |> Maybe.withDefault []


{-| All references to elements from a module.
-}
moduleReferences : ScopeProvider -> ModuleName -> List Reference
moduleReferences (Internal.ScopeProvider scopeProvider) moduleName =
    Dict.get moduleName scopeProvider.moduleReferences
        |> Maybe.withDefault []


moduleNameForType : ScopeProvider -> ModuleName -> String -> ModuleName
moduleNameForType (Internal.ScopeProvider scopeProvider) moduleName typeName =
    Scope.moduleNameForType (Internal.Scope scopeProvider.topLevel) moduleName typeName


fullModuleNameForType : ScopeProvider -> ModuleName -> String -> ModuleName
fullModuleNameForType (Internal.ScopeProvider scopeProvider) moduleName typeName =
    Scope.fullModuleNameForType (Internal.Scope scopeProvider.topLevel) moduleName typeName


moduleNameForValue : ScopeProvider -> Range -> ModuleName -> String -> ModuleName
moduleNameForValue ((Internal.ScopeProvider s) as scopeProvider) range moduleName typeName =
    Scope.moduleNameForValue
        (case moduleName of
            [] ->
                scopeAt scopeProvider range

            _ ->
                Internal.Scope s.topLevel
        )
        moduleName
        typeName


fullModuleNameForValue : ScopeProvider -> Range -> ModuleName -> String -> ModuleName
fullModuleNameForValue ((Internal.ScopeProvider s) as scopeProvider) range moduleName typeName =
    Scope.fullModuleNameForValue
        (case moduleName of
            [] ->
                scopeAt scopeProvider range

            _ ->
                Internal.Scope s.topLevel
        )
        moduleName
        typeName
