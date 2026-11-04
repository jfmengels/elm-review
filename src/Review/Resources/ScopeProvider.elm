module Review.Resources.ScopeProvider exposing
    ( ScopeProvider, topLevel
    , Reference, at, for, fullModuleNameForType, fullModuleNameForValue, importTypeBinding, importValueBinding, moduleNameForType, moduleNameForValue, moduleReferences, parent, qualifiedReferences, qualifyModuleNameForType, typeBinding, typeReferences, valueReferences
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Review.Resources.Binding exposing (Binding)
import Review.Resources.BindingId as BindingId
import Review.Resources.ImportBinding exposing (ImportBinding)
import Review.Resources.Scope as Scope exposing (Scope)
import Review.Resources.ScopeInternal as Internal exposing (ScopeInfo)


type alias ScopeProvider =
    Internal.ScopeProvider


type alias Reference =
    Internal.Reference


topLevel : ScopeProvider -> Scope
topLevel (Internal.ScopeProvider scopeProvider) =
    Internal.Scope scopeProvider.topLevel


at : ScopeProvider -> Range -> Scope
at (Internal.ScopeProvider scopeProvider) range =
    atHelp
        range
        scopeProvider.topLevel
        scopeProvider.topLevel.children


atHelp : Range -> ScopeInfo -> List Scope -> Scope
atHelp range bestMatch children =
    case children of
        [] ->
            Internal.Scope bestMatch

        (Internal.Scope scope) :: rest ->
            if isRangeIncludedIn range scope.range then
                atHelp range scope scope.children

            else
                atHelp range bestMatch rest


for : ScopeProvider -> Node a -> Scope
for (Internal.ScopeProvider scopeProvider) (Node range _) =
    atHelp
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
                at scopeProvider range

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
                at scopeProvider range

            _ ->
                Internal.Scope s.topLevel
        )
        moduleName
        typeName


qualifyModuleNameForType : ScopeProvider -> ModuleName -> String -> ModuleName
qualifyModuleNameForType (Internal.ScopeProvider scopeProvider) moduleName typeName =
    Scope.qualifyModuleNameForType (Internal.Scope scopeProvider.topLevel) moduleName typeName
