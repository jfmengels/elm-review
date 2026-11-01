module Review.Resources.Scope exposing
    ( Scope
    , bindings
    , canIntroduceName
    , children
    , fullModuleNameForType
    , fullModuleNameForValue
    , getBinding
    , moduleNameForType
    , moduleNameForValue
    , names
    , parentId
    , qualifyModuleNameForType
    , qualifyModuleNameForValue
    , range
    )

{-| REPLACEME

@docs ScopeProvider, topLevel

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import NonEmpty
import Review.Resources.Binding exposing (Binding)
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.ScopeInternal as Internal
import Set exposing (Set)


type alias Scope =
    Internal.Scope


names : Scope -> Set String
names (Internal.Scope scope) =
    Dict.foldl (\name _ set -> Set.insert name set) Set.empty scope.valueBindings


bindings : Scope -> Dict String Binding
bindings (Internal.Scope scope) =
    scope.valueBindings


range : Scope -> Range
range (Internal.Scope scope) =
    scope.range


parentId : Scope -> Maybe Internal.ScopeId
parentId (Internal.Scope scope) =
    scope.parentId


children : Scope -> List Scope
children (Internal.Scope scope) =
    scope.children


getBinding : Scope -> String -> Maybe Binding
getBinding (Internal.Scope scope) name =
    getBindingHelp (scope.valueBindings :: scope.parentBindings) name


getBindingHelp : List (Dict String Binding) -> String -> Maybe Binding
getBindingHelp bindingsStack name =
    case bindingsStack of
        [] ->
            Nothing

        bindings_ :: rest ->
            case Dict.get name bindings_ of
                Nothing ->
                    getBindingHelp rest name

                just ->
                    just


moduleNameForValue : Scope -> ModuleName -> String -> ModuleName
moduleNameForValue (Internal.Scope scope) moduleName name =
    case moduleName of
        [] ->
            if isValueInScope scope name then
                []

            else
                case Dict.get name scope.importValueBindings of
                    Just importBinding ->
                        ImportBinding.moduleName importBinding

                    Nothing ->
                        []

        [ moduleNameOrAlias ] ->
            case Dict.get moduleNameOrAlias scope.aliasToModuleName of
                Just aliases ->
                    NonEmpty.findWithHeadAsDefault
                        (\aliasedModuleName ->
                            case Dict.get aliasedModuleName scope.modules of
                                Just module_ ->
                                    isValueDeclaredInModule name module_

                                Nothing ->
                                    False
                        )
                        aliases

                Nothing ->
                    moduleName

        _ ->
            moduleName


fullModuleNameForValue : Scope -> ModuleName -> String -> ModuleName
fullModuleNameForValue (Internal.Scope scope) moduleName name =
    case moduleNameForValue (Internal.Scope scope) moduleName name of
        [] ->
            scope.moduleName

        realModuleName ->
            realModuleName


isValueDeclaredInModule : String -> Elm.Docs.Module -> Bool
isValueDeclaredInModule valueName module_ =
    List.any (.name >> (==) valueName) module_.values
        || List.any (.name >> (==) valueName) module_.aliases
        || List.any
            (\union -> List.any (Tuple.first >> (==) valueName) union.tags)
            module_.unions


moduleNameForType : Scope -> ModuleName -> String -> ModuleName
moduleNameForType (Internal.Scope scope) moduleName typeName =
    case moduleName of
        [] ->
            if Dict.member typeName scope.typeBindings then
                []

            else
                case Dict.get typeName scope.importTypeBindings of
                    Just importBinding ->
                        ImportBinding.moduleName importBinding

                    Nothing ->
                        []

        [ moduleNameOrAlias ] ->
            case Dict.get moduleNameOrAlias scope.aliasToModuleName of
                Just aliases ->
                    NonEmpty.findWithHeadAsDefault
                        (\aliasedModuleName ->
                            case Dict.get aliasedModuleName scope.modules of
                                Just module_ ->
                                    isTypeDeclaredInModule typeName module_

                                Nothing ->
                                    False
                        )
                        aliases

                Nothing ->
                    moduleName

        _ ->
            moduleName


fullModuleNameForType : Scope -> ModuleName -> String -> ModuleName
fullModuleNameForType (Internal.Scope scope) moduleName typeName =
    case moduleNameForType (Internal.Scope scope) moduleName typeName of
        [] ->
            scope.moduleName

        realModuleName ->
            realModuleName


isTypeDeclaredInModule : String -> Elm.Docs.Module -> Bool
isTypeDeclaredInModule typeName module_ =
    List.any (.name >> (==) typeName) module_.aliases
        || List.any (.name >> (==) typeName) module_.unions


canIntroduceName : Scope -> String -> Bool
canIntroduceName (Internal.Scope scope) name =
    -- TODO Should check for bindings in children as well
    not (isValueInScope scope name)


isValueInScope : Internal.ScopeInfo -> String -> Bool
isValueInScope scope name =
    Dict.member name scope.valueBindings
        || List.any (Dict.member name) scope.parentBindings


qualifyModuleNameForValue : Scope -> ModuleName -> String -> ModuleName
qualifyModuleNameForValue (Internal.Scope scope) moduleName valueName =
    if moduleName == [] || moduleName == scope.moduleName then
        []

    else if isValueInScope scope valueName then
        -- Value is shadowed locally, therefore use a qualified import
        getModuleQualifier scope moduleName

    else
        case Dict.get valueName scope.importValueBindings of
            Just importBinding ->
                -- Value has been imported
                if ImportBinding.moduleName importBinding == moduleName then
                    -- by the target module
                    []

                else
                    -- by another module, therefore use a qualified import
                    getModuleQualifier scope moduleName

            Nothing ->
                -- Value is not imported through `exposing (..)` or `exposing (value)`, therefore use a qualified import
                getModuleQualifier scope moduleName


qualifyModuleNameForType : Scope -> ModuleName -> String -> ModuleName
qualifyModuleNameForType (Internal.Scope scope) moduleName typeName =
    if moduleName == [] || moduleName == scope.moduleName then
        []

    else
        case Dict.get typeName scope.typeBindings of
            Just _ ->
                -- Type is shadowed locally, therefore use a qualified import
                getModuleQualifier scope moduleName

            Nothing ->
                case Dict.get typeName scope.importTypeBindings of
                    Just importBinding ->
                        -- Type has been imported
                        if ImportBinding.moduleName importBinding == moduleName then
                            -- by the target module
                            []

                        else
                            -- by another module, therefore use a qualified import
                            getModuleQualifier scope moduleName

                    Nothing ->
                        -- Type is not imported through `exposing (..)` or `exposing (Type)`, therefore use a qualified import
                        getModuleQualifier scope moduleName


getModuleQualifier : Internal.ScopeInfo -> ModuleName -> ModuleName
getModuleQualifier scope moduleName =
    Dict.get moduleName scope.moduleQualifiers
        |> Maybe.withDefault moduleName
