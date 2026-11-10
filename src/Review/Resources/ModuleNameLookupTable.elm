module Review.Resources.ModuleNameLookupTable exposing (compute)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import NonEmpty exposing (NonEmpty)
import Review.Project.DocsModule as DocsModule exposing (DocsModule)
import Review.Project.ProjectCache as ProjectCache exposing (ModuleCacheKey, ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Resources.Binding as Binding
import Review.Resources.BindingId as BindingId
import Review.Resources.ElmCorePrelude
import Review.Resources.ImportBinding as ImportBinding
import Review.Resources.Internal exposing (ModuleNameLookupTable)
import Review.Resources.Internal.Bindings as Bindings exposing (Binding(..), ImportBinding(..))
import Review.Resources.ModuleNameLookupTableBuilder as Builder exposing (ModuleNameLookupTableBuilder)
import Review.Resources.Scope as Scope
import Review.Resources.ScopeId as ScopeId
import Review.Resources.ScopeInternal as ScopeInternal exposing (Reference, ScopeId, ScopeInfo, SharedScopeInfo)
import Review.Resources.ScopeOrigin as ScopeOrigin exposing (ScopeOrigin)
import Review.Resources.ScopeProvider exposing (ScopeProvider)
import Review.Resources.Target as Target exposing (Target)
import Vendor.ListExtra as ListExtra


type alias Context =
    { scopeStack : NonEmpty ScopeInfo
    , scopeIdCounter : ScopeId.Counter
    , bindingIdCounter : BindingId.Counter
    , valueReferences : Dict Int (List Reference)
    , typeReferences : Dict Int (List Reference)
    , qualifiedReferences : Dict ModuleName (List Reference)
    , moduleReferences : Dict ModuleName (List Reference)
    , scopes : Dict ScopeId ScopeInfo
    , aliasToModuleName : Dict String (NonEmpty ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ImportBinding
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTable : ModuleNameLookupTableBuilder
    , branches : NonEmpty ( Range, ScopeInfo )
    }


compute : ModuleName -> OpaqueProjectModule -> ProjectCache -> ( ModuleNameLookupTable, ScopeProvider, ProjectCache )
compute moduleName module_ projectCache =
    -- Caching layer. Real computation is in computeHelp
    let
        {- This will be used as the cache key in terms of the imports.
           Since we assume that the code will be compiling at every stage, the only thing that causes the
           lookup table for a given module to be recomputed, are:
           1) Whether the module itself has changed (because the position of elements might have changed)
           2) Whether the dependencies have changed, in which case we in practice nuke the cache because that's easier and it's a rare case.
           3) If the exposed elements of the module's imports have changed.

           This data is about 3). In practice and because of how this algorithm is computed,
           if we have `import A exposing (a, b, C, D(..))`, then only a change to D's constructors
           can cause the lookup table to be different. So we only need to store the names of the elements that were
           imported "implicitly", through `exposing (..)` or `exposing (D(..))`.
        -}
        implicitImports : Dict String (List ProjectCache.ImportedElement)
        implicitImports =
            List.foldl
                (\node acc -> computeImplicitlyImportedElements projectCache.modules node acc)
                Dict.empty
                (ProjectModule.ast module_).imports

        cacheKey : ModuleCacheKey
        cacheKey =
            { implicitImports = implicitImports
            , contentHash = ProjectModule.contentHash module_
            }
    in
    case reuseFromCache cacheKey moduleName projectCache.lookupTables of
        Just { lookupTable, scopeProvider } ->
            ( lookupTable, scopeProvider, projectCache )

        Nothing ->
            let
                ( lookupTable, scopeProvider ) =
                    computeHelp moduleName (ProjectModule.ast module_) projectCache
            in
            ( lookupTable
            , scopeProvider
            , { projectCache
                | lookupTables =
                    Dict.insert moduleName
                        { key = cacheKey
                        , lookupTable = lookupTable
                        , scopeProvider = scopeProvider
                        }
                        projectCache.lookupTables
              }
            )


reuseFromCache :
    ModuleCacheKey
    -> ModuleName
    -> Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable, scopeProvider : ScopeProvider }
    -> Maybe { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable, scopeProvider : ScopeProvider }
reuseFromCache cacheKey moduleName lookupTables =
    case Dict.get moduleName lookupTables of
        Just cache ->
            if cache.key.contentHash == cacheKey.contentHash && cache.key.implicitImports == cacheKey.implicitImports then
                Just cache

            else
                Nothing

        Nothing ->
            Nothing


computeHelp : ModuleName -> Elm.Syntax.File.File -> ProjectCache -> ( ModuleNameLookupTable, ScopeProvider )
computeHelp moduleName moduleAst projectCache =
    let
        modules : Dict ModuleName Elm.Docs.Module
        modules =
            Dict.foldl
                (\key docs dict ->
                    Dict.insert key (DocsModule.mod docs) dict
                )
                projectCache.dependencies
                projectCache.modules

        importAcc : ImportAcc
        importAcc =
            List.foldl
                (importVisitor modules)
                (collectPreludeImports projectCache.dependencies)
                moduleAst.imports

        baseModuleContext : Context
        baseModuleContext =
            computeBaseModule
                moduleName
                modules
                (declarationListVisitor moduleAst.declarations importAcc.contents.bindingIdCounter)
                importAcc

        moduleContext : Context
        moduleContext =
            collectReferences moduleAst.declarations baseModuleContext

        topLevelScope : ScopeInfo
        topLevelScope =
            NonEmpty.head moduleContext.scopeStack

        scopes : Dict ScopeId ScopeInfo
        scopes =
            Dict.insert topLevelScope.id topLevelScope moduleContext.scopes
    in
    ( Builder.finalize moduleName moduleContext.lookupTable
    , ScopeInternal.create
        topLevelScope
        scopes
        moduleContext.valueReferences
        moduleContext.typeReferences
        moduleContext.qualifiedReferences
        moduleContext.moduleReferences
    )


computeImplicitlyImportedElements :
    Dict ModuleName DocsModule
    -> Node Import
    -> Dict String (List ProjectCache.ImportedElement)
    -> Dict String (List ProjectCache.ImportedElement)
computeImplicitlyImportedElements modules (Node _ import_) acc =
    case import_.exposingList of
        Nothing ->
            acc

        Just (Node _ (Exposing.Explicit list)) ->
            case Dict.get (Node.value import_.moduleName) modules of
                Just moduleDocs ->
                    collectExplicit (DocsModule.mod moduleDocs) list acc

                Nothing ->
                    acc

        Just (Node _ (Exposing.All _)) ->
            case Dict.get (Node.value import_.moduleName) modules of
                Just moduleDocs ->
                    collectAllExposed (DocsModule.mod moduleDocs) acc

                Nothing ->
                    acc


collectExplicit : Elm.Docs.Module -> List (Node TopLevelExpose) -> Dict String (List ProjectCache.ImportedElement) -> Dict String (List ProjectCache.ImportedElement)
collectExplicit moduleDocs list acc =
    let
        importedConstructors : List ProjectCache.ImportedElement
        importedConstructors =
            List.foldl
                (\(Node _ node) subAcc ->
                    case node of
                        Exposing.TypeExpose { name } ->
                            case ListExtra.find (\union -> union.name == name) moduleDocs.unions of
                                Just union ->
                                    insertConstructors union.tags subAcc

                                Nothing ->
                                    subAcc

                        _ ->
                            subAcc
                )
                (Dict.get moduleDocs.name acc |> Maybe.withDefault [])
                list
    in
    Dict.insert moduleDocs.name importedConstructors acc


collectAllExposed : Elm.Docs.Module -> Dict String (List ProjectCache.ImportedElement) -> Dict String (List ProjectCache.ImportedElement)
collectAllExposed moduleDocs acc =
    let
        importedElements : List ProjectCache.ImportedElement
        importedElements =
            Dict.get moduleDocs.name acc
                |> Maybe.withDefault []
                |> collectAllValues moduleDocs.values
                |> collectAllAliases moduleDocs.aliases
                |> collectAllTypes moduleDocs.unions
    in
    Dict.insert moduleDocs.name importedElements acc


collectAllValues : List Elm.Docs.Value -> List ProjectCache.ImportedElement -> List ProjectCache.ImportedElement
collectAllValues values acc =
    List.foldl
        (\{ name } subAcc -> ProjectCache.valueElement name :: subAcc)
        acc
        values


collectAllAliases : List Elm.Docs.Alias -> List ProjectCache.ImportedElement -> List ProjectCache.ImportedElement
collectAllAliases values acc =
    List.foldl
        (\{ name, tipe } subAcc ->
            ProjectCache.typeElement name
                :: (case tipe of
                        Elm.Type.Record _ Nothing ->
                            ProjectCache.valueElement name :: subAcc

                        _ ->
                            subAcc
                   )
        )
        acc
        values


collectAllTypes : List Elm.Docs.Union -> List ProjectCache.ImportedElement -> List ProjectCache.ImportedElement
collectAllTypes unions acc =
    List.foldl
        (\union subAcc -> ProjectCache.typeElement union.name :: insertConstructors union.tags subAcc)
        acc
        unions


insertConstructors : List ( String, a ) -> List ProjectCache.ImportedElement -> List ProjectCache.ImportedElement
insertConstructors tags acc =
    List.foldl
        (\( tagName, _ ) subSubAcc -> ProjectCache.valueElement tagName :: subSubAcc)
        acc
        tags


computeBaseModule :
    ModuleName
    -> Dict ModuleName Elm.Docs.Module
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> ImportAcc
    -> Context
computeBaseModule moduleName modules { typeBindings, bindings, bindingIdCounter } importAcc =
    let
        shared : SharedScopeInfo
        shared =
            { typeBindings = typeBindings
            , importValueBindings = importAcc.contents.importedFunctions
            , importTypeBindings = importAcc.contents.importedTypes
            , aliasToModuleName = importAcc.aliasToModuleName
            , moduleQualifiers = importAcc.moduleQualifiers
            , modules = modules
            }

        topLevelScope : ScopeInfo
        topLevelScope =
            { id = ScopeId.topLevel
            , moduleName = moduleName
            , parentId = Nothing
            , range = Range.empty -- TODO Use full range
            , origin = ScopeOrigin.TopLevel
            , valueBindings = bindings
            , parentBindings = []
            , children = []
            , shared = shared
            }
    in
    { scopeStack = NonEmpty.fromElement topLevelScope
    , scopeIdCounter = ScopeId.counter
    , bindingIdCounter = bindingIdCounter
    , scopes = Dict.empty
    , valueReferences = Dict.empty
    , typeReferences = Dict.empty
    , qualifiedReferences = Dict.empty
    , moduleReferences = Dict.empty
    , aliasToModuleName = importAcc.aliasToModuleName
    , importedFunctions = importedFunctionsFromImportedBindings importAcc.contents.importedFunctions
    , importedTypes = importAcc.contents.importedTypes
    , modules = modules
    , lookupTable = Builder.empty
    , branches = NonEmpty.fromElement ( Range.empty, topLevelScope )
    }


importedFunctionsFromImportedBindings : Dict String ImportBinding -> Dict String ModuleName
importedFunctionsFromImportedBindings importedFunctions =
    Dict.foldl (\name importBinding dict -> Dict.insert name (ImportBinding.moduleName importBinding) dict) Dict.empty importedFunctions


createScope : { id : ScopeId, parent : ScopeInfo, range : Range, origin : ScopeOrigin, bindings : Dict String Binding } -> ScopeInfo
createScope { id, parent, range, origin, bindings } =
    { id = id
    , moduleName = parent.moduleName
    , parentId = Just parent.id
    , range = range
    , origin = origin
    , valueBindings = bindings
    , parentBindings = parent.valueBindings :: parent.parentBindings
    , children = []
    , shared = parent.shared
    }


collectReferences : List (Node Declaration) -> Context -> Context
collectReferences declarations context =
    List.foldl
        (\((Node _ declaration) as node) ctx ->
            case declaration of
                Declaration.FunctionDeclaration function ->
                    ctx
                        |> declarationEnterVisitor node
                        |> visitExpressions (function.declaration |> Node.value |> .expression)
                        |> declarationExitVisitor node

                _ ->
                    declarationEnterVisitor node ctx
        )
        context
        declarations


visitExpressions : Node Expression -> Context -> Context
visitExpressions ((Node range _) as node) context =
    -- IGNORE TCO
    let
        ( branchRange, scope ) =
            NonEmpty.head context.branches
    in
    if range == branchRange then
        List.foldl
            visitExpressions
            (context
                |> enterScope scope
                |> expressionEnterVisitor node
            )
            (expressionChildren node)
            |> exitLetScope node
            |> exitScope

    else
        List.foldl
            visitExpressions
            (expressionEnterVisitor node context)
            (expressionChildren node)
            |> exitLetScope node


enterScope : ScopeInfo -> Context -> Context
enterScope scope context =
    { context
        | scopeStack = NonEmpty.cons scope context.scopeStack
        , branches = NonEmpty.pop context.branches
    }


exitScope : Context -> Context
exitScope context =
    let
        scope : ScopeInfo
        scope =
            NonEmpty.head context.scopeStack
    in
    { context
        | scopeStack = NonEmpty.popThenMap (\parent -> { parent | children = ScopeInternal.Scope scope :: parent.children }) context.scopeStack
        , scopes = Dict.insert scope.id scope context.scopes
    }


exitLetScope : Node Expression -> Context -> Context
exitLetScope (Node _ expr) context =
    case expr of
        Expression.LetExpression _ ->
            exitScope context

        _ ->
            context


declarationListVisitor :
    List (Node Declaration)
    -> BindingId.Counter
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
declarationListVisitor declarations bindingIdCounter =
    List.foldl registerDeclaration
        { typeBindings = Dict.empty
        , bindings = Dict.empty
        , bindingIdCounter = bindingIdCounter
        }
        declarations


registerDeclaration :
    Node Declaration
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
registerDeclaration (Node _ declaration) acc =
    case declaration of
        Declaration.FunctionDeclaration function ->
            addBinding
                (Node.value function.declaration).name
                Bindings.valueRealm
                acc

        Declaration.AliasDeclaration alias ->
            let
                (Node range name) =
                    alias.name

                createBinding_ : Bindings.Realm -> ( Binding, BindingId.Counter )
                createBinding_ realm =
                    BindingId.new
                        (\id ->
                            Binding
                                { id = id
                                , name = name
                                , range = range
                                , realm = realm
                                }
                        )
                        acc.bindingIdCounter
            in
            case alias.typeAnnotation of
                Node _ (TypeAnnotation.Record _) ->
                    let
                        ( binding, bindingIdCounter ) =
                            createBinding_ (Bindings.valueRealm + Bindings.typeRealm)
                    in
                    { typeBindings = Dict.insert name binding acc.typeBindings
                    , bindings = Dict.insert name binding acc.bindings
                    , bindingIdCounter = bindingIdCounter
                    }

                _ ->
                    let
                        ( binding, bindingIdCounter ) =
                            createBinding_ Bindings.typeRealm
                    in
                    { typeBindings = Dict.insert name binding acc.typeBindings
                    , bindings = acc.bindings
                    , bindingIdCounter = bindingIdCounter
                    }

        Declaration.CustomTypeDeclaration customType ->
            let
                ( typeBindings, bindingIdCounter ) =
                    createBinding customType.name Bindings.typeRealm acc.typeBindings acc.bindingIdCounter
            in
            List.foldl
                (\(Node _ constructor) context_ ->
                    addBinding constructor.name Bindings.valueRealm context_
                )
                { typeBindings = typeBindings, bindings = acc.bindings, bindingIdCounter = bindingIdCounter }
                customType.constructors

        Declaration.PortDeclaration signature ->
            addBinding signature.name Bindings.valueRealm acc

        Declaration.InfixDeclaration infix_ ->
            addBinding infix_.operator Bindings.valueRealm acc

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            acc


addBinding :
    Node String
    -> Bindings.Realm
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> { typeBindings : Dict String Binding, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
addBinding node realm acc =
    let
        ( bindings, bindingIdCounter ) =
            createBinding node realm acc.bindings acc.bindingIdCounter
    in
    { typeBindings = acc.typeBindings
    , bindings = bindings
    , bindingIdCounter = bindingIdCounter
    }


createBinding : Node String -> Bindings.Realm -> Dict String Binding -> BindingId.Counter -> ( Dict String Binding, BindingId.Counter )
createBinding (Node range name) realm bindings bindingIdCounter =
    BindingId.new
        (\id ->
            Dict.insert name
                (Binding
                    { id = id
                    , name = name
                    , range = range
                    , realm = realm
                    }
                )
                bindings
        )
        bindingIdCounter


createImportBinding : String -> ModuleName -> Bindings.Realm -> BindingId.Counter -> ( ImportBinding, BindingId.Counter )
createImportBinding name moduleName realm bindingIdCounter =
    BindingId.new
        (\id ->
            ImportBinding
                { id = id
                , name = name
                , moduleName = moduleName
                , realm = realm
                }
        )
        bindingIdCounter


insertImportBinding :
    String
    -> ModuleName
    -> Bindings.Realm
    -> ( Dict String ImportBinding, BindingId.Counter )
    -> ( Dict String ImportBinding, BindingId.Counter )
insertImportBinding name moduleName realm ( acc, bindingIdCounter ) =
    BindingId.new
        (\id ->
            Dict.insert name
                (ImportBinding
                    { id = id
                    , name = name
                    , moduleName = moduleName
                    , realm = realm
                    }
                )
                acc
        )
        bindingIdCounter



-- IMPORT VISITOR


type alias ImportAcc =
    { aliasToModuleName : Dict String (NonEmpty ModuleName)
    , moduleQualifiers : Dict ModuleName ModuleName
    , contents : ImportContentAcc
    }


type alias ImportContentAcc =
    { importedFunctions : Dict String ImportBinding
    , importedTypes : Dict String ImportBinding
    , bindingIdCounter : BindingId.Counter
    }


collectPreludeImports : Dict ModuleName Elm.Docs.Module -> ImportAcc
collectPreludeImports dependencies =
    { aliasToModuleName = Review.Resources.ElmCorePrelude.aliasToModuleName
    , moduleQualifiers = Review.Resources.ElmCorePrelude.moduleQualifiers
    , contents =
        List.foldl
            (\import_ acc -> registerImportedFunctionsAndTypes dependencies import_.moduleName import_.exposingList acc)
            { importedFunctions = Dict.empty
            , importedTypes = Dict.empty
            , bindingIdCounter = BindingId.counter
            }
            Review.Resources.ElmCorePrelude.modulesWithExposingList
    }


importVisitor : Dict ModuleName Elm.Docs.Module -> Node Import -> ImportAcc -> ImportAcc
importVisitor modules (Node _ import_) context =
    let
        aliasToModuleName : Dict String (NonEmpty ModuleName)
        aliasToModuleName =
            registerImportAlias import_ context.aliasToModuleName

        (Node _ qualifier) =
            Maybe.withDefault import_.moduleName import_.moduleAlias

        moduleQualifiers : Dict ModuleName ModuleName
        moduleQualifiers =
            Dict.insert (Node.value import_.moduleName) qualifier context.moduleQualifiers
    in
    { aliasToModuleName = aliasToModuleName
    , moduleQualifiers = moduleQualifiers
    , contents =
        case import_.exposingList of
            Nothing ->
                context.contents

            Just (Node _ exposing_) ->
                registerImportedFunctionsAndTypes
                    modules
                    (Node.value import_.moduleName)
                    exposing_
                    context.contents
    }


registerImportAlias : Import -> Dict String (NonEmpty ModuleName) -> Dict String (NonEmpty ModuleName)
registerImportAlias import_ aliasToModuleName =
    let
        moduleName : ModuleName
        moduleName =
            Node.value import_.moduleName

        qualifier : String
        qualifier =
            case import_.moduleAlias of
                Just (Node _ alias) ->
                    joinModuleName alias

                Nothing ->
                    joinModuleName moduleName

        toInsert : NonEmpty ModuleName
        toInsert =
            case Dict.get qualifier aliasToModuleName of
                Just previousValue ->
                    NonEmpty.cons moduleName previousValue

                Nothing ->
                    NonEmpty.fromElement moduleName
    in
    Dict.insert qualifier toInsert aliasToModuleName


registerImportedFunctionsAndTypes :
    Dict ModuleName Elm.Docs.Module
    -> ModuleName
    -> Exposing
    -> ImportContentAcc
    -> ImportContentAcc
registerImportedFunctionsAndTypes modules moduleName exposing_ context =
    let
        module_ : Elm.Docs.Module
        module_ =
            case Dict.get moduleName modules of
                Just mod ->
                    mod

                Nothing ->
                    { name = joinModuleName moduleName
                    , comment = ""
                    , unions = []
                    , values = []
                    , aliases = []
                    , binops = []
                    }
    in
    case exposing_ of
        Exposing.All _ ->
            registerImportedFunctionsAndTypesWhenExposingAll moduleName module_ context

        Exposing.Explicit topLevelExposeList ->
            valuesFromExposingList moduleName module_ topLevelExposeList context


registerImportedFunctionsAndTypesWhenExposingAll :
    ModuleName
    -> Elm.Docs.Module
    -> ImportContentAcc
    -> ImportContentAcc
registerImportedFunctionsAndTypesWhenExposingAll moduleName module_ importAcc =
    let
        ( importedTypes, bindingIdCounter ) =
            ( importAcc.importedTypes, importAcc.bindingIdCounter )
                |> collectBindings moduleName module_.unions Bindings.typeRealm
                |> collectBindings moduleName module_.aliases Bindings.typeRealm

        ( importedFunctions, finalCounter ) =
            ( importAcc.importedFunctions, bindingIdCounter )
                |> collectBindings moduleName module_.values Bindings.valueRealm
                |> collectBindings moduleName module_.binops Bindings.valueRealm
                |> foldCustomTypesIntoDict moduleName module_.unions
    in
    addRecordAliases moduleName
        module_.aliases
        { importedFunctions = importedFunctions
        , importedTypes = importedTypes
        , bindingIdCounter = finalCounter
        }


collectBindings :
    ModuleName
    -> List { a | name : String }
    -> Bindings.Realm
    -> ( Dict String ImportBinding, BindingId.Counter )
    -> ( Dict String ImportBinding, BindingId.Counter )
collectBindings moduleName list realm initial =
    List.foldl
        (\{ name } acc ->
            insertImportBinding name moduleName realm acc
        )
        initial
        list


foldCustomTypesIntoDict :
    ModuleName
    -> List Elm.Docs.Union
    -> ( Dict String ImportBinding, BindingId.Counter )
    -> ( Dict String ImportBinding, BindingId.Counter )
foldCustomTypesIntoDict moduleName unions initial =
    List.foldl
        (\union outerAcc ->
            List.foldl
                (\( name, _ ) acc ->
                    insertImportBinding name moduleName Bindings.valueRealm acc
                )
                outerAcc
                union.tags
        )
        initial
        unions


addRecordAliases : ModuleName -> List Elm.Docs.Alias -> ImportContentAcc -> ImportContentAcc
addRecordAliases moduleName list initial =
    List.foldl
        (\{ name, tipe } acc ->
            case tipe of
                Elm.Type.Record _ Nothing ->
                    let
                        ( typeAliasBinding, bindingIdCounter ) =
                            BindingId.new
                                (\id ->
                                    ImportBinding
                                        { id = id
                                        , name = name
                                        , moduleName = moduleName
                                        , realm = Bindings.bothRealms
                                        }
                                )
                                acc.bindingIdCounter
                    in
                    { importedFunctions = Dict.insert name typeAliasBinding acc.importedFunctions
                    , importedTypes = Dict.insert name typeAliasBinding acc.importedTypes
                    , bindingIdCounter = bindingIdCounter
                    }

                _ ->
                    let
                        ( typeAliasBinding, bindingIdCounter ) =
                            BindingId.new
                                (\id ->
                                    ImportBinding
                                        { id = id
                                        , name = name
                                        , moduleName = moduleName
                                        , realm = Bindings.typeRealm
                                        }
                                )
                                acc.bindingIdCounter
                    in
                    { importedFunctions = acc.importedFunctions
                    , importedTypes = Dict.insert name typeAliasBinding acc.importedTypes
                    , bindingIdCounter = bindingIdCounter
                    }
        )
        initial
        list


valuesFromExposingList :
    ModuleName
    -> Elm.Docs.Module
    -> List (Node TopLevelExpose)
    -> ImportContentAcc
    -> ImportContentAcc
valuesFromExposingList moduleName module_ topLevelExposeList acc =
    case topLevelExposeList of
        [] ->
            acc

        (Node _ topLevelExpose) :: rest ->
            case topLevelExpose of
                Exposing.InfixExpose name ->
                    let
                        ( importBinding, bindingIdCounter ) =
                            createImportBinding name moduleName Bindings.valueRealm acc.bindingIdCounter
                    in
                    valuesFromExposingList moduleName
                        module_
                        rest
                        { importedFunctions = Dict.insert name importBinding acc.importedFunctions
                        , importedTypes = acc.importedTypes
                        , bindingIdCounter = bindingIdCounter
                        }

                Exposing.FunctionExpose name ->
                    let
                        ( importBinding, bindingIdCounter ) =
                            createImportBinding name moduleName Bindings.valueRealm acc.bindingIdCounter
                    in
                    valuesFromExposingList moduleName
                        module_
                        rest
                        { importedFunctions = Dict.insert name importBinding acc.importedFunctions
                        , importedTypes = acc.importedTypes
                        , bindingIdCounter = bindingIdCounter
                        }

                Exposing.TypeOrAliasExpose name ->
                    let
                        hasConstructor : Bool
                        hasConstructor =
                            ListExtra.find (\alias -> alias.name == name) module_.aliases
                                |> Maybe.map (\alias -> isPlainRecordType alias.tipe)
                                |> Maybe.withDefault False

                        realm : Bindings.Realm
                        realm =
                            if hasConstructor then
                                Bindings.bothRealms

                            else
                                Bindings.typeRealm

                        ( importBinding, bindingIdCounter ) =
                            createImportBinding name moduleName realm acc.bindingIdCounter
                    in
                    valuesFromExposingList moduleName
                        module_
                        rest
                        { importedFunctions =
                            if hasConstructor then
                                Dict.insert name importBinding acc.importedFunctions

                            else
                                acc.importedFunctions
                        , importedTypes = Dict.insert name importBinding acc.importedTypes
                        , bindingIdCounter = bindingIdCounter
                        }

                Exposing.TypeExpose { name } ->
                    let
                        constructors : List ( String, List Elm.Type.Type )
                        constructors =
                            ListExtra.find (\union -> union.name == name) module_.unions
                                |> Maybe.map .tags
                                |> Maybe.withDefault []

                        ( importBinding, tmpBindingIdCounter ) =
                            createImportBinding name moduleName Bindings.typeRealm acc.bindingIdCounter

                        ( importedFunctions, bindingIdCounter ) =
                            List.foldl
                                (\( tag, _ ) subAcc ->
                                    insertImportBinding tag moduleName Bindings.valueRealm subAcc
                                )
                                ( acc.importedFunctions, tmpBindingIdCounter )
                                constructors
                    in
                    valuesFromExposingList moduleName
                        module_
                        rest
                        { importedFunctions = importedFunctions
                        , importedTypes = Dict.insert name importBinding acc.importedTypes
                        , bindingIdCounter = bindingIdCounter
                        }


isPlainRecordType : Elm.Type.Type -> Bool
isPlainRecordType type_ =
    case type_ of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


declarationEnterVisitor : Node Declaration -> Context -> Context
declarationEnterVisitor (Node nodeRange node) context =
    case node of
        Declaration.FunctionDeclaration function ->
            let
                (Node functionRange functionDeclaration) =
                    function.declaration

                newContext : Context
                newContext =
                    context
                        |> collectModuleNamesFromPattern functionDeclaration.arguments
                        |> (\ctx ->
                                case function.signature of
                                    Just (Node _ signature) ->
                                        collectModuleNamesFromTypeAnnotation
                                            [ signature.typeAnnotation ]
                                            ctx

                                    Nothing ->
                                        ctx
                           )
            in
            case functionDeclaration.arguments of
                [] ->
                    newContext

                _ ->
                    let
                        ( bindings, bindingIdCounter ) =
                            collectNamesFromPattern functionDeclaration.arguments ( Dict.empty, newContext.bindingIdCounter )

                        ( scopeId, scopeIdCounter ) =
                            ScopeId.new newContext.scopeIdCounter

                        scope : ScopeInfo
                        scope =
                            createScope
                                { id = scopeId
                                , parent = NonEmpty.head newContext.scopeStack
                                , range = functionRange
                                , origin = ScopeOrigin.Function (Node nodeRange function)
                                , bindings = bindings
                                }
                    in
                    { newContext
                        | scopeIdCounter = scopeIdCounter
                        , bindingIdCounter = bindingIdCounter
                        , scopeStack = NonEmpty.cons scope newContext.scopeStack
                    }

        Declaration.CustomTypeDeclaration { constructors } ->
            List.foldl
                (\(Node _ constructor) ctx ->
                    collectModuleNamesFromTypeAnnotation constructor.arguments ctx
                )
                context
                constructors

        Declaration.AliasDeclaration { typeAnnotation } ->
            collectModuleNamesFromTypeAnnotation [ typeAnnotation ] context

        Declaration.PortDeclaration signature ->
            collectModuleNamesFromTypeAnnotation [ signature.typeAnnotation ] context

        Declaration.InfixDeclaration signature ->
            let
                (Node range name) =
                    signature.function
            in
            addValueReference [] name range (ScopeInternal.InfixReference (Node nodeRange signature)) context

        Declaration.Destructuring _ _ ->
            context


declarationExitVisitor : Node Declaration -> Context -> Context
declarationExitVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration function ->
            case (Node.value function.declaration).arguments of
                [] ->
                    context

                _ ->
                    exitScope context

        _ ->
            context


collectNamesFromPattern : List (Node Pattern) -> ( Dict String Binding, BindingId.Counter ) -> ( Dict String Binding, BindingId.Counter )
collectNamesFromPattern patternsToVisit (( bindings, bindingIdCounter ) as acc) =
    case patternsToVisit of
        (Node patternRange pattern) :: restOfPatternsToVisit ->
            case pattern of
                Pattern.VarPattern name ->
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        (createBinding (Node patternRange name) Bindings.valueRealm bindings bindingIdCounter)

                Pattern.NamedPattern _ subPatterns ->
                    collectNamesFromPattern
                        (List.append subPatterns restOfPatternsToVisit)
                        acc

                Pattern.RecordPattern names ->
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        (List.foldl
                            (\node ( subAcc, counter ) ->
                                createBinding node Bindings.valueRealm subAcc counter
                            )
                            ( bindings, bindingIdCounter )
                            names
                        )

                Pattern.ParenthesizedPattern subPattern ->
                    collectNamesFromPattern (subPattern :: restOfPatternsToVisit) acc

                Pattern.AsPattern subPattern alias ->
                    collectNamesFromPattern
                        (subPattern :: restOfPatternsToVisit)
                        (createBinding alias Bindings.valueRealm bindings bindingIdCounter)

                Pattern.TuplePattern subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) acc

                Pattern.UnConsPattern left right ->
                    collectNamesFromPattern (left :: right :: restOfPatternsToVisit) acc

                Pattern.ListPattern subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) acc

                _ ->
                    collectNamesFromPattern restOfPatternsToVisit acc

        [] ->
            acc


collectModuleNamesFromPattern : List (Node Pattern) -> Context -> Context
collectModuleNamesFromPattern patternsToVisit context =
    case patternsToVisit of
        ((Node range pattern) as node) :: restOfPatternsToVisit ->
            case pattern of
                Pattern.NamedPattern { moduleName, name } subPatterns ->
                    collectModuleNamesFromPattern
                        (List.append subPatterns restOfPatternsToVisit)
                        (addValueReference moduleName
                            name
                            range
                            (ScopeInternal.Pattern node)
                            { context
                                | lookupTable = Builder.add range (moduleNameForValue context name moduleName) context.lookupTable
                            }
                        )

                Pattern.UnConsPattern left right ->
                    collectModuleNamesFromPattern (left :: right :: restOfPatternsToVisit) context

                Pattern.TuplePattern subPatterns ->
                    collectModuleNamesFromPattern (List.append subPatterns restOfPatternsToVisit) context

                Pattern.ParenthesizedPattern subPattern ->
                    collectModuleNamesFromPattern (subPattern :: restOfPatternsToVisit) context

                Pattern.AsPattern subPattern _ ->
                    collectModuleNamesFromPattern (subPattern :: restOfPatternsToVisit) context

                Pattern.ListPattern subPatterns ->
                    collectModuleNamesFromPattern (List.append subPatterns restOfPatternsToVisit) context

                _ ->
                    collectModuleNamesFromPattern restOfPatternsToVisit context

        [] ->
            context


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor ((Node nodeRange expr) as node) context =
    case expr of
        Expression.LetExpression letExpression ->
            let
                ( letScopeId, scopeIdCounter ) =
                    ScopeId.new context.scopeIdCounter

                letScope : ScopeInfo
                letScope =
                    createScope
                        { id = letScopeId
                        , parent = NonEmpty.head context.scopeStack
                        , range = nodeRange
                        , origin = ScopeOrigin.LetBlock (Node nodeRange letExpression)
                        , bindings = letBlockBindings
                        }

                ( letBlockBindings, letBlockBindingIdCounter ) =
                    List.foldr
                        (\(Node _ declaration) (( bindings, bindingIdCounter ) as acc) ->
                            case declaration of
                                Expression.LetFunction function ->
                                    createBinding
                                        (Node.value function.declaration).name
                                        Bindings.valueRealm
                                        bindings
                                        bindingIdCounter

                                Expression.LetDestructuring pattern _ ->
                                    collectNamesFromPattern [ pattern ] acc
                        )
                        ( Dict.empty, context.bindingIdCounter )
                        letExpression.declarations

                let_ :
                    { branches : NonEmpty ( Range, ScopeInfo )
                    , scopeIdCounter : ScopeId.Counter
                    , bindingIdCounter : BindingId.Counter
                    }
                let_ =
                    List.foldr
                        (\(Node declRange declaration) acc ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        { arguments, expression } =
                                            Node.value function.declaration
                                    in
                                    case arguments of
                                        [] ->
                                            acc

                                        _ ->
                                            let
                                                ( scopeId, scopeIdCounter_ ) =
                                                    ScopeId.new acc.scopeIdCounter

                                                ( bindings, bindingIdCounter ) =
                                                    collectNamesFromPattern arguments ( Dict.empty, acc.bindingIdCounter )

                                                scope : ScopeInfo
                                                scope =
                                                    createScope
                                                        { id = scopeId
                                                        , parent = letScope
                                                        , range = Node.range expression
                                                        , origin = ScopeOrigin.Function (Node declRange function)
                                                        , bindings = bindings
                                                        }
                                            in
                                            { branches = NonEmpty.cons ( Node.range expression, scope ) acc.branches
                                            , scopeIdCounter = scopeIdCounter_
                                            , bindingIdCounter = bindingIdCounter
                                            }

                                Expression.LetDestructuring _ _ ->
                                    acc
                        )
                        { branches = context.branches
                        , scopeIdCounter = scopeIdCounter
                        , bindingIdCounter = letBlockBindingIdCounter
                        }
                        letExpression.declarations

                newContext : Context
                newContext =
                    List.foldl
                        (\(Node _ declaration) ctx ->
                            case declaration of
                                Expression.LetFunction function ->
                                    case function.signature of
                                        Just (Node _ signature) ->
                                            ctx
                                                |> collectModuleNamesFromPattern (Node.value function.declaration).arguments
                                                |> collectModuleNamesFromTypeAnnotation [ signature.typeAnnotation ]

                                        Nothing ->
                                            collectModuleNamesFromPattern (Node.value function.declaration).arguments ctx

                                Expression.LetDestructuring pattern _ ->
                                    collectModuleNamesFromPattern [ pattern ] ctx
                        )
                        { context
                            | scopeStack =
                                NonEmpty.cons letScope context.scopeStack
                        }
                        letExpression.declarations
            in
            { newContext
                | branches = let_.branches
                , scopeIdCounter = let_.scopeIdCounter
                , bindingIdCounter = let_.bindingIdCounter
            }

        Expression.CaseExpression caseBlock ->
            let
                parent : ScopeInfo
                parent =
                    NonEmpty.head context.scopeStack
            in
            List.foldr
                (\(( pattern, Node expressionRange _ ) as caseBranch) ctx ->
                    let
                        ( scopeId, newScopeIdCounter ) =
                            ScopeId.new ctx.scopeIdCounter

                        ( bindings, bindingIdCounter_ ) =
                            collectNamesFromPattern [ pattern ] ( Dict.empty, ctx.bindingIdCounter )
                    in
                    { ctx
                        | branches =
                            NonEmpty.cons
                                ( expressionRange
                                , createScope
                                    { id = scopeId
                                    , parent = parent
                                    , range = expressionRange
                                    , origin = ScopeOrigin.CaseBranch caseBranch
                                    , bindings = bindings
                                    }
                                )
                                ctx.branches
                        , scopeIdCounter = newScopeIdCounter
                        , bindingIdCounter = bindingIdCounter_
                    }
                        |> collectModuleNamesFromPattern [ pattern ]
                )
                context
                caseBlock.cases

        Expression.FunctionOrValue moduleName name ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context name moduleName)
                        context.lookupTable
            }
                |> addValueReference moduleName name nodeRange (ScopeInternal.Expression node)

        Expression.RecordUpdateExpression (Node range name) _ ->
            { context
                | lookupTable =
                    Builder.add
                        range
                        (moduleNameForValue context name [])
                        context.lookupTable
            }
                |> addValueReference [] name range (ScopeInternal.Expression node)

        Expression.LambdaExpression ({ args, expression } as lambda) ->
            let
                range : Range
                range =
                    Node.range expression

                ( scopeId, scopeIdCounter ) =
                    ScopeId.new context.scopeIdCounter

                ( bindings, bindingIdCounter ) =
                    collectNamesFromPattern args ( Dict.empty, context.bindingIdCounter )
            in
            { context
                | scopeIdCounter = scopeIdCounter
                , bindingIdCounter = bindingIdCounter
                , branches =
                    NonEmpty.cons
                        ( range
                        , createScope
                            { id = scopeId
                            , parent = NonEmpty.head context.scopeStack
                            , range = range
                            , origin = ScopeOrigin.Lambda (Node nodeRange lambda)
                            , bindings = bindings
                            }
                        )
                        context.branches
            }
                |> collectModuleNamesFromPattern args

        Expression.PrefixOperator op ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context op [])
                        context.lookupTable
            }
                |> addValueReference [] op nodeRange (ScopeInternal.Expression node)

        Expression.OperatorApplication op _ _ _ ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context op [])
                        context.lookupTable
            }
                -- TODO Figure out exact position of operator
                |> addValueReference [] op nodeRange (ScopeInternal.Expression node)

        _ ->
            context


collectModuleNamesFromTypeAnnotation : List (Node TypeAnnotation) -> Context -> Context
collectModuleNamesFromTypeAnnotation typeAnnotationsToVisit context =
    case typeAnnotationsToVisit of
        ((Node nodeRange typeAnnotationNode) as node) :: remainingTypeAnnotationsToVisit ->
            case typeAnnotationNode of
                TypeAnnotation.Typed (Node range ( moduleName, name )) args ->
                    collectModuleNamesFromTypeAnnotation
                        (List.append args remainingTypeAnnotationsToVisit)
                        ({ context | lookupTable = Builder.add range (moduleNameForType context name moduleName) context.lookupTable }
                            |> addTypeReference moduleName name nodeRange node
                        )

                TypeAnnotation.Tupled nodes ->
                    collectModuleNamesFromTypeAnnotation
                        (List.append nodes remainingTypeAnnotationsToVisit)
                        context

                TypeAnnotation.Record fields ->
                    collectModuleNamesFromTypeAnnotation
                        (ListExtra.orderIndependentMapAppend (\(Node _ ( _, value )) -> value) fields remainingTypeAnnotationsToVisit)
                        context

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    collectModuleNamesFromTypeAnnotation
                        (ListExtra.orderIndependentMapAppend (\(Node _ ( _, value )) -> value) fields remainingTypeAnnotationsToVisit)
                        context

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    collectModuleNamesFromTypeAnnotation
                        (left :: right :: remainingTypeAnnotationsToVisit)
                        context

                _ ->
                    collectModuleNamesFromTypeAnnotation
                        remainingTypeAnnotationsToVisit
                        context

        [] ->
            context


{-| Get the name of the module where a value was defined.
A value can be either a function, a constant, a custom type constructor or a type alias (used as a function).

  - The second argument (`String`) is the name of the value
  - The third argument (`ModuleName`) is the module name that was used next to the value's name where you found it

If the element was defined in the current module, then the result will be `[]`.

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
    expressionVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue moduleName "button" ->
                if Scope.moduleNameForValue context.scope "button" moduleName == [ "Html" ] then
                    ( [ createError node ], context )

                else
                    ( [], context )

            _ ->
                ( [], context )

-}
moduleNameForValue : Context -> String -> ModuleName -> ModuleName
moduleNameForValue context valueName moduleName =
    case moduleName of
        [] ->
            if isInScope valueName context.scopeStack then
                []

            else
                Dict.get valueName context.importedFunctions
                    |> Maybe.withDefault []

        moduleNameOrAlias :: [] ->
            case Dict.get moduleNameOrAlias context.aliasToModuleName of
                Just aliases ->
                    NonEmpty.findWithHeadAsDefault
                        (\aliasedModuleName ->
                            case Dict.get aliasedModuleName context.modules of
                                Just module_ ->
                                    isValueDeclaredInModule valueName module_

                                Nothing ->
                                    False
                        )
                        aliases

                Nothing ->
                    moduleName

        _ ->
            moduleName


addValueReference : ModuleName -> String -> Range -> ScopeInternal.Kind -> Context -> Context
addValueReference moduleName valueName range kind context =
    case moduleName of
        [] ->
            let
                scope : ScopeInfo
                scope =
                    NonEmpty.head context.scopeStack
            in
            case Scope.valueBinding (ScopeInternal.Scope scope) valueName of
                Just binding ->
                    let
                        (BindingId.Id bindingId) =
                            Binding.id binding

                        reference : Reference
                        reference =
                            ScopeInternal.Reference
                                { target = Target.LocalBinding binding
                                , qualifier = []
                                , range = range
                                , kind = kind
                                }
                    in
                    { context | valueReferences = dictInsertCons bindingId reference context.valueReferences }

                Nothing ->
                    case Dict.get valueName scope.shared.importValueBindings of
                        Just importBinding ->
                            let
                                (BindingId.Id bindingId) =
                                    ImportBinding.id importBinding

                                reference : Reference
                                reference =
                                    ScopeInternal.Reference
                                        { target = Target.ImportBinding_ importBinding
                                        , qualifier = []
                                        , range = range
                                        , kind = kind
                                        }
                            in
                            { context
                                | valueReferences = dictInsertCons bindingId reference context.valueReferences
                                , moduleReferences = dictInsertCons (ImportBinding.moduleName importBinding) reference context.moduleReferences
                            }

                        Nothing ->
                            context

        _ ->
            let
                realModuleName : ModuleName
                realModuleName =
                    moduleNameForValue context valueName moduleName

                moduleReference : Reference
                moduleReference =
                    ScopeInternal.Reference
                        { target = Target.QualifiedReference { name = valueName, moduleName = realModuleName }
                        , qualifier = moduleName
                        , range = range
                        , kind = kind
                        }
            in
            { context
                | qualifiedReferences = dictInsertCons moduleName moduleReference context.qualifiedReferences
                , moduleReferences = dictInsertCons realModuleName moduleReference context.moduleReferences
            }


addTypeReference : ModuleName -> String -> Range -> Node TypeAnnotation -> Context -> Context
addTypeReference moduleName name range node context =
    case getTarget context name moduleName of
        Nothing ->
            context

        Just ( target, maybeBindingId ) ->
            let
                realModuleName : ModuleName
                realModuleName =
                    moduleNameForType context name moduleName

                typeReference : Reference
                typeReference =
                    ScopeInternal.Reference
                        { target = target
                        , qualifier = moduleName
                        , range = range
                        , kind = ScopeInternal.Type node
                        }
            in
            case maybeBindingId of
                Just (BindingId.Id bindingId) ->
                    { context
                        | typeReferences = dictInsertCons bindingId typeReference context.typeReferences
                        , moduleReferences =
                            case realModuleName of
                                [] ->
                                    context.moduleReferences

                                _ ->
                                    dictInsertCons realModuleName typeReference context.moduleReferences
                    }

                Nothing ->
                    { context
                        | qualifiedReferences =
                            if List.isEmpty moduleName then
                                context.qualifiedReferences

                            else
                                dictInsertCons moduleName typeReference context.qualifiedReferences
                        , moduleReferences = dictInsertCons realModuleName typeReference context.moduleReferences
                    }


dictInsertCons : comparable -> v -> Dict comparable (List v) -> Dict comparable (List v)
dictInsertCons key v dict =
    case Dict.get key dict of
        Just previous ->
            Dict.insert key (v :: previous) dict

        Nothing ->
            Dict.insert key [ v ] dict


{-| Get the name of the module where a type was defined.
A type can be either a custom type or a type alias.

  - The second argument (`String`) is the name of the type
  - The third argument (`ModuleName`) is the module name that was used next to the type name where you found it

-}
moduleNameForType : Context -> String -> ModuleName -> ModuleName
moduleNameForType context typeName moduleName =
    case moduleName of
        [] ->
            if Dict.member typeName (NonEmpty.head context.scopeStack).shared.typeBindings then
                []

            else
                case Dict.get typeName context.importedTypes of
                    Just importBinding ->
                        ImportBinding.moduleName importBinding

                    Nothing ->
                        []

        [ moduleNameOrAlias ] ->
            case Dict.get moduleNameOrAlias context.aliasToModuleName of
                Just aliases ->
                    NonEmpty.findWithHeadAsDefault
                        (\aliasedModuleName ->
                            case Dict.get aliasedModuleName context.modules of
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


getTarget : Context -> String -> ModuleName -> Maybe ( Target, Maybe BindingId.Id )
getTarget context typeName moduleName =
    case moduleName of
        [] ->
            case Dict.get typeName (NonEmpty.head context.scopeStack).shared.typeBindings of
                Just binding ->
                    Just ( Target.LocalBinding binding, Just (Binding.id binding) )

                Nothing ->
                    case Dict.get typeName (NonEmpty.head context.scopeStack).shared.importTypeBindings of
                        Just importBinding ->
                            Just ( Target.ImportBinding_ importBinding, Just (ImportBinding.id importBinding) )

                        Nothing ->
                            Nothing

        _ :: [] ->
            let
                targetModuleName : ModuleName
                targetModuleName =
                    case Dict.get (joinModuleName moduleName) context.aliasToModuleName of
                        Just aliases ->
                            NonEmpty.findWithHeadAsDefault
                                (\aliasedModuleName ->
                                    case Dict.get aliasedModuleName context.modules of
                                        Just module_ ->
                                            isTypeDeclaredInModule typeName module_

                                        Nothing ->
                                            False
                                )
                                aliases

                        Nothing ->
                            moduleName
            in
            Just ( Target.QualifiedReference { moduleName = targetModuleName, name = typeName }, Nothing )

        _ ->
            Just ( Target.QualifiedReference { moduleName = moduleName, name = typeName }, Nothing )


isValueDeclaredInModule : String -> Elm.Docs.Module -> Bool
isValueDeclaredInModule valueName module_ =
    List.any (.name >> (==) valueName) module_.values
        || List.any (.name >> (==) valueName) module_.aliases
        || List.any
            (\union -> List.any (Tuple.first >> (==) valueName) union.tags)
            module_.unions


isTypeDeclaredInModule : String -> Elm.Docs.Module -> Bool
isTypeDeclaredInModule typeName module_ =
    List.any (.name >> (==) typeName) module_.aliases
        || List.any (.name >> (==) typeName) module_.unions


isInScope : String -> NonEmpty ScopeInfo -> Bool
isInScope name scopes =
    NonEmpty.any (\{ valueBindings } -> Dict.member name valueBindings) scopes


joinModuleName : ModuleName -> String
joinModuleName name =
    String.join "." name


expressionChildren : Node Expression -> List (Node Expression)
expressionChildren (Node _ node) =
    case node of
        Expression.Application expressions ->
            expressions

        Expression.ListExpr elements ->
            elements

        Expression.RecordExpr fields ->
            List.map (\(Node _ ( _, expr )) -> expr) fields

        Expression.RecordUpdateExpression _ setters ->
            List.map (\(Node _ ( _, expr )) -> expr) setters

        Expression.ParenthesizedExpression expr ->
            [ expr ]

        Expression.OperatorApplication _ direction left right ->
            case direction of
                Infix.Left ->
                    [ left, right ]

                Infix.Right ->
                    [ right, left ]

                Infix.Non ->
                    [ left, right ]

        Expression.IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        Expression.LetExpression { expression, declarations } ->
            List.foldr
                (\(Node _ declaration) acc ->
                    case declaration of
                        Expression.LetFunction function ->
                            functionToExpression function :: acc

                        Expression.LetDestructuring _ expr ->
                            expr :: acc
                )
                [ expression ]
                declarations

        Expression.CaseExpression { expression, cases } ->
            expression
                :: List.map (\( _, caseExpression ) -> caseExpression) cases

        Expression.LambdaExpression { expression } ->
            [ expression ]

        Expression.TupledExpression expressions ->
            expressions

        Expression.Negation expr ->
            [ expr ]

        Expression.RecordAccess expr _ ->
            [ expr ]

        _ ->
            []


functionToExpression : Expression.Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression
