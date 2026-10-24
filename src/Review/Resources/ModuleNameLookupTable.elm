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
import Review.Resources.BindingId as BindingId
import Review.Resources.ElmCorePrelude
import Review.Resources.Internal exposing (ModuleNameLookupTable)
import Review.Resources.ModuleNameLookupTableBuilder as Builder exposing (ModuleNameLookupTableBuilder)
import Review.Resources.ScopeId as ScopeId
import Review.Resources.ScopeInternal as ScopeInternal exposing (Binding, ScopeId, ScopeInfo)
import Review.ScopeProvider exposing (ScopeProvider)
import Set exposing (Set)
import Vendor.ListExtra as ListExtra


type alias Context =
    { scopeStack : NonEmpty ScopeInfo
    , scopeIdCounter : ScopeId.Counter
    , bindingIdCounter : BindingId.Counter
    , scopes : Dict ScopeId ScopeInfo
    , localTypes : Set String
    , importAliases : Dict String (NonEmpty ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
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
                    computeHelp moduleName module_ projectCache
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


computeHelp : ModuleName -> OpaqueProjectModule -> ProjectCache -> ( ModuleNameLookupTable, ScopeProvider )
computeHelp moduleName module_ projectCache =
    let
        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        modules : Dict ModuleName Elm.Docs.Module
        modules =
            Dict.foldl
                (\key docs dict ->
                    Dict.insert key (DocsModule.mod docs) dict
                )
                projectCache.dependencies
                projectCache.modules

        baseModuleContext : Context
        baseModuleContext =
            computeBaseModule
                modules
                (declarationListVisitor moduleAst.declarations)
                (List.foldl
                    (importVisitor modules)
                    (collectPreludeImports projectCache.dependencies)
                    moduleAst.imports
                )

        moduleContext : Context
        moduleContext =
            collectLookupTable moduleAst.declarations baseModuleContext

        topLevelScope : ScopeInfo
        topLevelScope =
            NonEmpty.head moduleContext.scopeStack

        scopes : Dict ScopeId ScopeInfo
        scopes =
            Dict.insert topLevelScope.id topLevelScope moduleContext.scopes
    in
    ( Builder.finalize moduleName moduleContext.lookupTable
    , ScopeInternal.fromScopes moduleName topLevelScope scopes
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
    Dict ModuleName Elm.Docs.Module
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
    -> Context
computeBaseModule modules { localTypes, bindings, bindingIdCounter } { importedFunctions, importedTypes, importAliases } =
    let
        topLevelScope : ScopeInfo
        topLevelScope =
            { id = ScopeId.topLevel
            , parentId = Nothing
            , range = Range.empty -- TODO Use full range
            , bindings = bindings
            , children = []
            }
    in
    { scopeStack = NonEmpty.fromElement topLevelScope
    , scopeIdCounter = ScopeId.counter
    , bindingIdCounter = bindingIdCounter
    , scopes = Dict.empty
    , localTypes = localTypes
    , importAliases = importAliases
    , importedFunctions = importedFunctions
    , importedTypes = importedTypes
    , modules = modules
    , lookupTable = Builder.empty
    , branches = NonEmpty.fromElement ( Range.empty, topLevelScope )
    }


createScope : { id : ScopeId, parentId : ScopeId, range : Range, bindings : Dict String Binding } -> ScopeInfo
createScope { id, parentId, range, bindings } =
    { id = id
    , parentId = Just parentId
    , range = range
    , bindings = bindings
    , children = []
    }


collectLookupTable : List (Node Declaration) -> Context -> Context
collectLookupTable declarations context =
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
        | scopeStack =
            NonEmpty.mapHeadThenCons scope
                (\parent -> { parent | children = ScopeInternal.Scope scope :: parent.children })
                context.scopeStack
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
        | scopeStack = NonEmpty.pop context.scopeStack
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
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
declarationListVisitor declarations =
    List.foldl registerDeclaration
        { localTypes = Set.empty
        , bindings = Dict.empty
        , bindingIdCounter = BindingId.counter
        }
        declarations


registerDeclaration :
    Node Declaration
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
registerDeclaration (Node _ declaration) acc =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                (Node nameRange name) =
                    (Node.value function.declaration).name
            in
            addBinding name nameRange acc

        Declaration.AliasDeclaration alias ->
            let
                (Node nameRange name) =
                    alias.name

                localTypes : Set String
                localTypes =
                    Set.insert name acc.localTypes
            in
            case Node.value alias.typeAnnotation of
                TypeAnnotation.Record _ ->
                    let
                        ( binding, bindingIdCounter ) =
                            createBinding name nameRange acc.bindingIdCounter
                    in
                    { localTypes = localTypes
                    , bindings = Dict.insert binding.name binding acc.bindings
                    , bindingIdCounter = bindingIdCounter
                    }

                _ ->
                    { localTypes = localTypes
                    , bindings = acc.bindings
                    , bindingIdCounter = acc.bindingIdCounter
                    }

        Declaration.CustomTypeDeclaration customType ->
            List.foldl
                (\(Node _ constructor) context_ ->
                    let
                        (Node nameRange name) =
                            constructor.name
                    in
                    addBinding name nameRange context_
                )
                { acc | localTypes = Set.insert (Node.value customType.name) acc.localTypes }
                customType.constructors

        Declaration.PortDeclaration signature ->
            let
                (Node nameRange name) =
                    signature.name
            in
            addBinding name nameRange acc

        Declaration.InfixDeclaration infix_ ->
            let
                (Node nameRange name) =
                    infix_.operator
            in
            addBinding name nameRange acc

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            acc


addBinding :
    String
    -> Range
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
    -> { localTypes : Set String, bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
addBinding name range acc =
    let
        ( binding, bindingIdCounter ) =
            createBinding name range acc.bindingIdCounter
    in
    { localTypes = acc.localTypes
    , bindings = Dict.insert binding.name binding acc.bindings
    , bindingIdCounter = bindingIdCounter
    }


createBinding : String -> Range -> BindingId.Counter -> ( Binding, BindingId.Counter )
createBinding name range bindingIdCounter =
    let
        ( id, counter ) =
            BindingId.new bindingIdCounter
    in
    ( { id = id
      , name = name
      , range = range
      , references = []
      }
    , counter
    )



-- IMPORT VISITOR


collectPreludeImports : Dict ModuleName Elm.Docs.Module -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
collectPreludeImports dependencies =
    List.foldl
        (\import_ acc -> registerImportedFunctionsAndTypes dependencies import_.moduleName import_.exposingList acc)
        { importedFunctions = Dict.empty, importedTypes = Dict.empty, importAliases = Review.Resources.ElmCorePrelude.importAliases }
        Review.Resources.ElmCorePrelude.modulesWithExposingList


importVisitor :
    Dict ModuleName Elm.Docs.Module
    -> Node Import
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
importVisitor modules (Node _ import_) context =
    let
        importAliases : Dict String (NonEmpty ModuleName)
        importAliases =
            registerImportAlias import_ context.importAliases
    in
    case import_.exposingList of
        Nothing ->
            { importedFunctions = context.importedFunctions
            , importedTypes = context.importedTypes
            , importAliases = importAliases
            }

        Just (Node _ exposing_) ->
            registerImportedFunctionsAndTypes
                modules
                (Node.value import_.moduleName)
                exposing_
                { importedFunctions = context.importedFunctions
                , importedTypes = context.importedTypes
                , importAliases = importAliases
                }


registerImportAlias : Import -> Dict String (NonEmpty ModuleName) -> Dict String (NonEmpty ModuleName)
registerImportAlias import_ importAliases =
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
            case Dict.get qualifier importAliases of
                Just previousValue ->
                    NonEmpty.cons moduleName previousValue

                Nothing ->
                    NonEmpty.fromElement moduleName
    in
    Dict.insert qualifier toInsert importAliases


registerImportedFunctionsAndTypes :
    Dict ModuleName Elm.Docs.Module
    -> ModuleName
    -> Exposing
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
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
            registerImportedFunctionsAndTypesWithExplicit moduleName module_ topLevelExposeList context


registerImportedFunctionsAndTypesWhenExposingAll :
    ModuleName
    -> Elm.Docs.Module
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : Dict String (NonEmpty ModuleName) }
registerImportedFunctionsAndTypesWhenExposingAll moduleName module_ context =
    let
        foldIntoDict : List { a | name : comparable } -> Dict comparable ModuleName -> Dict comparable ModuleName
        foldIntoDict list dict =
            List.foldl (\{ name } acc -> Dict.insert name moduleName acc) dict list

        addRecordAliases : List Elm.Docs.Alias -> Dict String ModuleName -> Dict String ModuleName
        addRecordAliases list dict =
            List.foldl
                (\{ name, tipe } acc ->
                    if isPlainRecordType tipe then
                        Dict.insert name moduleName acc

                    else
                        acc
                )
                dict
                list

        foldCustomTypesIntoDict : List { a | tags : List ( String, b ) } -> Dict String ModuleName -> Dict String ModuleName
        foldCustomTypesIntoDict unions dict =
            List.foldl
                (\union acc ->
                    List.foldl (\( name, _ ) subAcc -> Dict.insert name moduleName subAcc) acc union.tags
                )
                dict
                unions

        importedFunctions : Dict String ModuleName
        importedFunctions =
            context.importedFunctions
                |> foldIntoDict module_.values
                |> foldIntoDict module_.binops
                |> addRecordAliases module_.aliases
                |> foldCustomTypesIntoDict module_.unions

        importedTypes : Dict String ModuleName
        importedTypes =
            context.importedTypes
                |> foldIntoDict module_.unions
                |> foldIntoDict module_.aliases
    in
    { importedFunctions = importedFunctions
    , importedTypes = importedTypes
    , importAliases = context.importAliases
    }


registerImportedFunctionsAndTypesWithExplicit :
    ModuleName
    -> Elm.Docs.Module
    -> List (Node TopLevelExpose)
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : c }
    -> { importedFunctions : Dict String ModuleName, importedTypes : Dict String ModuleName, importAliases : c }
registerImportedFunctionsAndTypesWithExplicit moduleName module_ topLevelExposeList context =
    let
        importedFunctions : Dict String ModuleName
        importedFunctions =
            valuesFromExposingList
                moduleName
                module_
                topLevelExposeList
                context.importedFunctions

        importedTypes : Dict String ModuleName
        importedTypes =
            List.foldl
                (\topLevelExpose acc ->
                    case typesFromExposingList topLevelExpose of
                        Just name ->
                            Dict.insert name moduleName acc

                        Nothing ->
                            acc
                )
                context.importedTypes
                topLevelExposeList
    in
    { importedFunctions = importedFunctions
    , importedTypes = importedTypes
    , importAliases = context.importAliases
    }


valuesFromExposingList : ModuleName -> Elm.Docs.Module -> List (Node TopLevelExpose) -> Dict String ModuleName -> Dict String ModuleName
valuesFromExposingList moduleName module_ topLevelExposeList acc =
    case topLevelExposeList of
        [] ->
            acc

        (Node _ topLevelExpose) :: rest ->
            case topLevelExpose of
                Exposing.InfixExpose operator ->
                    valuesFromExposingList moduleName module_ rest (Dict.insert operator moduleName acc)

                Exposing.FunctionExpose function ->
                    valuesFromExposingList moduleName module_ rest (Dict.insert function moduleName acc)

                Exposing.TypeOrAliasExpose name ->
                    if
                        ListExtra.find (\alias -> alias.name == name) module_.aliases
                            |> Maybe.map (\alias -> isPlainRecordType alias.tipe)
                            |> Maybe.withDefault False
                    then
                        valuesFromExposingList moduleName module_ rest (Dict.insert name moduleName acc)

                    else
                        -- Type is a custom type
                        valuesFromExposingList moduleName module_ rest acc

                Exposing.TypeExpose { name, open } ->
                    case open of
                        Just _ ->
                            let
                                newAcc : Dict String ModuleName
                                newAcc =
                                    case ListExtra.find (\union -> union.name == name) module_.unions of
                                        Just union ->
                                            List.foldl (\( tag, _ ) dict -> Dict.insert tag moduleName dict) acc union.tags

                                        Nothing ->
                                            acc
                            in
                            valuesFromExposingList moduleName module_ rest newAcc

                        Nothing ->
                            valuesFromExposingList moduleName module_ rest acc


isPlainRecordType : Elm.Type.Type -> Bool
isPlainRecordType type_ =
    case type_ of
        Elm.Type.Record _ Nothing ->
            True

        _ ->
            False


typesFromExposingList : Node TopLevelExpose -> Maybe String
typesFromExposingList (Node _ topLevelExpose) =
    case topLevelExpose of
        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose name ->
            Just name

        Exposing.TypeExpose { name } ->
            Just name


declarationEnterVisitor : Node Declaration -> Context -> Context
declarationEnterVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration function ->
            let
                (Node functionRange functionDeclaration) =
                    function.declaration

                lookupTableAfterArguments : ModuleNameLookupTableBuilder
                lookupTableAfterArguments =
                    collectModuleNamesFromPattern context functionDeclaration.arguments context.lookupTable

                finalLookupTable : ModuleNameLookupTableBuilder
                finalLookupTable =
                    case function.signature of
                        Just (Node _ signature) ->
                            collectModuleNamesFromTypeAnnotation
                                context
                                [ signature.typeAnnotation ]
                                lookupTableAfterArguments

                        Nothing ->
                            lookupTableAfterArguments
            in
            case functionDeclaration.arguments of
                [] ->
                    { context | lookupTable = finalLookupTable }

                _ ->
                    let
                        ( bindings, bindingIdCounter ) =
                            collectNamesFromPattern functionDeclaration.arguments context.bindingIdCounter Dict.empty

                        ( scopeId, scopeIdCounter ) =
                            ScopeId.new context.scopeIdCounter

                        scope : ScopeInfo
                        scope =
                            createScope
                                { id = scopeId
                                , parentId = (NonEmpty.head context.scopeStack).id
                                , range = functionRange
                                , bindings = bindings
                                }
                    in
                    { context
                        | lookupTable = finalLookupTable
                        , scopeIdCounter = scopeIdCounter
                        , bindingIdCounter = bindingIdCounter
                        , scopeStack =
                            NonEmpty.mapHeadThenCons scope
                                (\parent -> { parent | children = ScopeInternal.Scope scope :: parent.children })
                                context.scopeStack
                    }

        Declaration.CustomTypeDeclaration { constructors } ->
            { context
                | lookupTable =
                    List.foldl
                        (\(Node _ constructor) acc ->
                            collectModuleNamesFromTypeAnnotation context constructor.arguments acc
                        )
                        context.lookupTable
                        constructors
            }

        Declaration.AliasDeclaration { typeAnnotation } ->
            { context | lookupTable = collectModuleNamesFromTypeAnnotation context [ typeAnnotation ] context.lookupTable }

        Declaration.PortDeclaration signature ->
            { context | lookupTable = collectModuleNamesFromTypeAnnotation context [ signature.typeAnnotation ] context.lookupTable }

        _ ->
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


collectNamesFromPattern : List (Node Pattern) -> BindingId.Counter -> Dict String Binding -> ( Dict String Binding, BindingId.Counter )
collectNamesFromPattern patternsToVisit bindingIdCounter acc =
    case patternsToVisit of
        (Node patternRange pattern) :: restOfPatternsToVisit ->
            case pattern of
                Pattern.VarPattern name ->
                    let
                        ( binding, newCounter ) =
                            createBinding name patternRange bindingIdCounter
                    in
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        newCounter
                        (Dict.insert binding.name binding acc)

                Pattern.NamedPattern _ subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) bindingIdCounter acc

                Pattern.RecordPattern names ->
                    let
                        ( bindings, newCounter ) =
                            List.foldl
                                (\(Node nameRange name) ( subAcc, counter ) ->
                                    let
                                        ( binding, newCounter_ ) =
                                            createBinding name nameRange counter
                                    in
                                    ( Dict.insert binding.name binding subAcc
                                    , newCounter_
                                    )
                                )
                                ( acc, bindingIdCounter )
                                names
                    in
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        newCounter
                        bindings

                Pattern.ParenthesizedPattern subPattern ->
                    collectNamesFromPattern (subPattern :: restOfPatternsToVisit) bindingIdCounter acc

                Pattern.AsPattern subPattern (Node aliasRange aliasName) ->
                    let
                        ( binding, newCounter ) =
                            createBinding aliasName aliasRange bindingIdCounter
                    in
                    collectNamesFromPattern
                        (subPattern :: restOfPatternsToVisit)
                        newCounter
                        (Dict.insert binding.name binding acc)

                Pattern.TuplePattern subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) bindingIdCounter acc

                Pattern.UnConsPattern left right ->
                    collectNamesFromPattern (left :: right :: restOfPatternsToVisit) bindingIdCounter acc

                Pattern.ListPattern subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) bindingIdCounter acc

                _ ->
                    collectNamesFromPattern restOfPatternsToVisit bindingIdCounter acc

        [] ->
            ( acc, bindingIdCounter )


collectModuleNamesFromPattern : Context -> List (Node Pattern) -> ModuleNameLookupTableBuilder -> ModuleNameLookupTableBuilder
collectModuleNamesFromPattern context patternsToVisit acc =
    case patternsToVisit of
        (Node range pattern) :: restOfPatternsToVisit ->
            case pattern of
                Pattern.NamedPattern { moduleName, name } subPatterns ->
                    collectModuleNamesFromPattern
                        context
                        (List.append subPatterns restOfPatternsToVisit)
                        (Builder.add range (moduleNameForValue context name moduleName) acc)

                Pattern.UnConsPattern left right ->
                    collectModuleNamesFromPattern context (left :: right :: restOfPatternsToVisit) acc

                Pattern.TuplePattern subPatterns ->
                    collectModuleNamesFromPattern context (List.append subPatterns restOfPatternsToVisit) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectModuleNamesFromPattern context (subPattern :: restOfPatternsToVisit) acc

                Pattern.AsPattern subPattern _ ->
                    collectModuleNamesFromPattern context (subPattern :: restOfPatternsToVisit) acc

                Pattern.ListPattern subPatterns ->
                    collectModuleNamesFromPattern context (List.append subPatterns restOfPatternsToVisit) acc

                _ ->
                    collectModuleNamesFromPattern context restOfPatternsToVisit acc

        [] ->
            acc


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor (Node nodeRange node) context =
    case node of
        Expression.LetExpression letExpression ->
            let
                ( letScopeId, scopeIdCounter2 ) =
                    ScopeId.new context.scopeIdCounter

                letScope : ScopeInfo
                letScope =
                    createScope
                        { id = letScopeId
                        , parentId = (NonEmpty.head context.scopeStack).id
                        , range = nodeRange
                        , bindings = letBlock.bindings
                        }

                letBlock : { bindings : Dict String Binding, bindingIdCounter : BindingId.Counter }
                letBlock =
                    List.foldr
                        (\(Node _ declaration) acc ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        (Node nameRange name) =
                                            (Node.value function.declaration).name

                                        ( binding, bindingIdCounter ) =
                                            createBinding name nameRange acc.bindingIdCounter
                                    in
                                    { bindings = Dict.insert binding.name binding acc.bindings
                                    , bindingIdCounter = bindingIdCounter
                                    }

                                Expression.LetDestructuring pattern _ ->
                                    let
                                        ( bindings, bindingIdCounter ) =
                                            collectNamesFromPattern [ pattern ] acc.bindingIdCounter acc.bindings
                                    in
                                    { bindings = bindings
                                    , bindingIdCounter = bindingIdCounter
                                    }
                        )
                        { bindings = Dict.empty, bindingIdCounter = context.bindingIdCounter }
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
                                                ( scopeId, scopeIdCounter ) =
                                                    ScopeId.new acc.scopeIdCounter

                                                ( bindings, bindingIdCounter ) =
                                                    collectNamesFromPattern arguments acc.bindingIdCounter Dict.empty
                                            in
                                            { branches =
                                                NonEmpty.cons
                                                    ( Node.range expression
                                                    , createScope
                                                        { id = scopeId
                                                        , parentId = letScopeId
                                                        , range = declRange
                                                        , bindings = bindings
                                                        }
                                                    )
                                                    acc.branches
                                            , scopeIdCounter = scopeIdCounter
                                            , bindingIdCounter = bindingIdCounter
                                            }

                                Expression.LetDestructuring _ _ ->
                                    acc
                        )
                        { branches = NonEmpty.cons ( nodeRange, letScope ) context.branches
                        , scopeIdCounter = scopeIdCounter2
                        , bindingIdCounter = letBlock.bindingIdCounter
                        }
                        letExpression.declarations

                lookupTable : ModuleNameLookupTableBuilder
                lookupTable =
                    List.foldl
                        (\(Node _ declaration) acc ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        withDeclarationModuleName : ModuleNameLookupTableBuilder
                                        withDeclarationModuleName =
                                            collectModuleNamesFromPattern context
                                                (Node.value function.declaration).arguments
                                                acc
                                    in
                                    case function.signature of
                                        Just (Node _ signature) ->
                                            collectModuleNamesFromTypeAnnotation
                                                context
                                                [ signature.typeAnnotation ]
                                                withDeclarationModuleName

                                        Nothing ->
                                            withDeclarationModuleName

                                Expression.LetDestructuring pattern _ ->
                                    collectModuleNamesFromPattern context [ pattern ] acc
                        )
                        context.lookupTable
                        letExpression.declarations
            in
            { context
                | lookupTable = lookupTable
                , scopeStack = NonEmpty.cons letScope context.scopeStack
                , branches = let_.branches
                , scopeIdCounter = let_.scopeIdCounter
            }

        Expression.CaseExpression caseBlock ->
            let
                parentId : ScopeId
                parentId =
                    (NonEmpty.head context.scopeStack).id

                { branches, lookupTable, scopeIdCounter, bindingIdCounter } =
                    List.foldr
                        (\( pattern, Node expressionRange _ ) acc ->
                            let
                                ( scopeId, newScopeIdCounter ) =
                                    ScopeId.new acc.scopeIdCounter

                                ( bindings, bindingIdCounter_ ) =
                                    collectNamesFromPattern [ pattern ] acc.bindingIdCounter Dict.empty
                            in
                            { branches =
                                NonEmpty.cons
                                    ( expressionRange
                                    , createScope
                                        { id = scopeId
                                        , parentId = parentId
                                        , range = expressionRange
                                        , bindings = bindings
                                        }
                                    )
                                    acc.branches
                            , lookupTable = collectModuleNamesFromPattern context [ pattern ] acc.lookupTable
                            , scopeIdCounter = newScopeIdCounter
                            , bindingIdCounter = bindingIdCounter_
                            }
                        )
                        { branches = context.branches
                        , lookupTable = context.lookupTable
                        , scopeIdCounter = context.scopeIdCounter
                        , bindingIdCounter = context.bindingIdCounter
                        }
                        caseBlock.cases
            in
            { context
                | branches = branches
                , lookupTable = lookupTable
                , scopeIdCounter = scopeIdCounter
                , bindingIdCounter = bindingIdCounter
            }

        Expression.FunctionOrValue moduleName name ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context name moduleName)
                        context.lookupTable
            }
                |> addReference moduleName name nodeRange

        Expression.RecordUpdateExpression (Node range name) _ ->
            { context
                | lookupTable =
                    Builder.add
                        range
                        (moduleNameForValue context name [])
                        context.lookupTable
            }

        Expression.LambdaExpression { args, expression } ->
            let
                range : Range
                range =
                    Node.range expression

                ( scopeId, scopeIdCounter ) =
                    ScopeId.new context.scopeIdCounter

                ( bindings, bindingIdCounter ) =
                    collectNamesFromPattern args context.bindingIdCounter Dict.empty
            in
            { context
                | lookupTable = collectModuleNamesFromPattern context args context.lookupTable
                , scopeIdCounter = scopeIdCounter
                , bindingIdCounter = bindingIdCounter
                , branches =
                    NonEmpty.cons
                        ( range
                        , createScope
                            { id = scopeId
                            , parentId = (NonEmpty.head context.scopeStack).id
                            , range = range
                            , bindings = bindings
                            }
                        )
                        context.branches
            }

        Expression.PrefixOperator op ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context op [])
                        context.lookupTable
            }

        Expression.OperatorApplication op _ _ _ ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context op [])
                        context.lookupTable
            }

        _ ->
            context


collectModuleNamesFromTypeAnnotation : Context -> List (Node TypeAnnotation) -> ModuleNameLookupTableBuilder -> ModuleNameLookupTableBuilder
collectModuleNamesFromTypeAnnotation context typeAnnotationsToVisit acc =
    case typeAnnotationsToVisit of
        (Node _ typeAnnotationNode) :: remainingTypeAnnotationsToVisit ->
            case typeAnnotationNode of
                TypeAnnotation.Typed (Node range ( moduleName, name )) args ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (List.append args remainingTypeAnnotationsToVisit)
                        (Builder.add range (moduleNameForType context name moduleName) acc)

                TypeAnnotation.Tupled nodes ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (List.append nodes remainingTypeAnnotationsToVisit)
                        acc

                TypeAnnotation.Record fields ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentMapAppend (\(Node _ ( _, value )) -> value) fields remainingTypeAnnotationsToVisit)
                        acc

                TypeAnnotation.GenericRecord _ (Node _ fields) ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentMapAppend (\(Node _ ( _, value )) -> value) fields remainingTypeAnnotationsToVisit)
                        acc

                TypeAnnotation.FunctionTypeAnnotation left right ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (left :: right :: remainingTypeAnnotationsToVisit)
                        acc

                _ ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        remainingTypeAnnotationsToVisit
                        acc

        [] ->
            acc


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
            case Dict.get moduleNameOrAlias context.importAliases of
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


addReference : ModuleName -> String -> Range -> Context -> Context
addReference moduleName valueName range context =
    case moduleName of
        [] ->
            case
                NonEmpty.mapFirstThat
                    (\scope ->
                        Dict.get valueName scope.bindings
                            |> Maybe.map
                                (\binding ->
                                    let
                                        bindings : Dict String Binding
                                        bindings =
                                            Dict.insert valueName { binding | references = range :: binding.references } scope.bindings
                                    in
                                    { scope | bindings = bindings }
                                )
                    )
                    context.scopeStack
            of
                Just scopeStack ->
                    { context | scopeStack = scopeStack }

                Nothing ->
                    --Dict.get valueName context.importedFunctions
                    --    |> Maybe.withDefault []
                    -- TODO
                    context

        moduleNameOrAlias :: [] ->
            --case Dict.get moduleNameOrAlias context.importAliases of
            --    Just aliases ->
            --        NonEmpty.findWithHeadAsDefault
            --            (\aliasedModuleName ->
            --                case Dict.get aliasedModuleName context.modules of
            --                    Just module_ ->
            --                        isValueDeclaredInModule valueName module_
            --
            --                    Nothing ->
            --                        False
            --            )
            --            aliases
            --
            --    Nothing ->
            --        moduleName
            -- TODO
            context

        _ ->
            context


{-| Get the name of the module where a type was defined.
A type can be either a custom type or a type alias.

  - The second argument (`String`) is the name of the type
  - The third argument (`ModuleName`) is the module name that was used next to the type name where you found it

-}
moduleNameForType : Context -> String -> ModuleName -> ModuleName
moduleNameForType context typeName moduleName =
    case moduleName of
        [] ->
            if Set.member typeName context.localTypes then
                []

            else
                Dict.get typeName context.importedTypes
                    |> Maybe.withDefault []

        _ :: [] ->
            case Dict.get (joinModuleName moduleName) context.importAliases of
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
    NonEmpty.any (\{ bindings } -> Dict.member name bindings) scopes


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
