module Review.Resources.ModuleNameLookupTable exposing (compute)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
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
import Review.Resources.ElmCorePrelude
import Review.Resources.Internal exposing (ModuleNameLookupTable)
import Review.Resources.ModuleNameLookupTableBuilder as Builder exposing (ModuleNameLookupTableBuilder)
import Set exposing (Set)
import Vendor.ListExtra as ListExtra


type alias Context =
    { namesInScope : NonEmpty ScopeNames
    , scopeIdStack : NonEmpty Int
    , scopeIdCounter : Int
    , scopes : List Scope
    , localTypes : Set String
    , importAliases : Dict String (NonEmpty ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTable : ModuleNameLookupTableBuilder
    , branches : NonEmpty ( Range, ScopeNames )
    , caseToExit : NonEmpty Range
    }


type alias ScopeNames =
    Set String


type alias Scope =
    { id : Int
    , range : Range
    , names : ScopeNames
    }


compute : ModuleName -> OpaqueProjectModule -> ProjectCache -> ( ModuleNameLookupTable, ProjectCache )
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
        Just lookupTable ->
            ( lookupTable, projectCache )

        Nothing ->
            let
                lookupTable : ModuleNameLookupTable
                lookupTable =
                    computeHelp moduleName module_ projectCache
            in
            ( lookupTable
            , { projectCache | lookupTables = Dict.insert moduleName { key = cacheKey, lookupTable = lookupTable } projectCache.lookupTables }
            )


reuseFromCache :
    ModuleCacheKey
    -> ModuleName
    -> Dict ModuleName { key : ModuleCacheKey, lookupTable : ModuleNameLookupTable }
    -> Maybe ModuleNameLookupTable
reuseFromCache cacheKey moduleName lookupTables =
    case Dict.get moduleName lookupTables of
        Just cache ->
            if cache.key.contentHash == cacheKey.contentHash && cache.key.implicitImports == cacheKey.implicitImports then
                Just cache.lookupTable

            else
                Nothing

        Nothing ->
            Nothing


computeHelp : ModuleName -> OpaqueProjectModule -> ProjectCache -> ModuleNameLookupTable
computeHelp moduleName module_ projectCache =
    let
        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        baseModuleContext : Context
        baseModuleContext =
            Dict.foldl
                (\key docs dict ->
                    Dict.insert key (DocsModule.mod docs) dict
                )
                projectCache.dependencies
                projectCache.modules
                |> computeBaseModule

        moduleContext : Context
        moduleContext =
            List.foldl importVisitor baseModuleContext moduleAst.imports
                |> declarationListVisitor moduleAst.declarations
                |> collectLookupTable moduleAst.declarations
    in
    Builder.finalize moduleName moduleContext.lookupTable


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


computeBaseModule : Dict ModuleName Elm.Docs.Module -> Context
computeBaseModule modules =
    List.foldl importVisitor
        { namesInScope = NonEmpty.fromElement Set.empty
        , scopeIdCounter = 1
        , scopeIdStack = NonEmpty.fromElement 0
        , scopes = []
        , localTypes = Set.empty
        , importAliases = Dict.empty
        , importedFunctions = Dict.empty
        , importedTypes = Dict.empty
        , modules = modules
        , lookupTable = Builder.empty
        , branches = NonEmpty.fromElement ( Range.empty, Set.empty )
        , caseToExit = NonEmpty.fromElement Range.empty
        }
        Review.Resources.ElmCorePrelude.elmCorePrelude


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
visitExpressions node context =
    -- IGNORE TCO
    let
        newContext : Context
        newContext =
            context
                |> popScopeEnter node
                |> expressionEnterVisitor node
    in
    List.foldl
        visitExpressions
        newContext
        (expressionChildren node)
        |> popScopeExit node


declarationListVisitor : List (Node Declaration) -> Context -> Context
declarationListVisitor declarations context =
    List.foldl registerDeclaration context declarations


registerDeclaration : Node Declaration -> Context -> Context
registerDeclaration (Node _ declaration) context =
    case declaration of
        Declaration.FunctionDeclaration function ->
            let
                name : String
                name =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            addToScope name context

        Declaration.AliasDeclaration alias ->
            let
                name : String
                name =
                    Node.value alias.name

                registerAlias : Context -> Context
                registerAlias ctx =
                    case Node.value alias.typeAnnotation of
                        TypeAnnotation.Record _ ->
                            addToScope name ctx

                        _ ->
                            ctx
            in
            { context | localTypes = Set.insert name context.localTypes }
                |> registerAlias

        Declaration.CustomTypeDeclaration customType ->
            List.foldl
                (\(Node _ constructor) context_ ->
                    addToScope (Node.value constructor.name) context_
                )
                { context | localTypes = Set.insert (Node.value customType.name) context.localTypes }
                customType.constructors

        Declaration.PortDeclaration signature ->
            let
                name : String
                name =
                    Node.value signature.name
            in
            addToScope name context

        Declaration.InfixDeclaration infix_ ->
            let
                name : String
                name =
                    Node.value infix_.operator
            in
            addToScope name context

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            context


addToScope : String -> Context -> Context
addToScope name context =
    let
        newScopes : NonEmpty ScopeNames
        newScopes =
            NonEmpty.mapHead
                (\scope -> Set.insert name scope)
                context.namesInScope
    in
    { context | namesInScope = newScopes }



-- IMPORT VISITOR


importVisitor : Node Import -> Context -> Context
importVisitor (Node _ import_) context =
    let
        importAliases : Dict String (NonEmpty ModuleName)
        importAliases =
            registerImportAlias import_ context.importAliases
    in
    registerImportedFunctionsAndTypes import_ { context | importAliases = importAliases }


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


registerImportedFunctionsAndTypes : Import -> Context -> Context
registerImportedFunctionsAndTypes import_ context =
    case import_.exposingList of
        Nothing ->
            context

        Just (Node _ exposing_) ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value import_.moduleName

                module_ : Elm.Docs.Module
                module_ =
                    case Dict.get moduleName context.modules of
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
                    { context
                        | importedFunctions = importedFunctions
                        , importedTypes = importedTypes
                    }

                Exposing.Explicit topLevelExposeList ->
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
                    { context
                        | importedFunctions = importedFunctions
                        , importedTypes = importedTypes
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
                (Node _ functionDeclaration) =
                    function.declaration

                newScope : ScopeNames
                newScope =
                    parameters functionDeclaration.arguments

                newContext : Context
                newContext =
                    { context
                        | scopeIdCounter = context.scopeIdCounter + 1
                        , scopeIdStack = NonEmpty.cons context.scopeIdCounter context.scopeIdStack
                        , namesInScope = NonEmpty.cons newScope context.namesInScope
                    }

                lookupTableAfterArguments : ModuleNameLookupTableBuilder
                lookupTableAfterArguments =
                    collectModuleNamesFromPattern newContext functionDeclaration.arguments newContext.lookupTable

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
            { newContext | lookupTable = finalLookupTable }

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
declarationExitVisitor (Node range node) context =
    case node of
        Declaration.FunctionDeclaration _ ->
            { context
                | scopeIdStack = NonEmpty.pop context.scopeIdStack
                , namesInScope = NonEmpty.pop context.namesInScope
                , caseToExit = NonEmpty.pop context.caseToExit
                , scopes =
                    { id = NonEmpty.head context.scopeIdStack
                    , range = range
                    , names = NonEmpty.head context.namesInScope
                    }
                        :: context.scopes
            }

        _ ->
            context


parameters : List (Node Pattern) -> ScopeNames
parameters patterns =
    collectNamesFromPattern patterns Set.empty


collectNamesFromPattern : List (Node Pattern) -> ScopeNames -> ScopeNames
collectNamesFromPattern patternsToVisit acc =
    case patternsToVisit of
        (Node _ pattern) :: restOfPatternsToVisit ->
            case pattern of
                Pattern.VarPattern name ->
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        (Set.insert name acc)

                Pattern.NamedPattern _ subPatterns ->
                    collectNamesFromPattern (List.append subPatterns restOfPatternsToVisit) acc

                Pattern.RecordPattern names ->
                    collectNamesFromPattern
                        restOfPatternsToVisit
                        (List.foldl
                            (\(Node _ name) subAcc ->
                                Set.insert name subAcc
                            )
                            acc
                            names
                        )

                Pattern.ParenthesizedPattern subPattern ->
                    collectNamesFromPattern (subPattern :: restOfPatternsToVisit) acc

                Pattern.AsPattern subPattern (Node _ aliasName) ->
                    collectNamesFromPattern
                        (subPattern :: restOfPatternsToVisit)
                        (Set.insert aliasName acc)

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


popScopeEnter : Node Expression -> Context -> Context
popScopeEnter (Node range _) context =
    let
        ( branchRange, scope ) =
            NonEmpty.head context.branches
    in
    if range == branchRange then
        { context
            | scopeIdCounter = context.scopeIdCounter + 1
            , scopeIdStack = NonEmpty.cons context.scopeIdCounter context.scopeIdStack
            , namesInScope = NonEmpty.cons scope context.namesInScope
            , caseToExit = NonEmpty.cons range context.caseToExit
            , branches = NonEmpty.pop context.branches
        }

    else
        context


popScopeExit : Node Expression -> Context -> Context
popScopeExit (Node range _) context =
    if range == NonEmpty.head context.caseToExit then
        { context
            | scopeIdStack = NonEmpty.pop context.scopeIdStack
            , namesInScope = NonEmpty.pop context.namesInScope
            , caseToExit = NonEmpty.pop context.caseToExit
            , scopes =
                { id = NonEmpty.head context.scopeIdStack
                , range = range
                , names = NonEmpty.head context.namesInScope
                }
                    :: context.scopes
        }

    else
        context


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor (Node nodeRange node) context =
    case node of
        Expression.LetExpression letExpression ->
            let
                let_ : { scope : ScopeNames, branches : NonEmpty ( Range, ScopeNames ) }
                let_ =
                    List.foldr
                        (\(Node _ declaration) { scope, branches } ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        { name, expression, arguments } =
                                            Node.value function.declaration

                                        newScope : ScopeNames
                                        newScope =
                                            Set.insert (Node.value name) scope

                                        namesInBranch : ScopeNames
                                        namesInBranch =
                                            collectNamesFromPattern arguments Set.empty
                                    in
                                    { scope = newScope
                                    , branches = NonEmpty.cons ( Node.range expression, namesInBranch ) branches
                                    }

                                Expression.LetDestructuring pattern _ ->
                                    { scope = collectNamesFromPattern [ pattern ] scope
                                    , branches = branches
                                    }
                        )
                        { scope = Set.empty, branches = context.branches }
                        letExpression.declarations

                newContext : Context
                newContext =
                    { context
                        | namesInScope = NonEmpty.cons let_.scope context.namesInScope
                        , branches = let_.branches
                    }

                lookupTable : ModuleNameLookupTableBuilder
                lookupTable =
                    List.foldl
                        (\(Node _ declaration) acc ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        withDeclarationModuleName : ModuleNameLookupTableBuilder
                                        withDeclarationModuleName =
                                            collectModuleNamesFromPattern newContext
                                                (Node.value function.declaration).arguments
                                                acc
                                    in
                                    case function.signature of
                                        Just (Node _ signature) ->
                                            collectModuleNamesFromTypeAnnotation
                                                newContext
                                                [ signature.typeAnnotation ]
                                                withDeclarationModuleName

                                        Nothing ->
                                            withDeclarationModuleName

                                Expression.LetDestructuring pattern _ ->
                                    collectModuleNamesFromPattern newContext [ pattern ] acc
                        )
                        newContext.lookupTable
                        letExpression.declarations
            in
            { newContext | lookupTable = lookupTable }

        Expression.CaseExpression caseBlock ->
            let
                ( branches, lookupTable ) =
                    List.foldr
                        (\( pattern, Node expressionRange _ ) ( branchesAcc, lookupTableAcc ) ->
                            ( NonEmpty.cons
                                ( expressionRange
                                , collectNamesFromPattern [ pattern ] Set.empty
                                )
                                branchesAcc
                            , collectModuleNamesFromPattern context [ pattern ] lookupTableAcc
                            )
                        )
                        ( context.branches, context.lookupTable )
                        caseBlock.cases
            in
            { context
                | branches = branches
                , lookupTable = lookupTable
            }

        Expression.FunctionOrValue moduleName name ->
            { context
                | lookupTable =
                    Builder.add
                        nodeRange
                        (moduleNameForValue context name moduleName)
                        context.lookupTable
            }

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

                names : ScopeNames
                names =
                    collectNamesFromPattern args Set.empty
            in
            { context
                | lookupTable = collectModuleNamesFromPattern context args context.lookupTable
                , branches = NonEmpty.cons ( range, names ) context.branches
                , caseToExit = NonEmpty.cons range context.caseToExit
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
            if isInScope valueName context.namesInScope then
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


isInScope : String -> NonEmpty ScopeNames -> Bool
isInScope name scopes =
    NonEmpty.any (Set.member name) scopes


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
