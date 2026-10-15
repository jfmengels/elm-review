module Review.ModuleNameLookupTable.Compute exposing (compute, computeJustModuleDocs, findModuleDocumentation)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.TypeInference
import Elm.TypeInference.Type
import NonEmpty exposing (NonEmpty)
import Review.Cache.ContentHash exposing (ContentHash)
import Review.ModuleNameLookupTable.Builder as Builder exposing (ModuleNameLookupTableBuilder)
import Review.ModuleNameLookupTable.ComputeContext exposing (Context, Scope)
import Review.ModuleNameLookupTable.Internal exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Project.DocsModule as DocsModule exposing (DocsModule)
import Review.Project.ProjectCache as ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject exposing (ValidProject)
import Review.Resources.ElmCorePrelude
import Set
import Vendor.ListExtra as ListExtra


compute :
    ModuleName
    -> OpaqueProjectModule
    -> ValidProject
    ->
        { moduleNameLookupTable : ModuleNameLookupTable
        , moduleDocs : Elm.Docs.Module
        , project : ValidProject
        }
compute moduleName module_ project =
    let
        projectCache : ProjectCache
        projectCache =
            ValidProject.projectCache project

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

        cacheToReuse : Maybe { moduleNameLookupTable : ModuleNameLookupTable, moduleDocs : Elm.Docs.Module, project : ValidProject }
        cacheToReuse =
            case Dict.get moduleName projectCache.lookupTables of
                Just cache ->
                    if cache.key.contentHash == ProjectModule.contentHash module_ && cache.key.implicitImports == implicitImports then
                        Just
                            { moduleNameLookupTable = cache.lookupTable
                            , moduleDocs = Dict.get moduleName projectCache.modules |> Maybe.map DocsModule.mod |> Maybe.withDefault emptyModuleDocs
                            , project = project
                            }

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    case cacheToReuse of
        Just cache ->
            cache

        Nothing ->
            computeHelp
                { implicitImports = implicitImports
                , contentHash = ProjectModule.contentHash module_
                }
                moduleName
                module_
                project


computeJustModuleDocs :
    ModuleName
    -> OpaqueProjectModule
    -> ValidProject
    ->
        { moduleDocs : Elm.Docs.Module
        , project : ValidProject
        }
computeJustModuleDocs moduleName module_ project =
    let
        projectCache : ProjectCache
        projectCache =
            ValidProject.projectCache project
    in
    case Dict.get moduleName projectCache.lookupTables of
        Just cache ->
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
            in
            if cache.key.contentHash == ProjectModule.contentHash module_ && cache.key.implicitImports == implicitImports then
                { moduleDocs = Dict.get moduleName projectCache.modules |> Maybe.map DocsModule.mod |> Maybe.withDefault emptyModuleDocs
                , project = project
                }

            else
                computeJustModuleDocsHelp moduleName module_ project

        Nothing ->
            computeJustModuleDocsHelp moduleName module_ project


computeHelp :
    ProjectCache.ModuleCacheKey
    -> ModuleName
    -> OpaqueProjectModule
    -> ValidProject
    ->
        { moduleNameLookupTable : ModuleNameLookupTable
        , moduleDocs : Elm.Docs.Module
        , project : ValidProject
        }
computeHelp cacheKey moduleName module_ project =
    let
        projectCache : ProjectCache
        projectCache =
            ValidProject.projectCache project

        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        elmJsonContentHash : Maybe ContentHash
        elmJsonContentHash =
            ValidProject.elmJsonHash project

        ({ deps, baseModuleContext } as depsCache) =
            -- TODO Only invalidate the lookup tables if the dependencies in elm.json have changed?
            -- i.e. if only the description has changed but not the dependencies
            case reuseMaybe (elmJsonContentHash == projectCache.elmJsonContentHash) projectCache.dependencies of
                Just cache ->
                    cache

                Nothing ->
                    let
                        deps_ : Dict ModuleName Elm.Docs.Module
                        deps_ =
                            computeDependencies project
                    in
                    { deps = deps_, baseModuleContext = computeBaseModule moduleName (ValidProject.typeInferenceProject project) (preludeModuleDocs deps_) }

        dataForModuleDocs : DataForModuleDocs
        dataForModuleDocs =
            { getModule = ValidProject.getModuleByModuleName project
            , baseModuleContext = baseModuleContext
            , deps = deps
            }

        { imported, projectModules, types } =
            List.foldl
                (\node acc -> computeImportedModulesDocs dataForModuleDocs node acc)
                { imported = baseModuleContext.modules, projectModules = projectCache.modules, types = baseModuleContext.types }
                moduleAst.imports

        moduleContext : Context
        moduleContext =
            { baseModuleContext | modules = imported, types = types }
                |> collectModuleDocs moduleAst
                |> collectLookupTable moduleAst.declarations

        lookupTable : ModuleNameLookupTable
        lookupTable =
            Builder.finalize moduleName moduleContext.lookupTable

        moduleDocsForFile : Elm.Docs.Module
        moduleDocsForFile =
            { name = joinModuleName moduleName
            , comment = extractModuleDocumentation moduleAst
            , unions = moduleContext.exposedUnions
            , aliases = moduleContext.exposedAliases
            , values = moduleContext.exposedValues
            , binops = moduleContext.exposedBinops
            }

        modules : Dict ModuleName DocsModule
        modules =
            Dict.insert moduleName (DocsModule.create moduleDocsForFile) projectModules

        newProjectCache : ProjectCache
        newProjectCache =
            { dependencies = Just depsCache
            , elmJsonContentHash = elmJsonContentHash
            , modules = modules
            , lookupTables =
                Dict.insert moduleName
                    { key = cacheKey
                    , lookupTable = lookupTable
                    }
                    projectCache.lookupTables
            }
    in
    { moduleNameLookupTable = lookupTable
    , moduleDocs = moduleDocsForFile
    , project =
        project
            |> ValidProject.updateProjectCache newProjectCache
            |> ValidProject.setTypeInferenceProject moduleContext.types
    }


computeJustModuleDocsHelp :
    ModuleName
    -> OpaqueProjectModule
    -> ValidProject
    ->
        { moduleDocs : Elm.Docs.Module
        , project : ValidProject
        }
computeJustModuleDocsHelp moduleName module_ project =
    let
        projectCache : ProjectCache
        projectCache =
            ValidProject.projectCache project

        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        elmJsonContentHash : Maybe ContentHash
        elmJsonContentHash =
            ValidProject.elmJsonHash project

        ({ deps, baseModuleContext } as depsCache) =
            -- TODO Only invalidate the lookup tables if the dependencies in elm.json have changed?
            -- i.e. if only the description has changed but not the dependencies
            case reuseMaybe (elmJsonContentHash == projectCache.elmJsonContentHash) projectCache.dependencies of
                Just cache ->
                    cache

                Nothing ->
                    let
                        deps_ : Dict ModuleName Elm.Docs.Module
                        deps_ =
                            computeDependencies project
                    in
                    { deps = deps_, baseModuleContext = computeBaseModule moduleName (ValidProject.typeInferenceProject project) (preludeModuleDocs deps_) }

        dataForModuleDocs : DataForModuleDocs
        dataForModuleDocs =
            { getModule = ValidProject.getModuleByModuleName project
            , baseModuleContext = baseModuleContext
            , deps = deps
            }

        { imported, projectModules, types } =
            List.foldl
                (\node acc -> computeImportedModulesDocs dataForModuleDocs node acc)
                { imported = baseModuleContext.modules, projectModules = projectCache.modules, types = baseModuleContext.types }
                moduleAst.imports

        moduleContext : Context
        moduleContext =
            { baseModuleContext | modules = imported, types = types }
                |> collectModuleDocs moduleAst

        moduleDocsForFile : Elm.Docs.Module
        moduleDocsForFile =
            { name = joinModuleName moduleName
            , comment = extractModuleDocumentation moduleAst
            , unions = moduleContext.exposedUnions
            , aliases = moduleContext.exposedAliases
            , values = moduleContext.exposedValues
            , binops = moduleContext.exposedBinops
            }

        modules : Dict ModuleName DocsModule
        modules =
            Dict.insert moduleName (DocsModule.create moduleDocsForFile) projectModules

        newProjectCache : ProjectCache
        newProjectCache =
            { dependencies = Just depsCache
            , elmJsonContentHash = elmJsonContentHash
            , modules = modules
            , lookupTables = projectCache.lookupTables
            }
    in
    { moduleDocs = moduleDocsForFile
    , project =
        project
            |> ValidProject.updateProjectCache newProjectCache
            |> ValidProject.setTypeInferenceProject moduleContext.types
    }


reuseMaybe : Bool -> Maybe a -> Maybe a
reuseMaybe check maybe =
    if check then
        maybe

    else
        Nothing


emptyModuleDocs : Elm.Docs.Module
emptyModuleDocs =
    { name = ""
    , comment = ""
    , unions = []
    , aliases = []
    , values = []
    , binops = []
    }


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


type alias DataForModuleDocs =
    { getModule : ModuleName -> Maybe OpaqueProjectModule
    , baseModuleContext : Context
    , deps : Dict ModuleName Elm.Docs.Module
    }


computeImportedModulesDocs :
    DataForModuleDocs
    -> Node Import
    -> { imported : Dict ModuleName Elm.Docs.Module, projectModules : Dict ModuleName DocsModule, types : Elm.TypeInference.Project }
    -> { imported : Dict ModuleName Elm.Docs.Module, projectModules : Dict ModuleName DocsModule, types : Elm.TypeInference.Project }
computeImportedModulesDocs data (Node _ import_) acc =
    let
        importedModuleName : ModuleName
        importedModuleName =
            Node.value import_.moduleName
    in
    case Dict.get importedModuleName acc.projectModules of
        Just importedModule ->
            { imported = Dict.insert importedModuleName (DocsModule.mod importedModule) acc.imported
            , projectModules = acc.projectModules
            , types = acc.types
            }

        Nothing ->
            case data.getModule importedModuleName of
                Just importedModule ->
                    let
                        -- This occurs when an imported file hasn't been reviewed
                        -- (and therefore its module docs computation was skipped)
                        -- because it used its result cache.
                        { moduleDocs, projectModules, types } =
                            computeOnlyModuleDocs data importedModuleName importedModule acc.projectModules acc.types
                    in
                    { imported = Dict.insert importedModuleName moduleDocs acc.imported
                    , projectModules = projectModules
                    , types = types
                    }

                Nothing ->
                    case Dict.get importedModuleName data.deps of
                        Just importedModule ->
                            { imported = Dict.insert importedModuleName importedModule acc.imported
                            , projectModules = acc.projectModules
                            , types = acc.types
                            }

                        Nothing ->
                            acc


computeOnlyModuleDocs :
    DataForModuleDocs
    -> ModuleName
    -> OpaqueProjectModule
    -> Dict ModuleName DocsModule
    -> Elm.TypeInference.Project
    -> { moduleDocs : Elm.Docs.Module, projectModules : Dict ModuleName DocsModule, types : Elm.TypeInference.Project }
computeOnlyModuleDocs ({ baseModuleContext } as data) moduleName module_ baseProjectModules baseTypes =
    let
        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        { imported, projectModules, types } =
            List.foldl
                (\node acc -> computeImportedModulesDocs data node acc)
                { imported = data.baseModuleContext.modules, projectModules = baseProjectModules, types = baseTypes }
                moduleAst.imports

        moduleContext : Context
        moduleContext =
            { baseModuleContext | modules = imported, types = types }
                |> collectModuleDocs moduleAst

        moduleDocs : Elm.Docs.Module
        moduleDocs =
            { name = joinModuleName moduleName
            , comment = extractModuleDocumentation moduleAst
            , unions = moduleContext.exposedUnions
            , aliases = moduleContext.exposedAliases
            , values = moduleContext.exposedValues
            , binops = moduleContext.exposedBinops
            }
    in
    { moduleDocs = moduleDocs
    , projectModules = Dict.insert moduleName (DocsModule.create moduleDocs) projectModules
    , types = moduleContext.types
    }


computeDependencies : ValidProject -> Dict ModuleName Elm.Docs.Module
computeDependencies project =
    project
        |> ValidProject.directDependencies
        |> Dict.foldl
            (\_ dep acc ->
                List.foldl
                    (\dependencyModule subAcc ->
                        Dict.insert (String.split "." dependencyModule.name) dependencyModule subAcc
                    )
                    acc
                    (Review.Project.Dependency.modules dep)
            )
            Dict.empty


computeBaseModule : ModuleName -> Elm.TypeInference.Project -> Dict ModuleName Elm.Docs.Module -> Context
computeBaseModule moduleName types elmCorePreludeModules =
    List.foldl importVisitor
        { scopes = NonEmpty.fromElement Set.empty
        , types = types
        , moduleName = moduleName
        , localTypes = Set.empty
        , importAliases = Dict.empty
        , importedFunctions = Dict.empty
        , importedTypes = Dict.empty
        , modules = elmCorePreludeModules
        , exposesEverything = False
        , exposedNames = Dict.empty
        , exposedUnions = []
        , exposedAliases = []
        , exposedValues = []
        , exposedBinops = []
        , lookupTable = Builder.empty
        , branches = NonEmpty.fromElement ( Range.empty, Set.empty )
        , caseToExit = NonEmpty.fromElement Range.empty
        }
        Review.Resources.ElmCorePrelude.elmCorePrelude


extractModuleDocumentation : Elm.Syntax.File.File -> String
extractModuleDocumentation ast =
    extractDocumentation (findModuleDocumentation ast)


extractDocumentation : Maybe (Node String) -> String
extractDocumentation maybeDoc =
    case maybeDoc of
        Just (Node _ str) ->
            -- Remove surrounding {-| and -}
            String.slice 3 -2 str

        Nothing ->
            ""


findModuleDocumentation : Elm.Syntax.File.File -> Maybe (Node String)
findModuleDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                firstImport :: _ ->
                    (Node.range firstImport).start.row

                [] ->
                    case ast.declarations of
                        firstDeclaration :: _ ->
                            (Node.range firstDeclaration).start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    findModuleDocumentationBeforeCutOffLine cutOffLine ast.comments


findModuleDocumentationBeforeCutOffLine : Int -> List (Node String) -> Maybe (Node String)
findModuleDocumentationBeforeCutOffLine cutOffLine comments =
    case comments of
        [] ->
            Nothing

        ((Node range content) as comment) :: restOfComments ->
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just comment

            else
                findModuleDocumentationBeforeCutOffLine cutOffLine restOfComments


collectModuleDocs : Elm.Syntax.File.File -> Context -> Context
collectModuleDocs ast context =
    List.foldl importVisitor context ast.imports
        |> moduleDefinitionVisitor ast.moduleDefinition
        |> declarationListVisitor ast.declarations


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


preludeModuleDocs : Dict ModuleName Elm.Docs.Module -> Dict ModuleName Elm.Docs.Module
preludeModuleDocs deps =
    List.foldl
        (\(Node _ import_) acc ->
            let
                importedModuleName : ModuleName
                importedModuleName =
                    Node.value import_.moduleName
            in
            case Dict.get importedModuleName deps of
                Just importedModule ->
                    Dict.insert importedModuleName importedModule acc

                Nothing ->
                    acc
        )
        Dict.empty
        Review.Resources.ElmCorePrelude.elmCorePrelude


declarationListVisitor : List (Node Declaration) -> Context -> Context
declarationListVisitor declarations context =
    List.foldl (registerDeclaration declarations) context declarations


registerDeclaration : List (Node Declaration) -> Node Declaration -> Context -> Context
registerDeclaration allDeclarations (Node _ declaration) context =
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
            context
                |> addToScope name
                |> ifExposed (\ctx -> registerExposedValue function name ctx) name

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
                |> ifExposed (\ctx -> registerExposedTypeAlias alias ctx) name

        Declaration.CustomTypeDeclaration customType ->
            List.foldl
                (\(Node _ constructor) context_ ->
                    addToScope (Node.value constructor.name) context_
                )
                { context | localTypes = Set.insert (Node.value customType.name) context.localTypes }
                customType.constructors
                |> registerExposedCustomType customType

        Declaration.PortDeclaration signature ->
            let
                name : String
                name =
                    Node.value signature.name
            in
            context
                |> addToScope name
                |> ifExposed
                    (\ctx ->
                        registerPortValue signature.typeAnnotation name ctx
                    )
                    name

        Declaration.InfixDeclaration infix_ ->
            let
                name : String
                name =
                    Node.value infix_.operator
            in
            context
                |> addToScope name
                |> ifExposed
                    (\ctx ->
                        registerExposedBinop infix_ name allDeclarations ctx
                    )
                    name

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            context


addToScope : String -> Context -> Context
addToScope name context =
    let
        newScopes : NonEmpty Scope
        newScopes =
            NonEmpty.mapHead
                (\scope -> Set.insert name scope)
                context.scopes
    in
    { context | scopes = newScopes }


registerExposedValue : Expression.Function -> String -> Context -> Context
registerExposedValue function name context =
    let
        ( type_, types ) =
            case function.signature of
                Just (Node _ { typeAnnotation }) ->
                    ( syntaxTypeAnnotationToDocsType context typeAnnotation
                    , context.types
                    )

                Nothing ->
                    case Elm.TypeInference.getType context.moduleName (Node.range function.declaration) context.types of
                        ( Ok type__, newTypes ) ->
                            ( inferredTypeToDocsType type__, newTypes )

                        ( Err _, newTypes ) ->
                            ( Elm.Type.Tuple [], newTypes )
    in
    { context
        | exposedValues =
            { name = name
            , comment = extractDocumentation function.documentation
            , tipe = type_
            }
                :: context.exposedValues
        , types = types
    }


registerPortValue : Node TypeAnnotation -> String -> Context -> Context
registerPortValue typeAnnotation name context =
    { context
        | exposedValues =
            { name = name

            -- TODO Collect documentation for port
            , comment = ""
            , tipe = syntaxTypeAnnotationToDocsType context typeAnnotation
            }
                :: context.exposedValues
    }


registerExposedCustomType : Elm.Syntax.Type.Type -> Context -> Context
registerExposedCustomType union context =
    if context.exposesEverything then
        registerCustomType union union.constructors context

    else
        case Dict.get (Node.value union.name) context.exposedNames of
            Just True ->
                registerCustomType union union.constructors context

            Just False ->
                registerCustomType union [] context

            Nothing ->
                context


registerCustomType : Elm.Syntax.Type.Type -> List (Node Elm.Syntax.Type.ValueConstructor) -> Context -> Context
registerCustomType union constructors context =
    { context
        | exposedUnions =
            { name = Node.value union.name
            , comment = extractDocumentation union.documentation
            , args = List.map Node.value union.generics
            , tags =
                constructors
                    |> List.map
                        (\(Node _ { name, arguments }) ->
                            ( Node.value name
                            , List.map (syntaxTypeAnnotationToDocsType context) arguments
                            )
                        )
            }
                :: context.exposedUnions
    }


registerExposedTypeAlias : Elm.Syntax.TypeAlias.TypeAlias -> Context -> Context
registerExposedTypeAlias alias context =
    { context
        | exposedAliases =
            { name = Node.value alias.name
            , comment = extractDocumentation alias.documentation
            , args = List.map Node.value alias.generics
            , tipe = syntaxTypeAnnotationToDocsType context alias.typeAnnotation
            }
                :: context.exposedAliases
    }


ifExposed : (Context -> Context) -> String -> Context -> Context
ifExposed registerFn name context =
    if context.exposesEverything || Dict.member name context.exposedNames then
        registerFn context

    else
        context


registerExposedBinop : Infix -> String -> List (Node Declaration) -> Context -> Context
registerExposedBinop infix_ name allDeclarations context =
    let
        functionName : String
        functionName =
            Node.value infix_.function

        value : Elm.Docs.Value
        value =
            case
                ListExtra.findMap
                    (\(Node _ decl) ->
                        case decl of
                            Declaration.FunctionDeclaration function ->
                                if Node.value (Node.value function.declaration).name == functionName then
                                    Just
                                        { name = name
                                        , comment = extractDocumentation function.documentation
                                        , tipe =
                                            case function.signature of
                                                Just (Node _ { typeAnnotation }) ->
                                                    syntaxTypeAnnotationToDocsType context typeAnnotation

                                                Nothing ->
                                                    Elm.Type.Var "unknown"
                                        }

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                    allDeclarations
            of
                Just valueWithDetails ->
                    valueWithDetails

                Nothing ->
                    { name = name
                    , comment = ""
                    , tipe = Elm.Type.Var "unknown"
                    }
    in
    { context | exposedValues = value :: context.exposedValues }


syntaxTypeAnnotationToDocsType : Context -> Node TypeAnnotation -> Elm.Type.Type
syntaxTypeAnnotationToDocsType context (Node _ typeAnnotation) =
    -- IGNORE TCO
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Elm.Type.Var name

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) typeParameters ->
            let
                realModuleName : ModuleName
                realModuleName =
                    fullModuleNameForType context typeName moduleName
            in
            Elm.Type.Type (joinModuleName realModuleName ++ "." ++ typeName) (List.map (\node -> syntaxTypeAnnotationToDocsType context node) typeParameters)

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled list ->
            Elm.Type.Tuple (List.map (\node -> syntaxTypeAnnotationToDocsType context node) list)

        TypeAnnotation.Record updates ->
            Elm.Type.Record (recordUpdateToDocsType context updates) Nothing

        TypeAnnotation.GenericRecord (Node _ generic) (Node _ updates) ->
            Elm.Type.Record (recordUpdateToDocsType context updates) (Just generic)

        TypeAnnotation.FunctionTypeAnnotation left right ->
            Elm.Type.Lambda
                (syntaxTypeAnnotationToDocsType context left)
                (syntaxTypeAnnotationToDocsType context right)


recordUpdateToDocsType : Context -> List (Node TypeAnnotation.RecordField) -> List ( String, Elm.Type.Type )
recordUpdateToDocsType context updates =
    List.map
        (\(Node _ ( Node _ name, typeAnnotation )) ->
            ( name
            , syntaxTypeAnnotationToDocsType context typeAnnotation
            )
        )
        updates


inferredTypeToDocsType : Elm.TypeInference.Type.Type -> Elm.Type.Type
inferredTypeToDocsType type_ =
    -- IGNORE TCO
    case type_ of
        Elm.TypeInference.Type.TypeVar name ->
            Elm.Type.Var name

        Elm.TypeInference.Type.Named { moduleName, name, arguments } ->
            Elm.Type.Type (joinModuleName moduleName ++ "." ++ name) (List.map inferredTypeToDocsType arguments)

        Elm.TypeInference.Type.Unit ->
            Elm.Type.Tuple []

        Elm.TypeInference.Type.Tuple2 a b ->
            Elm.Type.Tuple [ inferredTypeToDocsType a, inferredTypeToDocsType b ]

        Elm.TypeInference.Type.Tuple3 a b c ->
            Elm.Type.Tuple [ inferredTypeToDocsType a, inferredTypeToDocsType b, inferredTypeToDocsType c ]

        Elm.TypeInference.Type.List subType ->
            Elm.Type.Type "List" [ inferredTypeToDocsType subType ]

        Elm.TypeInference.Type.Record { fields } ->
            Elm.Type.Record (inferredRecordTypeToDocsType fields) Nothing

        Elm.TypeInference.Type.ExtensibleRecord { fields, extensionTypevar } ->
            Elm.Type.Record (inferredRecordTypeToDocsType fields) (Just extensionTypevar)

        Elm.TypeInference.Type.Function { from, to } ->
            Elm.Type.Lambda
                (inferredTypeToDocsType from)
                (inferredTypeToDocsType to)

        Elm.TypeInference.Type.Int ->
            Elm.Type.Type "Basics.Int" []

        Elm.TypeInference.Type.Float ->
            Elm.Type.Type "Basics.Float" []

        Elm.TypeInference.Type.Char ->
            Elm.Type.Type "Char.Char" []

        Elm.TypeInference.Type.String ->
            Elm.Type.Type "String.String" []

        Elm.TypeInference.Type.Bool ->
            Elm.Type.Type "Basics.Bool" []

        Elm.TypeInference.Type.WebGLShader r ->
            Elm.Type.Type "WebGL.Shader"
                [ inferredTypeToDocsType r.attributes
                , inferredTypeToDocsType r.uniforms
                , inferredTypeToDocsType r.varyings
                ]


inferredRecordTypeToDocsType : Dict String Elm.TypeInference.Type.Type -> List ( String, Elm.Type.Type )
inferredRecordTypeToDocsType fields =
    Dict.foldr
        (\name typeAnnotation acc ->
            ( name
            , inferredTypeToDocsType typeAnnotation
            )
                :: acc
        )
        []
        fields



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> Context
moduleDefinitionVisitor (Node _ node) context =
    case Module.exposingList node of
        Exposing.All _ ->
            { context | exposesEverything = True }

        Exposing.Explicit list ->
            { context | exposedNames = exposedElements list }


exposedElements : List (Node Exposing.TopLevelExpose) -> Dict String Bool
exposedElements nodes =
    List.foldl
        (\(Node _ node) acc ->
            case node of
                Exposing.FunctionExpose name ->
                    Dict.insert name False acc

                Exposing.TypeOrAliasExpose name ->
                    Dict.insert name False acc

                Exposing.TypeExpose { name, open } ->
                    Dict.insert name (open /= Nothing) acc

                Exposing.InfixExpose name ->
                    Dict.insert name False acc
        )
        Dict.empty
        nodes



-- IMPORT VISITOR


importVisitor : Node Import -> Context -> Context
importVisitor (Node _ import_) context =
    context
        |> registerImportAlias import_
        |> registerImportExposed import_


registerImportAlias : Import -> Context -> Context
registerImportAlias import_ context =
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
            case Dict.get qualifier context.importAliases of
                Just previousValue ->
                    NonEmpty.cons moduleName previousValue

                Nothing ->
                    NonEmpty.fromElement moduleName
    in
    { context | importAliases = Dict.insert qualifier toInsert context.importAliases }


registerImportExposed : Import -> Context -> Context
registerImportExposed import_ context =
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
                    Dict.get moduleName context.modules
                        |> Maybe.withDefault
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
                    if List.any (\alias -> alias.name == name && isPlainRecordType alias.tipe) module_.aliases then
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
                                    List.foldl
                                        (\union subAcc ->
                                            if union.name == name then
                                                List.foldl (\( tag, _ ) subSubAcc -> Dict.insert tag moduleName subSubAcc) subAcc union.tags

                                            else
                                                subAcc
                                        )
                                        acc
                                        module_.unions
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

                newScope : Scope
                newScope =
                    parameters functionDeclaration.arguments

                newContext : Context
                newContext =
                    { context | scopes = NonEmpty.cons newScope context.scopes }

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
declarationExitVisitor (Node _ node) context =
    case node of
        Declaration.FunctionDeclaration _ ->
            { context | scopes = NonEmpty.pop context.scopes }

        _ ->
            context


parameters : List (Node Pattern) -> Scope
parameters patterns =
    collectNamesFromPattern patterns Set.empty


collectNamesFromPattern : List (Node Pattern) -> Scope -> Scope
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
            | scopes = NonEmpty.cons scope context.scopes
            , caseToExit = NonEmpty.cons range context.caseToExit
            , branches = NonEmpty.pop context.branches
        }

    else
        context


popScopeExit : Node Expression -> Context -> Context
popScopeExit (Node range _) context =
    if range == NonEmpty.head context.caseToExit then
        { context
            | scopes = NonEmpty.pop context.scopes
            , caseToExit = NonEmpty.pop context.caseToExit
        }

    else
        context


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor (Node nodeRange node) context =
    case node of
        Expression.LetExpression letExpression ->
            let
                let_ : { scope : Scope, branches : NonEmpty ( Range, Scope ) }
                let_ =
                    List.foldr
                        (\(Node _ declaration) { scope, branches } ->
                            case declaration of
                                Expression.LetFunction function ->
                                    let
                                        { name, expression, arguments } =
                                            Node.value function.declaration

                                        newScope : Scope
                                        newScope =
                                            Set.insert (Node.value name) scope

                                        namesInBranch : Scope
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
                        | scopes = NonEmpty.cons let_.scope context.scopes
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

                names : Scope
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
            if isInScope valueName context.scopes then
                []

            else
                Dict.get valueName context.importedFunctions
                    |> Maybe.withDefault []

        moduleNameOrAlias :: [] ->
            case Dict.get moduleNameOrAlias context.importAliases of
                Just aliases ->
                    case
                        NonEmpty.find
                            (\aliasedModuleName ->
                                case Dict.get aliasedModuleName context.modules of
                                    Just module_ ->
                                        isValueDeclaredInModule valueName module_

                                    Nothing ->
                                        False
                            )
                            aliases
                    of
                        Just aliasedModuleName ->
                            aliasedModuleName

                        Nothing ->
                            NonEmpty.head aliases

                Nothing ->
                    moduleName

        _ ->
            moduleName


fullModuleNameForType : Context -> String -> ModuleName -> ModuleName
fullModuleNameForType context typeName moduleName =
    case moduleNameForType context typeName moduleName of
        [] ->
            context.moduleName

        foundModuleName ->
            foundModuleName


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
                    case
                        NonEmpty.find
                            (\aliasedModuleName ->
                                case Dict.get aliasedModuleName context.modules of
                                    Just module_ ->
                                        isTypeDeclaredInModule typeName module_

                                    Nothing ->
                                        False
                            )
                            aliases
                    of
                        Just aliasedModuleName ->
                            aliasedModuleName

                        Nothing ->
                            NonEmpty.head aliases

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


isInScope : String -> NonEmpty Scope -> Bool
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
