module Review.ModuleNameLookupTable.Compute exposing (compute)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import NonEmpty exposing (NonEmpty)
import Review.Cache.ContentHash exposing (ContentHash)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal exposing (ModuleNameLookupTable)
import Review.Project.Dependency
import Review.Project.ProjectCache as ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject exposing (ValidProject)
import Set exposing (Set)
import Vendor.ListExtra as ListExtra


type alias Context =
    { scopes : NonEmpty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Set String
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , lookupTable : ModuleNameLookupTable
    }


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


type alias VariableInfo =
    { variableType : VariableType
    , node : Node String
    }


type VariableType
    = TopLevelVariable
    | CustomTypeConstructor
    | FunctionParameter
    | LetVariable
    | PatternVariable
    | Port


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }


compute : ModuleName -> OpaqueProjectModule -> ValidProject -> ( ModuleNameLookupTable, ValidProject )
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
           imported "implicitly", though `exposing (..)` or `exposing (D(..))`.
        -}
        implicitImports : Dict String (List ProjectCache.ImportedElement)
        implicitImports =
            List.foldl
                (\node acc -> computeImplicitlyImportedElements projectCache.modules node acc)
                Dict.empty
                (ProjectModule.ast module_).imports

        computeLookupTableForModule : () -> ( ModuleNameLookupTable, ValidProject )
        computeLookupTableForModule () =
            computeHelp
                { implicitImports = implicitImports
                , contentHash = ProjectModule.contentHash module_
                }
                moduleName
                module_
                project
    in
    case Dict.get moduleName projectCache.lookupTables of
        Just cache ->
            if cache.key.contentHash == ProjectModule.contentHash module_ && cache.key.implicitImports == implicitImports then
                ( cache.lookupTable, project )

            else
                computeLookupTableForModule ()

        Nothing ->
            computeLookupTableForModule ()


computeHelp : ProjectCache.ModuleCacheKey -> ModuleName -> OpaqueProjectModule -> ValidProject -> ( ModuleNameLookupTable, ValidProject )
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

        deps : Dict ModuleName Elm.Docs.Module
        deps =
            -- TODO Only invalidate the lookup tables the dependencies in elm.json have changed?
            case projectCache.dependenciesModules of
                Just cache ->
                    if elmJsonContentHash == cache.elmJsonContentHash then
                        cache.deps

                    else
                        computeDependencies project

                Nothing ->
                    computeDependencies project

        modulesByModuleName : Dict ModuleName OpaqueProjectModule
        modulesByModuleName =
            ValidProject.modulesByModuleName project

        ( imported, projectCacheWithComputedImports ) =
            List.foldl
                (\node acc -> computeImportedModulesDocs modulesByModuleName deps node acc)
                ( preludeModuleDocs deps, projectCache )
                moduleAst.imports

        ( lookupTable, modules ) =
            let
                moduleContext : Context
                moduleContext =
                    fromProjectToModule moduleName imported
                        |> collectModuleDocs moduleAst
                        |> collectLookupTable moduleAst.declarations
            in
            ( moduleContext.lookupTable
            , Dict.insert moduleName
                { name = String.join "." moduleName
                , comment = ""
                , unions = moduleContext.exposedUnions
                , aliases = moduleContext.exposedAliases
                , values = moduleContext.exposedValues
                , binops = []
                }
                projectCacheWithComputedImports.modules
            )

        newProjectCache : ProjectCache
        newProjectCache =
            { dependenciesModules = Just { elmJsonContentHash = elmJsonContentHash, deps = deps }
            , modules = modules
            , lookupTables =
                Dict.insert moduleName
                    { key = cacheKey
                    , lookupTable = lookupTable
                    }
                    projectCacheWithComputedImports.lookupTables
            }
    in
    ( lookupTable, ValidProject.updateProjectCache newProjectCache project )


computeOnlyModuleDocs :
    ModuleName
    -> OpaqueProjectModule
    -> Dict ModuleName OpaqueProjectModule
    -> Dict ModuleName Elm.Docs.Module
    -> ProjectCache
    -> ( Elm.Docs.Module, ProjectCache )
computeOnlyModuleDocs moduleName module_ modulesByModuleName deps projectCache =
    let
        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        ( imported, projectCacheWithComputedImports ) =
            List.foldl
                (\node acc -> computeImportedModulesDocs modulesByModuleName deps node acc)
                ( preludeModuleDocs deps, projectCache )
                moduleAst.imports

        moduleContext : Context
        moduleContext =
            fromProjectToModule moduleName imported
                |> collectModuleDocs moduleAst

        moduleDocs : Elm.Docs.Module
        moduleDocs =
            { name = String.join "." moduleName
            , comment = ""
            , unions = moduleContext.exposedUnions
            , aliases = moduleContext.exposedAliases
            , values = moduleContext.exposedValues
            , binops = []
            }

        modules : Dict ModuleName Elm.Docs.Module
        modules =
            Dict.insert moduleName moduleDocs projectCacheWithComputedImports.modules
    in
    ( moduleDocs, { projectCache | modules = modules } )


computeImplicitlyImportedElements :
    Dict ModuleName Elm.Docs.Module
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
                    collectExplicit moduleDocs list acc

                Nothing ->
                    acc

        Just (Node _ (Exposing.All _)) ->
            case Dict.get (Node.value import_.moduleName) modules of
                Just moduleDocs ->
                    collectAllExposed moduleDocs acc

                Nothing ->
                    acc


collectExplicit : Elm.Docs.Module -> List (Node TopLevelExpose) -> Dict String (List ProjectCache.ImportedElement) -> Dict String (List ProjectCache.ImportedElement)
collectExplicit moduleDocs list acc =
    let
        importedConstructors : List ProjectCache.ImportedElement
        importedConstructors =
            List.foldl
                (\node subAcc ->
                    case Node.value node of
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


computeImportedModulesDocs :
    Dict ModuleName OpaqueProjectModule
    -> Dict ModuleName Elm.Docs.Module
    -> Node Import
    -> ( Dict ModuleName Elm.Docs.Module, ProjectCache )
    -> ( Dict ModuleName Elm.Docs.Module, ProjectCache )
computeImportedModulesDocs modulesByModuleName deps (Node _ import_) ( accImported, accProjectCache ) =
    let
        importedModuleName : ModuleName
        importedModuleName =
            Node.value import_.moduleName
    in
    case Dict.get importedModuleName accProjectCache.modules of
        Just importedModule ->
            ( Dict.insert importedModuleName importedModule accImported, accProjectCache )

        Nothing ->
            case Dict.get importedModuleName modulesByModuleName of
                Just importedModule ->
                    let
                        ( importedModuleDocs, newProjectCacheAcc ) =
                            computeOnlyModuleDocs importedModuleName importedModule modulesByModuleName deps accProjectCache
                    in
                    ( Dict.insert importedModuleName importedModuleDocs accImported
                    , newProjectCacheAcc
                    )

                Nothing ->
                    case Dict.get importedModuleName deps of
                        Just importedModule ->
                            ( Dict.insert importedModuleName importedModule accImported, accProjectCache )

                        Nothing ->
                            ( accImported, accProjectCache )


computeDependencies : ValidProject -> Dict ModuleName Elm.Docs.Module
computeDependencies project =
    project
        |> ValidProject.directDependencies
        |> Dict.foldl (\_ dep acc -> List.append (Review.Project.Dependency.modules dep) acc) []
        |> List.foldl (\dependencyModule acc -> Dict.insert (String.split "." dependencyModule.name) dependencyModule acc) Dict.empty


fromProjectToModule : ModuleName -> Dict ModuleName Elm.Docs.Module -> Context
fromProjectToModule moduleName modules =
    { scopes = NonEmpty.fromElement emptyScope
    , localTypes = Set.empty
    , importAliases = Dict.empty
    , importedFunctions = Dict.empty
    , importedTypes = Dict.empty
    , modules = modules
    , exposesEverything = False
    , exposedNames = Set.empty
    , exposedUnions = []
    , exposedAliases = []
    , exposedValues = []
    , lookupTable = ModuleNameLookupTableInternal.empty moduleName
    }


collectModuleDocs : Elm.Syntax.File.File -> Context -> Context
collectModuleDocs ast context =
    List.foldl importVisitor context (elmCorePrelude ++ ast.imports)
        |> moduleDefinitionVisitor ast.moduleDefinition
        |> declarationListVisitor ast.declarations


collectLookupTable : List (Node Declaration) -> Context -> Context
collectLookupTable declarations context =
    List.foldl
        (\declaration ctx ->
            case Node.value declaration of
                Declaration.FunctionDeclaration function ->
                    ctx
                        |> declarationEnterVisitor declaration
                        |> visitExpressions (function.declaration |> Node.value |> .expression)
                        |> declarationExitVisitor declaration

                _ ->
                    declarationEnterVisitor declaration ctx
        )
        context
        declarations


visitExpressions : Node Expression -> Context -> Context
visitExpressions node context =
    -- IGNORE TCO
    context
        |> popScopeEnter node
        |> expressionEnterVisitor node
        |> (\newContext ->
                List.foldl
                    visitExpressions
                    newContext
                    (expressionChildren node)
           )
        |> popScopeExit node
        |> expressionExitVisitor node


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
        elmCorePrelude


elmCorePrelude : List (Node Import)
elmCorePrelude =
    let
        explicit : List Exposing.TopLevelExpose -> Maybe Exposing
        explicit exposed =
            exposed
                |> List.map (Node Range.emptyRange)
                |> Exposing.Explicit
                |> Just
    in
    -- These are the default imports implicitly added by the Elm compiler
    -- https://package.elm-lang.org/packages/elm/core/latest
    [ createFakeImport
        { moduleName = [ "Basics" ]
        , moduleAlias = Nothing
        , exposingList = Just <| Exposing.All Range.emptyRange
        }
    , createFakeImport
        { moduleName = [ "List" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "List", open = Nothing }
                , Exposing.InfixExpose "::"
                ]
        }
    , createFakeImport
        { moduleName = [ "Maybe" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Maybe", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "Result" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Result", open = Just Range.emptyRange }
                ]
        }
    , createFakeImport
        { moduleName = [ "String" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "String", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Char" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Char", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Tuple" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Debug" ]
        , moduleAlias = Nothing
        , exposingList = Nothing
        }
    , createFakeImport
        { moduleName = [ "Platform" ]
        , moduleAlias = Nothing
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Program", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Cmd" ]
        , moduleAlias = Just "Cmd"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Cmd", open = Nothing }
                ]
        }
    , createFakeImport
        { moduleName = [ "Platform", "Sub" ]
        , moduleAlias = Just "Sub"
        , exposingList =
            explicit
                [ Exposing.TypeExpose { name = "Sub", open = Nothing }
                ]
        }
    ]


createFakeImport : { moduleName : ModuleName, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Node Import
createFakeImport { moduleName, moduleAlias, exposingList } =
    Node Range.emptyRange
        { moduleName = Node Range.emptyRange moduleName
        , moduleAlias = moduleAlias |> Maybe.map (List.singleton >> Node Range.emptyRange)
        , exposingList = exposingList |> Maybe.map (Node Range.emptyRange)
        }


declarationListVisitor : List (Node Declaration) -> Context -> Context
declarationListVisitor declarations innerContext =
    List.foldl registerDeclaration innerContext declarations


registerDeclaration : Node Declaration -> Context -> Context
registerDeclaration declaration innerContext =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            let
                nameNode : Node String
                nameNode =
                    function.declaration
                        |> Node.value
                        |> .name
            in
            innerContext
                |> addToScope
                    { variableType = TopLevelVariable
                    , node = nameNode
                    }
                |> registerIfExposed (\name ctx -> registerExposedValue function name ctx) (Node.value nameNode)

        Declaration.AliasDeclaration alias ->
            { innerContext | localTypes = Set.insert (Node.value alias.name) innerContext.localTypes }
                |> (\ctx ->
                        case Node.value alias.typeAnnotation of
                            TypeAnnotation.Record _ ->
                                addToScope
                                    { variableType = TopLevelVariable
                                    , node = alias.name
                                    }
                                    ctx

                            _ ->
                                ctx
                   )
                |> registerIfExposed registerExposedTypeAlias (Node.value alias.name)

        Declaration.CustomTypeDeclaration { name, constructors } ->
            List.foldl
                (\constructor innerContext_ ->
                    let
                        constructorName : Node String
                        constructorName =
                            constructor |> Node.value |> .name
                    in
                    addToScope
                        { variableType = CustomTypeConstructor
                        , node = constructorName
                        }
                        innerContext_
                )
                { innerContext | localTypes = Set.insert (Node.value name) innerContext.localTypes }
                constructors
                |> registerIfExposed (\customTypeName ctx -> registerExposedCustomType constructors customTypeName ctx) (Node.value name)

        Declaration.PortDeclaration signature ->
            innerContext
                |> addToScope
                    { variableType = Port
                    , node = signature.name
                    }
                |> registerIfExposed
                    (\name ctx ->
                        registerExposedValue { documentation = Nothing, signature = Just (Node (Node.range declaration) signature) } name ctx
                    )
                    (Node.value signature.name)

        Declaration.InfixDeclaration _ ->
            innerContext

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            innerContext


addToScope : { variableType : VariableType, node : Node String } -> Context -> Context
addToScope variableData innerContext =
    let
        newScopes : NonEmpty Scope
        newScopes =
            registerVariable variableData innerContext.scopes
    in
    { innerContext | scopes = newScopes }


registerExposedValue : { a | documentation : Maybe (Node String), signature : Maybe (Node Signature) } -> String -> Context -> Context
registerExposedValue function name innerContext =
    { innerContext
        | exposedValues =
            { name = name
            , comment =
                case function.documentation of
                    Just strNode ->
                        Node.value strNode

                    Nothing ->
                        ""
            , tipe = convertTypeSignatureToDocsType innerContext function.signature
            }
                :: innerContext.exposedValues
    }


registerExposedCustomType : List (Node Elm.Syntax.Type.ValueConstructor) -> String -> Context -> Context
registerExposedCustomType constructors name innerContext =
    { innerContext
        | exposedUnions =
            { name = name
            , comment = ""

            -- TODO Get the args from the type. Not useful now but useful when we will provide type information
            , args = []
            , tags =
                constructors
                    -- TODO Get the constructor args from the type. Not useful now but useful when we will provide type information
                    |> List.map (\constructor -> ( Node.value (Node.value constructor).name, [] ))
            }
                :: innerContext.exposedUnions
    }


registerExposedTypeAlias : String -> Context -> Context
registerExposedTypeAlias name innerContext =
    { innerContext
        | exposedAliases =
            { name = name
            , comment = ""
            , args = []
            , tipe = Elm.Type.Tuple []
            }
                :: innerContext.exposedAliases
    }


registerIfExposed : (String -> Context -> Context) -> String -> Context -> Context
registerIfExposed registerFn name innerContext =
    if innerContext.exposesEverything || Set.member name innerContext.exposedNames then
        registerFn name innerContext

    else
        innerContext


convertTypeSignatureToDocsType : Context -> Maybe (Node Signature) -> Elm.Type.Type
convertTypeSignatureToDocsType innerContext maybeSignature =
    case maybeSignature of
        Just signature ->
            syntaxTypeAnnotationToDocsType innerContext (Node.value signature).typeAnnotation

        Nothing ->
            Elm.Type.Tuple []


syntaxTypeAnnotationToDocsType : Context -> Node TypeAnnotation -> Elm.Type.Type
syntaxTypeAnnotationToDocsType innerContext (Node _ typeAnnotation) =
    -- IGNORE TCO
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Elm.Type.Var name

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) typeParameters ->
            let
                realModuleName : ModuleName
                realModuleName =
                    moduleNameForType innerContext typeName moduleName
            in
            Elm.Type.Type (String.join "." realModuleName ++ "." ++ typeName) (List.map (\node -> syntaxTypeAnnotationToDocsType innerContext node) typeParameters)

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled list ->
            Elm.Type.Tuple (List.map (\node -> syntaxTypeAnnotationToDocsType innerContext node) list)

        TypeAnnotation.Record updates ->
            Elm.Type.Record (recordUpdateToDocsType innerContext updates) Nothing

        TypeAnnotation.GenericRecord (Node _ generic) (Node _ updates) ->
            Elm.Type.Record (recordUpdateToDocsType innerContext updates) (Just generic)

        TypeAnnotation.FunctionTypeAnnotation left right ->
            Elm.Type.Lambda
                (syntaxTypeAnnotationToDocsType innerContext left)
                (syntaxTypeAnnotationToDocsType innerContext right)


recordUpdateToDocsType : Context -> List (Node TypeAnnotation.RecordField) -> List ( String, Elm.Type.Type )
recordUpdateToDocsType innerContext updates =
    List.map
        (\(Node _ ( name, typeAnnotation )) ->
            ( Node.value name
            , syntaxTypeAnnotationToDocsType innerContext typeAnnotation
            )
        )
        updates


registerVariable : VariableInfo -> NonEmpty Scope -> NonEmpty Scope
registerVariable variableInfo scopes =
    NonEmpty.mapHead
        (\scope -> { scope | names = Dict.insert (Node.value variableInfo.node) variableInfo scope.names })
        scopes


updateScope : Context -> NonEmpty Scope -> Context
updateScope innerContext scopes =
    { innerContext | scopes = scopes }



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> Context -> Context
moduleDefinitionVisitor node innerContext =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            { innerContext | exposesEverything = True }

        Exposing.Explicit list ->
            { innerContext | exposedNames = exposedElements list }


exposedElements : List (Node Exposing.TopLevelExpose) -> Set String
exposedElements nodes =
    List.foldl
        (\node acc ->
            case Node.value node of
                Exposing.FunctionExpose name ->
                    Set.insert name acc

                Exposing.TypeOrAliasExpose name ->
                    Set.insert name acc

                Exposing.TypeExpose { name } ->
                    Set.insert name acc

                Exposing.InfixExpose _ ->
                    acc
        )
        Set.empty
        nodes



-- IMPORT VISITOR


importVisitor : Node Import -> Context -> Context
importVisitor (Node _ import_) innerContext =
    innerContext
        |> registerImportAlias import_
        |> registerImportExposed import_


registerImportAlias : Import -> Context -> Context
registerImportAlias import_ innerContext =
    case import_.moduleAlias of
        Nothing ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value import_.moduleName
            in
            case moduleName of
                singleSegmentModuleName :: [] ->
                    { innerContext
                        | importAliases =
                            Dict.update
                                singleSegmentModuleName
                                (\previousValue -> Just <| moduleName :: Maybe.withDefault [] previousValue)
                                innerContext.importAliases
                    }

                _ ->
                    innerContext

        Just alias ->
            { innerContext
                | importAliases =
                    Dict.update
                        (Node.value alias |> joinModuleName)
                        (\previousValue -> Just <| Node.value import_.moduleName :: Maybe.withDefault [] previousValue)
                        innerContext.importAliases
            }


registerImportExposed : Import -> Context -> Context
registerImportExposed import_ innerContext =
    case import_.exposingList |> Maybe.map Node.value of
        Nothing ->
            innerContext

        Just exposing_ ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value import_.moduleName

                module_ : Elm.Docs.Module
                module_ =
                    Dict.get moduleName innerContext.modules
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
                            innerContext.importedFunctions
                                |> foldIntoDict module_.values
                                |> foldIntoDict module_.binops
                                |> foldIntoDict module_.aliases
                                |> foldCustomTypesIntoDict module_.unions

                        importedTypes : Dict String ModuleName
                        importedTypes =
                            innerContext.importedTypes
                                |> foldIntoDict module_.unions
                                |> foldIntoDict module_.aliases
                    in
                    { innerContext
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
                                innerContext.importedFunctions

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
                                innerContext.importedTypes
                                topLevelExposeList
                    in
                    { innerContext
                        | importedFunctions = importedFunctions
                        , importedTypes = importedTypes
                    }


valuesFromExposingList : ModuleName -> Elm.Docs.Module -> List (Node TopLevelExpose) -> Dict String ModuleName -> Dict String ModuleName
valuesFromExposingList moduleName module_ topLevelExposeList acc =
    case topLevelExposeList of
        [] ->
            acc

        topLevelExpose :: rest ->
            case Node.value topLevelExpose of
                Exposing.InfixExpose operator ->
                    valuesFromExposingList moduleName module_ rest (Dict.insert operator moduleName acc)

                Exposing.FunctionExpose function ->
                    valuesFromExposingList moduleName module_ rest (Dict.insert function moduleName acc)

                Exposing.TypeOrAliasExpose name ->
                    if List.any (\alias -> alias.name == name) module_.aliases then
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


typesFromExposingList : Node TopLevelExpose -> Maybe String
typesFromExposingList topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose name ->
            Just name

        Exposing.TypeExpose { name } ->
            Just name


declarationEnterVisitor : Node Declaration -> Context -> Context
declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters (Node.value function.declaration).arguments }

                newContext : Context
                newContext =
                    context.scopes
                        |> NonEmpty.cons newScope
                        |> updateScope context

                lookupTableAfterArguments : ModuleNameLookupTable
                lookupTableAfterArguments =
                    collectModuleNamesFromPattern newContext (Node.value function.declaration).arguments newContext.lookupTable

                finalLookupTable : ModuleNameLookupTable
                finalLookupTable =
                    case function.signature of
                        Just signature ->
                            collectModuleNamesFromTypeAnnotation
                                context
                                [ (Node.value signature).typeAnnotation ]
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
declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            { context | scopes = NonEmpty.pop context.scopes }

        _ ->
            context


parameters : List (Node Pattern) -> Dict String VariableInfo
parameters patterns =
    collectNamesFromPattern FunctionParameter patterns Dict.empty


collectNamesFromPattern : VariableType -> List (Node Pattern) -> Dict String VariableInfo -> Dict String VariableInfo
collectNamesFromPattern variableType patternsToVisit acc =
    case patternsToVisit of
        pattern :: restOfPatternsToVisit ->
            case Node.value pattern of
                Pattern.VarPattern name ->
                    collectNamesFromPattern variableType
                        restOfPatternsToVisit
                        (Dict.insert
                            name
                            { node = Node (Node.range pattern) name
                            , variableType = variableType
                            }
                            acc
                        )

                Pattern.NamedPattern _ subPatterns ->
                    collectNamesFromPattern variableType (List.append subPatterns restOfPatternsToVisit) acc

                Pattern.RecordPattern names ->
                    collectNamesFromPattern variableType
                        restOfPatternsToVisit
                        (List.foldl
                            (\nameNode subAcc ->
                                Dict.insert
                                    (Node.value nameNode)
                                    { node = nameNode
                                    , variableType = variableType
                                    }
                                    subAcc
                            )
                            acc
                            names
                        )

                Pattern.ParenthesizedPattern subPattern ->
                    collectNamesFromPattern variableType (subPattern :: restOfPatternsToVisit) acc

                Pattern.AsPattern subPattern alias ->
                    collectNamesFromPattern variableType
                        (subPattern :: restOfPatternsToVisit)
                        (Dict.insert
                            (Node.value alias)
                            { node = alias
                            , variableType = variableType
                            }
                            acc
                        )

                Pattern.TuplePattern subPatterns ->
                    collectNamesFromPattern variableType (List.append subPatterns restOfPatternsToVisit) acc

                Pattern.UnConsPattern left right ->
                    collectNamesFromPattern variableType (left :: right :: restOfPatternsToVisit) acc

                Pattern.ListPattern subPatterns ->
                    collectNamesFromPattern variableType (List.append subPatterns restOfPatternsToVisit) acc

                _ ->
                    collectNamesFromPattern variableType restOfPatternsToVisit acc

        [] ->
            acc


collectModuleNamesFromPattern : Context -> List (Node Pattern) -> ModuleNameLookupTable -> ModuleNameLookupTable
collectModuleNamesFromPattern context patternsToVisit acc =
    case patternsToVisit of
        pattern :: restOfPatternsToVisit ->
            case Node.value pattern of
                Pattern.NamedPattern { moduleName, name } subPatterns ->
                    collectModuleNamesFromPattern
                        context
                        (List.append subPatterns restOfPatternsToVisit)
                        (ModuleNameLookupTableInternal.add (Node.range pattern) (moduleNameForValue context name moduleName) acc)

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
popScopeEnter node context =
    let
        currentScope : Scope
        currentScope =
            NonEmpty.head context.scopes

        caseExpression : Maybe ( Node Expression, Dict String VariableInfo )
        caseExpression =
            ListExtra.find (\( expressionNode, _ ) -> node == expressionNode) currentScope.cases
    in
    case caseExpression of
        Nothing ->
            context

        Just ( _, names ) ->
            { context | scopes = NonEmpty.cons { emptyScope | names = names, caseToExit = node } context.scopes }


popScopeExit : Node Expression -> Context -> Context
popScopeExit node context =
    let
        currentScope : Scope
        currentScope =
            NonEmpty.head context.scopes
    in
    if node == currentScope.caseToExit then
        { context | scopes = NonEmpty.pop context.scopes }

    else
        context


expressionEnterVisitor : Node Expression -> Context -> Context
expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                newContext : Context
                newContext =
                    List.foldl
                        (\declaration scopes ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    let
                                        { name, expression, arguments } =
                                            Node.value function.declaration

                                        withLetVariable : NonEmpty Scope
                                        withLetVariable =
                                            registerVariable
                                                { variableType = LetVariable
                                                , node = name
                                                }
                                                scopes
                                    in
                                    if List.isEmpty arguments then
                                        withLetVariable

                                    else
                                        let
                                            names : Dict String VariableInfo
                                            names =
                                                collectNamesFromPattern PatternVariable arguments Dict.empty
                                        in
                                        NonEmpty.mapHead (\scope -> { scope | cases = ( expression, names ) :: scope.cases }) withLetVariable

                                Expression.LetDestructuring _ _ ->
                                    scopes
                        )
                        (NonEmpty.cons emptyScope context.scopes)
                        declarations
                        |> updateScope context

                lookupTable : ModuleNameLookupTable
                lookupTable =
                    List.foldl
                        (\declaration acc ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    let
                                        withDeclarationModuleName : ModuleNameLookupTable
                                        withDeclarationModuleName =
                                            collectModuleNamesFromPattern newContext
                                                (Node.value function.declaration).arguments
                                                acc
                                    in
                                    case function.signature of
                                        Just signature ->
                                            collectModuleNamesFromTypeAnnotation
                                                context
                                                [ (Node.value signature).typeAnnotation ]
                                                withDeclarationModuleName

                                        Nothing ->
                                            withDeclarationModuleName

                                Expression.LetDestructuring pattern _ ->
                                    collectModuleNamesFromPattern newContext [ pattern ] acc
                        )
                        newContext.lookupTable
                        declarations
            in
            { newContext | lookupTable = lookupTable }

        Expression.CaseExpression caseBlock ->
            let
                ( cases, lookupTable ) =
                    List.foldl
                        (\( pattern, expression ) ( casesAcc, lookupTableAcc ) ->
                            ( ( expression
                              , collectNamesFromPattern PatternVariable [ pattern ] Dict.empty
                              )
                                :: casesAcc
                            , collectModuleNamesFromPattern context [ pattern ] lookupTableAcc
                            )
                        )
                        ( [], context.lookupTable )
                        caseBlock.cases
            in
            { context
                | scopes = NonEmpty.mapHead (\scope -> { scope | cases = cases }) context.scopes
                , lookupTable = lookupTable
            }

        Expression.FunctionOrValue moduleName name ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.add
                        (Node.range node)
                        (moduleNameForValue context name moduleName)
                        context.lookupTable
            }

        Expression.RecordUpdateExpression (Node range name) _ ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.add
                        range
                        (moduleNameForValue context name [])
                        context.lookupTable
            }

        Expression.LambdaExpression { args } ->
            { context | lookupTable = collectModuleNamesFromPattern context args context.lookupTable }

        Expression.PrefixOperator op ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.add
                        (Node.range node)
                        (moduleNameForValue context op [])
                        context.lookupTable
            }

        Expression.OperatorApplication op _ _ _ ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.add
                        (Node.range node)
                        (moduleNameForValue context op [])
                        context.lookupTable
            }

        _ ->
            context


collectModuleNamesFromTypeAnnotation : Context -> List (Node TypeAnnotation) -> ModuleNameLookupTable -> ModuleNameLookupTable
collectModuleNamesFromTypeAnnotation context typeAnnotationsToVisit acc =
    case typeAnnotationsToVisit of
        typeAnnotationNode :: remainingTypeAnnotationsToVisit ->
            case Node.value typeAnnotationNode of
                TypeAnnotation.Typed (Node range ( moduleName, name )) args ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (List.append args remainingTypeAnnotationsToVisit)
                        (ModuleNameLookupTableInternal.add range (moduleNameForType context name moduleName) acc)

                TypeAnnotation.Tupled nodes ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (List.append nodes remainingTypeAnnotationsToVisit)
                        acc

                TypeAnnotation.Record fields ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentMapAppend (\field -> field |> Node.value |> Tuple.second) fields remainingTypeAnnotationsToVisit)
                        acc

                TypeAnnotation.GenericRecord _ fields ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentMapAppend (\field -> field |> Node.value |> Tuple.second) (Node.value fields) remainingTypeAnnotationsToVisit)
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


expressionExitVisitor : Node Expression -> Context -> Context
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            { context | scopes = NonEmpty.pop context.scopes }

        Expression.CaseExpression _ ->
            { context | scopes = NonEmpty.mapHead (\scope -> { scope | cases = [] }) context.scopes }

        _ ->
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
            if isInScope valueName context.scopes then
                []

            else
                Dict.get valueName context.importedFunctions
                    |> Maybe.withDefault []

        moduleNameOrAlias :: [] ->
            case Dict.get moduleNameOrAlias context.importAliases of
                Just [ aliasedModuleName ] ->
                    aliasedModuleName

                Just aliases ->
                    case
                        ListExtra.find
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
                            List.head aliases
                                |> Maybe.withDefault moduleName

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
                Just [ aliasedModuleName ] ->
                    aliasedModuleName

                Just aliases ->
                    case
                        ListExtra.find
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
                            List.head aliases
                                |> Maybe.withDefault moduleName

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
    NonEmpty.any (.names >> Dict.member name) scopes


joinModuleName : ModuleName -> String
joinModuleName name =
    String.join "." name


expressionChildren : Node Expression -> List (Node Expression)
expressionChildren node =
    case Node.value node of
        Expression.Application expressions ->
            expressions

        Expression.ListExpr elements ->
            elements

        Expression.RecordExpr fields ->
            List.map (Node.value >> (\( _, expr ) -> expr)) fields

        Expression.RecordUpdateExpression _ setters ->
            List.map (Node.value >> (\( _, expr ) -> expr)) setters

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
                (\declaration acc ->
                    case Node.value declaration of
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
