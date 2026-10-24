module Review.Resources.DocsModules exposing
    ( compute
    , dummyModuleDocs
    , findModuleDocumentation
    , importedDocsHash
    )

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import Elm.TypeInference
import Elm.TypeInference.Type
import NonEmpty exposing (NonEmpty)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.Project.DocsModule as DocsModule exposing (DocsModule)
import Review.Project.ProjectCache as ProjectCache exposing (ProjectCache)
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject exposing (ValidProject)
import Review.Resources.ElmCorePrelude as ElmCorePrelude exposing (elmCorePrelude)
import Set exposing (Set)
import Vendor.IntSet as IntSet exposing (IntSet)
import Vendor.ListExtra as ListExtra


importedDocsHash : ValidProject -> IntSet -> ComparableContextHash Elm.Docs.Module
importedDocsHash project incoming =
    let
        modules : Dict ModuleName DocsModule
        modules =
            (ValidProject.projectCache project).modules
    in
    IntSet.foldl
        (\key acc ->
            case
                ValidProject.getGraphNode key project
                    |> Maybe.andThen (\graphModule -> ValidProject.getModuleByPath graphModule.node.label project)
                    |> Maybe.andThen (\module_ -> Dict.get (ProjectModule.moduleName module_) modules)
            of
                Just docsModule ->
                    DocsModule.hash docsModule :: acc

                Nothing ->
                    acc
        )
        []
        incoming
        |> ContextHash.toComparable


compute :
    ModuleName
    -> OpaqueProjectModule
    -> ValidProject
    -> { checkCacheKey : Bool }
    ->
        { moduleDocs : DocsModule
        , projectCache : ProjectCache
        }
compute moduleName module_ project { checkCacheKey } =
    let
        { importDocsModuleHashes, projectCache } =
            List.foldl
                (\(Node _ import_) acc ->
                    case computeImportedModulesDocs project (Node.value import_.moduleName) acc.projectCache of
                        Just res ->
                            { importDocsModuleHashes = DocsModule.hash res.moduleDocs :: acc.importDocsModuleHashes
                            , projectCache = res.projectCache
                            }

                        Nothing ->
                            acc
                )
                { importDocsModuleHashes = []
                , projectCache = ValidProject.projectCache project
                }
                (ProjectModule.ast module_).imports
    in
    case
        Dict.get moduleName projectCache.modules
            |> Maybe.andThen (DocsModule.checkCacheKeysMatch checkCacheKey importDocsModuleHashes (ProjectModule.contentHash module_))
    of
        Just mod ->
            { moduleDocs = mod
            , projectCache = projectCache
            }

        Nothing ->
            computeHelp importDocsModuleHashes moduleName module_ projectCache


computeImportedModulesDocs :
    ValidProject
    -> ModuleName
    -> ProjectCache
    -> Maybe { moduleDocs : DocsModule, projectCache : ProjectCache }
computeImportedModulesDocs project moduleName projectCache =
    case Dict.get moduleName projectCache.modules of
        Just moduleDocs ->
            Just
                { moduleDocs = moduleDocs
                , projectCache = projectCache
                }

        Nothing ->
            case ValidProject.getModuleByModuleName project moduleName of
                Just module_ ->
                    compute moduleName module_ project { checkCacheKey = False }
                        |> Just

                Nothing ->
                    Nothing


type alias Context =
    { moduleName : ModuleName
    , localTypes : Set String
    , importAliases : Dict String (NonEmpty ModuleName)
    , importedTypes : Dict String ModuleName
    , modules : Dict ModuleName DocsModule
    , dependencies : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Dict String Bool
    }


type alias DocsAcc =
    { types : Elm.TypeInference.Project
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , exposedBinops : List Elm.Docs.Binop
    }


computeHelp :
    List (ContextHash Elm.Docs.Module)
    -> ModuleName
    -> OpaqueProjectModule
    -> ProjectCache
    ->
        { moduleDocs : DocsModule
        , projectCache : ProjectCache
        }
computeHelp importDocsModuleHashes moduleName module_ projectCache =
    let
        moduleAst : Elm.Syntax.File.File
        moduleAst =
            ProjectModule.ast module_

        moduleContext : Context
        moduleContext =
            computeBaseModule moduleName projectCache.dependencies projectCache.modules

        ( moduleDocsForFile, newTypes ) =
            collectModuleDocs moduleName moduleAst moduleContext (ProjectCache.types projectCache)

        docsModule : DocsModule
        docsModule =
            DocsModule.create
                importDocsModuleHashes
                (ProjectModule.contentHash module_)
                moduleDocsForFile

        modules : Dict ModuleName DocsModule
        modules =
            if Maybe.map DocsModule.hash (Dict.get moduleName projectCache.modules) == Just (DocsModule.hash docsModule) then
                -- If the computed result is the same as already in the dict, leave the dict untouched
                projectCache.modules

            else
                Dict.insert moduleName docsModule projectCache.modules

        newProjectCache : ProjectCache
        newProjectCache =
            { dependencies = projectCache.dependencies
            , elmJsonContentHash = projectCache.elmJsonContentHash
            , types = Just newTypes
            , modules = modules
            , lookupTables = projectCache.lookupTables
            }
    in
    { moduleDocs = docsModule
    , projectCache = newProjectCache
    }


computeBaseModule : ModuleName -> Dict ModuleName Elm.Docs.Module -> Dict ModuleName DocsModule -> Context
computeBaseModule moduleName dependencies modules =
    let
        importedTypes : Dict String ModuleName
        importedTypes =
            List.foldl (\import_ acc -> registerImportedTypes dependencies modules import_ acc) Dict.empty elmCorePrelude
    in
    { moduleName = moduleName
    , localTypes = Set.empty
    , importAliases = ElmCorePrelude.importAliases
    , importedTypes = importedTypes
    , modules = modules
    , dependencies = dependencies
    , exposesEverything = False
    , exposedNames = Dict.empty
    }


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


collectModuleDocs : ModuleName -> Elm.Syntax.File.File -> Context -> Elm.TypeInference.Project -> ( Elm.Docs.Module, Elm.TypeInference.Project )
collectModuleDocs moduleName ast context types =
    let
        importAliases : Dict String (NonEmpty ModuleName)
        importAliases =
            List.foldl (\(Node _ import_) acc -> registerImportAlias import_ acc) context.importAliases ast.imports

        importedTypes : Dict String ModuleName
        importedTypes =
            List.foldl (\(Node _ import_) acc -> registerImportedTypes context.dependencies context.modules import_ acc) context.importedTypes ast.imports

        exposed : { exposesEverything : Bool, exposedNames : Dict String Bool }
        exposed =
            moduleDefinitionVisitor ast.moduleDefinition

        localTypes : Set String
        localTypes =
            List.foldl
                (\(Node _ declaration) acc ->
                    case declaration of
                        Declaration.AliasDeclaration alias ->
                            let
                                name : String
                                name =
                                    Node.value alias.name
                            in
                            Set.insert name acc

                        Declaration.CustomTypeDeclaration customType ->
                            Set.insert (Node.value customType.name) acc

                        _ ->
                            acc
                )
                Set.empty
                ast.declarations

        docsAcc : DocsAcc
        docsAcc =
            declarationListVisitor ast.declarations
                { context
                    | importAliases = importAliases
                    , importedTypes = importedTypes
                    , exposesEverything = exposed.exposesEverything
                    , exposedNames = exposed.exposedNames
                    , localTypes = localTypes
                }
                { types = types
                , exposedUnions = []
                , exposedAliases = []
                , exposedValues = []
                , exposedBinops = []
                }
    in
    ( { name = joinModuleName moduleName
      , comment = extractModuleDocumentation ast
      , unions = docsAcc.exposedUnions
      , aliases = docsAcc.exposedAliases
      , values = docsAcc.exposedValues
      , binops = docsAcc.exposedBinops
      }
    , docsAcc.types
    )


declarationListVisitor : List (Node Declaration) -> Context -> DocsAcc -> DocsAcc
declarationListVisitor declarations context collection =
    List.foldl (registerDeclaration context declarations) collection declarations


registerDeclaration : Context -> List (Node Declaration) -> Node Declaration -> DocsAcc -> DocsAcc
registerDeclaration context allDeclarations (Node _ declaration) acc =
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
            if isExposed name context then
                registerExposedValue function name context acc

            else
                acc

        Declaration.AliasDeclaration alias ->
            let
                name : String
                name =
                    Node.value alias.name
            in
            if isExposed name context then
                registerExposedTypeAlias alias context acc

            else
                acc

        Declaration.CustomTypeDeclaration customType ->
            registerExposedCustomType customType context acc

        Declaration.PortDeclaration signature ->
            let
                name : String
                name =
                    Node.value signature.name
            in
            if isExposed name context then
                registerPortValue signature.typeAnnotation name context acc

            else
                acc

        Declaration.InfixDeclaration infix_ ->
            let
                name : String
                name =
                    Node.value infix_.operator
            in
            if isExposed name context then
                registerExposedBinop infix_ name allDeclarations context acc

            else
                acc

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            acc


registerExposedValue : Expression.Function -> String -> Context -> DocsAcc -> DocsAcc
registerExposedValue function name context acc =
    let
        ( type_, types ) =
            case function.signature of
                Just (Node _ { typeAnnotation }) ->
                    ( syntaxTypeAnnotationToDocsType context typeAnnotation
                    , acc.types
                    )

                Nothing ->
                    case Elm.TypeInference.getType context.moduleName (Node.range function.declaration) acc.types of
                        ( Ok type__, newTypes ) ->
                            ( inferredTypeToDocsType type__, newTypes )

                        ( Err _, newTypes ) ->
                            ( Elm.Type.Tuple [], newTypes )
    in
    { acc
        | exposedValues =
            { name = name
            , comment = extractDocumentation function.documentation
            , tipe = type_
            }
                :: acc.exposedValues
        , types = types
    }


registerPortValue : Node TypeAnnotation -> String -> Context -> DocsAcc -> DocsAcc
registerPortValue typeAnnotation name context acc =
    { acc
        | exposedValues =
            { name = name

            -- TODO Collect documentation for port
            , comment = ""
            , tipe = syntaxTypeAnnotationToDocsType context typeAnnotation
            }
                :: acc.exposedValues
    }


registerExposedCustomType : Elm.Syntax.Type.Type -> Context -> DocsAcc -> DocsAcc
registerExposedCustomType union context acc =
    if context.exposesEverything then
        registerCustomType union union.constructors context acc

    else
        case Dict.get (Node.value union.name) context.exposedNames of
            Just True ->
                registerCustomType union union.constructors context acc

            Just False ->
                registerCustomType union [] context acc

            Nothing ->
                acc


registerCustomType : Elm.Syntax.Type.Type -> List (Node Elm.Syntax.Type.ValueConstructor) -> Context -> DocsAcc -> DocsAcc
registerCustomType union constructors context acc =
    { acc
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
                :: acc.exposedUnions
    }


registerExposedTypeAlias : Elm.Syntax.TypeAlias.TypeAlias -> Context -> DocsAcc -> DocsAcc
registerExposedTypeAlias alias context acc =
    { acc
        | exposedAliases =
            { name = Node.value alias.name
            , comment = extractDocumentation alias.documentation
            , args = List.map Node.value alias.generics
            , tipe = syntaxTypeAnnotationToDocsType context alias.typeAnnotation
            }
                :: acc.exposedAliases
    }


isExposed : String -> Context -> Bool
isExposed name context =
    context.exposesEverything || Dict.member name context.exposedNames


registerExposedBinop : Infix -> String -> List (Node Declaration) -> Context -> DocsAcc -> DocsAcc
registerExposedBinop infix_ name allDeclarations context acc =
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
    { acc | exposedValues = value :: acc.exposedValues }


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


moduleDefinitionVisitor : Node Module -> { exposesEverything : Bool, exposedNames : Dict String Bool }
moduleDefinitionVisitor (Node _ node) =
    case Module.exposingList node of
        Exposing.All _ ->
            { exposesEverything = True, exposedNames = Dict.empty }

        Exposing.Explicit list ->
            { exposesEverything = False, exposedNames = exposedElements list }


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


registerImportedTypes :
    Dict ModuleName Elm.Docs.Module
    -> Dict ModuleName DocsModule
    -> Import
    -> Dict String ModuleName
    -> Dict String ModuleName
registerImportedTypes dependencies modules import_ importedTypes =
    case import_.exposingList of
        Nothing ->
            importedTypes

        Just (Node _ exposing_) ->
            let
                moduleName : ModuleName
                moduleName =
                    Node.value import_.moduleName
            in
            registerImportedTypesWithExposing dependencies modules moduleName exposing_ importedTypes


registerImportedTypesWithExposing :
    Dict ModuleName Elm.Docs.Module
    -> Dict ModuleName DocsModule
    -> ModuleName
    -> Exposing
    -> Dict String ModuleName
    -> Dict String ModuleName
registerImportedTypesWithExposing dependencies modules moduleName exposing_ importedTypes =
    case exposing_ of
        Exposing.All _ ->
            let
                module_ : Elm.Docs.Module
                module_ =
                    case Dict.get moduleName modules of
                        Just mod ->
                            DocsModule.mod mod

                        Nothing ->
                            case Dict.get moduleName dependencies of
                                Just mod ->
                                    mod

                                Nothing ->
                                    dummyModuleDocs moduleName

                foldIntoDict : List { a | name : comparable } -> Dict comparable ModuleName -> Dict comparable ModuleName
                foldIntoDict list dict =
                    List.foldl (\{ name } acc -> Dict.insert name moduleName acc) dict list
            in
            importedTypes
                |> foldIntoDict module_.unions
                |> foldIntoDict module_.aliases

        Exposing.Explicit topLevelExposeList ->
            List.foldl
                (\topLevelExpose acc ->
                    case typesFromExposingList topLevelExpose of
                        Just name ->
                            Dict.insert name moduleName acc

                        Nothing ->
                            acc
                )
                importedTypes
                topLevelExposeList


dummyModuleDocs : ModuleName -> Elm.Docs.Module
dummyModuleDocs moduleName =
    { name = joinModuleName moduleName
    , comment = ""
    , unions = []
    , values = []
    , aliases = []
    , binops = []
    }


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
                    NonEmpty.findWithHeadAsDefault
                        (\aliasedModuleName ->
                            case Dict.get aliasedModuleName context.modules of
                                Just module_ ->
                                    isTypeDeclaredInModule typeName (DocsModule.mod module_)

                                Nothing ->
                                    case Dict.get aliasedModuleName context.dependencies of
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


isTypeDeclaredInModule : String -> Elm.Docs.Module -> Bool
isTypeDeclaredInModule typeName module_ =
    List.any (.name >> (==) typeName) module_.aliases
        || List.any (.name >> (==) typeName) module_.unions


joinModuleName : ModuleName -> String
joinModuleName name =
    String.join "." name
