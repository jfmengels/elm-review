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
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type
import NonEmpty exposing (NonEmpty)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal exposing (ModuleNameLookupTable)
import Review.Project
import Review.Project.Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..), ProjectModule)
import Set exposing (Set)
import Vendor.ListExtra as ListExtra


type alias ScopeProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    }


type alias ScopeModuleContext =
    { scopes : NonEmpty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String ModuleName
    , importedTypes : Dict String ModuleName
    , dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    , exposesEverything : Bool
    , exposedNames : Dict String Range
    , exposedUnions : List Elm.Docs.Union
    , exposedAliases : List Elm.Docs.Alias
    , exposedValues : List Elm.Docs.Value
    , exposedBinops : List Elm.Docs.Binop
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


compute : ModuleName -> ProjectModule -> Project -> ( ModuleNameLookupTable, Project )
compute moduleName module_ ((Project { dataCache }) as project) =
    let
        computeDependencies : () -> Dict String Elm.Docs.Module
        computeDependencies () =
            project
                |> Review.Project.directDependencies
                |> Dict.foldl (\_ dep acc -> ListExtra.orderIndependentAppend (Review.Project.Dependency.modules dep) acc) []
                |> List.foldl (\dependencyModule acc -> Dict.insert dependencyModule.name dependencyModule acc) Dict.empty

        elmJsonRaw : Maybe String
        elmJsonRaw =
            Maybe.map .raw (Review.Project.elmJson project)

        deps : Dict String Elm.Docs.Module
        deps =
            case dataCache.dependenciesModules of
                Just cache ->
                    if elmJsonRaw == cache.elmJsonRaw then
                        cache.deps

                    else
                        computeDependencies ()

                Nothing ->
                    computeDependencies ()

        projectContext : ScopeProjectContext
        projectContext =
            { dependenciesModules = deps
            , modules = dataCache.modules
            }

        moduleContext : ScopeModuleContext
        moduleContext =
            scope_fromProjectToModule moduleName projectContext
                |> collectLookupTable module_.ast

        newDataCache : Review.Project.Internal.DataCache
        newDataCache =
            { dependenciesModules = Just { elmJsonRaw = elmJsonRaw, deps = deps }
            , modules =
                Dict.insert moduleName
                    { name = String.join "." moduleName
                    , comment = ""
                    , unions = moduleContext.exposedUnions
                    , aliases = moduleContext.exposedAliases
                    , values = moduleContext.exposedValues
                    , binops = moduleContext.exposedBinops
                    }
                    dataCache.modules
            , lookupTables = Dict.insert moduleName moduleContext.lookupTable dataCache.lookupTables
            }
    in
    ( moduleContext.lookupTable, updateProject newDataCache project )


updateProject : Review.Project.Internal.DataCache -> Project -> Project
updateProject dataCache (Project project) =
    Project { project | dataCache = dataCache }


scope_fromProjectToModule : ModuleName -> ScopeProjectContext -> ScopeModuleContext
scope_fromProjectToModule moduleName projectContext =
    { scopes = NonEmpty.fromElement emptyScope
    , localTypes = Set.empty
    , importAliases = Dict.empty
    , importedFunctions = Dict.empty
    , importedTypes = Dict.empty
    , dependenciesModules = projectContext.dependenciesModules
    , modules = projectContext.modules
    , exposesEverything = False
    , exposedNames = Dict.empty
    , exposedUnions = []
    , exposedAliases = []
    , exposedValues = []
    , exposedBinops = []
    , lookupTable = ModuleNameLookupTableInternal.empty moduleName
    }


collectLookupTable : Elm.Syntax.File.File -> ScopeModuleContext -> ScopeModuleContext
collectLookupTable ast context =
    List.foldl scope_importVisitor context (elmCorePrelude ++ ast.imports)
        |> scope_moduleDefinitionVisitor ast.moduleDefinition
        |> scope_declarationListVisitor ast.declarations
        |> visitDeclarationsAndExpressions ast.declarations


visitDeclarationsAndExpressions : List (Node Declaration) -> ScopeModuleContext -> ScopeModuleContext
visitDeclarationsAndExpressions declarations context =
    List.foldl
        (\declaration ctx ->
            case Node.value declaration of
                Declaration.FunctionDeclaration function ->
                    ctx
                        |> scope_declarationEnterVisitor declaration
                        |> visitExpressions (function.declaration |> Node.value |> .expression)
                        |> scope_declarationExitVisitor declaration

                _ ->
                    scope_declarationEnterVisitor declaration ctx
        )
        context
        declarations


visitExpressions : Node Expression -> ScopeModuleContext -> ScopeModuleContext
visitExpressions node context =
    -- IGNORE TCO
    context
        |> scope_popScopeEnter node
        |> scope_expressionEnterVisitor node
        |> (\newContext ->
                List.foldl
                    visitExpressions
                    newContext
                    (expressionChildren node)
           )
        |> scope_popScopeExit node
        |> expressionExitVisitor node


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


scope_declarationListVisitor : List (Node Declaration) -> ScopeModuleContext -> ScopeModuleContext
scope_declarationListVisitor declarations innerContext =
    List.foldl scope_registerDeclaration innerContext declarations


scope_registerDeclaration : Node Declaration -> ScopeModuleContext -> ScopeModuleContext
scope_registerDeclaration declaration innerContext =
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
                |> registerIfExposed (registerExposedValue function) (Node.value nameNode)

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
                |> registerIfExposed (registerExposedCustomType constructors) (Node.value name)

        Declaration.PortDeclaration signature ->
            innerContext
                |> addToScope
                    { variableType = Port
                    , node = signature.name
                    }
                |> registerIfExposed (registerExposedValue { documentation = Nothing, signature = Just (Node (Node.range declaration) signature) }) (Node.value signature.name)

        Declaration.InfixDeclaration _ ->
            innerContext

        Declaration.Destructuring _ _ ->
            -- Not possible in 0.19 code
            innerContext


addToScope : { variableType : VariableType, node : Node String } -> ScopeModuleContext -> ScopeModuleContext
addToScope variableData innerContext =
    let
        newScopes : NonEmpty Scope
        newScopes =
            registerVariable
                variableData
                (Node.value variableData.node)
                innerContext.scopes
    in
    { innerContext | scopes = newScopes }


registerExposedValue : { a | documentation : Maybe (Node String), signature : Maybe (Node Signature) } -> String -> ScopeModuleContext -> ScopeModuleContext
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


registerExposedCustomType : List (Node Elm.Syntax.Type.ValueConstructor) -> String -> ScopeModuleContext -> ScopeModuleContext
registerExposedCustomType constructors name innerContext =
    { innerContext
        | exposedUnions =
            { name = name
            , comment = ""

            -- TODO
            , args = []
            , tags =
                constructors
                    -- TODO Constructor args?
                    |> ListExtra.orderIndependentMap (\constructor -> ( Node.value (Node.value constructor).name, [] ))
            }
                :: innerContext.exposedUnions
    }


registerExposedTypeAlias : String -> ScopeModuleContext -> ScopeModuleContext
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


registerIfExposed : (String -> ScopeModuleContext -> ScopeModuleContext) -> String -> ScopeModuleContext -> ScopeModuleContext
registerIfExposed registerFn name innerContext =
    if innerContext.exposesEverything || Dict.member name innerContext.exposedNames then
        registerFn name innerContext

    else
        innerContext


convertTypeSignatureToDocsType : ScopeModuleContext -> Maybe (Node Signature) -> Elm.Type.Type
convertTypeSignatureToDocsType innerContext maybeSignature =
    case maybeSignature of
        Just signature ->
            syntaxTypeAnnotationToDocsType innerContext (Node.value signature).typeAnnotation

        Nothing ->
            Elm.Type.Tuple []


syntaxTypeAnnotationToDocsType : ScopeModuleContext -> Node TypeAnnotation -> Elm.Type.Type
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
            Elm.Type.Type (String.join "." realModuleName ++ "." ++ typeName) (List.map (syntaxTypeAnnotationToDocsType innerContext) typeParameters)

        TypeAnnotation.Unit ->
            Elm.Type.Tuple []

        TypeAnnotation.Tupled list ->
            Elm.Type.Tuple (List.map (syntaxTypeAnnotationToDocsType innerContext) list)

        TypeAnnotation.Record updates ->
            Elm.Type.Record (recordUpdateToDocsType innerContext updates) Nothing

        TypeAnnotation.GenericRecord (Node _ generic) (Node _ updates) ->
            Elm.Type.Record (recordUpdateToDocsType innerContext updates) (Just generic)

        TypeAnnotation.FunctionTypeAnnotation left right ->
            Elm.Type.Lambda
                (syntaxTypeAnnotationToDocsType innerContext left)
                (syntaxTypeAnnotationToDocsType innerContext right)


recordUpdateToDocsType : ScopeModuleContext -> List (Node TypeAnnotation.RecordField) -> List ( String, Elm.Type.Type )
recordUpdateToDocsType innerContext updates =
    List.map
        (\(Node _ ( name, typeAnnotation )) ->
            ( Node.value name
            , syntaxTypeAnnotationToDocsType innerContext typeAnnotation
            )
        )
        updates


registerVariable : VariableInfo -> String -> NonEmpty Scope -> NonEmpty Scope
registerVariable variableInfo name scopes =
    NonEmpty.mapHead
        (\scope -> { scope | names = Dict.insert name variableInfo scope.names })
        scopes


updateScope : ScopeModuleContext -> NonEmpty Scope -> ScopeModuleContext
updateScope innerContext scopes =
    { innerContext | scopes = scopes }



-- MODULE DEFINITION VISITOR


scope_moduleDefinitionVisitor : Node Module -> ScopeModuleContext -> ScopeModuleContext
scope_moduleDefinitionVisitor node innerContext =
    case Module.exposingList (Node.value node) of
        Exposing.All _ ->
            { innerContext | exposesEverything = True }

        Exposing.Explicit list ->
            { innerContext | exposedNames = exposedElements list }


exposedElements : List (Node Exposing.TopLevelExpose) -> Dict String Range
exposedElements nodes =
    List.foldl
        (\node acc ->
            case Node.value node of
                Exposing.FunctionExpose name ->
                    Dict.insert name (Node.range node) acc

                Exposing.TypeOrAliasExpose name ->
                    Dict.insert name (Node.range node) acc

                Exposing.TypeExpose { name } ->
                    Dict.insert name (Node.range node) acc

                Exposing.InfixExpose _ ->
                    acc
        )
        Dict.empty
        nodes



-- IMPORT VISITOR


scope_importVisitor : Node Import -> ScopeModuleContext -> ScopeModuleContext
scope_importVisitor (Node _ import_) innerContext =
    innerContext
        |> registerImportAlias import_
        |> registerImportExposed import_


registerImportAlias : Import -> ScopeModuleContext -> ScopeModuleContext
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


registerImportExposed : Import -> ScopeModuleContext -> ScopeModuleContext
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
                    (case Dict.get moduleName innerContext.modules of
                        Just m ->
                            Just m

                        Nothing ->
                            Dict.get (joinModuleName moduleName) innerContext.dependenciesModules
                    )
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


scope_declarationEnterVisitor : Node Declaration -> ScopeModuleContext -> ScopeModuleContext
scope_declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters (Node.value function.declaration).arguments }

                newContext : ScopeModuleContext
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


scope_declarationExitVisitor : Node Declaration -> ScopeModuleContext -> ScopeModuleContext
scope_declarationExitVisitor node context =
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
                    collectNamesFromPattern variableType (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit) acc

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
                    collectNamesFromPattern variableType (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit) acc

                Pattern.UnConsPattern left right ->
                    collectNamesFromPattern variableType (left :: right :: restOfPatternsToVisit) acc

                Pattern.ListPattern subPatterns ->
                    collectNamesFromPattern variableType (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit) acc

                _ ->
                    collectNamesFromPattern variableType restOfPatternsToVisit acc

        [] ->
            acc


collectModuleNamesFromPattern : ScopeModuleContext -> List (Node Pattern) -> ModuleNameLookupTable -> ModuleNameLookupTable
collectModuleNamesFromPattern context patternsToVisit acc =
    case patternsToVisit of
        pattern :: restOfPatternsToVisit ->
            case Node.value pattern of
                Pattern.NamedPattern { moduleName, name } subPatterns ->
                    collectModuleNamesFromPattern
                        context
                        (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit)
                        (ModuleNameLookupTableInternal.add (Node.range pattern) (moduleNameForValue context name moduleName) acc)

                Pattern.UnConsPattern left right ->
                    collectModuleNamesFromPattern context (left :: right :: restOfPatternsToVisit) acc

                Pattern.TuplePattern subPatterns ->
                    collectModuleNamesFromPattern context (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit) acc

                Pattern.ParenthesizedPattern subPattern ->
                    collectModuleNamesFromPattern context (subPattern :: restOfPatternsToVisit) acc

                Pattern.AsPattern subPattern _ ->
                    collectModuleNamesFromPattern context (subPattern :: restOfPatternsToVisit) acc

                Pattern.ListPattern subPatterns ->
                    collectModuleNamesFromPattern context (ListExtra.orderIndependentAppend subPatterns restOfPatternsToVisit) acc

                _ ->
                    collectModuleNamesFromPattern context restOfPatternsToVisit acc

        [] ->
            acc


scope_popScopeEnter : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_popScopeEnter node context =
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


scope_popScopeExit : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_popScopeExit node context =
    let
        currentScope : Scope
        currentScope =
            NonEmpty.head context.scopes
    in
    if node == currentScope.caseToExit then
        { context | scopes = NonEmpty.pop context.scopes }

    else
        context


scope_expressionEnterVisitor : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations } ->
            let
                newContext : ScopeModuleContext
                newContext =
                    List.foldl
                        (\declaration scopes ->
                            case Node.value declaration of
                                Expression.LetFunction function ->
                                    let
                                        nameNode : Node String
                                        nameNode =
                                            function.declaration
                                                |> Node.value
                                                |> .name
                                    in
                                    registerVariable
                                        { variableType = LetVariable, node = nameNode }
                                        -- TODO Check if the name as 2nd arg is not redundant with the 1st argument's node field
                                        (Node.value nameNode)
                                        scopes

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


collectModuleNamesFromTypeAnnotation : ScopeModuleContext -> List (Node TypeAnnotation) -> ModuleNameLookupTable -> ModuleNameLookupTable
collectModuleNamesFromTypeAnnotation context typeAnnotationsToVisit acc =
    case typeAnnotationsToVisit of
        typeAnnotationNode :: remainingTypeAnnotationsToVisit ->
            case Node.value typeAnnotationNode of
                TypeAnnotation.Typed (Node range ( moduleName, name )) args ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentAppend args remainingTypeAnnotationsToVisit)
                        (ModuleNameLookupTableInternal.add range (moduleNameForType context name moduleName) acc)

                TypeAnnotation.Tupled nodes ->
                    collectModuleNamesFromTypeAnnotation
                        context
                        (ListExtra.orderIndependentAppend nodes remainingTypeAnnotationsToVisit)
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


expressionExitVisitor : Node Expression -> ScopeModuleContext -> ScopeModuleContext
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
moduleNameForValue : ScopeModuleContext -> String -> ModuleName -> ModuleName
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
                                        case Dict.get (joinModuleName aliasedModuleName) context.dependenciesModules of
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
moduleNameForType : ScopeModuleContext -> String -> ModuleName -> ModuleName
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
                                        case Dict.get (joinModuleName aliasedModuleName) context.dependenciesModules of
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