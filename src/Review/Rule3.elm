module Review.Rule3 exposing (ModuleVisitor, ProjectRuleSchema, fromModuleRuleSchema_New, fromProjectRuleSchema, newModuleRuleSchema_New, newProjectRuleSchema, withCommentsVisitor_New, withDeclarationEnterVisitor_New, withDeclarationExitVisitor_New, withDeclarationListVisitor_New, withDeclarationVisitor_New, withDependenciesVisitor, withElmJsonProjectVisitor, withExpressionEnterVisitor_New, withExpressionExitVisitor_New, withExpressionVisitor_New, withFinalModuleEvaluation_New, withFinalProjectEvaluation, withImportVisitor_New, withModuleDefinitionVisitor_New, withReadmeProjectVisitor, withSimpleCommentsVisitor_New, withSimpleDeclarationVisitor_New, withSimpleExpressionVisitor_New, withSimpleImportVisitor_New, withSimpleModuleDefinitionVisitor_New)

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Review.Context as Context exposing (Context)
import Review.Error exposing (InternalError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Metadata as Metadata
import Review.Project exposing (Project, ProjectModule)
import Review.Project.Dependency
import Review.Rule exposing (CacheEntryFor, Direction(..), ElmJsonKey(..), Error(..), Forbidden, ModuleRuleResultCache, ProjectRuleCache, ReadmeKey(..), Required, Rule(..), Visitor, accessInternalError, accumulateList, accumulateWithListOfVisitors, makeFinalEvaluation, makeFinalEvaluationForProject, setFilePathIfUnset, setRuleName, visitDeclaration, visitImport)
import Vendor.Graph as Graph


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext

        -- TODO add moduleVisitor or implement rule logic
        , moduleVisitor : ModuleVisitorState_New projectContext moduleContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , finalEvaluationFns : List (projectContext -> List (Error {}))

        -- Define this at the same time as the module visitor?
        --, traversalType : TraversalType
        }


newProjectRuleSchema : String -> projectContext -> ProjectRuleSchema {} projectContext moduleContext
newProjectRuleSchema name initialProjectContext =
    ProjectRuleSchema
        { name = name
        , initialProjectContext = initialProjectContext
        , moduleVisitor = NoModuleVisitor_New
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        , finalEvaluationFns = []

        --, traversalType : TraversalType
        }


type ModuleVisitorState_New projectContext moduleContext
    = NoModuleVisitor_New
    | HasVisitors_New (List (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext))
    | IsPrepared_New
        { visitors : List (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)

        --, moduleContext : ModuleContextFunctions projectContext moduleContext
        }


type
    ModuleVisitor schemaState projectContext moduleContext
    -- TODO Jeroen check if projectContext is necessary
    = ModuleVisitor
        { name : String
        , moduleContextCreator : Context projectContext moduleContext
        , moduleDefinitionVisitors : List (Visitor Module moduleContext)
        , commentsVisitors : List (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
        , importVisitors : List (Visitor Import moduleContext)
        , declarationListVisitors : List (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext ))
        , declarationVisitorsOnEnter : List (Visitor Declaration moduleContext)
        , declarationVisitorsOnExit : List (Visitor Declaration moduleContext)
        , expressionVisitorsOnEnter : List (Visitor Expression moduleContext)
        , expressionVisitorsOnExit : List (Visitor Expression moduleContext)
        , finalEvaluationFns : List (moduleContext -> List (Error {}))
        }


emptyModuleVisitor : String -> moduleContext -> ModuleVisitor { moduleContext : Required } () moduleContext
emptyModuleVisitor name moduleContext =
    ModuleVisitor
        { name = name
        , moduleContextCreator = Context.init (always moduleContext)
        , moduleDefinitionVisitors = []
        , commentsVisitors = []
        , importVisitors = []
        , declarationListVisitors = []
        , declarationVisitorsOnEnter = []
        , declarationVisitorsOnExit = []
        , expressionVisitorsOnEnter = []
        , expressionVisitorsOnExit = []
        , finalEvaluationFns = []
        }


withModuleVisitor :
    (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withModuleVisitor visitor (ProjectRuleSchema schema) =
    let
        previousModuleVisitors : List (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)
        previousModuleVisitors =
            case schema.moduleVisitor of
                NoModuleVisitor_New ->
                    []

                HasVisitors_New list ->
                    list

                IsPrepared_New _ ->
                    []
    in
    ProjectRuleSchema
        { schema
            | moduleVisitor =
                HasVisitors_New (removeExtensibleRecordTypeVariable_New visitor :: previousModuleVisitors)
        }


newModuleRuleSchema_New : String -> moduleContext -> ModuleVisitor { moduleContext : Required } () moduleContext
newModuleRuleSchema_New name moduleContext =
    emptyModuleVisitor name moduleContext


withModuleContext : Context () moduleContext -> ModuleVisitor { schema | moduleContext : Required } () moduleContext -> ModuleVisitor { schema | moduleContext : Forbidden } () moduleContext
withModuleContext moduleContextCreator (ModuleVisitor moduleVisitor) =
    ModuleVisitor { moduleVisitor | moduleContextCreator = moduleContextCreator }


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema_New : ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } () moduleContext -> Rule
fromModuleRuleSchema_New ((ModuleVisitor { name }) as schema) =
    runModuleRule_New
        (reverseVisitors_New schema)
        Nothing
        |> Rule name Exceptions.init


reverseVisitors_New : ModuleVisitor schemaState () moduleContext -> ModuleVisitor schemaState () moduleContext
reverseVisitors_New (ModuleVisitor schema) =
    ModuleVisitor
        { name = schema.name
        , moduleContextCreator = schema.moduleContextCreator
        , moduleDefinitionVisitors = List.reverse schema.moduleDefinitionVisitors
        , commentsVisitors = List.reverse schema.commentsVisitors
        , importVisitors = List.reverse schema.importVisitors
        , declarationListVisitors = List.reverse schema.declarationListVisitors
        , declarationVisitorsOnEnter = List.reverse schema.declarationVisitorsOnEnter
        , declarationVisitorsOnExit = schema.declarationVisitorsOnExit
        , expressionVisitorsOnEnter = List.reverse schema.expressionVisitorsOnEnter
        , expressionVisitorsOnExit = schema.expressionVisitorsOnExit
        , finalEvaluationFns = List.reverse schema.finalEvaluationFns
        }


runModuleRule_New : ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } () moduleContext -> Maybe ModuleRuleResultCache -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
runModuleRule_New ((ModuleVisitor schema) as moduleRuleSchema) maybePreviousCache exceptions project _ =
    let
        previousModuleResults : ModuleRuleResultCache
        previousModuleResults =
            Maybe.withDefault Dict.empty maybePreviousCache

        modulesToAnalyze : List ProjectModule
        modulesToAnalyze =
            project
                |> Review.Project.modules
                |> Exceptions.apply exceptions .path

        availableData : Context.AvailableData
        availableData =
            { metadata = Metadata.create { moduleNameNode = Node.Node Range.emptyRange [] }
            }

        moduleResults : ModuleRuleResultCache
        moduleResults =
            List.foldl
                (\module_ cache ->
                    if (Dict.get module_.path cache |> Maybe.map .source) == Just module_.source then
                        -- Module is unchanged, take what was in the cache already
                        cache

                    else
                        Dict.insert module_.path
                            { source = module_.source
                            , errors = computeErrors_New moduleRuleSchema availableData module_
                            }
                            cache
                )
                previousModuleResults
                modulesToAnalyze

        errors : List (Error {})
        errors =
            moduleResults
                |> Dict.values
                |> List.concatMap .errors
    in
    ( errors
    , runModuleRule_New
        moduleRuleSchema
        (Just moduleResults)
        |> Rule schema.name exceptions
    )


computeErrors_New : ModuleVisitor schemaState () moduleContext -> Context.AvailableData -> ProjectModule -> List (Error {})
computeErrors_New ((ModuleVisitor schema) as moduleVisitor) availableData module_ =
    let
        initialContext : moduleContext
        initialContext =
            Context.apply availableData () schema.moduleContextCreator
    in
    ( [], initialContext )
        |> accumulateWithListOfVisitors schema.moduleDefinitionVisitors module_.ast.moduleDefinition
        |> accumulateWithListOfVisitors schema.commentsVisitors module_.ast.comments
        |> accumulateList (visitImport schema.importVisitors) module_.ast.imports
        |> accumulateWithListOfVisitors schema.declarationListVisitors module_.ast.declarations
        |> accumulateList
            (visitDeclaration
                schema.declarationVisitorsOnEnter
                schema.declarationVisitorsOnExit
                schema.expressionVisitorsOnEnter
                schema.expressionVisitorsOnExit
            )
            module_.ast.declarations
        |> makeFinalEvaluation schema.finalEvaluationFns
        |> List.map (setRuleName schema.name >> setFilePathIfUnset module_)
        |> List.reverse


{-| This function that is supplied by the user will be stored in the `ProjectRuleSchema`,
but it contains an extensible record. This means that `ProjectRuleSchema` will
need an additional type variable for no useful value. Because we have full control
over the `ModuleRuleSchema` in this module, we can change the phantom type to be
whatever we want it to be, and we'll change it something that makes sense but
without the extensible record type variable.
-}
removeExtensibleRecordTypeVariable_New :
    (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { a | hasAtLeastOneVisitor : () } projectContext moduleContext)
    -> (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)
removeExtensibleRecordTypeVariable_New function =
    function >> (\(ModuleVisitor param) -> ModuleVisitor param)


withElmJsonProjectVisitor :
    (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withElmJsonProjectVisitor visitor (ProjectRuleSchema projectRuleSchema) =
    -- TODO BREAKING CHANGE, make elm.json mandatory
    ProjectRuleSchema { projectRuleSchema | elmJsonVisitors = visitor :: projectRuleSchema.elmJsonVisitors }


withReadmeProjectVisitor :
    (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withReadmeProjectVisitor visitor (ProjectRuleSchema projectRuleSchema) =
    ProjectRuleSchema { projectRuleSchema | readmeVisitors = visitor :: projectRuleSchema.readmeVisitors }


withDependenciesVisitor :
    (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDependenciesVisitor visitor (ProjectRuleSchema projectRuleSchema) =
    ProjectRuleSchema { projectRuleSchema | dependenciesVisitors = visitor :: projectRuleSchema.dependenciesVisitors }


withFinalProjectEvaluation :
    (projectContext -> List (Error {}))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withFinalProjectEvaluation visitor (ProjectRuleSchema projectRuleSchema) =
    ProjectRuleSchema { projectRuleSchema | finalEvaluationFns = visitor :: projectRuleSchema.finalEvaluationFns }


fromProjectRuleSchema : ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
fromProjectRuleSchema (ProjectRuleSchema schema) =
    Rule schema.name
        Exceptions.init
        (runProjectRule
            (ProjectRuleSchema
                { schema
                    | elmJsonVisitors = List.reverse schema.elmJsonVisitors
                    , readmeVisitors = List.reverse schema.readmeVisitors
                    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
                    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
                }
            )
            Nothing
        )



-- RUNNING PROJECT RULE


runProjectRule : ProjectRuleSchema schemaState projectContext moduleContext -> Maybe (ProjectRuleCache projectContext) -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
runProjectRule ((ProjectRuleSchema schema) as wrappedSchema) maybePreviousCache exceptions project nodeContexts =
    let
        cacheWithInitialContext : ProjectRuleCache projectContext
        cacheWithInitialContext =
            computeProjectContext (ProjectRuleSchema schema) project maybePreviousCache

        initialContext : projectContext
        initialContext =
            cacheWithInitialContext.dependencies.context

        --previousModuleContexts : Dict String (CacheEntry projectContext)
        --previousModuleContexts =
        --    case maybePreviousCache of
        --        Just { moduleContexts } ->
        --            moduleContexts
        --
        --        Nothing ->
        --            Dict.empty
        --
        --moduleVisitors :
        --    Maybe
        --        { visitors : List (ModuleRuleSchema {} projectContext moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } projectContext moduleContext)
        --        , moduleContext : ModuleContextOptions projectContext moduleContext
        --        }
        --moduleVisitors =
        --    case schema.moduleVisitor of
        --        NoModuleVisitor ->
        --            Nothing
        --
        --        HasVisitors _ ->
        --            Nothing
        --
        --        IsPrepared visitorInfo ->
        --            Just visitorInfo
        --
        --newCachedModuleContexts : Dict String (CacheEntry projectContext)
        --newCachedModuleContexts =
        --    case moduleVisitors of
        --        Just visitors ->
        --            computeModules
        --                wrappedSchema
        --                visitors
        --                project
        --                initialContext
        --                nodeContexts
        --                previousModuleContexts
        --
        --        Nothing ->
        --            Dict.empty
        --
        --contextsAndErrorsPerModule : List ( List (Error {}), projectContext )
        --contextsAndErrorsPerModule =
        --    newCachedModuleContexts
        --        |> Dict.values
        --        |> List.map (\cacheEntry -> ( cacheEntry.errors, cacheEntry.context ))
        errorsFromFinalEvaluation : List (Error {})
        errorsFromFinalEvaluation =
            let
                --    previousAllModulesContext : List projectContext
                --    previousAllModulesContext =
                --        previousModuleContexts
                --            |> Dict.values
                --            |> List.map .context
                --
                allModulesContext : List projectContext
                allModulesContext =
                    -- TODO Jeroen
                    --List.map Tuple.second contextsAndErrorsPerModule
                    []
            in
            case maybePreviousCache of
                Just previousCache ->
                    -- TODO JEROEN
                    --if initialContext == previousCache.dependencies.context && allModulesContext == previousAllModulesContext then
                    --    previousCache.finalEvaluationErrors
                    --
                    --else
                    errorsFromFinalEvaluationForProject wrappedSchema initialContext allModulesContext

                Nothing ->
                    errorsFromFinalEvaluationForProject wrappedSchema initialContext allModulesContext

        newCache : ProjectRuleCache projectContext
        newCache =
            { elmJson = cacheWithInitialContext.elmJson
            , readme = cacheWithInitialContext.readme
            , dependencies = cacheWithInitialContext.dependencies
            , moduleContexts = Dict.empty -- TODO Jeroen newCachedModuleContexts
            , finalEvaluationErrors = errorsFromFinalEvaluation
            }

        errors : List (Error {})
        errors =
            errorsFromCache newCache
                |> Exceptions.apply exceptions (accessInternalError >> .filePath)
                |> List.map (setRuleName schema.name)
    in
    ( errors, Rule schema.name exceptions (runProjectRule wrappedSchema (Just newCache)) )


computeProjectContext : ProjectRuleSchema schemaState projectContext moduleContext -> Project -> Maybe (ProjectRuleCache projectContext) -> ProjectRuleCache projectContext
computeProjectContext (ProjectRuleSchema schema) project maybePreviousCache =
    let
        projectElmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        projectElmJson =
            Review.Project.elmJson project

        elmJsonData : Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
        elmJsonData =
            Maybe.map
                (\elmJson ->
                    { elmJsonKey = ElmJsonKey { path = elmJson.path, raw = elmJson.raw }
                    , project = elmJson.project
                    }
                )
                projectElmJson

        readmeData : Maybe { readmeKey : ReadmeKey, content : String }
        readmeData =
            Review.Project.readme project
                |> Maybe.map
                    (\readme ->
                        { readmeKey = ReadmeKey { path = readme.path, content = readme.content }
                        , content = readme.content
                        }
                    )

        elmJsonCacheEntry : CacheEntryFor (Maybe { path : String, raw : String, project : Elm.Project.Project }) projectContext
        elmJsonCacheEntry =
            let
                computeElmJson : () -> CacheEntryFor (Maybe { path : String, raw : String, project : Elm.Project.Project }) projectContext
                computeElmJson () =
                    let
                        ( errorsForVisitor, contextForVisitor ) =
                            ( [], schema.initialProjectContext )
                                |> accumulateWithListOfVisitors schema.elmJsonVisitors elmJsonData
                    in
                    { value = projectElmJson
                    , errors = errorsForVisitor
                    , context = contextForVisitor
                    }
            in
            case maybePreviousCache of
                Just previousCache ->
                    if previousCache.elmJson.value == projectElmJson then
                        previousCache.elmJson

                    else
                        computeElmJson ()

                Nothing ->
                    computeElmJson ()

        readmeCacheEntry : CacheEntryFor (Maybe { readmeKey : ReadmeKey, content : String }) projectContext
        readmeCacheEntry =
            let
                computeReadme : () -> CacheEntryFor (Maybe { readmeKey : ReadmeKey, content : String }) projectContext
                computeReadme () =
                    let
                        ( errorsForVisitor, contextForVisitor ) =
                            ( elmJsonCacheEntry.errors, elmJsonCacheEntry.context )
                                |> accumulateWithListOfVisitors schema.readmeVisitors readmeData
                    in
                    { value = readmeData
                    , errors = errorsForVisitor
                    , context = contextForVisitor
                    }
            in
            case maybePreviousCache of
                Just previousCache ->
                    if
                        -- If the previous context stayed the same
                        (previousCache.elmJson.context /= elmJsonCacheEntry.context)
                            -- and the readme stayed the same
                            || (previousCache.readme.value == readmeData)
                    then
                        previousCache.readme

                    else
                        computeReadme ()

                Nothing ->
                    computeReadme ()

        dependenciesCacheEntry : CacheEntryFor (Dict String Review.Project.Dependency.Dependency) projectContext
        dependenciesCacheEntry =
            let
                dependencies : Dict String Review.Project.Dependency.Dependency
                dependencies =
                    Review.Project.dependencies project

                computeDependencies : () -> CacheEntryFor (Dict String Review.Project.Dependency.Dependency) projectContext
                computeDependencies () =
                    let
                        ( errorsForVisitor, contextForVisitor ) =
                            ( elmJsonCacheEntry.errors, elmJsonCacheEntry.context )
                                |> accumulateWithListOfVisitors schema.dependenciesVisitors dependencies
                    in
                    { value = dependencies
                    , errors = errorsForVisitor
                    , context = contextForVisitor
                    }
            in
            case maybePreviousCache of
                Just previousCache ->
                    if
                        -- If the previous context stayed the same
                        (previousCache.readme.context /= readmeCacheEntry.context)
                            -- and the readme stayed the same
                            || (previousCache.dependencies.value == dependencies)
                    then
                        previousCache.dependencies

                    else
                        computeDependencies ()

                Nothing ->
                    computeDependencies ()
    in
    { elmJson = elmJsonCacheEntry
    , readme = readmeCacheEntry
    , dependencies = dependenciesCacheEntry
    , moduleContexts = Dict.empty
    , finalEvaluationErrors = []
    }



--computeModules :
--    ProjectRuleSchema schemaState projectContext moduleContext
--    ->
--        { visitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
--        , moduleContext : ModuleContextFunctions projectContext moduleContext
--        }
--    -> Project
--    -> projectContext
--    -> List (Graph.NodeContext ModuleName ())
--    -> Dict String (CacheEntry projectContext)
--    -> Dict String (CacheEntry projectContext)
--computeModules (ProjectRuleSchema schema) visitors project initialContext nodeContexts startCache =
--    let
--        graph : Graph ModuleName ()
--        graph =
--            Review.Project.Internal.moduleGraph project
--
--        projectModulePaths : Set String
--        projectModulePaths =
--            project
--                |> Review.Project.modules
--                |> List.map .path
--                |> Set.fromList
--
--        modules : Dict ModuleName ProjectModule
--        modules =
--            project
--                |> Review.Project.modules
--                |> List.foldl
--                    (\module_ dict ->
--                        Dict.insert
--                            (getModuleName module_)
--                            module_
--                            dict
--                    )
--                    Dict.empty
--
--        newStartCache : Dict String (CacheEntry projectContext)
--        newStartCache =
--            startCache
--                |> Dict.filter (\path _ -> Set.member path projectModulePaths)
--
--        dummyInitialContext : moduleContext
--        dummyInitialContext =
--            visitors.moduleContext.fromProjectToModule
--                (ModuleKey "dummy")
--                (Node.Node Range.emptyRange [ "Dummy" ])
--                initialContext
--
--        moduleVisitor : ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
--        moduleVisitor =
--            List.foldl
--                (\addVisitors (ModuleRuleSchema moduleVisitorSchema) ->
--                    addVisitors (ModuleRuleSchema moduleVisitorSchema)
--                )
--                (emptySchema "" dummyInitialContext)
--                visitors.visitors
--                |> reverseVisitors
--
--        computeModule : Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext
--        computeModule cache importedModules module_ =
--            let
--                moduleKey : ModuleKey
--                moduleKey =
--                    ModuleKey module_.path
--
--                moduleNameNode_ : Node ModuleName
--                moduleNameNode_ =
--                    moduleNameNode module_.ast.moduleDefinition
--
--                initialModuleContext : moduleContext
--                initialModuleContext =
--                    case schema.traversalType of
--                        AllModulesInParallel ->
--                            visitors.moduleContext.fromProjectToModule
--                                moduleKey
--                                moduleNameNode_
--                                initialContext
--
--                        ImportedModulesFirst ->
--                            importedModules
--                                |> List.filterMap
--                                    (\importedModule ->
--                                        Dict.get importedModule.path cache
--                                            |> Maybe.map .context
--                                    )
--                                |> List.foldl visitors.moduleContext.foldProjectContexts initialContext
--                                |> visitors.moduleContext.fromProjectToModule moduleKey moduleNameNode_
--
--                ( moduleErrors, context ) =
--                    visitModuleForProjectRule
--                        moduleVisitor
--                        initialModuleContext
--                        module_
--            in
--            { source = module_.source
--            , errors = List.map (setFilePathIfUnset module_) moduleErrors
--            , context =
--                visitors.moduleContext.fromModuleToProject
--                    moduleKey
--                    moduleNameNode_
--                    context
--            }
--    in
--    List.foldl
--        (computeModuleAndCacheResult schema.traversalType modules graph computeModule)
--        ( newStartCache, Set.empty )
--        nodeContexts
--        |> Tuple.first


errorsFromFinalEvaluationForProject : ProjectRuleSchema schemaState projectContext moduleContext -> projectContext -> List projectContext -> List (Error {})
errorsFromFinalEvaluationForProject (ProjectRuleSchema schema) initialContext contextsPerModule =
    if List.isEmpty schema.finalEvaluationFns then
        []

    else
        --TODO Jeroen
        --let
        --    finalContext : projectContext
        --    finalContext =
        --        case schema.moduleVisitor of
        --            NoModuleVisitor ->
        --                initialContext
        --
        --            HasVisitors _ ->
        --                initialContext
        --
        --            IsPrepared { moduleContext } ->
        --                List.foldl moduleContext.foldProjectContexts initialContext contextsPerModule
        --in
        --makeFinalEvaluationForProject schema.finalEvaluationFns finalContext
        makeFinalEvaluationForProject schema.finalEvaluationFns initialContext


errorsFromCache : ProjectRuleCache projectContext -> List (Error {})
errorsFromCache cache =
    List.concat
        [ cache.elmJson.errors
        , cache.readme.errors
        , cache.dependencies.errors

        -- TODO Jeroen
        --, cache.moduleContexts
        --    |> Dict.values
        --    |> List.concatMap (\cacheEntry -> cacheEntry.errors)
        , cache.finalEvaluationErrors
        ]


withSimpleModuleDefinitionVisitor_New : (Node Module -> List (Error {})) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withSimpleModuleDefinitionVisitor_New visitor schema =
    withModuleDefinitionVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withModuleDefinitionVisitor_New : (Node Module -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withModuleDefinitionVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


withSimpleCommentsVisitor_New : (List (Node String) -> List (Error {})) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withSimpleCommentsVisitor_New visitor schema =
    withCommentsVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withCommentsVisitor_New : (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withCommentsVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | commentsVisitors = visitor :: schema.commentsVisitors }


withSimpleImportVisitor_New : (Node Import -> List (Error {})) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withSimpleImportVisitor_New visitor schema =
    withImportVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withImportVisitor_New : (Node Import -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withImportVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | importVisitors = visitor :: schema.importVisitors }


withSimpleDeclarationVisitor_New : (Node Declaration -> List (Error {})) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withSimpleDeclarationVisitor_New visitor schema =
    withDeclarationEnterVisitor_New
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withDeclarationVisitor_New : (Node Declaration -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDeclarationVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor
        { schema
            | declarationVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.declarationVisitorsOnEnter
            , declarationVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.declarationVisitorsOnExit
        }


withDeclarationEnterVisitor_New : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDeclarationEnterVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationVisitorsOnEnter = visitor :: schema.declarationVisitorsOnEnter }


withDeclarationExitVisitor_New : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDeclarationExitVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationVisitorsOnExit = visitor :: schema.declarationVisitorsOnExit }


withDeclarationListVisitor_New : (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDeclarationListVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


withSimpleExpressionVisitor_New : (Node Expression -> List (Error {})) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withSimpleExpressionVisitor_New visitor schema =
    withExpressionEnterVisitor_New
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withExpressionVisitor_New : (Node Expression -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withExpressionVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor
        { schema
            | expressionVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.expressionVisitorsOnEnter
            , expressionVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.expressionVisitorsOnExit
        }


withExpressionEnterVisitor_New : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withExpressionEnterVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | expressionVisitorsOnEnter = visitor :: schema.expressionVisitorsOnEnter }


withExpressionExitVisitor_New : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withExpressionExitVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | expressionVisitorsOnExit = visitor :: schema.expressionVisitorsOnExit }


withFinalModuleEvaluation_New : (moduleContext -> List (Error {})) -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withFinalModuleEvaluation_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }
