module Review.Rule3 exposing (ProjectRuleSchema, fromProjectRuleSchema, newProjectRuleSchema, withDependenciesVisitor, withElmJsonProjectVisitor, withFinalProjectEvaluation, withReadmeProjectVisitor)

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Context as Context
import Review.Error exposing (InternalError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Project exposing (Project)
import Review.Project.Dependency
import Review.Rule exposing (CacheEntryFor, ElmJsonKey(..), Error(..), ProjectRuleCache, ReadmeKey(..), Rule(..), accessInternalError, accumulateWithListOfVisitors, makeFinalEvaluationForProject, setRuleName)
import Vendor.Graph as Graph


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext

        -- TODO add moduleVisitor or implement rule logic
        --, moduleVisitor : ModuleVisitorState projectContext moduleContext
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

        --, moduleVisitor : ModuleVisitorState projectContext moduleContext
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        , finalEvaluationFns = []

        --, traversalType : TraversalType
        }


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
