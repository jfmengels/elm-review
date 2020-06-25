module Review.Visitor exposing
    ( Folder
    , RunnableModuleVisitor
    , RunnableProjectVisitor
    , TraversalAndFolder(..)
    , run
    )

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Review.Context as Context exposing (Context)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Metadata as Metadata
import Review.Project exposing (Project, ProjectModule)
import Review.Project.Dependency
import Review.Project.Internal
import Review.Rule
    exposing
        ( CacheEntry
        , CacheEntryFor
        , ElmJsonKey(..)
        , Error
        , Forbidden
        , ModuleKey(..)
        , ModuleRuleResultCache
        , ModuleVisitorFunctions
        , ProjectRuleCache
        , ReadmeKey(..)
        , Required
        , Rule(..)
        , Visitor
        , accessInternalError
        , accumulateList
        , accumulateWithListOfVisitors
        , computeModuleAndCacheResult
        , getModuleName
        , makeFinalEvaluation
        , moduleNameNode
        , setFilePathIfUnset
        , setRuleName
        , visitDeclaration
        , visitImport
        )
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


type alias RunnableModuleVisitor moduleContext =
    { moduleDefinitionVisitors : List (Visitor Module moduleContext)
    , commentsVisitors : List (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
    , importVisitors : List (Visitor Import moduleContext)
    , declarationListVisitors : List (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext ))
    , declarationVisitorsOnEnter : List (Visitor Declaration moduleContext)
    , declarationVisitorsOnExit : List (Visitor Declaration moduleContext)
    , expressionVisitorsOnEnter : List (Visitor Expression moduleContext)
    , expressionVisitorsOnExit : List (Visitor Expression moduleContext)
    , finalEvaluationFns : List (moduleContext -> List (Error {}))
    }


type alias RunnableProjectVisitor projectContext moduleContext =
    { name : String
    , initialProjectContext : projectContext
    , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , moduleVisitor : Maybe ( RunnableModuleVisitor moduleContext, Context projectContext moduleContext )
    , traversalAndFolder : TraversalAndFolder projectContext moduleContext
    , finalEvaluationFns : List (projectContext -> List (Error {}))
    }


type TraversalAndFolder projectContext moduleContext
    = TraverseAllModulesInParallel (Maybe (Folder projectContext moduleContext))
    | TraverseImportedModulesFirst (Folder projectContext moduleContext)


type alias Folder projectContext moduleContext =
    { fromModuleToProject : Context moduleContext projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }


run : RunnableProjectVisitor projectContext moduleContext -> Maybe (ProjectRuleCache projectContext) -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
run projectVisitor maybePreviousCache exceptions project nodeContexts =
    let
        cacheWithInitialContext : ProjectRuleCache projectContext
        cacheWithInitialContext =
            computeProjectContext projectVisitor project maybePreviousCache

        initialContext : projectContext
        initialContext =
            cacheWithInitialContext.dependencies.context

        previousModuleContexts : Dict String (CacheEntry projectContext)
        previousModuleContexts =
            case maybePreviousCache of
                Just { moduleContexts } ->
                    moduleContexts

                Nothing ->
                    Dict.empty

        newCachedModuleContexts : Dict String (CacheEntry projectContext)
        newCachedModuleContexts =
            case projectVisitor.moduleVisitor of
                Nothing ->
                    Dict.empty

                Just moduleVisitor ->
                    computeModules
                        projectVisitor
                        moduleVisitor
                        project
                        initialContext
                        nodeContexts
                        previousModuleContexts

        contextsAndErrorsPerModule : List ( List (Error {}), projectContext )
        contextsAndErrorsPerModule =
            newCachedModuleContexts
                |> Dict.values
                |> List.map (\cacheEntry -> ( cacheEntry.errors, cacheEntry.context ))

        errorsFromFinalEvaluation : List (Error {})
        errorsFromFinalEvaluation =
            let
                previousAllModulesContext : List projectContext
                previousAllModulesContext =
                    previousModuleContexts
                        |> Dict.values
                        |> List.map .context

                allModulesContext : List projectContext
                allModulesContext =
                    List.map Tuple.second contextsAndErrorsPerModule
            in
            case maybePreviousCache of
                Just previousCache ->
                    if initialContext == previousCache.dependencies.context && allModulesContext == previousAllModulesContext then
                        previousCache.finalEvaluationErrors

                    else
                        errorsFromFinalEvaluationForProject projectVisitor initialContext allModulesContext

                Nothing ->
                    errorsFromFinalEvaluationForProject projectVisitor initialContext allModulesContext

        newCache : ProjectRuleCache projectContext
        newCache =
            { elmJson = cacheWithInitialContext.elmJson
            , readme = cacheWithInitialContext.readme
            , dependencies = cacheWithInitialContext.dependencies
            , moduleContexts = newCachedModuleContexts
            , finalEvaluationErrors = errorsFromFinalEvaluation
            }

        errors : List (Error {})
        errors =
            errorsFromCache newCache
                |> Exceptions.apply exceptions (accessInternalError >> .filePath)
                |> List.map (setRuleName projectVisitor.name)
    in
    ( errors, Rule projectVisitor.name exceptions (run projectVisitor (Just newCache)) )


errorsFromCache : ProjectRuleCache projectContext -> List (Error {})
errorsFromCache cache =
    List.concat
        [ cache.elmJson.errors
        , cache.readme.errors
        , cache.dependencies.errors
        , cache.moduleContexts
            |> Dict.values
            |> List.concatMap (\cacheEntry -> cacheEntry.errors)
        , cache.finalEvaluationErrors
        ]



-- VISIT PROJECT


computeProjectContext : RunnableProjectVisitor projectContext moduleContext -> Project -> Maybe (ProjectRuleCache projectContext) -> ProjectRuleCache projectContext
computeProjectContext projectVisitor project maybePreviousCache =
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
                            ( [], projectVisitor.initialProjectContext )
                                |> accumulateWithListOfVisitors projectVisitor.elmJsonVisitors elmJsonData
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
                            ( [], elmJsonCacheEntry.context )
                                |> accumulateWithListOfVisitors projectVisitor.readmeVisitors readmeData
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
            -- TODO Rewrite these steps to make it less likely to make an error at every step
            let
                dependencies : Dict String Review.Project.Dependency.Dependency
                dependencies =
                    Review.Project.dependencies project

                computeDependencies : () -> CacheEntryFor (Dict String Review.Project.Dependency.Dependency) projectContext
                computeDependencies () =
                    let
                        ( errorsForVisitor, contextForVisitor ) =
                            ( [], readmeCacheEntry.context )
                                |> accumulateWithListOfVisitors projectVisitor.dependenciesVisitors dependencies
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
                            -- and the dependencies stayed the same
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



-- VISIT MODULES


computeModules :
    RunnableProjectVisitor projectContext moduleContext
    -> ( RunnableModuleVisitor moduleContext, Context projectContext moduleContext )
    -> Project
    -> projectContext
    -> List (Graph.NodeContext ModuleName ())
    -> Dict String (CacheEntry projectContext)
    -> Dict String (CacheEntry projectContext)
computeModules projectVisitor ( moduleVisitor, moduleContextCreator ) project initialProjectContext nodeContexts startCache =
    let
        graph : Graph ModuleName ()
        graph =
            Review.Project.Internal.moduleGraph project

        projectModulePaths : Set String
        projectModulePaths =
            project
                |> Review.Project.modules
                |> List.map .path
                |> Set.fromList

        modules : Dict ModuleName ProjectModule
        modules =
            project
                |> Review.Project.modules
                |> List.foldl
                    (\module_ dict ->
                        Dict.insert
                            (getModuleName module_)
                            module_
                            dict
                    )
                    Dict.empty

        newStartCache : Dict String (CacheEntry projectContext)
        newStartCache =
            startCache
                |> Dict.filter (\path _ -> Set.member path projectModulePaths)

        computeModule : Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext
        computeModule cache importedModules module_ =
            let
                moduleKey : ModuleKey
                moduleKey =
                    ModuleKey module_.path

                metadata : Metadata.Metadata
                metadata =
                    Metadata.create { moduleNameNode = moduleNameNode module_.ast.moduleDefinition }

                availableData : Context.AvailableData
                availableData =
                    { metadata = metadata
                    , moduleKey = moduleKey
                    }

                initialModuleContext : moduleContext
                initialModuleContext =
                    case projectVisitor.traversalAndFolder of
                        TraverseAllModulesInParallel _ ->
                            Context.apply availableData moduleContextCreator initialProjectContext

                        TraverseImportedModulesFirst { foldProjectContexts } ->
                            let
                                projectContext : projectContext
                                projectContext =
                                    importedModules
                                        |> List.filterMap
                                            (\importedModule ->
                                                Dict.get importedModule.path cache
                                                    |> Maybe.map .context
                                            )
                                        |> List.foldl foldProjectContexts initialProjectContext
                            in
                            -- It is never used anywhere else
                            Context.apply availableData moduleContextCreator projectContext

                ( moduleErrors, context ) =
                    visitModuleForProjectRule
                        moduleVisitor
                        initialModuleContext
                        module_
            in
            { source = module_.source
            , errors = List.map (setFilePathIfUnset module_) moduleErrors
            , context =
                case getFolderFromTraversal projectVisitor.traversalAndFolder of
                    Just { fromModuleToProject } ->
                        Context.apply availableData fromModuleToProject context

                    Nothing ->
                        initialProjectContext
            }
    in
    List.foldl
        (computeModuleAndCacheResult projectVisitor.traversalAndFolder modules graph computeModule)
        ( newStartCache, Set.empty )
        nodeContexts
        |> Tuple.first


computeModuleAndCacheResult :
    TraversalAndFolder projectContext moduleContext
    -> Dict ModuleName ProjectModule
    -> Graph ModuleName ()
    -> (Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext)
    -> Graph.NodeContext ModuleName ()
    -> ( Dict String (CacheEntry projectContext), Set ModuleName )
    -> ( Dict String (CacheEntry projectContext), Set ModuleName )
computeModuleAndCacheResult traversalAndFolder modules graph computeModule { node, incoming } ( cache, invalidatedModules ) =
    case Dict.get node.label modules of
        Nothing ->
            ( cache, invalidatedModules )

        Just module_ ->
            let
                importedModules : List ProjectModule
                importedModules =
                    case traversalAndFolder of
                        TraverseAllModulesInParallel _ ->
                            []

                        TraverseImportedModulesFirst _ ->
                            incoming
                                |> IntDict.keys
                                |> List.filterMap
                                    (\key ->
                                        Graph.get key graph
                                            |> Maybe.andThen (\nodeContext -> Dict.get nodeContext.node.label modules)
                                    )

                compute : Maybe (CacheEntry projectContext) -> ( Dict String (CacheEntry projectContext), Set ModuleName )
                compute previousResult =
                    let
                        result : CacheEntry projectContext
                        result =
                            computeModule cache importedModules module_
                    in
                    ( Dict.insert module_.path result cache
                    , if Just result.context /= Maybe.map .context previousResult then
                        Set.insert (getModuleName module_) invalidatedModules

                      else
                        invalidatedModules
                    )
            in
            case Dict.get module_.path cache of
                Nothing ->
                    compute Nothing

                Just cacheEntry ->
                    if cacheEntry.source == module_.source && (traversesAllModulesInParallel traversalAndFolder || noImportedModulesHaveANewContext importedModules invalidatedModules) then
                        -- The module's source and the module's imported modules' context are unchanged, we will later return the cached errors and context
                        ( cache, invalidatedModules )

                    else
                        compute (Just cacheEntry)


traversesAllModulesInParallel : TraversalAndFolder projectContext moduleContext -> Bool
traversesAllModulesInParallel traversalAndFolder =
    case traversalAndFolder of
        TraverseAllModulesInParallel _ ->
            True

        TraverseImportedModulesFirst _ ->
            False


noImportedModulesHaveANewContext : List ProjectModule -> Set ModuleName -> Bool
noImportedModulesHaveANewContext importedModules invalidatedModules =
    importedModules
        |> List.map getModuleName
        |> Set.fromList
        |> Set.intersect invalidatedModules
        |> Set.isEmpty


getFolderFromTraversal : TraversalAndFolder projectContext moduleContext -> Maybe (Folder projectContext moduleContext)
getFolderFromTraversal traversalAndFolder =
    case traversalAndFolder of
        TraverseAllModulesInParallel maybeFolder ->
            maybeFolder

        TraverseImportedModulesFirst folder ->
            Just folder


visitModuleForProjectRule : RunnableModuleVisitor moduleContext -> moduleContext -> ProjectModule -> ( List (Error {}), moduleContext )
visitModuleForProjectRule schema initialContext module_ =
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
        |> (\( errors, moduleContext ) -> ( makeFinalEvaluation schema.finalEvaluationFns ( errors, moduleContext ), moduleContext ))



-- FINAL EVALUATION


errorsFromFinalEvaluationForProject : RunnableProjectVisitor projectContext moduleContext -> projectContext -> List projectContext -> List (Error {})
errorsFromFinalEvaluationForProject projectVisitor initialContext contextsPerModule =
    if List.isEmpty projectVisitor.finalEvaluationFns then
        []

    else
        let
            finalContext : projectContext
            finalContext =
                case getFolderFromTraversal projectVisitor.traversalAndFolder of
                    Just { foldProjectContexts } ->
                        List.foldl foldProjectContexts initialContext contextsPerModule

                    Nothing ->
                        initialContext
        in
        List.concatMap
            (\finalEvaluationFn -> finalEvaluationFn finalContext)
            projectVisitor.finalEvaluationFns
