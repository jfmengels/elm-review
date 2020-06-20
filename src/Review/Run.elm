module Review.Run exposing (runProjectRule)


runProjectRule : ProjectRuleSchema schemaState projectContext moduleContext -> Maybe (ProjectRuleCache projectContext) -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
runProjectRule ((ProjectRuleSchema schema) as wrappedSchema) maybePreviousCache exceptions project nodeContexts =
    let
        cacheWithInitialContext : ProjectRuleCache projectContext
        cacheWithInitialContext =
            computeCacheWithInitialContext (ProjectRuleSchema schema) project maybePreviousCache

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

        moduleVisitors :
            Maybe
                { visitors : List (ModuleRuleSchema {} projectContext moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } projectContext moduleContext)
                , moduleContext : ModuleContextOptions projectContext moduleContext
                }
        moduleVisitors =
            case schema.moduleVisitor of
                NoModuleVisitor ->
                    Nothing

                HasVisitors _ ->
                    Nothing

                IsPrepared visitorInfo ->
                    Just visitorInfo

        newCachedModuleContexts : Dict String (CacheEntry projectContext)
        newCachedModuleContexts =
            case moduleVisitors of
                Just visitors ->
                    computeModules
                        wrappedSchema
                        visitors
                        project
                        initialContext
                        nodeContexts
                        previousModuleContexts

                Nothing ->
                    Dict.empty

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
                        errorsFromFinalEvaluationForProject wrappedSchema initialContext allModulesContext

                Nothing ->
                    errorsFromFinalEvaluationForProject wrappedSchema initialContext allModulesContext

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
                |> List.map (setRuleName schema.name)
    in
    ( errors, Rule schema.name exceptions (runProjectRule wrappedSchema (Just newCache)) )
