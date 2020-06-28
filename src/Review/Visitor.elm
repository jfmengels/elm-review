module Review.Visitor exposing
    ( Folder
    , RunnableModuleVisitor
    , RunnableProjectVisitor
    , TraversalAndFolder(..)
    , Visitor
    , run
    )

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Context as Context exposing (Context)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Metadata as Metadata
import Review.Project exposing (Project, ProjectModule)
import Review.Project.Dependency
import Review.Project.Internal
import Review.Rule
    exposing
        ( ElmJsonKey(..)
        , Error
        , ModuleKey(..)
        , ReadmeKey(..)
        , Rule(..)
        , accessInternalError
        , mapInternalError
        , setFilePathIfUnset
        )
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


type alias RunnableProjectVisitor projectContext moduleContext =
    { initialProjectContext : projectContext
    , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , moduleVisitor : Maybe ( RunnableModuleVisitor moduleContext, Context projectContext moduleContext )
    , traversalAndFolder : TraversalAndFolder projectContext moduleContext
    , finalEvaluationFns : List (projectContext -> List (Error {}))
    }


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


type alias Visitor nodeType context =
    Node nodeType -> context -> ( List (Error {}), context )


type TraversalAndFolder projectContext moduleContext
    = TraverseAllModulesInParallel (Maybe (Folder projectContext moduleContext))
    | TraverseImportedModulesFirst (Folder projectContext moduleContext)


type alias Folder projectContext moduleContext =
    { fromModuleToProject : Context moduleContext projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }


type alias ProjectRuleCache projectContext =
    { elmJson : CacheEntryFor (Maybe { path : String, raw : String, project : Elm.Project.Project }) projectContext
    , readme : CacheEntryFor (Maybe { readmeKey : ReadmeKey, content : String }) projectContext
    , dependencies : CacheEntryFor (Dict String Review.Project.Dependency.Dependency) projectContext
    , moduleContexts : Dict String (CacheEntry projectContext)
    , finalEvaluationErrors : List (Error {})
    }


type alias CacheEntry projectContext =
    { source : String
    , errors : List (Error {})
    , context : projectContext
    }


type alias CacheEntryFor value projectContext =
    { value : value
    , errors : List (Error {})
    , context : projectContext
    }


run : String -> RunnableProjectVisitor projectContext moduleContext -> Maybe (ProjectRuleCache projectContext) -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
run name projectVisitor maybePreviousCache exceptions project nodeContexts =
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
                        exceptions
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
            case projectVisitor.traversalAndFolder of
                TraverseAllModulesInParallel _ ->
                    errorsFromCache newCache

                TraverseImportedModulesFirst _ ->
                    errorsFromCache newCache
                        |> Exceptions.apply exceptions (accessInternalError >> .filePath)
    in
    ( List.map (setRuleName name) errors
    , Rule { exceptions = exceptions, ruleImplementation = run name projectVisitor (Just newCache) }
    )


setRuleName : String -> Error scope -> Error scope
setRuleName ruleName error_ =
    mapInternalError (\err -> { err | ruleName = ruleName }) error_


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
    -> Exceptions
    -> projectContext
    -> List (Graph.NodeContext ModuleName ())
    -> Dict String (CacheEntry projectContext)
    -> Dict String (CacheEntry projectContext)
computeModules projectVisitor ( moduleVisitor, moduleContextCreator ) project exceptions initialProjectContext nodeContexts startCache =
    let
        graph : Graph ModuleName ()
        graph =
            Review.Project.Internal.moduleGraph project

        moduleToAnalyze : List ProjectModule
        moduleToAnalyze =
            case projectVisitor.traversalAndFolder of
                TraverseAllModulesInParallel _ ->
                    Exceptions.apply
                        exceptions
                        .path
                        (Review.Project.modules project)

                TraverseImportedModulesFirst _ ->
                    Review.Project.modules project

        projectModulePaths : Set String
        projectModulePaths =
            moduleToAnalyze
                |> List.map .path
                |> Set.fromList

        modules : Dict ModuleName ProjectModule
        modules =
            List.foldl
                (\module_ dict ->
                    Dict.insert
                        (getModuleName module_)
                        module_
                        dict
                )
                Dict.empty
                moduleToAnalyze

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
                    Metadata.createMetadata { moduleNameNode = moduleNameNode module_.ast.moduleDefinition }

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


visitImport :
    List (Node Import -> moduleContext -> ( List (Error {}), moduleContext ))
    -> Node Import
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitImport importVisitors node moduleContext =
    visitNodeWithListOfVisitors importVisitors node ( [], moduleContext )


visitDeclaration :
    List (Visitor Declaration moduleContext)
    -> List (Visitor Declaration moduleContext)
    -> List (Visitor Expression moduleContext)
    -> List (Visitor Expression moduleContext)
    -> Node Declaration
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitDeclaration declarationVisitorsOnEnter declarationVisitorsOnExit expressionVisitorsOnEnter expressionVisitorsOnExit node moduleContext =
    ( [], moduleContext )
        |> visitNodeWithListOfVisitors declarationVisitorsOnEnter node
        |> accumulateList (visitExpression expressionVisitorsOnEnter expressionVisitorsOnExit) (expressionsInDeclaration node)
        |> visitNodeWithListOfVisitors declarationVisitorsOnExit node


visitExpression :
    List (Visitor Expression moduleContext)
    -> List (Visitor Expression moduleContext)
    -> Node Expression
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitExpression onEnter onExit node moduleContext =
    ( [], moduleContext )
        |> visitNodeWithListOfVisitors onEnter node
        |> accumulateList (visitExpression onEnter onExit) (expressionChildren node)
        |> visitNodeWithListOfVisitors onExit node


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluation : List (context -> List (Error {})) -> ( List (Error {}), context ) -> List (Error {})
makeFinalEvaluation finalEvaluationFns ( previousErrors, context ) =
    List.concat
        [ List.concatMap
            (\visitor -> visitor context)
            finalEvaluationFns
        , previousErrors
        ]


expressionChildren : Node Expression -> List (Node Expression)
expressionChildren node =
    case Node.value node of
        Expression.Application expressions ->
            expressions

        Expression.Literal _ ->
            []

        Expression.Integer _ ->
            []

        Expression.Floatable _ ->
            []

        Expression.UnitExpr ->
            []

        Expression.ListExpr elements ->
            elements

        Expression.FunctionOrValue _ _ ->
            []

        Expression.RecordExpr fields ->
            List.map (Node.value >> (\( _, expr ) -> expr)) fields

        Expression.RecordUpdateExpression _ setters ->
            List.map (Node.value >> (\( _, expr ) -> expr)) setters

        Expression.ParenthesizedExpression expr ->
            [ expr ]

        Expression.Operator _ ->
            []

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
            List.map
                (\declaration ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            functionToExpression function

                        Expression.LetDestructuring _ expr ->
                            expr
                )
                declarations
                ++ [ expression ]

        Expression.CaseExpression { expression, cases } ->
            expression
                :: List.map (\( _, caseExpression ) -> caseExpression) cases

        Expression.LambdaExpression { expression } ->
            [ expression ]

        Expression.TupledExpression expressions ->
            expressions

        Expression.PrefixOperator _ ->
            []

        Expression.Hex _ ->
            []

        Expression.Negation expr ->
            [ expr ]

        Expression.CharLiteral _ ->
            []

        Expression.RecordAccess expr _ ->
            [ expr ]

        Expression.RecordAccessFunction _ ->
            []

        Expression.GLSLExpression _ ->
            []


expressionsInDeclaration : Node Declaration -> List (Node Expression)
expressionsInDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            [ functionToExpression function ]

        Declaration.CustomTypeDeclaration _ ->
            []

        Declaration.AliasDeclaration _ ->
            []

        Declaration.Destructuring _ expr ->
            [ expr ]

        Declaration.PortDeclaration _ ->
            []

        Declaration.InfixDeclaration _ ->
            []


visitNodeWithListOfVisitors :
    List (Visitor nodeType moduleContext)
    -> Node nodeType
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitNodeWithListOfVisitors visitors node initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor node))
        initialErrorsAndContext
        visitors


functionToExpression : Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression



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


moduleNameNode : Node Module -> Node ModuleName
moduleNameNode node =
    case Node.value node of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName


getModuleName : ProjectModule -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Module.moduleName


accumulateWithListOfVisitors :
    List (a -> context -> ( List (Error {}), context ))
    -> a
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateWithListOfVisitors visitors element initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor element))
        initialErrorsAndContext
        visitors


accumulateList : (Node a -> context -> ( List (Error {}), context )) -> List (Node a) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulateList visitor nodes initialErrorsAndContext =
    List.foldl
        (\node -> accumulate (visitor node))
        initialErrorsAndContext
        nodes


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulate : (context -> ( List (Error {}), context )) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulate visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( newErrors ++ previousErrors, newContext )
