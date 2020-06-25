module Review.Rule3 exposing
    ( ModuleVisitor
    , ProjectRuleSchema
    , fromModuleRuleSchema_New
    , fromProjectRuleSchema
    , newModuleRuleSchema_New
    , newProjectRuleSchema
    , withCommentsVisitor_New
    , withContextFromImportedModules_New
    , withDeclarationEnterVisitor_New
    , withDeclarationExitVisitor_New
    , withDeclarationListVisitor_New
    , withDeclarationVisitor_New
    , withDependenciesProjectVisitor
    , withElmJsonProjectVisitor
    , withExpressionEnterVisitor_New
    , withExpressionExitVisitor_New
    , withExpressionVisitor_New
    , withFinalModuleEvaluation_New
    , withFinalProjectEvaluation
    , withImportVisitor_New
    , withModuleContext
    , withModuleContextCreator_New
    , withModuleDefinitionVisitor_New
    , withModuleVisitor_New
    , withReadmeProjectVisitor
    , withSimpleCommentsVisitor_New
    , withSimpleDeclarationVisitor_New
    , withSimpleExpressionVisitor_New
    , withSimpleImportVisitor_New
    , withSimpleModuleDefinitionVisitor_New
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
import Review.Rule exposing (CacheEntry, CacheEntryFor, Direction(..), ElmJsonKey(..), Error(..), Forbidden, ModuleKey(..), ModuleRuleResultCache, ModuleVisitorFunctions, ProjectRuleCache, ReadmeKey(..), Required, Rule(..), TraversalType(..), Visitor, accumulateList, accumulateWithListOfVisitors, makeFinalEvaluation, moduleNameNode, removeErrorPhantomType, setFilePathIfUnset, setRuleName, visitDeclaration, visitImport)
import Review.Visitor exposing (Folder)
import Vendor.Graph as Graph exposing (Graph)


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , moduleVisitors : List (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)
        , moduleContextCreator : Maybe (Context projectContext moduleContext)
        , folder : Maybe (Folder projectContext moduleContext)

        -- TODO Jeroen Only allow to set it if there is a folder, but not several times
        , traversalType : TraversalType
        , finalEvaluationFns : List (projectContext -> List (Error {}))
        }


newProjectRuleSchema : String -> projectContext -> ProjectRuleSchema { canAddModuleVisitor : (), withModuleContext : Forbidden } projectContext moduleContext
newProjectRuleSchema name initialProjectContext =
    ProjectRuleSchema
        { name = name
        , initialProjectContext = initialProjectContext
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        , moduleVisitors = []
        , moduleContextCreator = Nothing
        , folder = Nothing
        , traversalType = AllModulesInParallel
        , finalEvaluationFns = []
        }


type alias ModuleContextFunctions projectContext moduleContext =
    { fromProjectToModule : ModuleKey -> Node ModuleName -> projectContext -> moduleContext
    , fromModuleToProject : ModuleKey -> Node ModuleName -> moduleContext -> projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
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
    emptyModuleVisitor2 name (Context.init (always moduleContext))


emptyModuleVisitor2 : String -> Context projectContext moduleContext -> ModuleVisitor schemaState projectContext moduleContext
emptyModuleVisitor2 name moduleContextCreator =
    ModuleVisitor
        { name = name
        , moduleContextCreator = moduleContextCreator
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


withModuleVisitor_New :
    (ModuleVisitor {} projectContext moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } projectContext moduleContext)
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : () } projectContext moduleContext
    -- TODO BREAKING Change: add hasAtLeastOneVisitor : ()
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
withModuleVisitor_New visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | moduleVisitors = visitor :: schema.moduleVisitors }


newModuleRuleSchema_New : String -> moduleContext -> ModuleVisitor { moduleContext : Required } () moduleContext
newModuleRuleSchema_New name moduleContext =
    emptyModuleVisitor name moduleContext


withModuleContext :
    { fromProjectToModule : ModuleKey -> Node ModuleName -> projectContext -> moduleContext
    , fromModuleToProject : ModuleKey -> Node ModuleName -> moduleContext -> projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }
    -> ProjectRuleSchema { schemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : (), withModuleContext : Forbidden } projectContext moduleContext
withModuleContext functions (ProjectRuleSchema schema) =
    let
        moduleContextCreator : Context projectContext moduleContext
        moduleContextCreator =
            Context.init
                (\moduleKey metadata projectContext ->
                    functions.fromProjectToModule
                        moduleKey
                        (Metadata.moduleNameNode metadata)
                        projectContext
                )
                |> Context.withModuleKey
                |> Context.withMetadata
    in
    ProjectRuleSchema
        { schema
            | moduleContextCreator = Just moduleContextCreator
            , folder =
                Just
                    { fromModuleToProject =
                        Context.init (\moduleKey metadata moduleContext -> functions.fromModuleToProject moduleKey (Metadata.moduleNameNode metadata) moduleContext)
                            |> Context.withModuleKey
                            |> Context.withMetadata
                    , foldProjectContexts = functions.foldProjectContexts
                    }
        }


withContextFromImportedModules_New : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleSchema schemaState projectContext moduleContext
withContextFromImportedModules_New (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | traversalType = ImportedModulesFirst }


{-| TODO Jeroen check if this is used. If not, use this name for setting the module context creator on project rules
-}
withModuleContextCreator_New : Context () moduleContext -> ModuleVisitor { schema | moduleContext : Required } () moduleContext -> ModuleVisitor { schema | moduleContext : Forbidden } () moduleContext
withModuleContextCreator_New moduleContextCreator (ModuleVisitor moduleVisitor) =
    ModuleVisitor { moduleVisitor | moduleContextCreator = moduleContextCreator }


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema_New : ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } () moduleContext -> Rule
fromModuleRuleSchema_New ((ModuleVisitor { name }) as schema) =
    runModuleRule_New
        (reverseVisitors_New schema)
        Nothing
        |> Rule name Exceptions.init


reverseVisitors_New : ModuleVisitor schemaState projectContext moduleContext -> ModuleVisitor schemaState projectContext moduleContext
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

        moduleResults : ModuleRuleResultCache
        moduleResults =
            List.foldl
                (\module_ cache ->
                    if (Dict.get module_.path cache |> Maybe.map .source) == Just module_.source then
                        -- Module is unchanged, take what was in the cache already
                        cache

                    else
                        let
                            availableData : Context.AvailableData
                            availableData =
                                { metadata = Metadata.create { moduleNameNode = moduleNameNode module_.ast.moduleDefinition }
                                , moduleKey = ModuleKey module_.path
                                }
                        in
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
computeErrors_New (ModuleVisitor schema) availableData module_ =
    let
        initialContext : moduleContext
        initialContext =
            Context.apply availableData schema.moduleContextCreator ()
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
    -- TODO BREAKING CHANGE, Rename to withElmJsonVisitor
    ProjectRuleSchema { projectRuleSchema | elmJsonVisitors = visitor :: projectRuleSchema.elmJsonVisitors }


withReadmeProjectVisitor :
    (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withReadmeProjectVisitor visitor (ProjectRuleSchema projectRuleSchema) =
    -- TODO BREAKING CHANGE, Rename to withReadmeVisitor
    ProjectRuleSchema { projectRuleSchema | readmeVisitors = visitor :: projectRuleSchema.readmeVisitors }


withDependenciesProjectVisitor :
    (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDependenciesProjectVisitor visitor (ProjectRuleSchema projectRuleSchema) =
    -- TODO BREAKING CHANGE, Rename to withDependenciesVisitor
    ProjectRuleSchema { projectRuleSchema | dependenciesVisitors = visitor :: projectRuleSchema.dependenciesVisitors }


withFinalProjectEvaluation :
    (projectContext -> List (Error { useErrorForModule : () }))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withFinalProjectEvaluation visitor (ProjectRuleSchema projectRuleSchema) =
    let
        removeErrorPhantomTypeFromEvaluation : (projectContext -> List (Error b)) -> (projectContext -> List (Error {}))
        removeErrorPhantomTypeFromEvaluation function projectContext =
            function projectContext
                |> List.map removeErrorPhantomType
    in
    ProjectRuleSchema { projectRuleSchema | finalEvaluationFns = removeErrorPhantomTypeFromEvaluation visitor :: projectRuleSchema.finalEvaluationFns }


fromProjectRuleSchema : ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
fromProjectRuleSchema ((ProjectRuleSchema schema) as projectRuleSchema) =
    Rule schema.name
        Exceptions.init
        (Review.Visitor.run (fromProjectRuleSchemaToRunnableProjectVisitor projectRuleSchema) Nothing)


fromProjectRuleSchemaToRunnableProjectVisitor : ProjectRuleSchema schemaState projectContext moduleContext -> Review.Visitor.RunnableProjectVisitor projectContext moduleContext
fromProjectRuleSchemaToRunnableProjectVisitor (ProjectRuleSchema schema) =
    { name = schema.name
    , initialProjectContext = schema.initialProjectContext
    , elmJsonVisitors = List.reverse schema.elmJsonVisitors
    , readmeVisitors = List.reverse schema.readmeVisitors
    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
    , moduleVisitor = mergeModuleVisitors schema.name schema.moduleContextCreator schema.moduleVisitors
    , traversalAndFolder =
        case ( schema.traversalType, schema.folder ) of
            ( AllModulesInParallel, _ ) ->
                Review.Visitor.TraverseAllModulesInParallel schema.folder

            ( ImportedModulesFirst, Just folder ) ->
                Review.Visitor.TraverseImportedModulesFirst folder

            ( ImportedModulesFirst, Nothing ) ->
                -- TODO Jeroen Only allow to set it if there is a folder, but not several times
                Review.Visitor.TraverseAllModulesInParallel Nothing
    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
    }


mergeModuleVisitors :
    String
    -> Maybe (Context projectContext moduleContext)
    -> List (ModuleVisitor schemaState1 projectContext moduleContext -> ModuleVisitor schemaState2 projectContext moduleContext)
    -> Maybe ( Review.Visitor.RunnableModuleVisitor moduleContext, Context projectContext moduleContext )
mergeModuleVisitors name maybeModuleContextCreator visitors =
    case ( maybeModuleContextCreator, List.isEmpty visitors ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, True ) ->
            Nothing

        ( Just moduleContextCreator, False ) ->
            Just
                ( List.foldl
                    (\addVisitors (ModuleVisitor moduleVisitorSchema) ->
                        addVisitors (ModuleVisitor moduleVisitorSchema)
                    )
                    (emptyModuleVisitor2 name moduleContextCreator)
                    visitors
                    |> fromModuleRuleSchemaToRunnableModuleVisitor
                , moduleContextCreator
                )


fromModuleRuleSchemaToRunnableModuleVisitor : ModuleVisitor schemaState projectContext moduleContext -> Review.Visitor.RunnableModuleVisitor moduleContext
fromModuleRuleSchemaToRunnableModuleVisitor (ModuleVisitor schema) =
    --
    --    { name = name
    --        , moduleContextCreator = moduleContextCreator
    --        , moduleDefinitionVisitors = []
    --        , commentsVisitors = []
    --        , importVisitors = []
    --        , declarationListVisitors = []
    --        , declarationVisitorsOnEnter = []
    --        , declarationVisitorsOnExit = []
    --        , expressionVisitorsOnEnter = []
    --        , expressionVisitorsOnExit = []
    --        , finalEvaluationFns = []
    --        }
    { moduleDefinitionVisitors = List.reverse schema.moduleDefinitionVisitors
    , commentsVisitors = List.reverse schema.commentsVisitors
    , importVisitors = List.reverse schema.importVisitors
    , declarationListVisitors = List.reverse schema.declarationListVisitors
    , declarationVisitorsOnEnter = List.reverse schema.declarationVisitorsOnEnter
    , declarationVisitorsOnExit = schema.declarationVisitorsOnExit
    , expressionVisitorsOnEnter = List.reverse schema.expressionVisitorsOnEnter
    , expressionVisitorsOnExit = schema.expressionVisitorsOnExit
    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
    }


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
