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
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Review.Context as Context exposing (Context)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Metadata as Metadata
import Review.Project.Dependency
import Review.Rule exposing (CacheEntry, CacheEntryFor, Direction(..), ElmJsonKey(..), Error(..), Forbidden, ModuleKey(..), ModuleRuleResultCache, ModuleVisitorFunctions, ProjectRuleCache, ReadmeKey(..), Required, Rule(..), TraversalType(..), Visitor, removeErrorPhantomType)
import Review.Visitor exposing (Folder)


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , moduleVisitors : List (ModuleVisitor {} moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } moduleContext)
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
    ModuleVisitor schemaState moduleContext
    -- TODO Jeroen check if projectContext is necessary
    = ModuleVisitor
        { name : String
        , initialModuleContext : Maybe moduleContext
        , moduleContextCreator : Context () moduleContext
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


withModuleVisitor_New :
    (ModuleVisitor {} moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } moduleContext)
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : () } projectContext moduleContext
    -- TODO BREAKING Change: add hasAtLeastOneVisitor : ()
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
withModuleVisitor_New visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | moduleVisitors = visitor :: schema.moduleVisitors }


newModuleRuleSchema_New : String -> moduleContext -> ModuleVisitor { moduleContext : Required } moduleContext
newModuleRuleSchema_New name initialModuleContext =
    ModuleVisitor
        { name = name
        , initialModuleContext = Just initialModuleContext
        , moduleContextCreator = Context.init (always initialModuleContext)
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


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema_New : ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext -> Rule
fromModuleRuleSchema_New ((ModuleVisitor schema) as moduleVisitor) =
    let
        initialContext : moduleContext
        initialContext =
            case schema.initialModuleContext of
                Just initialModuleContext ->
                    initialModuleContext

                Nothing ->
                    Debug.todo "Define initial module context"

        --elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> moduleContext -> ( List (Error {}), moduleContext ))
        --elmJsonVisitors =
        --    schema.elmJsonVisitors
        projectRule : ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext moduleContext
        projectRule =
            ProjectRuleSchema
                { name = schema.name
                , initialProjectContext = initialContext
                , elmJsonVisitors = [] -- List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
                , readmeVisitors = [] -- (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
                , dependenciesVisitors = [] -- (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
                , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
                , moduleContextCreator = Just (Context.init (always initialContext))
                , folder = Nothing

                -- TODO Jeroen Only allow to set it if there is a folder, but not several times
                , traversalType = AllModulesInParallel
                , finalEvaluationFns = []
                }
    in
    fromProjectRuleSchema projectRule


{-| This function that is supplied by the user will be stored in the `ProjectRuleSchema`,
but it contains an extensible record. This means that `ProjectRuleSchema` will
need an additional type variable for no useful value. Because we have full control
over the `ModuleRuleSchema` in this module, we can change the phantom type to be
whatever we want it to be, and we'll change it something that makes sense but
without the extensible record type variable.
-}
removeExtensibleRecordTypeVariable :
    (ModuleVisitor {} moduleContext -> ModuleVisitor { a | hasAtLeastOneVisitor : () } moduleContext)
    -> (ModuleVisitor {} moduleContext -> ModuleVisitor { hasAtLeastOneVisitor : () } moduleContext)
removeExtensibleRecordTypeVariable function =
    function >> (\(ModuleVisitor param) -> ModuleVisitor param)



--runModuleRule_New
--    (reverseVisitors_New moduleVisitor)
--    Nothing
--    |> Rule schema.name Exceptions.init
--fromProjectRuleSchema : ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
--fromProjectRuleSchema ((ProjectRuleSchema schema) as projectRuleSchema) =
--    Rule schema.name
--        Exceptions.init
--        (Review.Visitor.run (fromProjectRuleSchemaToRunnableProjectVisitor projectRuleSchema) Nothing)
--fromProjectRuleSchemaToRunnableProjectVisitor : ProjectRuleSchema schemaState projectContext moduleContext -> Review.Visitor.RunnableProjectVisitor projectContext moduleContext
--fromProjectRuleSchemaToRunnableProjectVisitor (ProjectRuleSchema schema) =
--    { name = schema.name
--    , initialProjectContext = schema.initialProjectContext
--    , elmJsonVisitors = List.reverse schema.elmJsonVisitors
--    , readmeVisitors = List.reverse schema.readmeVisitors
--    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
--    , moduleVisitor = mergeModuleVisitors schema.name schema.moduleContextCreator schema.moduleVisitors
--    , folder = schema.folder
--
--    -- TODO Jeroen Only allow to set it if there is a folder, but not several times
--    , traversalType = schema.traversalType
--    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
--    }


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
    , moduleVisitor = mergeModuleVisitors schema.initialProjectContext schema.moduleContextCreator schema.moduleVisitors
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
    projectContext
    -> Maybe (Context projectContext moduleContext)
    -> List (ModuleVisitor schemaState1 moduleContext -> ModuleVisitor schemaState2 moduleContext)
    -> Maybe ( Review.Visitor.RunnableModuleVisitor moduleContext, Context projectContext moduleContext )
mergeModuleVisitors initialProjectContext maybeModuleContextCreator visitors =
    case ( maybeModuleContextCreator, List.isEmpty visitors ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, True ) ->
            Nothing

        ( Just moduleContextCreator, False ) ->
            let
                dummyAvailableData : Context.AvailableData
                dummyAvailableData =
                    { metadata = Metadata.create { moduleNameNode = Node.Node Range.emptyRange [] }
                    , moduleKey = ModuleKey "dummy"
                    }

                initialModuleContext : moduleContext
                initialModuleContext =
                    Context.apply dummyAvailableData moduleContextCreator initialProjectContext

                emptyModuleVisitor : ModuleVisitor schemaState moduleContext
                emptyModuleVisitor =
                    ModuleVisitor
                        { name = ""
                        , initialModuleContext = Just initialModuleContext
                        , moduleContextCreator = Context.init (always initialModuleContext)
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
            in
            Just
                ( List.foldl
                    (\addVisitors (ModuleVisitor moduleVisitorSchema) ->
                        addVisitors (ModuleVisitor moduleVisitorSchema)
                    )
                    emptyModuleVisitor
                    visitors
                    |> fromModuleRuleSchemaToRunnableModuleVisitor
                , moduleContextCreator
                )


fromModuleRuleSchemaToRunnableModuleVisitor : ModuleVisitor schemaState moduleContext -> Review.Visitor.RunnableModuleVisitor moduleContext
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


withSimpleModuleDefinitionVisitor_New : (Node Module -> List (Error {})) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleModuleDefinitionVisitor_New visitor schema =
    withModuleDefinitionVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withModuleDefinitionVisitor_New : (Node Module -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withModuleDefinitionVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


withSimpleCommentsVisitor_New : (List (Node String) -> List (Error {})) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleCommentsVisitor_New visitor schema =
    withCommentsVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withCommentsVisitor_New : (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCommentsVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | commentsVisitors = visitor :: schema.commentsVisitors }


withSimpleImportVisitor_New : (Node Import -> List (Error {})) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleImportVisitor_New visitor schema =
    withImportVisitor_New (\node moduleContext -> ( visitor node, moduleContext )) schema


withImportVisitor_New : (Node Import -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withImportVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | importVisitors = visitor :: schema.importVisitors }


withSimpleDeclarationVisitor_New : (Node Declaration -> List (Error {})) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleDeclarationVisitor_New visitor schema =
    withDeclarationEnterVisitor_New
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withDeclarationVisitor_New : (Node Declaration -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor
        { schema
            | declarationVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.declarationVisitorsOnEnter
            , declarationVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.declarationVisitorsOnExit
        }


withDeclarationEnterVisitor_New : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationEnterVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationVisitorsOnEnter = visitor :: schema.declarationVisitorsOnEnter }


withDeclarationExitVisitor_New : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationExitVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationVisitorsOnExit = visitor :: schema.declarationVisitorsOnExit }


withDeclarationListVisitor_New : (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationListVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


withSimpleExpressionVisitor_New : (Node Expression -> List (Error {})) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleExpressionVisitor_New visitor schema =
    withExpressionEnterVisitor_New
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withExpressionVisitor_New : (Node Expression -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor
        { schema
            | expressionVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.expressionVisitorsOnEnter
            , expressionVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.expressionVisitorsOnExit
        }


withExpressionEnterVisitor_New : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionEnterVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | expressionVisitorsOnEnter = visitor :: schema.expressionVisitorsOnEnter }


withExpressionExitVisitor_New : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleVisitor schemaState moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionExitVisitor_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | expressionVisitorsOnExit = visitor :: schema.expressionVisitorsOnExit }


withFinalModuleEvaluation_New : (moduleContext -> List (Error {})) -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext -> ModuleVisitor { schemaState | hasAtLeastOneVisitor : () } moduleContext
withFinalModuleEvaluation_New visitor (ModuleVisitor schema) =
    ModuleVisitor { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }
