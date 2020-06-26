module Review.Rule3 exposing
    ( ModuleRuleSchema
    , ProjectRuleSchema
    , fromModuleRuleSchema
    , fromProjectRuleSchema
    , newModuleRuleSchema
    , newProjectRuleSchema
    , review
    , withCommentsVisitor
    , withContextFromImportedModules
    , withDeclarationEnterVisitor
    , withDeclarationExitVisitor
    , withDeclarationListVisitor
    , withDeclarationVisitor
    , withDependenciesModuleVisitor
    , withDependenciesProjectVisitor
    , withElmJsonModuleVisitor
    , withElmJsonProjectVisitor
    , withExpressionEnterVisitor
    , withExpressionExitVisitor
    , withExpressionVisitor
    , withFinalModuleEvaluation
    , withFinalProjectEvaluation
    , withImportVisitor
    , withModuleContext
    , withModuleDefinitionVisitor
    , withModuleVisitor
    , withReadmeModuleVisitor
    , withReadmeProjectVisitor
    , withSimpleCommentsVisitor
    , withSimpleDeclarationVisitor
    , withSimpleExpressionVisitor
    , withSimpleImportVisitor
    , withSimpleModuleDefinitionVisitor
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
import Review.Error exposing (ReviewError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Metadata as Metadata
import Review.Project exposing (Project)
import Review.Project.Dependency
import Review.Project.Internal
import Review.Rule
    exposing
        ( Direction(..)
        , ElmJsonKey(..)
        , Error(..)
        , Forbidden
        , ModuleKey(..)
        , ReadmeKey(..)
        , Required
        , Rule(..)
        , TraversalType(..)
        , duplicateModuleNames
        , errorToReviewError
        , parsingError
        , removeErrorPhantomType
        )
import Review.Visitor exposing (Folder, Visitor)
import Vendor.Graph as Graph


type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , moduleVisitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        , moduleContextCreator : Maybe (Context projectContext moduleContext)
        , folder : Maybe (Folder projectContext moduleContext)

        -- TODO Jeroen Only allow to set it if there is a folder, but not several times
        , traversalType : TraversalType

        -- TODO Jeroen Only allow to set it if there is a folder and module visitors?
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


type ModuleRuleSchema schemaState moduleContext
    = ModuleRuleSchema
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

        -- Project visitors
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> moduleContext -> moduleContext)
        , readmeVisitors : List (Maybe String -> moduleContext -> moduleContext)
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
        }


withModuleVisitor :
    (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : () } projectContext moduleContext
    -- TODO BREAKING Change: add hasAtLeastOneVisitor : ()
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
withModuleVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | moduleVisitors = visitor :: schema.moduleVisitors }


newModuleRuleSchema : String -> moduleContext -> ModuleRuleSchema { canCollectProjectData : () } moduleContext
newModuleRuleSchema name initialModuleContext =
    ModuleRuleSchema
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
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
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


withContextFromImportedModules : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleSchema schemaState projectContext moduleContext
withContextFromImportedModules (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | traversalType = ImportedModulesFirst }


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema : ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> Rule
fromModuleRuleSchema ((ModuleRuleSchema schema) as moduleVisitor) =
    -- TODO BREAKING CHANGE Add canCollectData as a pre-requisite to using fromModuleRuleSchema
    ProjectRuleSchema
        { name = schema.name
        , initialProjectContext = getInitialContextFromModuleRule moduleVisitor
        , elmJsonVisitors = compactProjectDataVisitors (Maybe.map .project) schema.elmJsonVisitors
        , readmeVisitors = compactProjectDataVisitors (Maybe.map .content) schema.readmeVisitors
        , dependenciesVisitors = compactProjectDataVisitors identity schema.dependenciesVisitors
        , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
        , moduleContextCreator = Just (Context.init identity)
        , folder = Nothing
        , traversalType = AllModulesInParallel
        , finalEvaluationFns = []
        }
        |> fromProjectRuleSchema


getInitialContextFromModuleRule : ModuleRuleSchema schemaState moduleContext -> moduleContext
getInitialContextFromModuleRule ((ModuleRuleSchema schema) as moduleVisitor) =
    case schema.initialModuleContext of
        Just initialModuleContext ->
            initialModuleContext

        Nothing ->
            -- Hack: For module rules, we know we will always have a module context
            -- I am adding a `|>` to prevent TCO from kicking in, so that people get a runtime crash and an report it
            -- rather than a mysterious.
            -- Note: People can call this rule on a module rule built in `withModuleVisitor`, which we will prevent
            -- in the next major version
            moduleVisitor |> getInitialContextFromModuleRule


compactProjectDataVisitors : (rawData -> data) -> List (data -> moduleContext -> moduleContext) -> List (rawData -> moduleContext -> ( List nothing, moduleContext ))
compactProjectDataVisitors getData visitors =
    if List.isEmpty visitors then
        []

    else
        [ \rawData moduleContext ->
            let
                data : data
                data =
                    getData rawData
            in
            ( []
            , List.foldl
                (\visitor moduleContext_ -> visitor data moduleContext_)
                moduleContext
                (List.reverse visitors)
            )
        ]


{-| This function that is supplied by the user will be stored in the `ProjectRuleSchema`,
but it contains an extensible record. This means that `ProjectRuleSchema` will
need an additional type variable for no useful value. Because we have full control
over the `ModuleRuleSchema` in this module, we can change the phantom type to be
whatever we want it to be, and we'll change it something that makes sense but
without the extensible record type variable.
-}
removeExtensibleRecordTypeVariable :
    (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { a | hasAtLeastOneVisitor : () } moduleContext)
    -> (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
removeExtensibleRecordTypeVariable function =
    function >> (\(ModuleRuleSchema param) -> ModuleRuleSchema param)


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the project's
[`elm.json`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project) file.

The following example forbids exposing a module in an "Internal" directory in your `elm.json` file.

    import Elm.Module
    import Elm.Project
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    type alias Context =
        Maybe Elm.Project.Project

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "DoNoExposeInternalModules" Nothing
            |> Rule.withElmJsonModuleVisitor elmJsonVisitor
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromModuleRuleSchema

    elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
    elmJsonVisitor elmJson context =
        elmJson

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
    moduleDefinitionVisitor node context =
        let
            moduleName : List String
            moduleName =
                Node.value node |> Module.moduleName
        in
        if List.member "Internal" moduleName then
            case context of
                Just (Elm.Project.Package { exposed }) ->
                    let
                        exposedModules : List String
                        exposedModules =
                            case exposed of
                                Elm.Project.ExposedList names ->
                                    names
                                        |> List.map Elm.Module.toString

                                Elm.Project.ExposedDict fakeDict ->
                                    fakeDict
                                        |> List.concatMap Tuple.second
                                        |> List.map Elm.Module.toString
                    in
                    if List.member (String.join "." moduleName) exposedModules then
                        ( [ Rule.error "Do not expose modules in `Internal` as part of the public API" (Node.range node) ], context )

                    else
                        ( [], context )

                _ ->
                    ( [], context )

        else
            ( [], context )

-}
withElmJsonModuleVisitor :
    (Maybe Elm.Project.Project -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withElmJsonModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | elmJsonVisitors = visitor :: schema.elmJsonVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit
the project's `README.md` file.
-}
withReadmeModuleVisitor :
    (Maybe String -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withReadmeModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | readmeVisitors = visitor :: schema.readmeVisitors }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will visit the project's
[dependencies](./Review-Project-Dependency).

You can use this look at the modules contained in dependencies, which can make the rule very precise when it targets
specific functions.

-}
withDependenciesModuleVisitor :
    (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withDependenciesModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | dependenciesVisitors = visitor :: schema.dependenciesVisitors }


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
    Rule
        { exceptions = Exceptions.init
        , ruleImplementation = Review.Visitor.run schema.name (fromProjectRuleSchemaToRunnableProjectVisitor projectRuleSchema) Nothing
        }


fromProjectRuleSchemaToRunnableProjectVisitor : ProjectRuleSchema schemaState projectContext moduleContext -> Review.Visitor.RunnableProjectVisitor projectContext moduleContext
fromProjectRuleSchemaToRunnableProjectVisitor (ProjectRuleSchema schema) =
    { initialProjectContext = schema.initialProjectContext
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
                Review.Visitor.TraverseAllModulesInParallel Nothing
    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
    }


mergeModuleVisitors :
    projectContext
    -> Maybe (Context projectContext moduleContext)
    -> List (ModuleRuleSchema schemaState1 moduleContext -> ModuleRuleSchema schemaState2 moduleContext)
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

                emptyModuleVisitor : ModuleRuleSchema schemaState moduleContext
                emptyModuleVisitor =
                    ModuleRuleSchema
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
                        , elmJsonVisitors = []
                        , readmeVisitors = []
                        , dependenciesVisitors = []
                        }
            in
            Just
                ( List.foldl
                    (\addVisitors (ModuleRuleSchema moduleVisitorSchema) ->
                        addVisitors (ModuleRuleSchema moduleVisitorSchema)
                    )
                    emptyModuleVisitor
                    visitors
                    |> fromModuleRuleSchemaToRunnableModuleVisitor
                , moduleContextCreator
                )


fromModuleRuleSchemaToRunnableModuleVisitor : ModuleRuleSchema schemaState moduleContext -> Review.Visitor.RunnableModuleVisitor moduleContext
fromModuleRuleSchemaToRunnableModuleVisitor (ModuleRuleSchema schema) =
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


withSimpleModuleDefinitionVisitor : (Node Module -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleModuleDefinitionVisitor visitor schema =
    withModuleDefinitionVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


withModuleDefinitionVisitor : (Node Module -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withModuleDefinitionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


withSimpleCommentsVisitor : (List (Node String) -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleCommentsVisitor visitor schema =
    withCommentsVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


withCommentsVisitor : (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCommentsVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | commentsVisitors = visitor :: schema.commentsVisitors }


withSimpleImportVisitor : (Node Import -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleImportVisitor visitor schema =
    withImportVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


withImportVisitor : (Node Import -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withImportVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | importVisitors = visitor :: schema.importVisitors }


withSimpleDeclarationVisitor : (Node Declaration -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleDeclarationVisitor visitor schema =
    withDeclarationEnterVisitor
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withDeclarationVisitor : (Node Declaration -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | declarationVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.declarationVisitorsOnEnter
            , declarationVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.declarationVisitorsOnExit
        }


withDeclarationEnterVisitor : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationVisitorsOnEnter = visitor :: schema.declarationVisitorsOnEnter }


withDeclarationExitVisitor : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationVisitorsOnExit = visitor :: schema.declarationVisitorsOnExit }


withDeclarationListVisitor : (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationListVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


withSimpleExpressionVisitor : (Node Expression -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleExpressionVisitor visitor schema =
    withExpressionEnterVisitor
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


withExpressionVisitor : (Node Expression -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | expressionVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.expressionVisitorsOnEnter
            , expressionVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.expressionVisitorsOnExit
        }


withExpressionEnterVisitor : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | expressionVisitorsOnEnter = visitor :: schema.expressionVisitorsOnEnter }


withExpressionExitVisitor : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | expressionVisitorsOnExit = visitor :: schema.expressionVisitorsOnExit }


withFinalModuleEvaluation : (moduleContext -> List (Error {})) -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withFinalModuleEvaluation visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }



-- REVIEWING


{-| Review a project and gives back the errors raised by the given rules.

Note that you won't need to use this function when writing a rule. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review.Project as Project exposing (Project, ProjectModule)
    import Review.Rule as Rule exposing (Rule)

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new
            |> Project.addModule { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
            |> Project.addModule { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }

    doReview =
        let
            ( errors, rulesWithCachedValues ) =
                Rule.review rules project
        in
        doSomethingWithTheseValues

The resulting `List Rule` is the same list of rules given as input, but with an
updated internal cache to make it faster to re-run the rules on the same project.
If you plan on re-reviewing with the same rules and project, for instance to
review the project after a file has changed, you may want to store the rules in
your `Model`.

The rules are functions, so doing so will make your model unable to be
exported/imported with `elm/browser`'s debugger, and may cause a crash if you try
to compare them or the model that holds them.

-}
review : List Rule -> Project -> ( List ReviewError, List Rule )
review rules project =
    case Review.Project.modulesThatFailedToParse project of
        [] ->
            case Review.Project.modules project |> duplicateModuleNames Dict.empty of
                Just duplicate ->
                    let
                        paths : String
                        paths =
                            duplicate.paths
                                |> List.sort
                                |> List.map (\s -> "\n  - " ++ s)
                                |> String.join ""

                        moduleNames : String
                        moduleNames =
                            String.join "." duplicate.moduleName
                    in
                    ( [ Review.Error.ReviewError
                            { filePath = "GLOBAL ERROR"
                            , ruleName = "Incorrect project"
                            , message = "Found several modules named `" ++ moduleNames ++ "`"
                            , details =
                                [ "I found several modules with the name `" ++ moduleNames ++ "`. Depending on how I choose to resolve this, I might give you different reports. Since this is a compiler error anyway, I require this problem to be solved. Please fix this then try running `elm-review` again."
                                , "Here are the paths to some of the files that share a module name:" ++ paths
                                , "It is possible that you requested me to look at several projects, and that modules from each project share the same name. I don't recommend reviewing several projects at the same time, as I can only handle one `elm.json`. I instead suggest running `elm-review` twice, once for each project."
                                ]
                            , range = { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                            , fixes = Nothing
                            , target = Review.Error.Global
                            }
                      ]
                    , rules
                    )

                Nothing ->
                    let
                        sortedModules : Result (Graph.Edge ()) (List (Graph.NodeContext ModuleName ()))
                        sortedModules =
                            project
                                |> Review.Project.Internal.moduleGraph
                                |> Graph.checkAcyclic
                                |> Result.map Graph.topologicalSort
                    in
                    case sortedModules of
                        Err _ ->
                            ( [ Review.Error.ReviewError
                                    { filePath = "GLOBAL ERROR"
                                    , ruleName = "Incorrect project"
                                    , message = "Import cycle discovered"
                                    , details =
                                        [ "I detected an import cycle in your project. This prevents me from working correctly, and results in a error for the Elm compiler anyway. Please resolve it using the compiler's suggestions, then try running `elm-review` again."
                                        ]
                                    , range = { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                                    , fixes = Nothing
                                    , target = Review.Error.Global
                                    }
                              ]
                            , rules
                            )

                        Ok nodeContexts ->
                            runRules rules project nodeContexts
                                |> Tuple.mapFirst (List.map errorToReviewError)

        modulesThatFailedToParse ->
            ( List.map parsingError modulesThatFailedToParse, rules )


runRules : List Rule -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), List Rule )
runRules rules project nodeContexts =
    List.foldl
        (\(Rule { exceptions, ruleImplementation }) ( errors, previousRules ) ->
            let
                ( ruleErrors, ruleWithCache ) =
                    ruleImplementation exceptions project nodeContexts
            in
            ( List.concat [ List.map removeErrorPhantomType ruleErrors, errors ], ruleWithCache :: previousRules )
        )
        ( [], [] )
        rules
