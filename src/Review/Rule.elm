module Review.Rule exposing
    ( Rule
    , ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema
    , withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , newModuleRuleSchemaUsingContextCreator
    , withModuleDefinitionVisitor
    , withCommentsVisitor
    , withImportVisitor
    , Direction(..), withDeclarationEnterVisitor, withDeclarationExitVisitor, withDeclarationVisitor, withDeclarationListVisitor
    , withExpressionEnterVisitor, withExpressionExitVisitor, withExpressionVisitor
    , withFinalModuleEvaluation
    , withElmJsonModuleVisitor, withReadmeModuleVisitor, withDependenciesModuleVisitor
    , ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules
    , ContextCreator, Metadata, initContextCreator, isInSourceDirectories, moduleNameFromMetadata, moduleNameNodeFromMetadata, withMetadata, withModuleNameLookupTable, withModuleKey
    , Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, errorForElmJsonWithFix, ReadmeKey, errorForReadme, errorForReadmeWithFix
    , globalError, configurationError
    , ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath, errorTarget
    , ignoreErrorsForDirectories, ignoreErrorsForFiles
    , review, reviewV2, ProjectData, ruleName, getConfigurationError
    , Required, Forbidden
    )

{-| This module contains functions that are used for writing rules.

**NOTE**: If you want to **create a package** containing `elm-review` rules, I highly recommend using the
[CLI's](https://github.com/jfmengels/node-elm-review/) `elm-review new-package` subcommand. This will create a new package that will help you use the best practices and give you helpful tools like easy auto-publishing. More information is available in the maintenance file generated along with it.

If you want to **add/create a rule** for the package or for your local configuration, then I recommend using `elm-review new-rule`, which will create a source and test file which you can use as a starting point. For packages, it will add the rule everywhere it should be present (`exposed-modules`, README, ...).


# How does it work?

`elm-review` reads the modules, `elm.json`, dependencies and `README.md` from your project,
and turns each module into an [Abstract Syntax Tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree),
a tree-like structure which represents your source code, using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/).

`elm-review` then feeds all this data into `review rules`, that traverse them to report problems.
The way that review rules go through the data depends on whether it is a [module rule](#creating-a-module-rule) or a [project rule](#creating-a-project-rule).

`elm-review` relies on the [`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/),
and all the node types you'll see will be coming from there. You are likely to
need to have the documentation for that package open when writing a rule.

There are plenty of examples in this documentation, and you can also look at the
source code of existing rules to better grasp how they work.

**NOTE**: These examples are only here to showcase how to write rules and how a function can
be used. They are not necessarily good rules to enforce. See the
[section on whether to write a rule](./#when-to-write-or-enable-a-rule) for more on that.
Even if you think they are good ideas to enforce, they are often not complete, as there are other
patterns you would want to forbid, but that are not handled by the example.


# What makes a good rule

Apart from the rationale on [whether a rule should be written](./#when-to-write-or-enable-a-rule),
here are a few tips on what makes a rule helpful.

A review rule is an automated communication tool which sends messages to
developers who have written patterns your rule wishes to prevent. As all
communication, the message is important.


## A good rule name

The name of the rule (`NoUnusedVariables`, `NoDebug`, ...) should try to convey
really quickly what kind of pattern we're dealing with. Ideally, a user who
encounters this pattern for the first time could guess the problem just from the
name. And a user who encountered it several times should know how to fix the
problem just from the name too.

I recommend having the name of the module containing the rule be the same as the
rule name. This will make it easier to find the module in the project or on
the packages website when trying to get more information.


## A helpful error message and details

The error message should give more information about the problem. It is split
into two parts:

  - The `message`: A short sentence that describes the forbidden pattern. A user
    that has encountered this error multiple times should know exactly what to do.
    Example: "Function `foo` is never used". With this information, a user who
    knows the rule probably knows that a function needs to be removed from the
    source code, and also knows which one.
  - The `details`: All the additional information that can be useful to the
    user, such as the rationale behind forbidding the pattern, and suggestions
    for a solution or alternative.

When writing the error message that the user will see, try to make them be as
helpful as the messages the compiler gives you when it encounters a problem.


## The smallest section of code that makes sense

When creating an error, you need to specify under which section of the code this
message appears. This is where you would see squiggly lines in your editor when
you have review or compiler errors.

To make the error easier to spot, it is best to make this section as small as
possible, as long as that makes sense. For instance, in a rule that would forbid
`Debug.log`, you would the error to appear under `Debug.log`, not on the whole
function which contains this piece of code.


## Good rule documentation

The rule documentation should give the same information as what you would see in
the error message.

If published in a package, the rule documentation should explain when not to
enable the rule in the user's review configuration. For instance, for a rule that
makes sure that a package is publishable by ensuring that all docs are valid,
the rule might say something along the lines of "If you are writing an
application, then you should not use this rule".

Additionally, it could give a few examples of patterns that will be reported and
of patterns that will not be reported, so that users can have a better grasp of
what to expect.


# Strategies for writing rules effectively


## Use Test-Driven Development

This package comes with [`Review.Test`](./Review-Test), which works with [`elm-test`](https://github.com/elm-explorations/test).
I recommend reading through [`the strategies for effective testing`](./Review-Test#strategies-for-effective-testing) before
starting writing a rule.


## Look at the documentation for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/)

`elm-review` is heavily dependent on the types that [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/)
provides. If you don't understand the AST it provides, you will have a hard time
implementing the rule you wish to create.


# Writing a Rule

@docs Rule


## Creating a module rule

A "module rule" looks at modules (i.e. files) one by one. When it finishes looking at a module and reporting errors,
it forgets everything about the module it just analyzed before starting to look at a different module. You should create one of these if you
do not need to know the contents of a different module in the project, such as what functions are exposed.
If you do need that information, you should create a [project rule](#creating-a-project-rule).

If you are new to writing rules, I would recommend learning how to build a module rule first, as they are in practice a
simpler version of project rules.

The traversal of a module rule is the following:

  - Read project-related info (only collect data in the context in these steps)
      - The `elm.json` file, visited by [`withElmJsonModuleVisitor`](#withElmJsonModuleVisitor)
      - The `README.md` file, visited by [`withReadmeModuleVisitor`](#withReadmeModuleVisitor)
      - The definition for dependencies, visited by [`withDependenciesModuleVisitor`](#withDependenciesModuleVisitor)
  - Visit the Elm module (in the following order)
      - The module definition, visited by [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) and [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor)
      - The module's list of comments, visited by [`withSimpleCommentsVisitor`](#withSimpleCommentsVisitor) and [`withCommentsVisitor`](#withCommentsVisitor)
      - Each import, visited by [`withSimpleImportVisitor`](#withSimpleImportVisitor) and [`withImportVisitor`](#withImportVisitor)
      - The list of declarations, visited by [`withDeclarationListVisitor`](#withDeclarationListVisitor)
      - Each declaration, visited by [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor), [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) and [`withDeclarationExitVisitor`](#withDeclarationExitVisitor).
        Before evaluating the next declaration, the expression contained in the declaration
        will be visited recursively by [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor), [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor)
      - A final evaluation is made when the module has fully been visited, using [`withFinalModuleEvaluation`](#withFinalModuleEvaluation)

Evaluating/visiting a node means two things:

  - Detecting patterns and reporting errors
  - Collecting data in a "context" (called `moduleContext` for module rules) to have more information available in a later
    node evaluation. You can only use the context and update it with "non-simple with\*" visitor functions.
    I recommend using the "simple with\*" visitor functions if you don't need to do either, as they are simpler to use

@docs ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema


## Builder functions without context

@docs withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor


## Builder functions with context

@docs newModuleRuleSchemaUsingContextCreator
@docs withModuleDefinitionVisitor
@docs withCommentsVisitor
@docs withImportVisitor
@docs Direction, withDeclarationEnterVisitor, withDeclarationExitVisitor, withDeclarationVisitor, withDeclarationListVisitor
@docs withExpressionEnterVisitor, withExpressionExitVisitor, withExpressionVisitor
@docs withFinalModuleEvaluation


## Builder functions to analyze the project's data

@docs withElmJsonModuleVisitor, withReadmeModuleVisitor, withDependenciesModuleVisitor


## Creating a project rule

Project rules can look at the global picture of an Elm project. Contrary to module
rules, which forget everything about the module they were looking at when going from
one module to another, project rules can retain information about previously
analyzed modules, and use it to report errors when analyzing a different module or
after all modules have been visited.

Project rules can also report errors in the `elm.json` or the `README.md` files.

If you are new to writing rules, I would recommend learning [how to build a module rule](#creating-a-module-rule)
first, as they are in practice a simpler version of project rules.

@docs ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules


## Requesting more information

@docs ContextCreator, Metadata, initContextCreator, isInSourceDirectories, moduleNameFromMetadata, moduleNameNodeFromMetadata, withMetadata, withModuleNameLookupTable, withModuleKey


## Errors

@docs Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, errorForElmJsonWithFix, ReadmeKey, errorForReadme, errorForReadmeWithFix
@docs globalError, configurationError
@docs ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath, errorTarget


## Configuring exceptions

There are situations where you don't want review rules to report errors:

1.  You copied and updated over an external library because one of your needs wasn't met, and you don't want to modify it more than necessary.
2.  Your project contains generated source code, over which you have no control or for which you do not care that some rules are enforced (like the reports of unused variables).
3.  You want to introduce a rule progressively, because there are too many errors in the project for you to fix in one go. You can then ignore the parts of the project where the problem has not yet been solved, and fix them as you go.
4.  You wish to disable some rules for tests files (or enable some only for tests).

You can use the following functions to ignore errors in directories or files.

**NOTE**: Even though they can be used to disable any errors, I **strongly recommend against**
doing so if you are not in the situations listed above. I highly recommend you
leave a comment explaining the reason why you use these functions, or to
communicate with your colleagues if you see them adding exceptions without
reason or seemingly inappropriately.

@docs ignoreErrorsForDirectories, ignoreErrorsForFiles


# Running rules

@docs review, reviewV2, ProjectData, ruleName, getConfigurationError


# Internals

@docs Required, Forbidden

-}

import Ansi
import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing, TopLevelExpose)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
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
import Json.Encode as Encode
import Review.ElmProjectEncoder
import Review.Error exposing (InternalError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal
import Review.Project exposing (ProjectModule)
import Review.Project.Dependency exposing (Dependency)
import Review.Project.Internal exposing (Project(..))
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


{-| Represents a construct able to analyze a project and report
unwanted patterns.

You can create [module rules](#creating-a-module-rule) or [project rules](#creating-a-project-rule).

-}
type Rule
    = Rule
        { name : String
        , exceptions : Exceptions
        , requestedData : RequestedData
        , ruleImplementation : Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
        , configurationError : Maybe { message : String, details : List String }
        }


{-| Represents a schema for a module [`Rule`](#Rule).

Start by using [`newModuleRuleSchema`](#newModuleRuleSchema), then add visitors to look at the parts of the code you are interested in.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

-}
type ModuleRuleSchema schemaState moduleContext
    = ModuleRuleSchema
        { name : String
        , initialModuleContext : Maybe moduleContext
        , moduleContextCreator : ContextCreator () moduleContext
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



-- REVIEWING


{-| **DEPRECATED:** Use [`reviewV2`](#reviewV2) instead.

Review a project and gives back the errors raised by the given rules.

Note that you won't need to use this function when writing a rule. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review.Project as Project exposing (Project)
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
                    ( [ duplicateModulesGlobalError duplicate ]
                    , rules
                    )

                Nothing ->
                    let
                        moduleGraph : Graph (List String) ()
                        moduleGraph =
                            Review.Project.Internal.moduleGraph project

                        sortedModules : Result (Graph.Edge ()) (List (Graph.NodeContext ModuleName ()))
                        sortedModules =
                            moduleGraph
                                |> Graph.checkAcyclic
                                |> Result.map Graph.topologicalSort
                    in
                    case sortedModules of
                        Err edge ->
                            ( importCycleError moduleGraph edge, rules )

                        Ok nodeContexts ->
                            let
                                scopeResult :
                                    { errors : List (Error {})
                                    , rule : Rule
                                    , cache : ProjectRuleCache ScopeProjectContext
                                    , extract : Maybe Extract
                                    }
                                scopeResult =
                                    runProjectVisitor
                                        "DUMMY"
                                        scopeRule
                                        Nothing
                                        Exceptions.init
                                        project
                                        nodeContexts

                                moduleNameLookupTables : Maybe (Dict ModuleName ModuleNameLookupTable)
                                moduleNameLookupTables =
                                    Maybe.map (\(Extract moduleNameLookupTables_) -> moduleNameLookupTables_) scopeResult.extract

                                projectWithLookupTable : Project
                                projectWithLookupTable =
                                    let
                                        (Project p) =
                                            project
                                    in
                                    Project { p | moduleNameLookupTables = moduleNameLookupTables }
                            in
                            if not (List.isEmpty scopeResult.errors) then
                                ( List.map errorToReviewError scopeResult.errors, rules )

                            else
                                runRules rules projectWithLookupTable nodeContexts
                                    |> Tuple.mapFirst (List.map errorToReviewError)

        modulesThatFailedToParse ->
            ( List.map parsingError modulesThatFailedToParse, rules )


{-| Review a project and gives back the errors raised by the given rules.

Note that you won't need to use this function when writing a rule. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review.Project as Project exposing (Project)
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
            { errors, rules, projectData } =
                -- Replace `config` by `rules` next time you call reviewV2
                -- Replace `Nothing` by `projectData` next time you call reviewV2
                Rule.reviewV2 config Nothing project
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
reviewV2 : List Rule -> Maybe ProjectData -> Project -> { errors : List ReviewError, rules : List Rule, projectData : Maybe ProjectData }
reviewV2 rules maybeProjectData project =
    checkForConfigurationErrors rules
        |> Result.andThen (\() -> checkForModulesThatFailedToParse project)
        |> Result.andThen (\() -> checkForDuplicateModules project)
        |> Result.andThen (\() -> getModulesSortedByImport project)
        |> Result.map (runReview project rules maybeProjectData)
        |> Result.mapError
            (\errors ->
                { errors = errors
                , rules = rules
                , projectData = maybeProjectData
                }
            )
        |> extractResultValue


checkForConfigurationErrors : List Rule -> Result (List ReviewError) ()
checkForConfigurationErrors rules =
    let
        errors : List ReviewError
        errors =
            List.filterMap
                (\rule ->
                    Maybe.map
                        (\{ message, details } ->
                            SpecifiedError
                                { filePath = "CONFIGURATION ERROR"
                                , ruleName = ruleName rule
                                , message = message
                                , details = details
                                , range = Range.emptyRange
                                , fixes = Nothing
                                , target = Review.Error.Global
                                }
                                |> errorToReviewError
                        )
                        (getConfigurationError rule)
                )
                rules
    in
    if List.isEmpty errors then
        Ok ()

    else
        Err errors


checkForModulesThatFailedToParse : Project -> Result (List ReviewError) ()
checkForModulesThatFailedToParse project =
    case Review.Project.modulesThatFailedToParse project of
        [] ->
            Ok ()

        modulesThatFailedToParse ->
            Err (List.map parsingError modulesThatFailedToParse)


checkForDuplicateModules : Project -> Result (List ReviewError) ()
checkForDuplicateModules project =
    case Review.Project.modules project |> duplicateModuleNames Dict.empty of
        Just duplicate ->
            Err [ duplicateModulesGlobalError duplicate ]

        Nothing ->
            Ok ()


getModulesSortedByImport : Project -> Result (List Review.Error.ReviewError) (List (Graph.NodeContext ModuleName ()))
getModulesSortedByImport project =
    let
        moduleGraph : Graph (List String) ()
        moduleGraph =
            project
                |> Review.Project.Internal.moduleGraph
    in
    moduleGraph
        |> Graph.checkAcyclic
        |> Result.map Graph.topologicalSort
        |> Result.mapError (importCycleError moduleGraph)


importCycleError : Graph ModuleName e -> Graph.Edge e -> List ReviewError
importCycleError moduleGraph edge =
    let
        cycle : List ModuleName
        cycle =
            findCycle moduleGraph edge
                |> List.reverse
    in
    [ elmReviewGlobalError
        { message = "Your module imports form a cycle"
        , details =
            [ printCycle cycle
            , "Learn more about why this is disallowed and how to break cycles here:<https://elm-lang.org/0.19.1/import-cycles>"
            ]
        }
        |> setRuleName "Incorrect project"
        |> errorToReviewError
    ]


findCycle : Graph n e -> Graph.Edge e -> List n
findCycle graph edge =
    let
        initialCycle : List (Graph.Node n)
        initialCycle =
            Graph.guidedBfs Graph.alongIncomingEdges (visitorDiscoverCycle edge.to) [ edge.from ] [] graph
                |> Tuple.first
    in
    findSmallerCycle graph initialCycle initialCycle
        |> List.map .label


findSmallerCycle : Graph n e -> List (Graph.Node n) -> List (Graph.Node n) -> List (Graph.Node n)
findSmallerCycle graph currentBest nodesToVisit =
    case nodesToVisit of
        [] ->
            currentBest

        startingNode :: restOfNodes ->
            let
                cycle : List (Graph.Node n)
                cycle =
                    Graph.guidedBfs Graph.alongIncomingEdges (visitorDiscoverCycle startingNode.id) [ startingNode.id ] [] graph
                        |> Tuple.first

                newBest : List (Graph.Node n)
                newBest =
                    if List.length cycle > 0 && List.length cycle < List.length currentBest then
                        cycle

                    else
                        currentBest
            in
            if List.length newBest == 1 then
                newBest

            else
                findSmallerCycle graph newBest restOfNodes


reachedTarget : Graph.NodeId -> List (Graph.NodeContext n e) -> Bool
reachedTarget targetNode path =
    Maybe.map (.node >> .id) (List.head path) == Just targetNode


visitorDiscoverCycle : Graph.NodeId -> List (Graph.NodeContext n e) -> Int -> List (Graph.Node n) -> List (Graph.Node n)
visitorDiscoverCycle targetNode path distance acc =
    if List.isEmpty acc then
        -- We haven't found the cycle yet
        if distance == 0 then
            case List.head path of
                Just head ->
                    if IntDict.member head.node.id head.incoming then
                        [ head.node ]

                    else
                        acc

                Nothing ->
                    acc

        else if reachedTarget targetNode path then
            List.map .node path

        else
            []

    else
        -- We already found the cycle
        acc


printCycle : List ModuleName -> String
printCycle moduleNames =
    moduleNames
        |> List.map (String.join "." >> Ansi.yellow)
        |> String.join "\n    │     ↓\n    │    "
        |> wrapInCycle


wrapInCycle : String -> String
wrapInCycle string =
    "    ┌─────┐\n    │    " ++ string ++ "\n    └─────┘"


extractResultValue : Result a a -> a
extractResultValue result =
    case result of
        Ok a ->
            a

        Err a ->
            a


runReview : Project -> List Rule -> Maybe ProjectData -> List (Graph.NodeContext ModuleName ()) -> { errors : List ReviewError, rules : List Rule, projectData : Maybe ProjectData }
runReview ((Project p) as project) rules maybeProjectData nodeContexts =
    let
        scopeResult : { projectData : Maybe ProjectData, extract : Maybe Extract }
        scopeResult =
            if needsToComputeScope rules then
                let
                    { cache, extract } =
                        runProjectVisitor
                            "DUMMY"
                            scopeRule
                            (Maybe.map extractProjectData maybeProjectData)
                            Exceptions.init
                            project
                            nodeContexts
                in
                { projectData = Just (ProjectData cache)
                , extract = extract
                }

            else
                { projectData = Nothing
                , extract = Nothing
                }

        moduleNameLookupTables : Maybe (Dict ModuleName ModuleNameLookupTable)
        moduleNameLookupTables =
            Maybe.map (\(Extract moduleNameLookupTables_) -> moduleNameLookupTables_) scopeResult.extract

        projectWithLookupTables : Project
        projectWithLookupTables =
            Project { p | moduleNameLookupTables = moduleNameLookupTables }
    in
    let
        ( errors, newRules ) =
            runRules rules projectWithLookupTables nodeContexts
    in
    { errors = List.map errorToReviewError errors
    , rules = newRules
    , projectData = scopeResult.projectData
    }


{-| Internal cache about the project.
-}
type ProjectData
    = ProjectData (ProjectRuleCache ScopeProjectContext)


extractProjectData : ProjectData -> ProjectRuleCache ScopeProjectContext
extractProjectData (ProjectData data) =
    data


needsToComputeScope : List Rule -> Bool
needsToComputeScope rules =
    List.any
        (\(Rule { requestedData }) ->
            let
                (RequestedData requestedData_) =
                    requestedData
            in
            requestedData_.moduleNameLookupTable
        )
        rules


duplicateModulesGlobalError : { moduleName : ModuleName, paths : List String } -> ReviewError
duplicateModulesGlobalError duplicate =
    let
        paths : String
        paths =
            duplicate.paths
                |> List.sort
                |> List.map (\s -> "\n  - " ++ s)
                |> String.concat
    in
    elmReviewGlobalError
        { message = "Found several modules named `" ++ String.join "." duplicate.moduleName ++ "`"
        , details =
            [ "I found several modules with the name `" ++ String.join "." duplicate.moduleName ++ "`. Depending on how I choose to resolve this, I might give you different reports. Since this is a compiler error anyway, I require this problem to be solved. Please fix this then try running `elm-review` again."
            , "Here are the paths to some of the files that share a module name:" ++ paths
            , "It is possible that you requested me to look at several projects, and that modules from each project share the same name. I don't recommend reviewing several projects at the same time, as I can only handle one `elm.json`. I instead suggest running `elm-review` twice, once for each project."
            ]
        }
        |> errorToReviewError


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


duplicateModuleNames : Dict ModuleName String -> List ProjectModule -> Maybe { moduleName : ModuleName, paths : List String }
duplicateModuleNames visitedModules projectModules =
    case projectModules of
        [] ->
            Nothing

        projectModule :: restOfModules ->
            let
                moduleName : ModuleName
                moduleName =
                    getModuleName projectModule
            in
            case Dict.get moduleName visitedModules of
                Nothing ->
                    duplicateModuleNames
                        (Dict.insert moduleName projectModule.path visitedModules)
                        restOfModules

                Just path ->
                    Just
                        { moduleName = moduleName
                        , paths =
                            path
                                :: projectModule.path
                                :: (restOfModules
                                        |> List.filter (\p -> getModuleName p == moduleName)
                                        |> List.map .path
                                   )
                        }


{-| Get the name of a rule.

You should not have to use this when writing a rule.

-}
ruleName : Rule -> String
ruleName (Rule rule) =
    rule.name


{-| Get the configuration error for a rule.

You should not have to use this when writing a rule. You might be looking for [`configurationError`](#configurationError) instead.

-}
getConfigurationError : Rule -> Maybe { message : String, details : List String }
getConfigurationError (Rule rule) =
    rule.configurationError


{-| **DEPRECATED**

This is used in [`withDeclarationVisitor`](#withDeclarationVisitor) and [`withDeclarationVisitor`](#withDeclarationVisitor),
which are deprecated and will be removed in the next major version. This type will be removed along with them.

To replicate the same behavior, take a look at

  - [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) and [`withDeclarationExitVisitor`](#withDeclarationExitVisitor).
  - [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor).

**/DEPRECATED**

Represents whether a node is being traversed before having seen its children (`OnEnter`ing the node), or after (`OnExit`ing the node).

When visiting the AST, declaration and expression nodes are visited twice: once
with `OnEnter`, before the children of the node are visited, and once with
`OnExit`, after the children of the node have been visited.
In most cases, you'll only want to handle the `OnEnter` case, but there are cases
where you'll want to visit a [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node)
after having seen its children.

For instance, if you are trying to detect the unused variables defined inside
of a let expression, you will want to collect the declaration of variables,
note which ones are used, and at the end of the block report the ones that weren't used.

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
    expressionVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
                ( [], markVariableAsUsed context name )

            -- Find variables declared in let expression
            ( Rule.OnEnter, Expression.LetExpression letBlock ) ->
                ( [], registerVariables context letBlock )

            -- When exiting the let expression, report the variables that were not used.
            ( Rule.OnExit, Expression.LetExpression _ ) ->
                ( unusedVariables context |> List.map createError, context )

            _ ->
                ( [], context )

-}
type Direction
    = OnEnter
    | OnExit


{-| Creates a schema for a module rule. Will require adding module visitors
calling [`fromModuleRuleSchema`](#fromModuleRuleSchema) to create a usable
[`Rule`](#Rule). Use "with\*" functions from this module, like
[`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) or [`withSimpleImportVisitor`](#withSimpleImportVisitor)
to make it report something.

The first argument is the rule name. I _highly_ recommend naming it just like the
module name (including all the `.` there may be).

The second argument is the initial `moduleContext`, i.e. the data that the rule will
accumulate as the module will be traversed, and allows the rule to know/remember
what happens in other parts of the module. If you don't need a context, I
recommend specifying `()`, and using functions from this module with names
starting with "withSimple".

**NOTE**: Do not store functions, JSON values or regular expressions in your contexts, as they will be
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#(==)).

    module My.Rule.Name exposing (rule)

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "My.Rule.Name" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

If you do need information from other parts of the module, then you should specify
an initial context, and I recommend using "with\*" functions without "Simple" in
their name, like [`withExpressionEnterVisitor`](#withExpressionEnterVisitor),
[`withImportVisitor`](#withImportVisitor) or [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnusedVariables" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        { declaredVariables : List String
        , usedVariables : List String
        }

    initialContext : Context
    initialContext =
        { declaredVariables = [], usedVariables = [] }

-}
newModuleRuleSchema : String -> moduleContext -> ModuleRuleSchema { canCollectProjectData : () } moduleContext
newModuleRuleSchema name initialModuleContext =
    ModuleRuleSchema
        { name = name
        , initialModuleContext = Just initialModuleContext
        , moduleContextCreator = initContextCreator (always initialModuleContext)
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


{-| Same as [`newModuleRuleSchema`](#newModuleRuleSchema), except that you can request for data to help initialize the context.
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#(==)).

    module My.Rule.Name exposing (rule)

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "My.Rule.Name" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

If you do need information from other parts of the module, then you should specify
an initial context, and I recommend using "with\*" functions without "Simple" in
their name, like [`withExpressionEnterVisitor`](#withExpressionEnterVisitor),
[`withImportVisitor`](#withImportVisitor) or [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchemaUsingContextCreator "Rule.Name" contextCreator
            -- visitors
            |> Rule.fromModuleRuleSchema

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\metadata () ->
                { hasTodoBeenImported = False
                , hasToStringBeenImported = False
                , isInSourceDirectories = Rule.isInSourceDirectories metadata
                }
            )
            |> Rule.withMetadata

-}
newModuleRuleSchemaUsingContextCreator : String -> ContextCreator () moduleContext -> ModuleRuleSchema {} moduleContext
newModuleRuleSchemaUsingContextCreator name moduleContextCreator =
    ModuleRuleSchema
        { name = name
        , initialModuleContext = Nothing
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
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        }


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema : ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> Rule
fromModuleRuleSchema ((ModuleRuleSchema schema) as moduleVisitor) =
    -- TODO BREAKING CHANGE Add canCollectData as a pre-requisite to using fromModuleRuleSchema
    case schema.initialModuleContext of
        Just initialModuleContext ->
            ProjectRuleSchema
                { name = schema.name
                , initialProjectContext = initialModuleContext
                , elmJsonVisitors = compactProjectDataVisitors (Maybe.map .project) schema.elmJsonVisitors
                , readmeVisitors = compactProjectDataVisitors (Maybe.map .content) schema.readmeVisitors
                , dependenciesVisitors = compactProjectDataVisitors identity schema.dependenciesVisitors
                , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
                , moduleContextCreator = Just (initContextCreator identity)
                , folder = Nothing
                , traversalType = AllModulesInParallel
                , finalEvaluationFns = []
                , dataExtractor = Nothing
                }
                |> fromProjectRuleSchema

        Nothing ->
            ProjectRuleSchema
                { name = schema.name
                , initialProjectContext = ()
                , elmJsonVisitors = []
                , readmeVisitors = []
                , dependenciesVisitors = []
                , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
                , moduleContextCreator = Just schema.moduleContextCreator
                , folder = Nothing
                , traversalType = AllModulesInParallel
                , finalEvaluationFns = []
                , dataExtractor = Nothing
                }
                |> fromProjectRuleSchema


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



-- PROJECT RULES


{-| Represents a schema for a project [`Rule`](#Rule).

See the documentation for [`newProjectRuleSchema`](#newProjectRuleSchema) for
how to create a project rule.

-}
type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , initialProjectContext : projectContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , moduleVisitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        , moduleContextCreator : Maybe (ContextCreator projectContext moduleContext)
        , folder : Maybe (Folder projectContext moduleContext)

        -- TODO Jeroen Only allow to set it if there is a folder, but not several times
        , traversalType : TraversalType

        -- TODO Jeroen Only allow to set it if there is a folder and module visitors?
        , finalEvaluationFns : List (projectContext -> List (Error {}))

        -- TODO Breaking change only allow a single data extractor, and only for project rules
        , dataExtractor : Maybe (projectContext -> Extract)
        }


type TraversalType
    = AllModulesInParallel
      -- TODO Add way to traverse in opposite order
    | ImportedModulesFirst


{-| Creates a schema for a project rule. Will require adding project visitors and calling
[`fromProjectRuleSchema`](#fromProjectRuleSchema) to create a usable [`Rule`](#Rule).

The first argument is the rule name. I _highly_ recommend naming it just like the
module name (including all the `.` there may be).

The second argument is the initial `projectContext`, i.e. the data that the rule will
accumulate as the project will be traversed, and allows the rule to know/remember
what happens in other parts of the project.

**NOTE**: Do not store functions, JSON values or regular expressions in your contexts, as they will be
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#(==)).

Project rules traverse the project in the following order:

  - Read and/or report errors in project files
      - The `elm.json` file, visited by [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
      - The `README.md` file, visited by [`withReadmeProjectVisitor`](#withReadmeProjectVisitor)
      - The definition for dependencies, visited by [`withDependenciesProjectVisitor`](#withDependenciesProjectVisitor)
  - The Elm modules one by one, visited by [`withModuleVisitor`](#withModuleVisitor),
    following the same traversal order as for module rules but without reading the project files (`elm.json`, ...).
  - A final evaluation when all modules have been visited, using [`withFinalProjectEvaluation`](#withFinalProjectEvaluation)

Evaluating/visiting a node means two things:

  - Detecting patterns and reporting errors
  - Collecting data in a "context", which will be either a `projectContext` or a `moduleContext` depending on the part of the project being visited, to have more information available in a later
    part of the traversal evaluation.

-}
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
        , dataExtractor = Nothing
        }


{-| Create a [`Rule`](#Rule) from a configured [`ProjectRuleSchema`](#ProjectRuleSchema).
-}
fromProjectRuleSchema : ProjectRuleSchema { schemaState | withModuleContext : Forbidden, hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
fromProjectRuleSchema ((ProjectRuleSchema schema) as projectRuleSchema) =
    Rule
        { name = schema.name
        , exceptions = Exceptions.init
        , requestedData =
            case schema.moduleContextCreator of
                Just (ContextCreator _ requestedData) ->
                    requestedData

                Nothing ->
                    RequestedData { metadata = False, moduleNameLookupTable = False }
        , ruleImplementation =
            \exceptions project nodeContexts ->
                let
                    result : { errors : List (Error {}), rule : Rule, cache : ProjectRuleCache projectContext, extract : Maybe Extract }
                    result =
                        runProjectVisitor schema.name
                            (fromProjectRuleSchemaToRunnableProjectVisitor projectRuleSchema)
                            Nothing
                            exceptions
                            project
                            nodeContexts
                in
                ( result.errors, result.rule )
        , configurationError = Nothing
        }


fromProjectRuleSchemaToRunnableProjectVisitor : ProjectRuleSchema schemaState projectContext moduleContext -> RunnableProjectVisitor projectContext moduleContext
fromProjectRuleSchemaToRunnableProjectVisitor (ProjectRuleSchema schema) =
    { initialProjectContext = schema.initialProjectContext
    , elmJsonVisitors = List.reverse schema.elmJsonVisitors
    , readmeVisitors = List.reverse schema.readmeVisitors
    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
    , moduleVisitor = mergeModuleVisitors schema.initialProjectContext schema.moduleContextCreator schema.moduleVisitors
    , traversalAndFolder =
        case ( schema.traversalType, schema.folder ) of
            ( AllModulesInParallel, _ ) ->
                TraverseAllModulesInParallel schema.folder

            ( ImportedModulesFirst, Just folder ) ->
                TraverseImportedModulesFirst folder

            ( ImportedModulesFirst, Nothing ) ->
                TraverseAllModulesInParallel Nothing
    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
    , dataExtractor = schema.dataExtractor
    , requestedData =
        case schema.moduleContextCreator of
            Just (ContextCreator _ requestedData) ->
                requestedData

            Nothing ->
                RequestedData { metadata = False, moduleNameLookupTable = False }
    }


mergeModuleVisitors :
    projectContext
    -> Maybe (ContextCreator projectContext moduleContext)
    -> List (ModuleRuleSchema schemaState1 moduleContext -> ModuleRuleSchema schemaState2 moduleContext)
    -> Maybe ( RunnableModuleVisitor moduleContext, ContextCreator projectContext moduleContext )
mergeModuleVisitors initialProjectContext maybeModuleContextCreator visitors =
    case ( maybeModuleContextCreator, List.isEmpty visitors ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, True ) ->
            Nothing

        ( Just moduleContextCreator, False ) ->
            let
                dummyAvailableData : AvailableData
                dummyAvailableData =
                    { metadata =
                        createMetadata
                            { moduleNameNode = Node.Node Range.emptyRange []
                            , isInSourceDirectories = True
                            }
                    , moduleKey = ModuleKey "dummy"
                    , moduleNameLookupTable = ModuleNameLookupTableInternal.empty
                    }

                initialModuleContext : moduleContext
                initialModuleContext =
                    applyContextCreator dummyAvailableData moduleContextCreator initialProjectContext

                emptyModuleVisitor : ModuleRuleSchema schemaState moduleContext
                emptyModuleVisitor =
                    ModuleRuleSchema
                        { name = ""
                        , initialModuleContext = Just initialModuleContext
                        , moduleContextCreator = initContextCreator (always initialModuleContext)
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


fromModuleRuleSchemaToRunnableModuleVisitor : ModuleRuleSchema schemaState moduleContext -> RunnableModuleVisitor moduleContext
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


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will
visit the project's Elm modules.

A module visitor behaves like a module rule, except that it won't visit the
project files, as those have already been seen by other visitors for project rules (such
as [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)).

`withModuleVisitor` takes a function that takes an already initialized module
rule schema and adds visitors to it, using the same functions as for building a
[`ModuleRuleSchema`](#ModuleRuleSchema).

When you use `withModuleVisitor`, you will be required to use [`withModuleContext`](#withModuleContext),
in order to specify how to create a `moduleContext` from a `projectContext` and vice-versa.

-}
withModuleVisitor :
    (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { moduleSchemaState | hasAtLeastOneVisitor : () } moduleContext)
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : () } projectContext moduleContext
    -- TODO BREAKING Change: add hasAtLeastOneVisitor : ()
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
withModuleVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | moduleVisitors = removeExtensibleRecordTypeVariable visitor :: schema.moduleVisitors }


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


{-| Creates a rule that will **only** report a configuration error, which stops `elm-review` from reviewing the project
until the user has addressed the issue.

When writing rules, some of them may take configuration arguments that specify what exactly the rule should do.
I recommend to define custom types to limit the possibilities of what can be considered valid and invalid configuration,
so that the user gets information from the compiler when the configuration is unexpected.

Unfortunately it is not always possible or practical to let the type system forbid invalid possibilities, and you may need to
manually parse or validate the arguments.

    rule : SomeCustomConfiguration -> Rule
    rule config =
        case parseFunctionName config.functionName of
            Nothing ->
                Rule.configurationError "RuleName"
                    { message = config.functionName ++ " is not a valid function name"
                    , details =
                        [ "I was expecting functionName to be a valid Elm function name."
                        , "When that is not the case, I am not able to function as expected."
                        ]
                    }

            Just functionName ->
                Rule.newModuleRuleSchema "RuleName" ()
                    |> Rule.withExpressionEnterVisitor (expressionVisitor functionName)
                    |> Rule.fromModuleRuleSchema

When you need to look at the project before determining whether something is actually a configuration error, for instance
when reporting that a targeted function does not fit some criteria (unexpected arguments, ...), you should go for more
usual errors like [`error`](#error) or potentially [`globalError`](#globalError). [`error`](#error) would be better because
it will give the user a starting place to fix the issue.

Be careful that the rule name is the same for the rule and for the configuration error.

The `message` and `details` represent the [message you want to display to the user](#a-helpful-error-message-and-details).
The `details` is a list of paragraphs, and each item will be visually separated
when shown to the user. The details may not be empty, and this will be enforced
by the tests automatically.

-}
configurationError : String -> { message : String, details : List String } -> Rule
configurationError name configurationError_ =
    Rule
        { name = name
        , exceptions = Exceptions.init
        , requestedData = RequestedData { metadata = False, moduleNameLookupTable = False }
        , ruleImplementation = \_ _ _ -> ( [], configurationError name configurationError_ )
        , configurationError = Just configurationError_
        }


{-| Used for phantom type constraints. You can safely ignore this type.
-}
type Required
    = Required Never


{-| Used for phantom type constraints. You can safely ignore this type.
-}
type Forbidden
    = Forbidden Never


{-| Specify, if the project rule has a [module visitor](#withModuleVisitor), how to:

  - convert a project context to a module context, through [`fromProjectToModule`]
  - convert a module context to a project context, through [`fromModuleToProject`]
  - fold (merge) project contexts, through [`foldProjectContexts`]

**NOTE**: I suggest reading the section about [`foldProjectContexts`] carefully,
as it is one whose implementation you will need to do carefully.

In project rules, we separate the context related to the analysis of the project
as a whole and the context related to the analysis of a single module into a
`projectContext` and a `moduleContext` respectively. We do this because in most
project rules you won't need all the data from the `projectContext` to analyze a
module, and some data from the module context will not make sense inside the
project context.

When visiting modules, `elm-review` follows a kind of map-reduce architecture.
The idea is the following: it starts with an initial `projectContext` and collects data
from project-related files into it. Then, it visits every module with an initial
`moduleContext` derived from a `projectContext`. At the end of a module's visit,
the final `moduleContext` will be transformed ("map") to a `projectContext`.
All or some of the `projectContext`s will then be folded into a single one,
before being used in the [final project evaluation] or to compute another module's
initial `moduleContext`.

This will help make the result of the review as consistent as possible, by
having the results be independent of the order the modules are visited. This also
gives internal guarantees as to what needs to be re-computed when re-analyzing
the project, which leads to huge performance boosts in watch mode or after fixes
have been applied.

The following sections will explain each function, and will be summarized by an
example.


### `fromProjectToModule`

The initial `moduleContext` of the module visitor is computed using `fromProjectToModule`
from a `projectContext`. By default, this `projectContext` will be the result of
visiting the project-related files (`elm.json`, `README.md`, ...).
If [`withContextFromImportedModules`] was used, then the value will be this last
`projectContext`, folded with each imported module's resulting `projectContext`,
using [`foldProjectContexts`].

The [`ModuleKey`] will allow you to report errors for this specific module
using [`errorForModule`](#errorForModule) from the [final project evaluation] or
while visiting another module. If you plan to do that, you should store this in
the `moduleContext`. You can also get it from [`fromModuleToProject`], so choose
what's most convenient.

The [`Node`] containing the module name is passed for convenience, so you don't
have to visit the module definition just to get the module name. Just like what
it is in [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-ModuleName),
the value will be `[ "My", "Module" ]` if the module name is `My.Module`.


### `fromModuleToProject`

When a module has finished being analyzed, the final `moduleContext` will be
converted into a `projectContext`, so that it can later be folded with the other
project contexts using `foldProjectContexts`. The resulting `projectContext`
will be fed into the [final project evaluation] and potentially into
[`fromProjectToModule`] for modules that import the current one.

Similarly to `fromProjectToModule`, the [`Node`] containing the module name and
the [`ModuleKey`] are passed for convenience, so you don't have to store them in
the `moduleContext` only to store them in the `projectContext`.


### `foldProjectContexts`

This function folds two `projectContext` into one. This function requires a few
traits to always be true.

  - `projectContext`s should be "merged" together, not "subtracted". If for instance
    you want to detect the unused exports of a module, do not remove a declared
    export when you have found it used. Instead, store and accumulate the declared
    and used functions (both probably as `Set`s or `Dict`s), and in the final evaluation,
    filter out the declared functions if they are in the set of used functions.
  - The order of folding should not matter: `foldProjectContexts b (foldProjectContexts a initial)`
    should equal `foldProjectContexts a (foldProjectContexts b initial)`.
    [`List.concat`](https://package.elm-lang.org/packages/elm/core/latest/List#concat).
  - Folding an element twice into another should give the same result as folding
    it once. In other words, `foldProjectContexts a (foldProjectContexts a initial)`
    should equal `foldProjectContexts a initial`. You will likely need to use functions
    like [`Set.union`](https://package.elm-lang.org/packages/elm/core/latest/Set#union)
    and [`Dict.union`](https://package.elm-lang.org/packages/elm/core/latest/Dict#union)
    over addition and functions like
    [`List.concat`](https://package.elm-lang.org/packages/elm/core/latest/List#concat).

It is not necessary for the function to be commutative (i.e. that
`foldProjectContexts a b` equals `foldProjectContexts b a`). It is fine to take
the value from the "initial" `projectContext` and ignore the other one, especially
for data computed in the project-related visitors (for which you will probably
define a dummy value in the `fromModuleToProject` function). If it helps, imagine
that the second argument is the initial `projectContext`, or that it is an accumulator
just like in `List.foldl`.


### Summary example - Reporting unused exported functions

As an example, we will write a rule that reports functions that get exported
but are unused in the rest of the project.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "NoUnusedExportedFunctions" initialProjectContext
            -- Omitted, but this will collect the list of exposed modules for packages.
            -- We don't want to report functions that are exposed
            |> Rule.withElmJsonProjectVisitor elmJsonVisitor
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContext
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.withFinalProjectEvaluation finalEvaluationForProject
            |> Rule.fromProjectRuleSchema

    moduleVisitor :
        Rule.ModuleRuleSchema {} ModuleContext
        -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
    moduleVisitor schema =
        schema
            -- Omitted, but this will collect the exposed functions
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            -- Omitted, but this will collect uses of exported functions
            |> Rule.withExpressionEnterVisitor expressionVisitor

    type alias ProjectContext =
        { -- Modules exposed by the package, that we should not report
          exposedModules : Set ModuleName
        , exposedFunctions :
            -- An entry for each module
            Dict
                ModuleName
                { -- To report errors in this module
                  moduleKey : Rule.ModuleKey

                -- An entry for each function with its location
                , exposed : Dict String Range
                }
        , used : Set ( ModuleName, String )
        }

    type alias ModuleContext =
        { isExposed : Bool
        , exposed : Dict String Range
        , used : Set ( ModuleName, String )
        }

    initialProjectContext : ProjectContext
    initialProjectContext =
        { exposedModules = Set.empty
        , modules = Dict.empty
        , used = Set.empty
        }

    fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
    fromProjectToModule moduleKey moduleName projectContext =
        { isExposed = Set.member (Node.value moduleName) projectContext.exposedModules
        , exposed = Dict.empty
        , used = Set.empty
        }

    fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
    fromModuleToProject moduleKey moduleName moduleContext =
        { -- We don't care about this value, we'll take
          -- the one from the initial context when folding
          exposedModules = Set.empty
        , exposedFunctions =
            if moduleContext.isExposed then
                -- If the module is exposed, don't collect the exported functions
                Dict.empty

            else
                -- Create a dictionary with all the exposed functions, associated to
                -- the module that was just visited
                Dict.singleton
                    (Node.value moduleName)
                    { moduleKey = moduleKey
                    , exposed = moduleContext.exposed
                    }
        , used = moduleContext.used
        }

    foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
    foldProjectContexts newContext previousContext =
        { -- Always take the one from the "initial" context,
          -- which is always the second argument
          exposedModules = previousContext.exposedModules

        -- Collect the exposed functions from the new context and the previous one.
        -- We could use `Dict.merge`, but in this case, that doesn't change anything
        , exposedFunctions = Dict.union previousContext.modules newContext.modules

        -- Collect the used functions from the new context and the previous one
        , used = Set.union newContext.used previousContext.used
        }

    finalEvaluationForProject : ProjectContext -> List (Error { useErrorForModule : () })
    finalEvaluationForProject projectContext =
        -- Implementation of `unusedFunctions` omitted, but it returns the list
        -- of unused functions, along with the associated module key and range
        unusedFunctions projectContext
            |> List.map
                (\{ moduleKey, functionName, range } ->
                    Rule.errorForModule moduleKey
                        { message = "Function `" ++ functionName ++ "` is never used"
                        , details = [ "<Omitted>" ]
                        }
                        range
                )

[`ModuleKey`]: #ModuleKey
[`Node`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node
[`fromProjectToModule`]: #-fromprojecttomodule-
[`fromModuleToProject`]: #-frommoduletoproject-
[`foldProjectContexts`]: #-foldprojectcontexts-
[final project evaluation]: #withFinalProjectEvaluation
[`withContextFromImportedModules`]: #withContextFromImportedModules

-}
withModuleContext :
    { fromProjectToModule : ModuleKey -> Node ModuleName -> projectContext -> moduleContext
    , fromModuleToProject : ModuleKey -> Node ModuleName -> moduleContext -> projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }
    -> ProjectRuleSchema { schemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : (), withModuleContext : Forbidden } projectContext moduleContext
withModuleContext functions (ProjectRuleSchema schema) =
    let
        moduleContextCreator : ContextCreator projectContext moduleContext
        moduleContextCreator =
            initContextCreator
                (\moduleKey metadata projectContext ->
                    functions.fromProjectToModule
                        moduleKey
                        (moduleNameNodeFromMetadata metadata)
                        projectContext
                )
                |> withModuleKey
                |> withMetadata
    in
    ProjectRuleSchema
        { schema
            | moduleContextCreator = Just moduleContextCreator
            , folder =
                Just
                    { fromModuleToProject =
                        initContextCreator (\moduleKey metadata moduleContext -> functions.fromModuleToProject moduleKey (moduleNameNodeFromMetadata metadata) moduleContext)
                            |> withModuleKey
                            |> withMetadata
                    , foldProjectContexts = functions.foldProjectContexts
                    }
        }


{-| Use a [`ContextCreator`](#ContextCreator) to initialize your `moduleContext` and `projectContext`. This will allow
you to request more information

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "NoMissingSubscriptionsCall" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextUsingContextCreator
                { fromProjectToModule = Rule.initContextCreator fromProjectToModule
                , fromModuleToProject =
                    Rule.initContextCreator fromModuleToProject
                        |> Rule.withModuleKey
                        |> Rule.withMetadata
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.fromProjectRuleSchema

    fromProjectToModule : ProjectContext -> ModuleContext
    fromProjectToModule projectContext =
        { -- something
        }

    fromModuleToProject : Rule.ModuleKey -> Metadata -> ModuleContext -> ProjectContext
    fromModuleToProject moduleKey metadata moduleContext =
        { moduleKeys = Dict.singleton (Rule.moduleNameFromMetadata metadata) moduleKey
        }

-}
withModuleContextUsingContextCreator :
    { fromProjectToModule : ContextCreator projectContext moduleContext
    , fromModuleToProject : ContextCreator moduleContext projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }
    -> ProjectRuleSchema { schemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : (), withModuleContext : Forbidden } projectContext moduleContext
withModuleContextUsingContextCreator functions (ProjectRuleSchema schema) =
    ProjectRuleSchema
        { schema
            | moduleContextCreator = Just functions.fromProjectToModule
            , folder =
                Just
                    { fromModuleToProject = functions.fromModuleToProject
                    , foldProjectContexts = functions.foldProjectContexts
                    }
        }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will visit the project's
[`elm.json`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project) file.

It works exactly like [`withElmJsonModuleVisitor`](#withElmJsonModuleVisitor).
The visitor will be called before any module is evaluated.

-}
withElmJsonProjectVisitor :
    (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withElmJsonProjectVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | elmJsonVisitors = removeErrorPhantomTypeFromVisitor visitor :: schema.elmJsonVisitors }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will visit
the project's `README.md` file.

It works exactly like [`withReadmeModuleVisitor`](#withReadmeModuleVisitor).
The visitor will be called before any module is evaluated.

-}
withReadmeProjectVisitor :
    (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withReadmeProjectVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | readmeVisitors = removeErrorPhantomTypeFromVisitor visitor :: schema.readmeVisitors }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will visit the project's
[dependencies](./Review-Project-Dependency).

It works exactly like [`withDependenciesModuleVisitor`](#withDependenciesModuleVisitor). The visitor will be called before any
module is evaluated.

-}
withDependenciesProjectVisitor :
    (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDependenciesProjectVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | dependenciesVisitors = removeErrorPhantomTypeFromVisitor visitor :: schema.dependenciesVisitors }


{-| Add a function that makes a final evaluation of the project based only on the
data that was collected in the `projectContext`. This can be useful if you can't report something until you have visited
all the modules in the project.

It works similarly [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

**NOTE**: Do not create errors using the [`error`](#error) function using `withFinalProjectEvaluation`, but using [`errorForModule`](#errorForModule)
instead. When the project is evaluated in this function, you are not in the "context" of an Elm module (the idiomatic "context", not `projectContext` or `moduleContext`).
That means that if you call [`error`](#error), we won't know which module to associate the error to.

-}
withFinalProjectEvaluation :
    (projectContext -> List (Error { useErrorForModule : () }))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema schemaState projectContext moduleContext
withFinalProjectEvaluation visitor (ProjectRuleSchema schema) =
    let
        removeErrorPhantomTypeFromEvaluation : (projectContext -> List (Error b)) -> (projectContext -> List (Error {}))
        removeErrorPhantomTypeFromEvaluation function projectContext =
            function projectContext
                |> List.map removeErrorPhantomType
    in
    ProjectRuleSchema { schema | finalEvaluationFns = removeErrorPhantomTypeFromEvaluation visitor :: schema.finalEvaluationFns }


type Extract
    = Extract (Dict ModuleName ModuleNameLookupTable)


withDataExtractor :
    (projectContext -> Extract)
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema schemaState projectContext moduleContext
withDataExtractor dataExtractor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | dataExtractor = Just dataExtractor }


removeErrorPhantomTypeFromVisitor : (element -> projectContext -> ( List (Error b), projectContext )) -> (element -> projectContext -> ( List (Error {}), projectContext ))
removeErrorPhantomTypeFromVisitor function element projectContext =
    function element projectContext
        |> Tuple.mapFirst (List.map removeErrorPhantomType)


{-| Allows the rule to have access to the context of the modules imported by the
currently visited module. You can use for instance to know what is exposed in a
different module.

When you finish analyzing a module, the `moduleContext` is turned into a `projectContext`
through [`fromModuleToProject`](#newProjectRuleSchema). Before analyzing a module,
the `projectContext`s of its imported modules get folded into a single one
starting with the initial context (that may have visited the
[`elm.json` file](#withElmJsonProjectVisitor) and/or the [project's dependencies](#withDependenciesProjectVisitor))
using [`foldProjectContexts`](#newProjectRuleSchema).

If there is information about another module that you wish to access, you should
therefore store it in the `moduleContext`, and have it persist when transitioning
to a `projectContext` and back to a `moduleContext`.

You can only access data from imported modules, not from modules that import the
current module. If you need to do so, I suggest collecting all the information
you need, and re-evaluate if from [the final project evaluation function](#withFinalProjectEvaluation).

If you don't use this function, you will only be able to access the contents of
the initial context. The benefit is that when re-analyzing the project, after a
fix or when a file was changed in watch mode, much less work will need to be done
and the analysis will be much faster, because we know other files won't influence
the results of other modules' analysis.

-}
withContextFromImportedModules : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleSchema schemaState projectContext moduleContext
withContextFromImportedModules (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | traversalType = ImportedModulesFirst }


setFilePathIfUnset : ProjectModule -> Error scope -> Error scope
setFilePathIfUnset module_ error_ =
    case error_ of
        UnspecifiedError err ->
            SpecifiedError
                { err
                    | filePath =
                        case err.filePath of
                            "" ->
                                module_.path

                            _ ->
                                err.filePath
                }

        SpecifiedError _ ->
            error_


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's [module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`) and report patterns.

The following example forbids having `_` in any part of a module name.

    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnderscoreInModuleName" ()
            |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> List (Error {})
    moduleDefinitionVisitor node =
        if List.any (String.contains "") (Node.value node |> Module.moduleName) then
            [ Rule.error
                { message = "Do not use `_` in a module name"
                , details = [ "By convention, Elm modules names use Pascal case (like `MyModuleName`). Please rename your module using this format." ]
                }
                (Node.range node)
            ]

        else
            []

Note: `withSimpleModuleDefinitionVisitor` is a simplified version of [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor),
which isn't passed a `context` and doesn't return one. You can use `withSimpleModuleDefinitionVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleModuleDefinitionVisitor : (Node Module -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleModuleDefinitionVisitor visitor schema =
    withModuleDefinitionVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's comments.

This visitor will give you access to the list of all comments in the module all at once.

The following example forbids words like "TODO" appearing in a comment.

    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoTodoComment" ()
            |> Rule.withSimpleCommentsVisitor commentsVisitor
            |> Rule.fromModuleRuleSchema

    commentsVisitor : List (Node String) -> List (Error {})
    commentsVisitor comments =
        comments
            |> List.concatMap
                (\commentNode ->
                    String.indexes "TODO" (Node.value commentNode)
                        |> List.map (errorAtPosition (Node.range commentNode))
                )

    errorAtPosition : Range -> Int -> Error {}
    errorAtPosition range index =
        Rule.error
            { message = "TODO needs to be handled"
            , details = [ "At fruits.com, we prefer not to have lingering TODO comments. Either fix the TODO now or create an issue for it." ]
            }
            -- Here you would ideally only target the TODO keyword
            -- or the rest of the line it appears on,
            -- so you would change `range` using `index`.
            range

Note: `withSimpleCommentsVisitor` is a simplified version of [`withCommentsVisitor`](#withCommentsVisitor),
which isn't passed a `context` and doesn't return one. You can use `withCommentsVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleCommentsVisitor : (List (Node String) -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleCommentsVisitor visitor schema =
    withCommentsVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's [import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import) (`import Html as H exposing (div)`) in order of their definition and report patterns.

The following example forbids using the core Html package and suggests using
`elm-css` instead.

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoCoreHtml" ()
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> List (Error {})
    importVisitor node =
        let
            moduleName : List String
            moduleName =
                node
                    |> Node.value
                    |> .moduleName
                    |> Node.value
        in
        case moduleName of
            [ "Html" ] ->
                [ Rule.error
                    { message = "Use `elm-css` instead of the core HTML package."
                    , details =
                        [ "At fruits.com, we chose to use the `elm-css` package (https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css) to build our HTML and CSS rather than the core Html package. To keep things simple, we think it is best to not mix these different libraries."
                        , "The API is very similar, but instead of using the `Html` module, use the `Html.Styled`. CSS is then defined using the Html.Styled.Attributes.css function (https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html-Styled-Attributes#css)."
                        ]
                    }
                    (Node.range node)
                ]

            _ ->
                []

Note: `withSimpleImportVisitor` is a simplified version of [`withImportVisitor`](#withImportVisitor),
which isn't passed a `context` and doesn't return one. You can use `withSimpleImportVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleImportVisitor : (Node Import -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleImportVisitor visitor schema =
    withImportVisitor (\node moduleContext -> ( visitor node, moduleContext )) schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`)
and report patterns. The declarations will be visited in the order of their definition.

The following example forbids declaring a function or a value without a type
annotation.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingTypeAnnotation" ()
            |> Rule.withSimpleDeclarationVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    declarationVisitor : Node Declaration -> List (Error {})
    declarationVisitor node =
        case Node.value node of
            Declaration.FunctionDeclaration { signature, declaration } ->
                case signature of
                    Just _ ->
                        []

                    Nothing ->
                        let
                            functionName : String
                            functionName =
                                declaration |> Node.value |> .name |> Node.value
                        in
                        [ Rule.error
                            { message = "Missing type annotation for `" ++ functionName ++ "`"
                            , details =
                                [ "Type annotations are very helpful for people who read your code. It can give a lot of information without having to read the contents of the function. When encountering problems, the compiler will also give much more precise and helpful information to help you solve the problem."
                                , "To add a type annotation, add a line like `" functionName ++ " : ()`, and replace the `()` by the type of the function. If you don't replace `()`, the compiler should give you a suggestion of what the type should be."
                                ]
                            }
                            (Node.range node)
                        ]

            _ ->
                []

Note: `withSimpleDeclarationVisitor` is a simplified version of [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor),
which isn't passed a `context` and doesn't return one either. You can use `withSimpleDeclarationVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleDeclarationVisitor : (Node Declaration -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleDeclarationVisitor visitor schema =
    withDeclarationEnterVisitor
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`). The expressions are visited in pre-order
depth-first search, meaning that an expression will be visited, then its first
child, the first child's children (and so on), then the second child (and so on).

The following example forbids using the Debug module.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    expressionVisitor : Node Expression -> List (Error {})
    expressionVisitor node =
        case Node.value node of
            Expression.FunctionOrValue moduleName fnName ->
                if List.member "Debug" moduleName then
                    [ Rule.error
                        { message = "Remove the use of `Debug` before shipping to production"
                        , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                        }
                        (Node.range node)
                    ]

                else
                    []

            _ ->
                []

Note: `withSimpleExpressionVisitor` is a simplified version of [`withExpressionEnterVisitor`](#withExpressionEnterVisitor),
which isn't passed a `context` and doesn't return one either. You can use `withSimpleExpressionVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleExpressionVisitor : (Node Expression -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleExpressionVisitor visitor schema =
    withExpressionEnterVisitor
        (\node moduleContext -> ( visitor node, moduleContext ))
        schema


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


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

The following example forbids the use of `Html.button` except in the "Button" module.
The example is simplified to only forbid the use of the `Html.button` expression.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    type Context
        = HtmlButtonIsAllowed
        | HtmlButtonIsForbidden

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHtmlButton" HtmlButtonIsForbidden
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
    moduleDefinitionVisitor node context =
        if (Node.value node |> Module.moduleName) == [ "Button" ] then
            ( [], HtmlButtonIsAllowed )

        else
            ( [], HtmlButtonIsForbidden )

    expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
    expressionVisitor node context =
        case context of
            HtmlButtonIsAllowed ->
                ( [], context )

            HtmlButtonIsForbidden ->
                case Node.value node of
                    Expression.FunctionOrValue [ "Html" ] "button" ->
                        ( [ Rule.error
                                { message = "Do not use `Html.button` directly"
                                , details = [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

Tip: If you do not need to collect data in this visitor, you may wish to use the
simpler [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) function.

Tip: The rule above is very brittle. What if `button` was imported using `import Html exposing (button)` or `import Html exposing (..)`, or if `Html` was aliased (`import Html as H`)? Then the rule above would
not catch and report the use `Html.button`. To handle this, check out [`withModuleNameLookupTable`](#withModuleNameLookupTable).

-}
withModuleDefinitionVisitor : (Node Module -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withModuleDefinitionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's comments, collect data in
the `context` and/or report patterns.

This visitor will give you access to the list of all comments in the module all at once.

Tip: If you do not need to collect data in this visitor, you may wish to use the
simpler [`withSimpleCommentsVisitor`](#withSimpleCommentsVisitor) function.

-}
withCommentsVisitor : (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCommentsVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | commentsVisitors = visitor :: schema.commentsVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition, collect data
in the `context` and/or report patterns.

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`).

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    type alias Context =
        { elmUiWasImported : Bool
        , elmCssWasImported : Bool
        }

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUsingBothHtmlAndHtmlStyled" initialContext
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    initialContext : Context
    initialContext =
        { elmUiWasImported = False
        , elmCssWasImported = False
        }

    error : Node Import -> Error {}
    error node =
        Rule.error
            { message = "Do not use both `elm-ui` and `elm-css`"
            , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
            }
            (Node.range node)

    importVisitor : Node Import -> Context -> ( List (Error {}), Context )
    importVisitor node context =
        case Node.value node |> .moduleName |> Node.value of
            [ "Element" ] ->
                if context.elmCssWasImported then
                    ( [ error node ]
                    , { context | elmUiWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { context | elmUiWasImported = True }
                    )

            [ "Html", "Styled" ] ->
                if context.elmUiWasImported then
                    ( [ error node ]
                    , { context | elmCssWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { context | elmCssWasImported = True }
                    )

            _ ->
                ( [], context )

This example was written in a different way in the example for [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleImportVisitor`](#withSimpleImportVisitor) function.

-}
withImportVisitor : (Node Import -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withImportVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | importVisitors = visitor :: schema.importVisitors }


{-| **DEPRECATED**

Use [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) and [`withDeclarationExitVisitor`](#withDeclarationExitVisitor) instead.
In the next major version, this function will be removed and [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) will be renamed to `withDeclarationVisitor`.

**/DEPRECATED**

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

Contrary to [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor), the
visitor function will be called twice with different [`Direction`](#Direction)
values. It will be visited with `OnEnter`, then the children will be visited,
and then it will be visited again with `OnExit`. If you do not check the value of
the `Direction` parameter, you might end up with duplicate errors and/or an
unexpected `moduleContext`. Read more about [`Direction` here](#Direction).

The following example forbids exposing a function or a value without it having a
type annotation.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Exposing as Exposing
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    type ExposedFunctions
        = All
        | OnlySome (List String)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingDocumentationForExposedFunctions" (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
    moduleDefinitionVisitor node context =
        case Node.value node |> Module.exposingList of
            Exposing.All _ ->
                ( [], All )

            Exposing.Explicit exposedValues ->
                ( [], OnlySome (List.filterMap exposedFunctionName exposedValues) )

    exposedFunctionName : Node Exposing.TopLevelExpose -> Maybe String
    exposedFunctionName value =
        case Node.value value of
            Exposing.FunctionExpose functionName ->
                Just functionName

            _ ->
                Nothing

    declarationVisitor : Node Declaration -> Direction -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
    declarationVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Declaration.FunctionDeclaration { documentation, declaration } ) ->
                let
                    functionName : String
                    functionName =
                        Node.value declaration |> .name |> Node.value
                in
                if documentation == Nothing && isExposed context functionName then
                    ( [ Rule.error
                            { message = "Exposed function " ++ functionName ++ " is missing a type annotation"
                            , details =
                                [ "Type annotations are very helpful for people who use the module. It can give a lot of information without having to read the contents of the function."
                                , "To add a type annotation, add a line like `" functionName ++ " : ()`, and replace the `()` by the type of the function. If you don't replace `()`, the compiler should give you a suggestion of what the type should be."
                                ]
                            }
                            (Node.range node)
                      ]
                    , context
                    )

                else
                    ( [], context )

            _ ->
                ( [], context )

    isExposed : ExposedFunctions -> String -> Bool
    isExposed exposedFunctions name =
        case exposedFunctions of
            All ->
                True

            OnlySome exposedList ->
                List.member name exposedList

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) function.

-}
withDeclarationVisitor : (Node Declaration -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | declarationVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.declarationVisitorsOnEnter
            , declarationVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.declarationVisitorsOnExit
        }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

The following example forbids exposing a function or a value without it having a
type annotation.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Exposing as Exposing
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    type ExposedFunctions
        = All
        | OnlySome (List String)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingDocumentationForExposedFunctions" (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationEnterVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
    moduleDefinitionVisitor node context =
        case Node.value node |> Module.exposingList of
            Exposing.All _ ->
                ( [], All )

            Exposing.Explicit exposedValues ->
                ( [], OnlySome (List.filterMap exposedFunctionName exposedValues) )

    exposedFunctionName : Node Exposing.TopLevelExpose -> Maybe String
    exposedFunctionName value =
        case Node.value value of
            Exposing.FunctionExpose functionName ->
                Just functionName

            _ ->
                Nothing

    declarationVisitor : Node Declaration -> ExposedFunctions -> ( List (Error {}), ExposedFunctions )
    declarationVisitor node direction context =
        case Node.value node of
            Declaration.FunctionDeclaration { documentation, declaration } ->
                let
                    functionName : String
                    functionName =
                        Node.value declaration |> .name |> Node.value
                in
                if documentation == Nothing && isExposed context functionName then
                    ( [ Rule.error
                            { message = "Exposed function " ++ functionName ++ " is missing a type annotation"
                            , details =
                                [ "Type annotations are very helpful for people who use the module. It can give a lot of information without having to read the contents of the function."
                                , "To add a type annotation, add a line like `" functionName ++ " : ()`, and replace the `()` by the type of the function. If you don't replace `()`, the compiler should give you a suggestion of what the type should be."
                                ]
                            }
                            (Node.range node)
                      ]
                    , context
                    )

                else
                    ( [], context )

            _ ->
                ( [], context )

    isExposed : ExposedFunctions -> String -> Bool
    isExposed exposedFunctions name =
        case exposedFunctions of
            All ->
                True

            OnlySome exposedList ->
                List.member name exposedList

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) function.

-}
withDeclarationEnterVisitor : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationVisitorsOnEnter = visitor :: schema.declarationVisitorsOnEnter }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

The following example reports unused parameters from top-level declarations.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
            |> Rule.withDeclarationExitVisitor declarationExitVisitor
            -- Omitted, but this marks parameters as used
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    declarationEnterVisitor : Node Declaration -> fContext -> ( List (Error {}), Context )
    declarationEnterVisitor node context =
        case Node.value node of
            Declaration.FunctionDeclaration function ->
                ( [], registerArguments context function )

            _ ->
                ( [], context )

    declarationExitVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
    declarationExitVisitor node context =
        case Node.value node of
            -- When exiting the function expression, report the parameters that were not used.
            Declaration.FunctionDeclaration function ->
                ( unusedParameters context |> List.map createError, removeArguments context )

            _ ->
                ( [], context )

-}
withDeclarationExitVisitor : (Node Declaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationVisitorsOnExit = visitor :: schema.declarationVisitorsOnExit }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns.

It is similar to [withDeclarationVisitor](#withDeclarationVisitor), but the
visitor used with this function is called before the visitor added with
[withDeclarationVisitor](#withDeclarationVisitor). You can use this visitor in
order to look ahead and add the module's types and variables into your context,
before visiting the contents of the module using [withDeclarationVisitor](#withDeclarationVisitor)
and [withExpressionEnterVisitor](#withExpressionEnterVisitor). Otherwise, using
[withDeclarationVisitor](#withDeclarationVisitor) is probably a simpler choice.

-}
withDeclarationListVisitor : (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationListVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


{-| **DEPRECATED**

Use [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor) instead.
In the next major version, this function will be removed and [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) will be renamed to `withExpressionVisitor`.

**/DEPRECATED**

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`), collect data in the `context` and/or report patterns.
The expressions are visited in pre-order depth-first search, meaning that an
expression will be visited, then its first child, the first child's children
(and so on), then the second child (and so on).

Contrary to [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor), the
visitor function will be called twice with different [`Direction`](#Direction)
values. It will be visited with `OnEnter`, then the children will be visited,
and then it will be visited again with `OnExit`. If you do not check the value of
the `Direction` parameter, you might end up with duplicate errors and/or an
unexpected `moduleContext`. Read more about [`Direction` here](#Direction).

The following example forbids the use of `Debug.log` even when it is imported like
`import Debug exposing (log)`.

    import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    type Context
        = DebugLogWasNotImported
        | DebugLogWasImported

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Error {}), Context )
    importVisitor node context =
        case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
            ( [ "Debug" ], Just (Exposing.All _) ) ->
                ( [], DebugLogWasImported )

            ( [ "Debug" ], Just (Exposing.Explicit exposedFunctions) ) ->
                let
                    isLogFunction : Node Exposing.TopLevelExpose -> Bool
                    isLogFunction exposeNode =
                        case Node.value exposeNode of
                            Exposing.FunctionExpose "log" ->
                                True

                            _ ->
                                False
                in
                if List.any isLogFunction exposedFunctions then
                    ( [], DebugLogWasImported )

                else
                    ( [], DebugLogWasNotImported )

            _ ->
                ( [], DebugLogWasNotImported )

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
    expressionVisitor node direction context =
        case context of
            DebugLogWasNotImported ->
                ( [], context )

            DebugLogWasImported ->
                case ( direction, Node.value node ) of
                    ( Rule.OnEnter, Expression.FunctionOrValue [] "log" ) ->
                        ( [ Rule.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) function.

-}
withExpressionVisitor : (Node Expression -> Direction -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | expressionVisitorsOnEnter = (\node ctx -> visitor node OnEnter ctx) :: schema.expressionVisitorsOnEnter
            , expressionVisitorsOnExit = (\node ctx -> visitor node OnExit ctx) :: schema.expressionVisitorsOnExit
        }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`), collect data in the `context` and/or report patterns.
The expressions are visited in pre-order depth-first search, meaning that an
expression will be visited, then its first child, the first child's children
(and so on), then the second child (and so on).

Contrary to [`withExpressionVisitor`](#withExpressionVisitor), the
visitor function will be called only once, when the expression is "entered",
meaning before its children are visited.

The following example forbids the use of `Debug.log` even when it is imported like
`import Debug exposing (log)`.

    import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    type Context
        = DebugLogWasNotImported
        | DebugLogWasImported

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Error {}), Context )
    importVisitor node context =
        case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
            ( [ "Debug" ], Just (Exposing.All _) ) ->
                ( [], DebugLogWasImported )

            ( [ "Debug" ], Just (Exposing.Explicit exposedFunctions) ) ->
                let
                    isLogFunction : Node Exposing.TopLevelExpose -> Bool
                    isLogFunction exposeNode =
                        case Node.value exposeNode of
                            Exposing.FunctionExpose "log" ->
                                True

                            _ ->
                                False
                in
                if List.any isLogFunction exposedFunctions then
                    ( [], DebugLogWasImported )

                else
                    ( [], DebugLogWasNotImported )

            _ ->
                ( [], DebugLogWasNotImported )

    expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
    expressionVisitor node context =
        case context of
            DebugLogWasNotImported ->
                ( [], context )

            DebugLogWasImported ->
                case Node.value node of
                    Expression.FunctionOrValue [] "log" ->
                        ( [ Rule.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) function.

-}
withExpressionEnterVisitor : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | expressionVisitorsOnEnter = visitor :: schema.expressionVisitorsOnEnter }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`), collect data in the `context` and/or report patterns.
The expressions are visited in pre-order depth-first search, meaning that an
expression will be visited, then its first child, the first child's children
(and so on), then the second child (and so on).

Contrary to [`withExpressionEnterVisitor`](#withExpressionEnterVisitor), the
visitor function will be called when the expression is "exited",
meaning after its children are visited.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withExpressionEnterVisitor expressionEnterVisitor
            |> Rule.withExpressionExitVisitor expressionExitVisitor
            |> Rule.fromModuleRuleSchema

    expressionEnterVisitor : Node Expression -> fContext -> ( List (Error {}), Context )
    expressionEnterVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue moduleName name ->
                ( [], markVariableAsUsed context name )

            -- Find variables declared in let expression
            Expression.LetExpression letBlock ->
                ( [], registerVariables context letBlock )

            _ ->
                ( [], context )

    expressionExitVisitor : Node Expression -> Context -> ( List (Error {}), Context )
    expressionExitVisitor node context =
        case Node.value node of
            -- When exiting the let expression, report the variables that were not used.
            Expression.LetExpression _ ->
                ( unusedVariables context |> List.map createError, removeVariables context )

            _ ->
                ( [], context )

-}
withExpressionExitVisitor : (Node Expression -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withExpressionExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | expressionVisitorsOnExit = visitor :: schema.expressionVisitorsOnExit }


{-| Add a function that makes a final evaluation of the module based only on the
data that was collected in the `moduleContext`. This can be useful if you can't or if
it is hard to determine something as you traverse the module.

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`). Note that this is the same one written in the example
for [`withImportVisitor`](#withImportVisitor), but using [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

    import Dict as Dict exposing (Dict)
    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Review.Rule as Rule exposing (Error, Rule)

    type alias Context =
        Dict (List String) Range

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUsingBothHtmlAndHtmlStyled" Dict.empty
            |> Rule.withImportVisitor importVisitor
            |> Rule.withFinalModuleEvaluation finalEvaluation
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Error {}), Context )
    importVisitor node context =
        ( [], Dict.insert (Node.value node |> .moduleName |> Node.value) (Node.range node) context )

    finalEvaluation : Context -> List (Error {})
    finalEvaluation context =
        case ( Dict.get [ "Element" ] context, Dict.get [ "Html", "Styled" ] context ) of
            ( Just elmUiRange, Just _ ) ->
                [ Rule.error
                    { message = "Do not use both `elm-ui` and `elm-css`"
                    , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
                    }
                    elmUiRange
                ]

            _ ->
                []

-}
withFinalModuleEvaluation : (moduleContext -> List (Error {})) -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withFinalModuleEvaluation visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }



-- ERRORS


{-| Represents an error found by a [`Rule`](#Rule). These are created by the
rules.
-}
type Error scope
    = UnspecifiedError InternalError
    | SpecifiedError InternalError


removeErrorPhantomType : Error something -> Error {}
removeErrorPhantomType err =
    case err of
        UnspecifiedError internalError ->
            UnspecifiedError internalError

        SpecifiedError internalError ->
            SpecifiedError internalError


{-| Represents an error found by a [`Rule`](#Rule). These are the ones that will
be reported to the user.

If you are building a [`Rule`](#Rule), you shouldn't have to use this.

-}
type alias ReviewError =
    Review.Error.ReviewError


{-| Create an [`Error`](#Error). Use it when you find a pattern that the rule should forbid.

The `message` and `details` represent the [message you want to display to the user].
The `details` is a list of paragraphs, and each item will be visually separated
when shown to the user. The details may not be empty, and this will be enforced
by the tests automatically.

    error : Node a -> Error
    error node =
        Rule.error
            { message = "Remove the use of `Debug` before shipping to production"
            , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
            }
            (Node.range node)

The [`Range`] corresponds to the location where the error should be shown, i.e. where to put the squiggly lines in an editor.
In most cases, you can get it using [`Node.range`].

[message you want to display to the user]: #a-helpful-error-message-and-details

[`Range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range
[`Node.range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#range

-}
error : { message : String, details : List String } -> Range -> Error {}
error { message, details } range =
    UnspecifiedError
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = Nothing
        , target = Review.Error.Module
        }


{-| Creates an [`Error`](#Error), just like the [`error`](#error) function, but
provides an automatic fix that the user can apply.

    import Review.Fix as Fix

    error : Node a -> Error
    error node =
        Rule.errorWithFix
            { message = "Remove the use of `Debug` before shipping to production"
            , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
            }
            (Node.range node)
            [ Fix.removeRange (Node.range node) ]

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then it will give the same error as if you had
called [`error`](#error) instead.

**Note**: Each fix applies on a location in the code, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
errorWithFix : { message : String, details : List String } -> Range -> List Fix -> Error {}
errorWithFix info range fixes =
    error info range
        |> withFixes fixes


{-| A key to be able to report an error for a specific module. You need such a
key in order to use the [`errorForModule`](#errorForModule) function. This is to
prevent creating errors for modules you have not visited, or files that do not exist.

You can get a `ModuleKey` from the `fromProjectToModule` and `fromModuleToProject`
functions that you define when using [`newProjectRuleSchema`](#newProjectRuleSchema).

-}
type ModuleKey
    = ModuleKey String


{-| Just like [`error`](#error), create an [`Error`](#Error) but for a specific module, instead of the module that is being
visited.

You will need a [`ModuleKey`](#ModuleKey), which you can get from the `fromProjectToModule` and `fromModuleToProject`
functions that you define when using [`newProjectRuleSchema`](#newProjectRuleSchema).

-}
errorForModule : ModuleKey -> { message : String, details : List String } -> Range -> Error scope
errorForModule (ModuleKey path) { message, details } range =
    SpecifiedError
        { message = message
        , ruleName = ""
        , details = details
        , range = range
        , filePath = path
        , fixes = Nothing
        , target = Review.Error.Module
        }


{-| Just like [`errorForModule`](#errorForModule), create an [`Error`](#Error) for a specific module, but
provides an automatic fix that the user can apply.

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then it will give the same error as if you had
called [`errorForModule`](#errorForModule) instead.

**Note**: Each fix applies on a location in the code, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
errorForModuleWithFix : ModuleKey -> { message : String, details : List String } -> Range -> List Fix -> Error scope
errorForModuleWithFix moduleKey info range fixes =
    errorForModule moduleKey info range
        |> withFixes fixes


{-| A key to be able to report an error for the `elm.json` file. You need this
key in order to use the [`errorForElmJson`](#errorForElmJson) function. This is
to prevent creating errors for it if you have not visited it.

You can get a `ElmJsonKey` using the [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor) function.

-}
type ElmJsonKey
    = ElmJsonKey
        { path : String
        , raw : String
        , project : Elm.Project.Project
        }


{-| Create an [`Error`](#Error) for the `elm.json` file.

You will need an [`ElmJsonKey`](#ElmJsonKey), which you can get from the [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
function.

The second argument is a function that takes the `elm.json` content as a raw string,
and returns the error details. Using the raw string, you should try and find the
most fitting [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range)
possible for the error.

-}
errorForElmJson : ElmJsonKey -> (String -> { message : String, details : List String, range : Range }) -> Error scope
errorForElmJson (ElmJsonKey { path, raw }) getErrorInfo =
    let
        errorInfo : { message : String, details : List String, range : Range }
        errorInfo =
            getErrorInfo raw
    in
    SpecifiedError
        { message = errorInfo.message
        , ruleName = ""
        , details = errorInfo.details
        , range = errorInfo.range
        , filePath = path
        , fixes = Nothing
        , target = Review.Error.ElmJson
        }


{-| Create an [`Error`](#Error) for the `elm.json` file.

You will need an [`ElmJsonKey`](#ElmJsonKey), which you can get from the [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
function.

The second argument is a function that takes the `elm.json` content as a raw string,
and returns the error details. Using the raw string, you should try and find the
most fitting [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range)
possible for the error.

The third argument is a function that takes the [`elm.json`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project)
and returns a different one that will be suggested as a fix. If the function returns `Nothing`, no fix will be applied.

The `elm.json` will be the same as the one you got from [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor), use either depending on what you find most practical.

-}
errorForElmJsonWithFix : ElmJsonKey -> (String -> { message : String, details : List String, range : Range }) -> (Elm.Project.Project -> Maybe Elm.Project.Project) -> Error scope
errorForElmJsonWithFix (ElmJsonKey elmJson) getErrorInfo getFix =
    let
        errorInfo : { message : String, details : List String, range : Range }
        errorInfo =
            getErrorInfo elmJson.raw
    in
    SpecifiedError
        { message = errorInfo.message
        , ruleName = ""
        , details = errorInfo.details
        , range = errorInfo.range
        , filePath = elmJson.path
        , fixes =
            Maybe.map
                (\updatedProject ->
                    let
                        encoded : String
                        encoded =
                            updatedProject
                                |> Review.ElmProjectEncoder.encode
                                |> Encode.encode 4
                    in
                    [ Fix.replaceRangeBy
                        { start = { row = 1, column = 1 }, end = { row = 100000000, column = 1 } }
                        (encoded ++ "\n")
                    ]
                )
                (getFix elmJson.project)
        , target = Review.Error.ElmJson
        }


{-| A key to be able to report an error for the `README.md` file. You need this
key in order to use the [`errorForReadme`](#errorForReadme) function. This is
to prevent creating errors for it if you have not visited it.

You can get a `ReadmeKey` using the [`withReadmeProjectVisitor`](#withReadmeProjectVisitor) function.

-}
type ReadmeKey
    = ReadmeKey
        { path : String
        , content : String
        }


{-| Create an [`Error`](#Error) for the `README.md` file.

You will need an [`ReadmeKey`](#ReadmeKey), which you can get from the [`withReadmeProjectVisitor`](#withReadmeProjectVisitor)
function.

-}
errorForReadme : ReadmeKey -> { message : String, details : List String } -> Range -> Error scope
errorForReadme (ReadmeKey { path }) { message, details } range =
    SpecifiedError
        { message = message
        , ruleName = ""
        , filePath = path
        , details = details
        , range = range
        , fixes = Nothing
        , target = Review.Error.Readme
        }


{-| Just like [`errorForReadme`](#errorForReadme), create an [`Error`](#Error) for the `README.md` file, but
provides an automatic fix that the user can apply.

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then it will give the same error as if you had
called [`errorForReadme`](#errorForReadme) instead.

**Note**: Each fix applies on a location in the code, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
errorForReadmeWithFix : ReadmeKey -> { message : String, details : List String } -> Range -> List Fix -> Error scope
errorForReadmeWithFix readmeKey info range fixes =
    errorForReadme readmeKey info range
        |> withFixes fixes


elmReviewGlobalError : { message : String, details : List String } -> Error scope
elmReviewGlobalError { message, details } =
    SpecifiedError
        { filePath = "GLOBAL ERROR"
        , ruleName = ""
        , message = message
        , details = details
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.Global
        }


{-| Create an [`Error`](#Error) that is not attached to any specific location in the project.

This can be useful when needing to report problems that are not tied to any file. For instance for reporting missing elements like a module that was expected to be there.

This is however **NOT** the recommended way when it is possible to attach an error to a location (even if it is simply the module name of a file's module declaration),
because [giving hints to where the problem is] makes it easier for the user to solve it.

The `message` and `details` represent the [message you want to display to the user].
The `details` is a list of paragraphs, and each item will be visually separated
when shown to the user. The details may not be empty, and this will be enforced
by the tests automatically.

    error : String -> Error scope
    error moduleName =
        Rule.globalError
            { message = "Could not find module " ++ moduleName
            , details =
                [ "You mentioned the module " ++ moduleName ++ " in the configuration of this rule, but it could not be found."
                , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                ]
            }

[giving hints to where the problem is]: #the-smallest-section-of-code-that-makes-sense
[message you want to display to the user]: #a-helpful-error-message-and-details

[`Range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range
[`Node.range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#range

-}
globalError : { message : String, details : List String } -> Error scope
globalError { message, details } =
    SpecifiedError
        { filePath = "GLOBAL ERROR"
        , ruleName = ""
        , message = message
        , details = details
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.UserGlobal
        }


parsingError : { path : String, source : String } -> ReviewError
parsingError rawFile =
    Review.Error.ReviewError
        { filePath = rawFile.path
        , ruleName = "ParsingError"
        , message = rawFile.path ++ " is not a correct Elm module"
        , details =
            [ "I could not understand the content of this file, and this prevents me from analyzing it. It is highly likely that the contents of the file is not correct Elm code."
            , "I need this file to be fixed before analyzing the rest of the project. If I didn't, I would potentially report incorrect things."
            , "Hint: Try running `elm make`. The compiler should give you better hints on how to resolve the problem."
            ]
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.Module
        }


{-| Give a list of fixes to automatically fix the error.

    import Review.Fix as Fix

    error : Node a -> Error
    error node =
        Rule.error
            { message = "Remove the use of `Debug` before shipping to production"
            , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
            }
            (Node.range node)
            |> withFixes [ Fix.removeRange (Node.range node) ]

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If you pass `withFixes` an empty list, the error will be considered as having no
automatic fix available. Calling `withFixes` several times on an error will
overwrite the previous fixes.

Fixes for the `elm.json` file will be ignored.

**Note**: Each fix applies on a location in the code, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
withFixes : List Fix -> Error scope -> Error scope
withFixes fixes error_ =
    mapInternalError
        (\err ->
            if List.isEmpty fixes then
                { err | fixes = Nothing }

            else
                case err.target of
                    Review.Error.Module ->
                        { err | fixes = Just fixes }

                    Review.Error.Readme ->
                        { err | fixes = Just fixes }

                    Review.Error.ElmJson ->
                        err

                    Review.Error.Global ->
                        err

                    Review.Error.UserGlobal ->
                        err
        )
        error_


errorToReviewError : Error scope -> ReviewError
errorToReviewError error_ =
    Review.Error.ReviewError (accessInternalError error_)


{-| Get the name of the rule that triggered this [`Error`](#Error).
-}
errorRuleName : ReviewError -> String
errorRuleName (Review.Error.ReviewError err) =
    err.ruleName


{-| Get the error message of an [`Error`](#Error).
-}
errorMessage : ReviewError -> String
errorMessage (Review.Error.ReviewError err) =
    err.message


{-| Get the error details of an [`Error`](#Error).
-}
errorDetails : ReviewError -> List String
errorDetails (Review.Error.ReviewError err) =
    err.details


{-| Get the [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range)
of an [`Error`](#Error).
-}
errorRange : ReviewError -> Range
errorRange (Review.Error.ReviewError err) =
    err.range


{-| Get the automatic [`fixes`](./Review-Fix#Fix) of an [`Error`](#Error), if it
defined any.
-}
errorFixes : ReviewError -> Maybe (List Fix)
errorFixes (Review.Error.ReviewError err) =
    err.fixes


{-| Get the file path of an [`Error`](#Error).
-}
errorFilePath : ReviewError -> String
errorFilePath (Review.Error.ReviewError err) =
    err.filePath


{-| Get the target of an [`Error`](#Error).
-}
errorTarget : ReviewError -> Review.Error.Target
errorTarget (Review.Error.ReviewError err) =
    err.target


mapInternalError : (InternalError -> InternalError) -> Error scope -> Error scope
mapInternalError fn err =
    case err of
        UnspecifiedError internal ->
            UnspecifiedError (fn internal)

        SpecifiedError internal ->
            SpecifiedError (fn internal)


accessInternalError : Error scope -> InternalError
accessInternalError error_ =
    case error_ of
        UnspecifiedError internalError ->
            internalError

        SpecifiedError internalError ->
            internalError



-- EXCEPTION CONFIGURATION


{-| Ignore the errors reported for modules in specific directories of the project.

Use it when you don't want to get review errors for generated source code or for
libraries that you forked and copied over to your project.

    config : List Rule
    config =
        [ Some.Rule.rule
            |> Rule.ignoreErrorsForDirectories [ "generated-source/", "vendor/" ]
        , Some.Other.Rule.rule
        ]

If you want to ignore some directories for all of your rules, you can apply
`ignoreErrorsForDirectories` like this:

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]
            |> List.map (Rule.ignoreErrorsForDirectories [ "generated-source/", "vendor/" ])

The paths should be relative to the `elm.json` file, just like the ones for the
`elm.json`'s `source-directories`.

You can apply `ignoreErrorsForDirectories`several times for a rule, to add more
ignored directories.

You can also use it when writing a rule. We can hardcode in the rule that a rule
is not applicable to a folder, like `tests/` for instance. The following example
forbids using `Debug.todo` anywhere in the code, except in tests.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForDirectories [ "tests/" ]

    expressionVisitor : Node Expression -> List (Error {})
    expressionVisitor node =
        case Node.value node of
            Expression.FunctionOrValue [ "Debug" ] "todo" ->
                [ Rule.error
                    { message = "Remove the use of `Debug.todo` before shipping to production"
                    , details = [ "`Debug.todo` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                    }
                    (Node.range node)
                ]

            _ ->
                []

-}
ignoreErrorsForDirectories : List String -> Rule -> Rule
ignoreErrorsForDirectories directories (Rule rule) =
    Rule
        { name = rule.name
        , exceptions = Exceptions.addDirectories directories rule.exceptions
        , requestedData = rule.requestedData
        , ruleImplementation = rule.ruleImplementation
        , configurationError = rule.configurationError
        }


{-| Ignore the errors reported for specific file paths.
Use it when you don't want to review generated source code or files from external
sources that you copied over to your project and don't want to be touched.

    config : List Rule
    config =
        [ Some.Rule.rule
            |> Rule.ignoreErrorsForFiles [ "src/Some/File.elm" ]
        , Some.Other.Rule.rule
        ]

If you want to ignore some files for all of your rules, you can apply
`ignoreErrorsForFiles` like this:

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]
            |> List.map (Rule.ignoreErrorsForFiles [ "src/Some/File.elm" ])

The paths should be relative to the `elm.json` file, just like the ones for the
`elm.json`'s `source-directories`.

You can apply `ignoreErrorsForFiles` several times for a rule, to add more
ignored files.

You can also use it when writing a rule. We can simplify the example from [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor)
by hardcoding an exception into the rule (that forbids the use of `Html.button` except in the "Button" module).

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHtmlButton"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForFiles [ "src/Button.elm" ]

    expressionVisitor : Node Expression -> List (Error {})
    expressionVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue [ "Html" ] "button" ->
                [ Rule.error
                    { message = "Do not use `Html.button` directly"
                    , details = [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website." ]
                    }
                    (Node.range node)
                ]

            _ ->
                []

-}
ignoreErrorsForFiles : List String -> Rule -> Rule
ignoreErrorsForFiles files (Rule rule) =
    Rule
        { name = rule.name
        , exceptions = Exceptions.addFiles files rule.exceptions
        , requestedData = rule.requestedData
        , ruleImplementation = rule.ruleImplementation
        , configurationError = rule.configurationError
        }



-- VISITOR
-- TODO BREAKING CHANGE Move this into a separate module later on


type alias RunnableProjectVisitor projectContext moduleContext =
    { initialProjectContext : projectContext
    , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , moduleVisitor : Maybe ( RunnableModuleVisitor moduleContext, ContextCreator projectContext moduleContext )
    , traversalAndFolder : TraversalAndFolder projectContext moduleContext
    , finalEvaluationFns : List (projectContext -> List (Error {}))
    , dataExtractor : Maybe (projectContext -> Extract)
    , requestedData : RequestedData
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
    { fromModuleToProject : ContextCreator moduleContext projectContext
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


runProjectVisitor :
    String
    -> RunnableProjectVisitor projectContext moduleContext
    -> Maybe (ProjectRuleCache projectContext)
    -> Exceptions
    -> Project
    -> List (Graph.NodeContext ModuleName ())
    -> { errors : List (Error {}), rule : Rule, cache : ProjectRuleCache projectContext, extract : Maybe Extract }
runProjectVisitor name projectVisitor maybePreviousCache exceptions project nodeContexts =
    let
        ( cacheWithInitialContext, hasInitialContextChanged ) =
            computeProjectContext projectVisitor project maybePreviousCache

        initialContext : projectContext
        initialContext =
            cacheWithInitialContext.dependencies.context

        previousModuleContexts : Dict String (CacheEntry projectContext)
        previousModuleContexts =
            if hasInitialContextChanged then
                Dict.empty

            else
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

        previousAllModulesContext : List projectContext
        previousAllModulesContext =
            previousModuleContexts
                |> Dict.values
                |> List.map .context

        allModulesContext : List projectContext
        allModulesContext =
            List.map Tuple.second contextsAndErrorsPerModule

        errorsFromFinalEvaluation : List (Error {})
        errorsFromFinalEvaluation =
            case maybePreviousCache of
                Just previousCache ->
                    if allModulesContext == previousAllModulesContext then
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
            Exceptions.apply exceptions (accessInternalError >> .filePath) (errorsFromCache newCache)
    in
    { errors = List.map (setRuleName name) errors
    , rule =
        Rule
            { name = name
            , exceptions = exceptions
            , requestedData = projectVisitor.requestedData
            , ruleImplementation =
                \newExceptions newProject newNodeContexts ->
                    let
                        result : { errors : List (Error {}), rule : Rule, cache : ProjectRuleCache projectContext, extract : Maybe Extract }
                        result =
                            runProjectVisitor
                                name
                                projectVisitor
                                (Just newCache)
                                newExceptions
                                newProject
                                newNodeContexts
                    in
                    ( result.errors, result.rule )
            , configurationError = Nothing
            }
    , cache = newCache
    , extract =
        case
            projectVisitor.dataExtractor
        of
            Just dataExtractor ->
                let
                    -- TODO This is already computed during the final project evaluation
                    -- Re-use the data instead of re-computing
                    contextToAnalyze : projectContext
                    contextToAnalyze =
                        case getFolderFromTraversal projectVisitor.traversalAndFolder of
                            Just { foldProjectContexts } ->
                                List.foldl foldProjectContexts initialContext allModulesContext

                            Nothing ->
                                initialContext
                in
                Just (dataExtractor contextToAnalyze)

            Nothing ->
                Nothing
    }


setRuleName : String -> Error scope -> Error scope
setRuleName ruleName_ error_ =
    mapInternalError (\err -> { err | ruleName = ruleName_ }) error_


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


computeProjectContext : RunnableProjectVisitor projectContext moduleContext -> Project -> Maybe (ProjectRuleCache projectContext) -> ( ProjectRuleCache projectContext, Bool )
computeProjectContext projectVisitor project maybePreviousCache =
    let
        projectElmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
        projectElmJson =
            Review.Project.elmJson project

        elmJsonData : Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
        elmJsonData =
            Maybe.map
                (\elmJson ->
                    { elmJsonKey = ElmJsonKey elmJson
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
                        (previousCache.elmJson.context == elmJsonCacheEntry.context)
                            -- and the readme stayed the same
                            && (previousCache.readme.value == readmeData)
                    then
                        previousCache.readme

                    else
                        computeReadme ()

                Nothing ->
                    computeReadme ()

        ( dependenciesCacheEntry, hasAnythingChanged ) =
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
                        (previousCache.readme.context == readmeCacheEntry.context)
                            -- and the dependencies stayed the same
                            && (previousCache.dependencies.value == dependencies)
                    then
                        ( previousCache.dependencies, False )

                    else
                        ( computeDependencies (), True )

                Nothing ->
                    ( computeDependencies (), True )
    in
    ( { elmJson = elmJsonCacheEntry
      , readme = readmeCacheEntry
      , dependencies = dependenciesCacheEntry
      , moduleContexts = Dict.empty
      , finalEvaluationErrors = []
      }
    , hasAnythingChanged
    )



-- VISIT MODULES


computeModules :
    RunnableProjectVisitor projectContext moduleContext
    -> ( RunnableModuleVisitor moduleContext, ContextCreator projectContext moduleContext )
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

        moduleNameLookupTables : Dict ModuleName ModuleNameLookupTable
        moduleNameLookupTables =
            Review.Project.Internal.moduleNameLookupTables project

        modulesToAnalyze : List ProjectModule
        modulesToAnalyze =
            case projectVisitor.traversalAndFolder of
                TraverseAllModulesInParallel (Just _) ->
                    Review.Project.modules project

                TraverseAllModulesInParallel Nothing ->
                    -- Performance: avoid visiting modules when they're ignored and they
                    -- can't influence the rest of the review.
                    Exceptions.apply
                        exceptions
                        .path
                        (Review.Project.modules project)

                TraverseImportedModulesFirst _ ->
                    Review.Project.modules project

        projectModulePaths : Set String
        projectModulePaths =
            modulesToAnalyze
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
                modulesToAnalyze

        newStartCache : Dict String (CacheEntry projectContext)
        newStartCache =
            Dict.filter (\path _ -> Set.member path projectModulePaths) startCache

        computeModule : Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext
        computeModule cache importedModules module_ =
            let
                moduleKey : ModuleKey
                moduleKey =
                    ModuleKey module_.path

                metadata : Metadata
                metadata =
                    createMetadata
                        { moduleNameNode = moduleNameNode module_.ast.moduleDefinition
                        , isInSourceDirectories = module_.isInSourceDirectories
                        }

                availableData : AvailableData
                availableData =
                    { metadata = metadata
                    , moduleKey = moduleKey
                    , moduleNameLookupTable =
                        Dict.get (Review.Project.Internal.getModuleName module_) moduleNameLookupTables
                            |> Maybe.withDefault ModuleNameLookupTableInternal.empty
                    }

                initialModuleContext : moduleContext
                initialModuleContext =
                    case projectVisitor.traversalAndFolder of
                        TraverseAllModulesInParallel _ ->
                            applyContextCreator availableData moduleContextCreator initialProjectContext

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
                            applyContextCreator availableData moduleContextCreator projectContext

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
                        applyContextCreator availableData fromModuleToProject context

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
                    if cacheEntry.source == module_.source && contextFromImportedModulesIsUnchanged traversalAndFolder importedModules invalidatedModules then
                        -- The module's source and the module's imported modules' context are unchanged, we will later return the cached errors and context
                        ( cache, invalidatedModules )

                    else
                        compute (Just cacheEntry)


contextFromImportedModulesIsUnchanged : TraversalAndFolder projectContext moduleContext -> List ProjectModule -> Set ModuleName -> Bool
contextFromImportedModulesIsUnchanged traversalAndFolder importedModules invalidatedModules =
    case traversalAndFolder of
        TraverseAllModulesInParallel _ ->
            True

        TraverseImportedModulesFirst _ ->
            List.any
                (\importedModule -> Set.member (getModuleName importedModule) invalidatedModules)
                importedModules
                |> not


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



-- INITIALIZING WITH CONTEXT
-- TODO Move this to a different module later on


{-| Create a context based on some other context.
Use functions like [`withMetadata`](#withMetadata) to request more information.

`from` will usually be a `projectContext` and `to` a `moduleContext`.

-}
type ContextCreator from to
    = ContextCreator (AvailableData -> from -> to) RequestedData


type RequestedData
    = RequestedData
        { metadata : Bool
        , moduleNameLookupTable : Bool
        }


{-| Initialize a new context creator.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\metadata () ->
                { moduleName = Rule.moduleNameFromMetadata metadata

                -- ...other fields
                }
            )
            |> Rule.withMetadata

-}
initContextCreator : (from -> to) -> ContextCreator from to
initContextCreator fromProjectToModule =
    -- TODO Try to get rid of the ()/from when using in a module rule
    ContextCreator
        (always fromProjectToModule)
        (RequestedData
            { metadata = False
            , moduleNameLookupTable = False
            }
        )


applyContextCreator : AvailableData -> ContextCreator from to -> from -> to
applyContextCreator data (ContextCreator fn _) from =
    fn data from


{-| Request metadata about the module.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\metadata () ->
                { moduleName = Rule.moduleNameFromMetadata metadata
                , moduleNameNode = Rule.moduleNameNodeFromMetadata metadata
                , isInSourceDirectories = Rule.isInSourceDirectories metadata

                -- ...other fields
                }
            )
            |> Rule.withMetadata

-}
withMetadata : ContextCreator Metadata (from -> to) -> ContextCreator from to
withMetadata (ContextCreator fn (RequestedData requested)) =
    ContextCreator
        (\data -> fn data data.metadata)
        (RequestedData { requested | metadata = True })


{-| Requests the module name lookup table for the types and functions inside a module.

When encountering a `Expression.FunctionOrValue ModuleName String` (among other nodes where we refer to a function or value),
the module name available represents the module name that is in the source code. But that module name can be an alias to
a different import, or it can be empty, meaning that it refers to a local value or one that has been imported explicitly
or implicitly. Resolving which module the type or function comes from can be a bit tricky sometimes, and I recommend against
doing it yourself.

`elm-review` computes this for you already. Store this value inside your module context, then use
[`ModuleNameLookupTable.moduleNameFor`](./Review-ModuleNameLookupTable#moduleNameFor) or
[`ModuleNameLookupTable.moduleNameAt`](./Review-ModuleNameLookupTable#moduleNameAt) to get the name of the module the
type or value comes from.

    import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)

    type alias Context =
        { lookupTable : ModuleNameLookupTable }

    rule : Rule
    rule =
        Rule.newModuleRuleSchemaUsingContextCreator "NoHtmlButton" contextCreator
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForFiles [ "src/Colors.elm" ]

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\lookupTable () -> { lookupTable = lookupTable })
            |> Rule.withModuleNameLookupTable

    expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
    expressionVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue _ "color" ->
                if ModuleNameLookupTable.moduleNameFor context.lookupTable node == Just [ "Css" ] then
                    ( [ Rule.error
                            { message = "Do not use `Css.color` directly, use the Colors module instead"
                            , details = [ "We made a module which contains all the available colors of our design system. Use the functions in there instead." ]
                            }
                            (Node.range node)
                      ]
                    , context
                    )

                else
                    ( [], context )

            _ ->
                ( [], context )

Note: If you have been using [`elm-review-scope`](https://github.com/jfmengels/elm-review-scope) before, you should use this instead.

-}
withModuleNameLookupTable : ContextCreator ModuleNameLookupTable (from -> to) -> ContextCreator from to
withModuleNameLookupTable (ContextCreator fn (RequestedData requested)) =
    ContextCreator
        (\data -> fn data data.moduleNameLookupTable)
        (RequestedData { requested | moduleNameLookupTable = True })


{-| Request the [module key](ModuleKey) for this module.

    rule =
        Rule.newProjectRuleSchema "NoMissingSubscriptionsCall" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextUsingContextCreator
                { fromProjectToModule = Rule.initContextCreator fromProjectToModule
                , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey
                , foldProjectContexts = foldProjectContexts
                }

-}
withModuleKey : ContextCreator ModuleKey (from -> to) -> ContextCreator from to
withModuleKey (ContextCreator fn (RequestedData requested)) =
    ContextCreator
        (\data -> fn data data.moduleKey)
        (RequestedData { requested | metadata = True })


type alias AvailableData =
    { metadata : Metadata
    , moduleKey : ModuleKey
    , moduleNameLookupTable : ModuleNameLookupTable
    }



-- METADATA


{-| Metadata for the module being visited.

Do not store the metadata directly in your context. Prefer storing the individual pieces of information.

-}
type Metadata
    = Metadata
        { moduleNameNode : Node ModuleName
        , isInSourceDirectories : Bool
        }


createMetadata : { moduleNameNode : Node ModuleName, isInSourceDirectories : Bool } -> Metadata
createMetadata data =
    Metadata data


{-| Get the module name of the current module.
-}
moduleNameFromMetadata : Metadata -> ModuleName
moduleNameFromMetadata (Metadata metadata) =
    Node.value metadata.moduleNameNode


{-| Get the [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node) to the module name of the current module.
-}
moduleNameNodeFromMetadata : Metadata -> Node ModuleName
moduleNameNodeFromMetadata (Metadata metadata) =
    metadata.moduleNameNode


{-| Learn whether the current module is in the "source-directories" of the project. You can use this information to
know whether the module is part of the tests or of the production code.
-}
isInSourceDirectories : Metadata -> Bool
isInSourceDirectories (Metadata metadata) =
    metadata.isInSourceDirectories



-- SCOPE RULE


scopeRule : RunnableProjectVisitor ScopeProjectContext ScopeModuleContext
scopeRule =
    newProjectRuleSchema "elm-review__SCOPE" scope_initialProjectContext
        |> withContextFromImportedModules
        |> withDependenciesProjectVisitor (scope_internalDependenciesVisitor |> scope_pairWithNoErrors)
        |> withModuleVisitor scope_moduleVisitor
        |> withModuleContext
            { fromProjectToModule = scope_fromProjectToModule
            , fromModuleToProject = scope_fromModuleToProject
            , foldProjectContexts = scope_foldProjectContexts
            }
        |> withDataExtractor (\projectContext -> Extract projectContext.lookupTables)
        |> fromProjectRuleSchemaToRunnableProjectVisitor


type alias ScopeProjectContext =
    { dependenciesModules : Dict String Elm.Docs.Module
    , modules : Dict ModuleName Elm.Docs.Module
    , lookupTables : Dict ModuleName ModuleNameLookupTable
    }


type alias ScopeModuleContext =
    { scopes : Nonempty Scope
    , localTypes : Set String
    , importAliases : Dict String (List ModuleName)
    , importedFunctions : Dict String (List String)
    , importedTypes : Dict String (List String)
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



-- PROJECT VISITOR


scope_initialProjectContext : ScopeProjectContext
scope_initialProjectContext =
    { dependenciesModules = Dict.empty
    , modules = Dict.empty
    , lookupTables = Dict.empty
    }


{-| Get a `Scope.ModuleContext` from a `Scope.ProjectContext`. Use this in your own
`fromProjectToModule`.

    fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
    fromProjectToModule moduleKey moduleName projectContext =
        { scope = Scope.fromProjectToModule projectContext.scope

        -- ...other fields
        }

-}
scope_fromProjectToModule : a -> b -> ScopeProjectContext -> ScopeModuleContext
scope_fromProjectToModule _ _ projectContext =
    { scopes = nonemptyList_fromElement emptyScope
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
    , lookupTable = ModuleNameLookupTableInternal.empty
    }
        |> registerPrelude


scope_fromModuleToProject : a -> Node ModuleName -> ScopeModuleContext -> ScopeProjectContext
scope_fromModuleToProject _ moduleName moduleContext =
    { dependenciesModules = Dict.empty
    , modules =
        Dict.singleton (Node.value moduleName)
            { name = String.join "." (Node.value moduleName)
            , comment = ""
            , unions = moduleContext.exposedUnions
            , aliases = moduleContext.exposedAliases
            , values = moduleContext.exposedValues
            , binops = moduleContext.exposedBinops
            }
    , lookupTables = Dict.singleton (Node.value moduleName) moduleContext.lookupTable
    }


scope_foldProjectContexts : ScopeProjectContext -> ScopeProjectContext -> ScopeProjectContext
scope_foldProjectContexts newContext previousContext =
    { dependenciesModules = previousContext.dependenciesModules
    , modules = Dict.union previousContext.modules newContext.modules
    , lookupTables = Dict.union newContext.lookupTables previousContext.lookupTables
    }



-- SCOPE


type alias Scope =
    { names : Dict String VariableInfo
    , cases : List ( Node Expression, Dict String VariableInfo )
    , caseToExit : Node Expression
    }


emptyScope : Scope
emptyScope =
    { names = Dict.empty
    , cases = []
    , caseToExit = Node Range.emptyRange (Expression.Literal "root")
    }


scope_moduleVisitor : ModuleRuleSchema schemaState ScopeModuleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ScopeModuleContext
scope_moduleVisitor schema =
    schema
        |> withModuleDefinitionVisitor (scope_moduleDefinitionVisitor |> scope_pairWithNoErrors)
        |> withImportVisitor (scope_importVisitor |> scope_pairWithNoErrors)
        |> withDeclarationListVisitor (scope_declarationListVisitor |> scope_pairWithNoErrors)
        |> withDeclarationEnterVisitor (scope_declarationEnterVisitor |> scope_pairWithNoErrors)
        |> withDeclarationExitVisitor (scope_declarationExitVisitor |> scope_pairWithNoErrors)
        |> withExpressionEnterVisitor
            (\visitedElement context ->
                ( []
                , context
                    |> scope_popScopeEnter visitedElement
                    |> scope_expressionEnterVisitor visitedElement
                )
            )
        |> withExpressionExitVisitor
            (\visitedElement context ->
                ( []
                , context
                    |> scope_popScopeExit visitedElement
                    |> expressionExitVisitor visitedElement
                )
            )


scope_pairWithNoErrors : (visited -> context -> context) -> visited -> context -> ( List nothing, context )
scope_pairWithNoErrors fn visited context =
    ( [], fn visited context )



-- DEPENDENCIES


scope_internalDependenciesVisitor : Dict String Dependency -> { context | dependenciesModules : Dict String Elm.Docs.Module } -> { context | dependenciesModules : Dict String Elm.Docs.Module }
scope_internalDependenciesVisitor dependencies innerContext =
    let
        dependenciesModules : Dict String Elm.Docs.Module
        dependenciesModules =
            dependencies
                |> Dict.values
                |> List.concatMap Review.Project.Dependency.modules
                |> List.map (\dependencyModule -> ( dependencyModule.name, dependencyModule ))
                |> Dict.fromList
    in
    { innerContext | dependenciesModules = dependenciesModules }


registerPrelude : ScopeModuleContext -> ScopeModuleContext
registerPrelude innerContext =
    List.foldl
        (\import_ ctx ->
            ctx
                |> registerImportAlias import_
                |> registerImportExposed import_
        )
        innerContext
        elmCorePrelude


elmCorePrelude : List Import
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


createFakeImport : { moduleName : List String, exposingList : Maybe Exposing, moduleAlias : Maybe String } -> Import
createFakeImport { moduleName, moduleAlias, exposingList } =
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
        newScopes : Nonempty Scope
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
                case Maybe.map Node.value function.documentation of
                    Just str ->
                        str

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
                    |> List.map (\constructor -> ( Node.value (Node.value constructor).name, [] ))
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
    case maybeSignature |> Maybe.map (Node.value >> .typeAnnotation) of
        Just typeAnnotation ->
            syntaxTypeAnnotationToDocsType innerContext typeAnnotation

        Nothing ->
            Elm.Type.Tuple []


syntaxTypeAnnotationToDocsType : ScopeModuleContext -> Node TypeAnnotation -> Elm.Type.Type
syntaxTypeAnnotationToDocsType innerContext (Node _ typeAnnotation) =
    case typeAnnotation of
        TypeAnnotation.GenericType name ->
            Elm.Type.Var name

        TypeAnnotation.Typed (Node _ ( moduleName, typeName )) typeParameters ->
            let
                realModuleName : List String
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


registerVariable : VariableInfo -> String -> Nonempty Scope -> Nonempty Scope
registerVariable variableInfo name scopes =
    nonemptyList_mapHead
        (\scope -> { scope | names = Dict.insert name variableInfo scope.names })
        scopes


updateScope : ScopeModuleContext -> Nonempty Scope -> ScopeModuleContext
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
    nodes
        |> List.filterMap
            (\node ->
                case Node.value node of
                    Exposing.FunctionExpose name ->
                        Just ( name, Node.range node )

                    Exposing.TypeOrAliasExpose name ->
                        Just ( name, Node.range node )

                    Exposing.TypeExpose { name } ->
                        Just ( name, Node.range node )

                    Exposing.InfixExpose _ ->
                        Nothing
            )
        |> Dict.fromList



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
                moduleName : List String
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
                moduleName : List String
                moduleName =
                    Node.value import_.moduleName

                module_ : Elm.Docs.Module
                module_ =
                    (case Dict.get (joinModuleName moduleName) innerContext.dependenciesModules of
                        Just m ->
                            Just m

                        Nothing ->
                            Dict.get moduleName innerContext.modules
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
                        nameWithModuleName : { r | name : String } -> ( String, List String )
                        nameWithModuleName { name } =
                            ( name, moduleName )

                        exposedValues : Dict String (List String)
                        exposedValues =
                            List.concat
                                [ List.concatMap
                                    (\union ->
                                        List.map (\( name, _ ) -> ( name, moduleName )) union.tags
                                    )
                                    module_.unions
                                , List.map nameWithModuleName module_.values
                                , List.map nameWithModuleName module_.aliases
                                , List.map nameWithModuleName module_.binops
                                ]
                                |> Dict.fromList

                        exposedTypes : Dict String (List String)
                        exposedTypes =
                            List.concat
                                [ List.map nameWithModuleName module_.unions
                                , List.map nameWithModuleName module_.aliases
                                ]
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctions = Dict.union innerContext.importedFunctions exposedValues
                        , importedTypes = Dict.union innerContext.importedTypes exposedTypes
                    }

                Exposing.Explicit topLevelExposeList ->
                    let
                        exposedValues : Dict String (List String)
                        exposedValues =
                            topLevelExposeList
                                |> List.concatMap (valuesFromExposingList module_)
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList

                        exposedTypes : Dict String (List String)
                        exposedTypes =
                            topLevelExposeList
                                |> List.filterMap typesFromExposingList
                                |> List.map (\name -> ( name, moduleName ))
                                |> Dict.fromList
                    in
                    { innerContext
                        | importedFunctions = Dict.union innerContext.importedFunctions exposedValues
                        , importedTypes = Dict.union innerContext.importedTypes exposedTypes
                    }


valuesFromExposingList : Elm.Docs.Module -> Node TopLevelExpose -> List String
valuesFromExposingList module_ topLevelExpose =
    case Node.value topLevelExpose of
        Exposing.InfixExpose operator ->
            [ operator ]

        Exposing.FunctionExpose function ->
            [ function ]

        Exposing.TypeOrAliasExpose name ->
            if List.any (\alias -> alias.name == name) module_.aliases then
                [ name ]

            else
                -- Type is a custom type
                []

        Exposing.TypeExpose { name, open } ->
            case open of
                Just _ ->
                    module_.unions
                        |> List.filter (\union -> union.name == name)
                        |> List.concatMap .tags
                        |> List.map Tuple.first

                Nothing ->
                    []


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


scope_declarationEnterVisitor : Node Declaration -> ScopeModuleContext -> ScopeModuleContext
scope_declarationEnterVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                newScope : Scope
                newScope =
                    { emptyScope | names = parameters <| .arguments <| Node.value function.declaration }

                newContext : ScopeModuleContext
                newContext =
                    context.scopes
                        |> nonemptyList_cons newScope
                        |> updateScope context

                moduleNamesFromSignature : List ( Range, ModuleName )
                moduleNamesFromSignature =
                    function.signature
                        |> Maybe.map (Node.value >> .typeAnnotation >> collectModuleNamesFromTypeAnnotation newContext)
                        |> Maybe.withDefault []

                moduleNamesFromArguments : List ( Range, ModuleName )
                moduleNamesFromArguments =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.concatMap (collectModuleNamesFromPattern newContext)

                lookupTable : ModuleNameLookupTable
                lookupTable =
                    ModuleNameLookupTableInternal.addMultiple
                        (moduleNamesFromSignature ++ moduleNamesFromArguments)
                        newContext.lookupTable
            in
            { newContext | lookupTable = lookupTable }

        Declaration.CustomTypeDeclaration { constructors } ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.addMultiple
                        (constructors |> List.concatMap (Node.value >> .arguments) |> List.concatMap (collectModuleNamesFromTypeAnnotation context))
                        context.lookupTable
            }

        Declaration.AliasDeclaration { typeAnnotation } ->
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.addMultiple
                        (collectModuleNamesFromTypeAnnotation context typeAnnotation)
                        context.lookupTable
            }

        Declaration.PortDeclaration signature ->
            let
                moduleNamesFromSignature : List ( Range, ModuleName )
                moduleNamesFromSignature =
                    collectModuleNamesFromTypeAnnotation context signature.typeAnnotation

                lookupTable : ModuleNameLookupTable
                lookupTable =
                    ModuleNameLookupTableInternal.addMultiple
                        moduleNamesFromSignature
                        context.lookupTable
            in
            { context | lookupTable = lookupTable }

        _ ->
            context


scope_declarationExitVisitor : Node Declaration -> ScopeModuleContext -> ScopeModuleContext
scope_declarationExitVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration _ ->
            { context | scopes = nonemptyList_pop context.scopes }

        _ ->
            context


parameters : List (Node Pattern) -> Dict String VariableInfo
parameters patterns =
    List.concatMap collectNamesFromPattern patterns
        |> List.map
            (\node ->
                ( Node.value node
                , { node = node
                  , variableType = FunctionParameter
                  }
                )
            )
        |> Dict.fromList


collectNamesFromPattern : Node Pattern -> List (Node String)
collectNamesFromPattern pattern =
    case Node.value pattern of
        Pattern.TuplePattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.RecordPattern names ->
            names

        Pattern.UnConsPattern left right ->
            List.concatMap collectNamesFromPattern [ left, right ]

        Pattern.ListPattern subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.VarPattern name ->
            [ Node (Node.range pattern) name ]

        Pattern.NamedPattern _ subPatterns ->
            List.concatMap collectNamesFromPattern subPatterns

        Pattern.AsPattern subPattern alias ->
            alias :: collectNamesFromPattern subPattern

        Pattern.ParenthesizedPattern subPattern ->
            collectNamesFromPattern subPattern

        _ ->
            []


collectModuleNamesFromPattern : ScopeModuleContext -> Node Pattern -> List ( Range, ModuleName )
collectModuleNamesFromPattern context pattern =
    case Node.value pattern of
        Pattern.TuplePattern subPatterns ->
            List.concatMap (collectModuleNamesFromPattern context) subPatterns

        Pattern.UnConsPattern left right ->
            List.concatMap (collectModuleNamesFromPattern context) [ left, right ]

        Pattern.ListPattern subPatterns ->
            List.concatMap (collectModuleNamesFromPattern context) subPatterns

        Pattern.NamedPattern { moduleName, name } subPatterns ->
            ( Node.range pattern, moduleNameForValue context name moduleName ) :: List.concatMap (collectModuleNamesFromPattern context) subPatterns

        Pattern.AsPattern subPattern _ ->
            collectModuleNamesFromPattern context subPattern

        Pattern.ParenthesizedPattern subPattern ->
            collectModuleNamesFromPattern context subPattern

        _ ->
            []


scope_popScopeEnter : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_popScopeEnter node context =
    let
        currentScope : Scope
        currentScope =
            nonemptyList_head context.scopes

        caseExpression : Maybe ( Node Expression, Dict String VariableInfo )
        caseExpression =
            findInList (\( expressionNode, _ ) -> node == expressionNode) currentScope.cases
    in
    case caseExpression of
        Nothing ->
            context

        Just ( _, names ) ->
            { context | scopes = nonemptyList_cons { emptyScope | names = names, caseToExit = node } context.scopes }


scope_popScopeExit : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_popScopeExit node context =
    let
        currentScope : Scope
        currentScope =
            nonemptyList_head context.scopes
    in
    if node == currentScope.caseToExit then
        { context | scopes = nonemptyList_pop context.scopes }

    else
        context


scope_expressionEnterVisitor : Node Expression -> ScopeModuleContext -> ScopeModuleContext
scope_expressionEnterVisitor node context =
    case Node.value node of
        Expression.LetExpression { declarations, expression } ->
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
                        (nonemptyList_cons emptyScope context.scopes)
                        declarations
                        |> updateScope context

                moduleNames : List ( Range, ModuleName )
                moduleNames =
                    declarations
                        |> List.concatMap
                            (\declaration ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        (function.signature
                                            |> Maybe.map (Node.value >> .typeAnnotation >> collectModuleNamesFromTypeAnnotation newContext)
                                            |> Maybe.withDefault []
                                        )
                                            ++ (function.declaration
                                                    |> Node.value
                                                    |> .arguments
                                                    |> List.concatMap (collectModuleNamesFromPattern newContext)
                                               )

                                    Expression.LetDestructuring pattern _ ->
                                        collectModuleNamesFromPattern newContext pattern
                            )
            in
            { newContext | lookupTable = ModuleNameLookupTableInternal.addMultiple moduleNames newContext.lookupTable }

        Expression.CaseExpression caseBlock ->
            let
                cases : List ( Node Expression, Dict String VariableInfo )
                cases =
                    List.map
                        (\( pattern, expression ) ->
                            ( expression
                            , collectNamesFromPattern pattern
                                |> List.map
                                    (\node_ ->
                                        ( Node.value node_
                                        , { node = node_
                                          , variableType = PatternVariable
                                          }
                                        )
                                    )
                                |> Dict.fromList
                            )
                        )
                        caseBlock.cases

                moduleNames : List ( Range, ModuleName )
                moduleNames =
                    List.concatMap
                        (\( pattern, _ ) -> collectModuleNamesFromPattern context pattern)
                        caseBlock.cases
            in
            { context
                | scopes = nonemptyList_mapHead (\scope -> { scope | cases = cases }) context.scopes
                , lookupTable = ModuleNameLookupTableInternal.addMultiple moduleNames context.lookupTable
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
            { context
                | lookupTable =
                    ModuleNameLookupTableInternal.addMultiple
                        (List.concatMap (collectModuleNamesFromPattern context) args)
                        context.lookupTable
            }

        _ ->
            context


collectModuleNamesFromTypeAnnotation : ScopeModuleContext -> Node TypeAnnotation -> List ( Range, ModuleName )
collectModuleNamesFromTypeAnnotation context typeAnnotationNode =
    case Node.value typeAnnotationNode of
        TypeAnnotation.Typed (Node range ( moduleName, name )) args ->
            ( range, moduleNameForType context name moduleName ) :: List.concatMap (collectModuleNamesFromTypeAnnotation context) args

        TypeAnnotation.Tupled nodes ->
            List.concatMap (collectModuleNamesFromTypeAnnotation context) nodes

        TypeAnnotation.Record fields ->
            List.concatMap (Node.value >> Tuple.second >> collectModuleNamesFromTypeAnnotation context) fields

        TypeAnnotation.GenericRecord _ fields ->
            fields
                |> Node.value
                |> List.concatMap (Node.value >> Tuple.second >> collectModuleNamesFromTypeAnnotation context)

        TypeAnnotation.FunctionTypeAnnotation left right ->
            collectModuleNamesFromTypeAnnotation context left ++ collectModuleNamesFromTypeAnnotation context right

        _ ->
            []


expressionExitVisitor : Node Expression -> ScopeModuleContext -> ScopeModuleContext
expressionExitVisitor node context =
    case Node.value node of
        Expression.LetExpression _ ->
            { context | scopes = nonemptyList_pop context.scopes }

        Expression.CaseExpression _ ->
            { context | scopes = nonemptyList_mapHead (\scope -> { scope | cases = [] }) context.scopes }

        _ ->
            context


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate list =
    case list of
        [] ->
            Nothing

        a :: rest ->
            if predicate a then
                Just a

            else
                findInList predicate rest



-- ACCESS


{-| Get the name of the module where a value was defined.
A value can be either a function, a constant, a custom type constructor or a type alias (used as a function).

  - The second argument (`String`) is the name of the value
  - The third argument (`List String`) is the module name that was used next to the value's name where you found it

If the element was defined in the current module, then the result will be `[]`.

    expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
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
moduleNameForValue : ScopeModuleContext -> String -> List String -> List String
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
                        findInList
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
  - The third argument (`List String`) is the module name that was used next to the type name where you found it

-}
moduleNameForType : ScopeModuleContext -> String -> List String -> List String
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
                        findInList
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


isInScope : String -> Nonempty Scope -> Bool
isInScope name scopes =
    nonemptyList_any (.names >> Dict.member name) scopes



-- MISC


joinModuleName : List String -> String
joinModuleName name =
    String.join "." name



{- INLINED NONEMPTYLIST

   Copied contents of mgold/elm-nonempty-list, and trimmed down unused functions.

   This is to avoid dependency conflicts when mgold/elm-nonempty-list would release a new major version.

   A list that cannot be empty. The head and tail can be accessed without Maybes. Most other list functions are
   available.


   # Definition

   @docs Nonempty


   # Create

   @docs fromElement


   # Access

   @docs head


   # Inspect

   @docs any


   # Convert

   @docs cons, pop


   # Map

   @docs mapHead


   # Original copyright notice

   Copyright (c) 2015, Max Goldstein

   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

       * Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

       * Redistributions in binary form must reproduce the above
         copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials provided
         with the distribution.

       * Neither the name of Max Goldstein nor the names of other
         contributors may be used to endorse or promote products derived
         from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}


{-| The Nonempty type. If you have both a head and tail, you can construct a
nonempty list directly. Otherwise use the helpers below instead.
-}
type Nonempty a
    = Nonempty a (List a)


{-| Create a singleton list with the given element.
-}
nonemptyList_fromElement : a -> Nonempty a
nonemptyList_fromElement x =
    Nonempty x []


{-| Return the head of the list.
-}
nonemptyList_head : Nonempty a -> a
nonemptyList_head (Nonempty x _) =
    x


{-| Determine if any elements satisfy the predicate.
-}
nonemptyList_any : (a -> Bool) -> Nonempty a -> Bool
nonemptyList_any f (Nonempty x xs) =
    f x || List.any f xs


{-| Add another element as the head of the list, pushing the previous head to the tail.
-}
nonemptyList_cons : a -> Nonempty a -> Nonempty a
nonemptyList_cons y (Nonempty x xs) =
    Nonempty y (x :: xs)


{-| Pop and discard the head, or do nothing for a singleton list. Useful if you
want to exhaust a list but hang on to the last item indefinitely.
pop (Nonempty 3 [ 2, 1 ]) --> Nonempty 2 [1]
pop (Nonempty 1 []) --> Nonempty 1 []
-}
nonemptyList_pop : Nonempty a -> Nonempty a
nonemptyList_pop (Nonempty x xs) =
    case xs of
        [] ->
            Nonempty x xs

        y :: ys ->
            Nonempty y ys


{-| Map the head to a value of the same type
-}
nonemptyList_mapHead : (a -> a) -> Nonempty a -> Nonempty a
nonemptyList_mapHead fn (Nonempty x xs) =
    Nonempty (fn x) xs
