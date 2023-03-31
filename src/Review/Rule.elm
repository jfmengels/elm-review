module Review.Rule exposing
    ( Rule
    , ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema
    , withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , newModuleRuleSchemaUsingContextCreator
    , withModuleDefinitionVisitor
    , withModuleDocumentationVisitor
    , withCommentsVisitor
    , withImportVisitor
    , Direction(..), withDeclarationEnterVisitor, withDeclarationExitVisitor, withDeclarationVisitor, withDeclarationListVisitor
    , withExpressionEnterVisitor, withExpressionExitVisitor, withExpressionVisitor
    , withCaseBranchEnterVisitor, withCaseBranchExitVisitor
    , withLetDeclarationEnterVisitor, withLetDeclarationExitVisitor
    , providesFixesForModuleRule
    , withFinalModuleEvaluation
    , withElmJsonModuleVisitor, withReadmeModuleVisitor, withDirectDependenciesModuleVisitor, withDependenciesModuleVisitor
    , ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDirectDependenciesProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules
    , providesFixesForProjectRule
    , ContextCreator, initContextCreator, withModuleName, withModuleNameNode, withIsInSourceDirectories, withFilePath, withIsFileIgnored, withModuleNameLookupTable, withModuleKey, withSourceCodeExtractor, withFullAst, withModuleDocumentation
    , Metadata, withMetadata, moduleNameFromMetadata, moduleNameNodeFromMetadata, isInSourceDirectories
    , Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, errorForElmJsonWithFix, ReadmeKey, errorForReadme, errorForReadmeWithFix
    , globalError, configurationError
    , ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath, errorTarget
    , ignoreErrorsForDirectories, ignoreErrorsForFiles, filterErrorsForFiles
    , withDataExtractor, preventExtract
    , reviewV3, reviewV2, review, ProjectData, ruleName, ruleProvidesFixes, ruleKnowsAboutIgnoredFiles, withRuleId, getConfigurationError
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
      - The definition for dependencies, visited by [`withDirectDependenciesModuleVisitor`](#withDirectDependenciesModuleVisitor) and [`withDependenciesModuleVisitor`](#withDependenciesModuleVisitor)
  - Visit the Elm module (in the following order)
      - The module definition, visited by [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) and [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor)
      - The module documentation, visited by [`withModuleDocumentationVisitor`](#withModuleDocumentationVisitor)
      - The module's list of comments, visited by [`withSimpleCommentsVisitor`](#withSimpleCommentsVisitor) and [`withCommentsVisitor`](#withCommentsVisitor)
      - Each import, visited by [`withSimpleImportVisitor`](#withSimpleImportVisitor) and [`withImportVisitor`](#withImportVisitor)
      - The list of declarations, visited by [`withDeclarationListVisitor`](#withDeclarationListVisitor)
      - Each declaration, visited in the following order:
          - [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) and [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor)
          - The expression contained in the declaration will be visited recursively by [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor), [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor).
          - [`withDeclarationExitVisitor`](#withDeclarationExitVisitor)
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
@docs withModuleDocumentationVisitor
@docs withCommentsVisitor
@docs withImportVisitor
@docs Direction, withDeclarationEnterVisitor, withDeclarationExitVisitor, withDeclarationVisitor, withDeclarationListVisitor
@docs withExpressionEnterVisitor, withExpressionExitVisitor, withExpressionVisitor
@docs withCaseBranchEnterVisitor, withCaseBranchExitVisitor
@docs withLetDeclarationEnterVisitor, withLetDeclarationExitVisitor
@docs providesFixesForModuleRule
@docs withFinalModuleEvaluation


## Builder functions to analyze the project's data

@docs withElmJsonModuleVisitor, withReadmeModuleVisitor, withDirectDependenciesModuleVisitor, withDependenciesModuleVisitor


## Creating a project rule

Project rules can look at the global picture of an Elm project. Contrary to module
rules, which forget everything about the module they were looking at when going from
one module to another, project rules can retain information about previously
analyzed modules, and use it to report errors when analyzing a different module or
after all modules have been visited.

Project rules can also report errors in the `elm.json` or the `README.md` files.

If you are new to writing rules, I would recommend learning [how to build a module rule](#creating-a-module-rule)
first, as they are in practice a simpler version of project rules.

@docs ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDirectDependenciesProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules
@docs providesFixesForProjectRule


## Requesting more information

@docs ContextCreator, initContextCreator, withModuleName, withModuleNameNode, withIsInSourceDirectories, withFilePath, withIsFileIgnored, withModuleNameLookupTable, withModuleKey, withSourceCodeExtractor, withFullAst, withModuleDocumentation


### Requesting more information (DEPRECATED)

@docs Metadata, withMetadata, moduleNameFromMetadata, moduleNameNodeFromMetadata, isInSourceDirectories


## Errors

@docs Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, errorForElmJsonWithFix, ReadmeKey, errorForReadme, errorForReadmeWithFix
@docs globalError, configurationError
@docs ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath, errorTarget


## Configuring exceptions

There are situations where you don't want review rules to report errors:

1.  You copied and updated over an external library because one of your needs wasn't met, and you don't want to modify it more than necessary.
2.  Your project contains generated source code, over which you have no control or for which you do not care that some rules are enforced (like the reports of unused variables).
3.  You want to introduce a rule progressively, because there are too many errors in the project for you to fix in one go. You can then ignore the parts of the project where the problem has not yet been solved, and fix them as you go.
4.  You wrote a rule that is very specific and should only be applied to a portion of your code.
5.  You wish to disable some rules for tests files (or enable some only for tests).

You can use the following functions to ignore errors in directories or files, or only report errors found in specific directories or files.

**NOTE**: Even though they can be used to disable any errors, I **strongly recommend against**
doing so if you are not in the situations listed above. I highly recommend you
leave a comment explaining the reason why you use these functions, or to
communicate with your colleagues if you see them adding exceptions without
reason or seemingly inappropriately.

@docs ignoreErrorsForDirectories, ignoreErrorsForFiles, filterErrorsForFiles


## Extract information

As you might have seen so far, `elm-review` has quite a nice way of traversing the files of a project and collecting data.

While you have only seen the tool be used to report errors, you can also use it to extract information from
the codebase. You can use this to gain insight into your codebase, or provide information to other tools to enable
powerful integrations.

You can read more about how to use this in [_Extract information_ in the README](./#extract-information), and you can
find the tools to extract data below.

@docs withDataExtractor, preventExtract


# Running rules

@docs reviewV3, reviewV2, review, ProjectData, ruleName, ruleProvidesFixes, ruleKnowsAboutIgnoredFiles, withRuleId, getConfigurationError


# Internals

@docs Required, Forbidden

-}

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Range)
import Interface as If
import Json.Encode as Encode
import Review.Cache as Cache
import Review.Cache.ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)
import Review.ElmProjectEncoder
import Review.Error exposing (InternalError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.FilePath exposing (FilePath)
import Review.Fix as Fix exposing (Fix)
import Review.Fix.FixedErrors as FixedErrors exposing (FixedErrors)
import Review.Fix.Internal as InternalFix
import Review.ImportCycle as ImportCycle
import Review.Logger as Logger
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.ModuleNameLookupTable.Compute
import Review.ModuleNameLookupTable.Internal as ModuleNameLookupTableInternal
import Review.Options as ReviewOptions exposing (ReviewOptions)
import Review.Options.Internal as InternalOptions exposing (ReviewOptionsData, ReviewOptionsInternal(..))
import Review.Project.Dependency
import Review.Project.Internal exposing (Project)
import Review.Project.InvalidProjectError as InvalidProjectError
import Review.Project.ProjectModule as ProjectModule exposing (OpaqueProjectModule)
import Review.Project.Valid as ValidProject exposing (ValidProject)
import Review.RequestedData as RequestedData exposing (RequestedData(..))
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict
import Vendor.ListExtra as ListExtra
import Vendor.Zipper as Zipper exposing (Zipper)


{-| Represents a construct able to analyze a project and report
unwanted patterns.

You can create [module rules](#creating-a-module-rule) or [project rules](#creating-a-project-rule).

-}
type Rule
    = Rule
        { name : String
        , id : Int
        , exceptions : Exceptions
        , requestedData : RequestedData
        , providesFixes : Bool
        , extractsData : Bool
        , ruleImplementation : ReviewOptionsData -> Int -> Exceptions -> FixedErrors -> ValidProject -> List RuleProjectVisitor -> { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
        , ruleProjectVisitor : Result { message : String, details : List String } (ChangeableRuleData -> RuleProjectVisitor)
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
    = ModuleRuleSchema (ModuleRuleSchemaData moduleContext)


type alias ModuleRuleSchemaData moduleContext =
    { name : String
    , initialModuleContext : Maybe moduleContext
    , moduleContextCreator : ContextCreator () moduleContext
    , moduleDefinitionVisitor : Maybe (Visitor Module moduleContext)
    , moduleDocumentationVisitor : Maybe (Maybe (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
    , commentsVisitor : Maybe (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
    , importVisitor : Maybe (Node Import -> moduleContext -> ( List (Error {}), moduleContext ))
    , declarationListVisitor : Maybe (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext ))
    , declarationVisitorOnEnter : Maybe (Visitor Declaration moduleContext)
    , declarationVisitorOnExit : Maybe (Visitor Declaration moduleContext)
    , expressionVisitorsOnEnter : Maybe (Visitor Expression moduleContext)
    , expressionVisitorsOnExit : Maybe (Visitor Expression moduleContext)
    , letDeclarationVisitorOnEnter : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext ))
    , letDeclarationVisitorOnExit : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext ))
    , caseBranchVisitorOnEnter : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext ))
    , caseBranchVisitorOnExit : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext ))
    , finalEvaluationFn : Maybe (moduleContext -> List (Error {}))
    , providesFixes : Bool

    -- Project visitors
    , elmJsonVisitor : Maybe (Maybe Elm.Project.Project -> moduleContext -> moduleContext)
    , readmeVisitor : Maybe (Maybe String -> moduleContext -> moduleContext)
    , dependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    , directDependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
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
    case ValidProject.parse project of
        Err (InvalidProjectError.SomeModulesFailedToParse pathsThatFailedToParse) ->
            ( List.map parsingError pathsThatFailedToParse, rules )

        Err (InvalidProjectError.DuplicateModuleNames duplicate) ->
            ( [ duplicateModulesGlobalError duplicate ], rules )

        Err (InvalidProjectError.ImportCycleError cycle) ->
            ( [ importCycleError cycle ], rules )

        Err InvalidProjectError.NoModulesError ->
            ( [ elmReviewGlobalError
                    { message = "This project does not contain any Elm modules"
                    , details = [ "I need to look at some Elm modules. Maybe you have specified folders that do not exist?" ]
                    }
                    |> setRuleName "Incorrect project"
                    |> errorToReviewError
              ]
            , rules
            )

        Ok ( validProject, _ ) ->
            case checkForConfigurationErrors validProject rules of
                Err configurationErrors ->
                    ( configurationErrors, rules )

                Ok ruleProjectVisitors ->
                    let
                        runRulesResult : { errors : List ReviewError, fixedErrors : FixedErrors, rules : List Rule, project : ValidProject, extracts : Dict String Encode.Value }
                        runRulesResult =
                            runRules ReviewOptions.defaults rules ruleProjectVisitors validProject
                    in
                    ( runRulesResult.errors, runRulesResult.rules )


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
    case
        getModulesSortedByImport project
            |> Result.andThen
                (\( validProject, _ ) ->
                    checkForConfigurationErrors validProject rules
                        |> Result.map (Tuple.pair validProject)
                )
    of
        Ok ( validProject, ruleProjectVisitors ) ->
            runReviewForV2 ReviewOptions.defaults validProject rules ruleProjectVisitors

        Err errors ->
            { errors = errors
            , rules = rules
            , projectData = maybeProjectData
            }


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
            { errors, rules, projectData, extracts } =
                -- Replace `config` by `rules` next time you call reviewV2
                -- Replace `Nothing` by `projectData` next time you call reviewV2
                Rule.reviewV3 config Nothing project
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
reviewV3 :
    ReviewOptions
    -> List Rule
    -> Project
    ->
        { errors : List ReviewError
        , fixedErrors : Dict String (List ReviewError)
        , rules : List Rule
        , project : Project
        , extracts : Dict String Encode.Value
        }
reviewV3 reviewOptions rules project =
    case
        getModulesSortedByImport project
            |> Result.andThen
                (\( validProject, _ ) ->
                    checkForConfigurationErrors validProject rules
                        |> Result.map (Tuple.pair validProject)
                )
    of
        Ok ( validProject, ruleProjectVisitors ) ->
            let
                result : { errors : List ReviewError, fixedErrors : FixedErrors, rules : List Rule, project : ValidProject, extracts : Dict String Encode.Value }
                result =
                    runRules reviewOptions rules ruleProjectVisitors validProject
            in
            { errors = result.errors
            , fixedErrors = FixedErrors.toDict result.fixedErrors
            , rules = result.rules
            , project = ValidProject.toRegularProject result.project
            , extracts = result.extracts
            }

        Err errors ->
            { errors = errors
            , fixedErrors = Dict.empty
            , rules = rules
            , project = project
            , extracts = Dict.empty
            }


checkForConfigurationErrors : ValidProject -> List Rule -> Result (List ReviewError) (List RuleProjectVisitor)
checkForConfigurationErrors project rules =
    let
        ( allRulesToRun, allConfigurationErrors ) =
            List.foldl
                (\(Rule rule) ( rulesToRun, configurationErrors ) ->
                    case rule.ruleProjectVisitor of
                        Ok ruleProjectVisitor ->
                            ( ruleProjectVisitor
                                { exceptions = rule.exceptions
                                , ruleId = rule.id
                                , requestedData = rule.requestedData
                                }
                                :: rulesToRun
                            , configurationErrors
                            )

                        Err { message, details } ->
                            ( rulesToRun
                            , Review.Error.ReviewError
                                { filePath = "CONFIGURATION ERROR"
                                , ruleName = rule.name
                                , message = message
                                , details = details
                                , range = Range.emptyRange
                                , fixes = Nothing
                                , target = Review.Error.Global
                                , preventsExtract = False
                                }
                                :: configurationErrors
                            )
                )
                ( [], [] )
                rules
    in
    if List.isEmpty allConfigurationErrors then
        Ok allRulesToRun

    else
        Err allConfigurationErrors


getModulesSortedByImport : Project -> Result (List ReviewError) ( ValidProject, Zipper (Graph.NodeContext FilePath ()) )
getModulesSortedByImport project =
    case ValidProject.parse project of
        Err (InvalidProjectError.SomeModulesFailedToParse pathsThatFailedToParse) ->
            Err (List.map parsingError pathsThatFailedToParse)

        Err (InvalidProjectError.DuplicateModuleNames duplicate) ->
            Err [ duplicateModulesGlobalError duplicate ]

        Err (InvalidProjectError.ImportCycleError cycle) ->
            Err [ importCycleError cycle ]

        Err InvalidProjectError.NoModulesError ->
            Err
                [ elmReviewGlobalError
                    { message = "This project does not contain any Elm modules"
                    , details = [ "I need to look at some Elm modules. Maybe you have specified folders that do not exist?" ]
                    }
                    |> setRuleName "Incorrect project"
                    |> errorToReviewError
                ]

        Ok result ->
            Ok result


importCycleError : List ModuleName -> ReviewError
importCycleError cycle =
    ImportCycle.error cycle
        |> elmReviewGlobalError
        |> setRuleName "Incorrect project"
        |> errorToReviewError


runReviewForV2 : ReviewOptions -> ValidProject -> List Rule -> List RuleProjectVisitor -> { errors : List ReviewError, rules : List Rule, projectData : Maybe ProjectData }
runReviewForV2 reviewOptions project rules ruleProjectVisitors =
    let
        runResult : { errors : List ReviewError, fixedErrors : FixedErrors, rules : List Rule, project : ValidProject, extracts : Dict String Encode.Value }
        runResult =
            runRules reviewOptions rules ruleProjectVisitors project
    in
    { errors = runResult.errors
    , rules = runResult.rules
    , projectData = Nothing
    }



-- PROJECT DATA


{-| Internal cache about the project.
-}
type
    ProjectData
    -- This is not used in practice anymore
    = ProjectData Never


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


runRules :
    ReviewOptions
    -> List Rule
    -> List RuleProjectVisitor
    -> ValidProject
    -> { errors : List ReviewError, fixedErrors : FixedErrors, rules : List Rule, project : ValidProject, extracts : Dict String Encode.Value }
runRules (ReviewOptionsInternal reviewOptions) rules ruleProjectVisitors project =
    let
        result : { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
        result =
            runRulesHelp
                reviewOptions
                (moveFixableRulesFirst rules)
                { fixedErrors = FixedErrors.empty
                , ruleProjectVisitors = ruleProjectVisitors
                , project = project
                }
    in
    { errors = List.concatMap (\(RuleProjectVisitor rule) -> rule.getErrors () |> List.map errorToReviewError) result.ruleProjectVisitors
    , fixedErrors = result.fixedErrors
    , rules = List.map (\(RuleProjectVisitor rule) -> rule.backToRule ()) result.ruleProjectVisitors
    , project = result.project
    , extracts = List.foldl (\(RuleProjectVisitor rule) dict -> rule.addDataExtract dict) Dict.empty result.ruleProjectVisitors
    }


moveFixableRulesFirst : List Rule -> List Rule
moveFixableRulesFirst rules =
    List.sortBy
        (\(Rule rule) ->
            if rule.name == "NoUnused.Variables" then
                0

            else if rule.name == "NoUnused.Exports" then
                1

            else if rule.providesFixes then
                2

            else
                3
        )
        rules


runRulesHelp :
    ReviewOptionsData
    -> List Rule
    -> { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
    -> { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
runRulesHelp reviewOptions remainingRules acc =
    case remainingRules of
        [] ->
            acc

        (Rule { name, id, exceptions, ruleImplementation }) :: restOfRules ->
            let
                result : { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
                result =
                    ruleImplementation reviewOptions id exceptions acc.fixedErrors acc.project acc.ruleProjectVisitors
            in
            if InternalOptions.shouldAbort reviewOptions result.fixedErrors then
                { fixedErrors = result.fixedErrors
                , ruleProjectVisitors = result.ruleProjectVisitors
                , project = result.project
                }

            else if FixedErrors.hasChanged result.fixedErrors acc.fixedErrors then
                runRulesHelp
                    reviewOptions
                    restOfRules
                    { fixedErrors = result.fixedErrors
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , project = result.project
                    }

            else
                runRulesHelp
                    reviewOptions
                    restOfRules
                    { fixedErrors = result.fixedErrors
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , project = result.project
                    }


{-| Let `elm-review` know that this rule may provide fixes in the reported errors.

This information is hard for `elm-review` to deduce on its own, but can be very useful for improving the performance of
the tool while running in fix mode.

If your rule is a project rule, then you should use [`providesFixesForProjectRule`](#providesFixesForProjectRule) instead.

-}
providesFixesForModuleRule : ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema schemaState moduleContext
providesFixesForModuleRule (ModuleRuleSchema moduleRuleSchema) =
    ModuleRuleSchema { moduleRuleSchema | providesFixes = True }


{-| Let `elm-review` know that this rule may provide fixes in the reported errors.

This information is hard for `elm-review` to deduce on its own, but can be very useful for improving the performance of
the tool while running in fix mode.

If your rule is a module rule, then you should use [`providesFixesForModuleRule`](#providesFixesForModuleRule) instead.

-}
providesFixesForProjectRule : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleSchema schemaState projectContext moduleContext
providesFixesForProjectRule (ProjectRuleSchema projectRuleSchema) =
    ProjectRuleSchema { projectRuleSchema | providesFixes = True }


{-| Get the name of a rule.

You should not have to use this when writing a rule.

-}
ruleName : Rule -> String
ruleName (Rule rule) =
    rule.name


{-| Indicates whether the rule provides fixes.

You should not have to use this when writing a rule.

-}
ruleProvidesFixes : Rule -> Bool
ruleProvidesFixes (Rule rule) =
    -- TODO Breaking change: This should be an internal detail, not shown to the user
    rule.providesFixes


{-| Indicates whether the rule knows about which files are ignored.

You should not have to use this when writing a rule.

-}
ruleKnowsAboutIgnoredFiles : Rule -> Bool
ruleKnowsAboutIgnoredFiles (Rule rule) =
    -- TODO Breaking change: This should be an internal detail, not shown to the user
    let
        (RequestedData requestedData) =
            rule.requestedData
    in
    requestedData.ignoredFiles


{-| Assign an id to a rule. This id should be unique.

    config =
        [ rule1, rule2, rule3 ]
            |> List.indexedMap Rule.withUniqueId

You should not have to use this when writing a rule.

-}
withRuleId : Int -> Rule -> Rule
withRuleId id (Rule rule) =
    Rule { rule | id = id }


{-| Get the configuration error for a rule.

You should not have to use this when writing a rule. You might be looking for [`configurationError`](#configurationError) instead.

-}
getConfigurationError : Rule -> Maybe { message : String, details : List String }
getConfigurationError (Rule rule) =
    case rule.ruleProjectVisitor of
        Ok _ ->
            Nothing

        Err err ->
            Just err


{-| **@deprecated**

This is used in [`withDeclarationVisitor`](#withDeclarationVisitor) and [`withDeclarationVisitor`](#withDeclarationVisitor),
which are deprecated and will be removed in the next major version. This type will be removed along with them.

To replicate the same behavior, take a look at

  - [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) and [`withDeclarationExitVisitor`](#withDeclarationExitVisitor).
  - [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor).

**/@deprecated**

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

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Rule.Error {}), Context )
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
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

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
        , moduleDefinitionVisitor = Nothing
        , moduleDocumentationVisitor = Nothing
        , commentsVisitor = Nothing
        , importVisitor = Nothing
        , declarationListVisitor = Nothing
        , declarationVisitorOnEnter = Nothing
        , declarationVisitorOnExit = Nothing
        , expressionVisitorsOnEnter = Nothing
        , expressionVisitorsOnExit = Nothing
        , letDeclarationVisitorOnEnter = Nothing
        , letDeclarationVisitorOnExit = Nothing
        , caseBranchVisitorOnEnter = Nothing
        , caseBranchVisitorOnExit = Nothing
        , finalEvaluationFn = Nothing
        , elmJsonVisitor = Nothing
        , readmeVisitor = Nothing
        , dependenciesVisitor = Nothing
        , directDependenciesVisitor = Nothing
        , providesFixes = False
        }


{-| Same as [`newModuleRuleSchema`](#newModuleRuleSchema), except that you can request for data to help initialize the context.
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

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
            (\isInSourceDirectories () ->
                { hasTodoBeenImported = False
                , hasToStringBeenImported = False
                , isInSourceDirectories = isInSourceDirectories
                }
            )
            |> Rule.withIsInSourceDirectories

-}
newModuleRuleSchemaUsingContextCreator : String -> ContextCreator () moduleContext -> ModuleRuleSchema {} moduleContext
newModuleRuleSchemaUsingContextCreator name moduleContextCreator =
    ModuleRuleSchema
        { name = name
        , initialModuleContext = Nothing
        , moduleContextCreator = moduleContextCreator
        , moduleDefinitionVisitor = Nothing
        , moduleDocumentationVisitor = Nothing
        , commentsVisitor = Nothing
        , importVisitor = Nothing
        , declarationListVisitor = Nothing
        , declarationVisitorOnEnter = Nothing
        , declarationVisitorOnExit = Nothing
        , expressionVisitorsOnEnter = Nothing
        , expressionVisitorsOnExit = Nothing
        , letDeclarationVisitorOnEnter = Nothing
        , letDeclarationVisitorOnExit = Nothing
        , caseBranchVisitorOnEnter = Nothing
        , caseBranchVisitorOnExit = Nothing
        , finalEvaluationFn = Nothing
        , elmJsonVisitor = Nothing
        , readmeVisitor = Nothing
        , dependenciesVisitor = Nothing
        , directDependenciesVisitor = Nothing
        , providesFixes = False
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
                , elmJsonVisitor = compactProjectDataVisitors (Maybe.map .project) schema.elmJsonVisitor
                , readmeVisitor = compactProjectDataVisitors (Maybe.map .content) schema.readmeVisitor
                , directDependenciesVisitor = compactProjectDataVisitors identity schema.directDependenciesVisitor
                , dependenciesVisitor = compactProjectDataVisitors identity schema.dependenciesVisitor
                , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
                , moduleContextCreator = Just (initContextCreator identity)
                , folder = Nothing
                , providesFixes = schema.providesFixes
                , traversalType = AllModulesInParallel
                , finalEvaluationFn = Nothing
                , dataExtractor = Nothing
                }
                |> fromProjectRuleSchema

        Nothing ->
            ProjectRuleSchema
                { name = schema.name
                , initialProjectContext = ()
                , elmJsonVisitor = Nothing
                , readmeVisitor = Nothing
                , directDependenciesVisitor = Nothing
                , dependenciesVisitor = Nothing
                , moduleVisitors = [ removeExtensibleRecordTypeVariable (always moduleVisitor) ]
                , moduleContextCreator = Just schema.moduleContextCreator
                , folder = Nothing
                , providesFixes = schema.providesFixes
                , traversalType = AllModulesInParallel
                , finalEvaluationFn = Nothing
                , dataExtractor = Nothing
                }
                |> fromProjectRuleSchema


compactProjectDataVisitors : (rawData -> data) -> Maybe (data -> moduleContext -> moduleContext) -> Maybe (rawData -> moduleContext -> ( List nothing, moduleContext ))
compactProjectDataVisitors getData maybeVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just (\rawData moduleContext -> ( [], visitor (getData rawData) moduleContext ))



-- PROJECT RULES


{-| Represents a schema for a project [`Rule`](#Rule).

See the documentation for [`newProjectRuleSchema`](#newProjectRuleSchema) for
how to create a project rule.

-}
type ProjectRuleSchema schemaState projectContext moduleContext
    = ProjectRuleSchema (ProjectRuleSchemaData projectContext moduleContext)


type alias ProjectRuleSchemaData projectContext moduleContext =
    { name : String
    , initialProjectContext : projectContext
    , elmJsonVisitor : Maybe (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    , readmeVisitor : Maybe (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    , directDependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , dependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , moduleVisitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
    , moduleContextCreator : Maybe (ContextCreator projectContext moduleContext)
    , folder : Maybe (Folder projectContext moduleContext)
    , providesFixes : Bool

    -- TODO Jeroen Only allow to set it if there is a folder, but not several times
    , traversalType : TraversalType

    -- TODO Jeroen Only allow to set it if there is a folder and module visitors?
    , finalEvaluationFn : Maybe (projectContext -> List (Error {}))

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
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

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
        , elmJsonVisitor = Nothing
        , readmeVisitor = Nothing
        , directDependenciesVisitor = Nothing
        , dependenciesVisitor = Nothing
        , moduleVisitors = []
        , moduleContextCreator = Nothing
        , folder = Nothing
        , providesFixes = False
        , traversalType = AllModulesInParallel
        , finalEvaluationFn = Nothing
        , dataExtractor = Nothing
        }


{-| Create a [`Rule`](#Rule) from a configured [`ProjectRuleSchema`](#ProjectRuleSchema).
-}
fromProjectRuleSchema : ProjectRuleSchema { schemaState | withModuleContext : Forbidden, hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
fromProjectRuleSchema ((ProjectRuleSchema schema) as projectRuleSchema) =
    Rule
        { name = schema.name
        , id = 0
        , exceptions = Exceptions.init
        , requestedData =
            RequestedData.combine
                (Maybe.map requestedDataFromContextCreator schema.moduleContextCreator)
                (Maybe.map (.fromModuleToProject >> requestedDataFromContextCreator) schema.folder)
        , extractsData = schema.dataExtractor /= Nothing
        , providesFixes = schema.providesFixes
        , ruleImplementation =
            \reviewOptions ruleId exceptions fixedErrors project ruleProjectVisitors ->
                runProjectVisitor
                    { reviewOptions = reviewOptions
                    , projectVisitor = fromProjectRuleSchemaToRunnableProjectVisitor projectRuleSchema
                    , exceptions = exceptions
                    }
                    ruleProjectVisitors
                    (removeUnknownModulesFromInitialCache project (initialCacheMarker schema.name ruleId emptyCache))
                    fixedErrors
                    project
        , ruleProjectVisitor = Ok (\ruleData -> createRuleProjectVisitor schema ruleData)
        }


initialCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
initialCacheMarker _ _ cache =
    cache


removeUnknownModulesFromInitialCache : ValidProject -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
removeUnknownModulesFromInitialCache validProject projectRuleCache =
    { projectRuleCache | moduleContexts = Dict.filter (\path _ -> ValidProject.doesModuleExist path validProject) projectRuleCache.moduleContexts }


emptyCache : ProjectRuleCache projectContext
emptyCache =
    { elmJson = Nothing
    , readme = Nothing
    , dependencies = Nothing
    , moduleContexts = Dict.empty
    , finalEvaluationErrors = Nothing
    , extract = Nothing
    }


fromProjectRuleSchemaToRunnableProjectVisitor : ProjectRuleSchema schemaState projectContext moduleContext -> RunnableProjectVisitor projectContext moduleContext
fromProjectRuleSchemaToRunnableProjectVisitor (ProjectRuleSchema schema) =
    { name = schema.name
    , initialProjectContext = schema.initialProjectContext
    , elmJsonVisitor = schema.elmJsonVisitor
    , readmeVisitor = schema.readmeVisitor
    , directDependenciesVisitor = schema.directDependenciesVisitor
    , dependenciesVisitor = schema.dependenciesVisitor
    , moduleVisitor =
        mergeModuleVisitors schema.name schema.initialProjectContext schema.moduleContextCreator schema.moduleVisitors
            |> Maybe.map (Tuple.mapFirst fromModuleRuleSchemaToRunnableModuleVisitor)
    , traversalAndFolder =
        case ( schema.traversalType, schema.folder ) of
            ( AllModulesInParallel, _ ) ->
                TraverseAllModulesInParallel schema.folder

            ( ImportedModulesFirst, Just folder ) ->
                TraverseImportedModulesFirst folder

            ( ImportedModulesFirst, Nothing ) ->
                TraverseAllModulesInParallel Nothing
    , finalEvaluationFn = schema.finalEvaluationFn
    , providesFixes = schema.providesFixes
    , requestedData =
        case schema.moduleContextCreator of
            Just (ContextCreator _ requestedData) ->
                requestedData

            Nothing ->
                RequestedData.none
    }


mergeModuleVisitors :
    String
    -> projectContext
    -> Maybe (ContextCreator projectContext moduleContext)
    -> List (ModuleRuleSchema schemaState1 moduleContext -> ModuleRuleSchema schemaState2 moduleContext)
    -> Maybe ( ModuleRuleSchema {} moduleContext, ContextCreator projectContext moduleContext )
mergeModuleVisitors ruleName_ initialProjectContext maybeModuleContextCreator visitors =
    case maybeModuleContextCreator of
        Nothing ->
            Nothing

        Just moduleContextCreator ->
            if List.isEmpty visitors then
                Nothing

            else
                Just (mergeModuleVisitorsHelp ruleName_ initialProjectContext moduleContextCreator visitors)


mergeModuleVisitorsHelp :
    String
    -> projectContext
    -> ContextCreator projectContext moduleContext
    -> List (ModuleRuleSchema schemaState1 moduleContext -> ModuleRuleSchema schemaState2 moduleContext)
    -> ( ModuleRuleSchema {} moduleContext, ContextCreator projectContext moduleContext )
mergeModuleVisitorsHelp ruleName_ initialProjectContext moduleContextCreator visitors =
    let
        dummyAst : Elm.Syntax.File.File
        dummyAst =
            { moduleDefinition =
                Node.Node Range.emptyRange
                    (Module.NormalModule
                        { moduleName = Node.Node Range.emptyRange []
                        , exposingList = Node.Node Range.emptyRange (Exposing.Explicit [])
                        }
                    )
            , imports = []
            , declarations = []
            , comments = []
            }

        dummyAvailableData : AvailableData
        dummyAvailableData =
            { ast = dummyAst
            , moduleKey = ModuleKey "dummy"
            , moduleNameLookupTable = ModuleNameLookupTableInternal.empty []
            , extractSourceCode = always "dummy"
            , filePath = "dummy file path"
            , isInSourceDirectories = True
            , isFileIgnored = False
            }

        initialModuleContext : moduleContext
        initialModuleContext =
            applyContextCreator dummyAvailableData moduleContextCreator initialProjectContext

        emptyModuleVisitor : ModuleRuleSchema schemaState moduleContext
        emptyModuleVisitor =
            ModuleRuleSchema
                { name = ruleName_
                , initialModuleContext = Just initialModuleContext
                , moduleContextCreator = initContextCreator (always initialModuleContext)
                , moduleDefinitionVisitor = Nothing
                , moduleDocumentationVisitor = Nothing
                , commentsVisitor = Nothing
                , importVisitor = Nothing
                , declarationListVisitor = Nothing
                , declarationVisitorOnEnter = Nothing
                , declarationVisitorOnExit = Nothing
                , expressionVisitorsOnEnter = Nothing
                , expressionVisitorsOnExit = Nothing
                , letDeclarationVisitorOnEnter = Nothing
                , letDeclarationVisitorOnExit = Nothing
                , caseBranchVisitorOnEnter = Nothing
                , caseBranchVisitorOnExit = Nothing
                , finalEvaluationFn = Nothing
                , elmJsonVisitor = Nothing
                , readmeVisitor = Nothing
                , dependenciesVisitor = Nothing
                , directDependenciesVisitor = Nothing
                , providesFixes = False
                }
    in
    ( List.foldl
        (\addVisitors (ModuleRuleSchema moduleVisitorSchema) ->
            addVisitors (ModuleRuleSchema moduleVisitorSchema)
        )
        emptyModuleVisitor
        visitors
        |> (\(ModuleRuleSchema moduleVisitorSchema) -> ModuleRuleSchema moduleVisitorSchema)
    , moduleContextCreator
    )


fromModuleRuleSchemaToRunnableModuleVisitor : ModuleRuleSchema schemaState moduleContext -> RunnableModuleVisitor moduleContext
fromModuleRuleSchemaToRunnableModuleVisitor (ModuleRuleSchema schema) =
    { moduleDefinitionVisitor = schema.moduleDefinitionVisitor
    , moduleDocumentationVisitor = schema.moduleDocumentationVisitor
    , commentsVisitor = schema.commentsVisitor
    , importVisitor = schema.importVisitor
    , declarationListVisitor = schema.declarationListVisitor
    , declarationAndExpressionVisitor = createDeclarationAndExpressionVisitor schema
    , finalEvaluationFn = schema.finalEvaluationFn
    }


createDeclarationAndExpressionVisitor : ModuleRuleSchemaData moduleContext -> List (Node Declaration) -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext )
createDeclarationAndExpressionVisitor schema =
    if shouldVisitDeclarations schema then
        case createExpressionVisitor schema of
            Just expressionVisitor ->
                \nodes initialErrorsAndContext ->
                    List.foldl
                        (\node acc ->
                            visitDeclaration
                                schema.declarationVisitorOnEnter
                                schema.declarationVisitorOnExit
                                expressionVisitor
                                node
                                acc
                        )
                        initialErrorsAndContext
                        nodes

            Nothing ->
                let
                    visitor : Node Declaration -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext )
                    visitor node acc =
                        visitOnlyDeclaration
                            schema.declarationVisitorOnEnter
                            schema.declarationVisitorOnExit
                            node
                            acc
                in
                \nodes initialErrorsAndContext ->
                    List.foldl visitor initialErrorsAndContext nodes

    else
        case createExpressionVisitor schema of
            Just expressionVisitor ->
                \nodes initialErrorsAndContext ->
                    List.foldl
                        (\node acc -> visitDeclarationButOnlyExpressions expressionVisitor node acc)
                        initialErrorsAndContext
                        nodes

            Nothing ->
                \_ errorsAndContext -> errorsAndContext


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
    -- IGNORE TCO
    Rule
        { name = name
        , id = 0
        , exceptions = Exceptions.init
        , requestedData = RequestedData.none
        , extractsData = False
        , providesFixes = False
        , ruleImplementation = \_ _ _ fixedErrors project _ -> { fixedErrors = fixedErrors, ruleProjectVisitors = [], project = project }
        , ruleProjectVisitor = Err configurationError_
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
        , exposedFunctions = Dict.union newContext.modules previousContext.modules

        -- Collect the used functions from the new context and the previous one
        , used = Set.union newContext.used previousContext.used
        }

    finalEvaluationForProject : ProjectContext -> List (Rule.Error { useErrorForModule : () })
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
                (\moduleKey moduleNameNode_ projectContext ->
                    functions.fromProjectToModule
                        moduleKey
                        moduleNameNode_
                        projectContext
                )
                |> withModuleKey
                |> withModuleNameNode
    in
    ProjectRuleSchema
        { schema
            | moduleContextCreator = Just moduleContextCreator
            , folder =
                Just
                    { fromModuleToProject =
                        initContextCreator (\moduleKey moduleNameNode_ moduleContext -> functions.fromModuleToProject moduleKey moduleNameNode_ moduleContext)
                            |> withModuleKey
                            |> withModuleNameNode
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
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.fromProjectRuleSchema

    fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
    fromProjectToModule =
        Rule.initContextCreator
            (\projectContext ->
                { -- something
                }
            )

    fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
    fromModuleToProject =
        Rule.initContextCreator
            (\moduleKey moduleName moduleContext ->
                { moduleKeys = Dict.singleton moduleName moduleKey
                }
            )
            |> Rule.withModuleKey
            |> Rule.withModuleName

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
    ProjectRuleSchema { schema | elmJsonVisitor = Just (combineVisitors (removeErrorPhantomTypeFromVisitor visitor) schema.elmJsonVisitor) }


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
    ProjectRuleSchema { schema | readmeVisitor = Just (combineVisitors (removeErrorPhantomTypeFromVisitor visitor) schema.readmeVisitor) }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will examine the project's
[dependencies](./Review-Project-Dependency).

It works exactly like [`withDependenciesModuleVisitor`](#withDependenciesModuleVisitor). The visitor will be called before any
module is evaluated.

-}
withDependenciesProjectVisitor :
    (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDependenciesProjectVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | dependenciesVisitor = Just (combineVisitors (removeErrorPhantomTypeFromVisitor visitor) schema.dependenciesVisitor) }


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) which will examine the project's
direct [dependencies](./Review-Project-Dependency).

It works exactly like [`withDependenciesModuleVisitor`](#withDependenciesModuleVisitor). The visitor will be called before any
module is evaluated.

-}
withDirectDependenciesProjectVisitor :
    (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withDirectDependenciesProjectVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | directDependenciesVisitor = Just (combineVisitors (removeErrorPhantomTypeFromVisitor visitor) schema.directDependenciesVisitor) }


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
        combinedVisitor : projectContext -> List (Error {})
        combinedVisitor =
            case schema.finalEvaluationFn of
                Nothing ->
                    \projectContext ->
                        visitor projectContext
                            |> removeErrorPhantomTypes

                Just previousVisitor ->
                    -- TODO Optimize this
                    \projectContext -> List.append (visitor projectContext |> removeErrorPhantomTypes) (previousVisitor projectContext)
    in
    ProjectRuleSchema { schema | finalEvaluationFn = Just combinedVisitor }


type Extract
    = Extract Encode.Value


{-| Extract arbitrary data from the codebase, which can be accessed by running

```bash
elm-review --report=json --extract
```

and by reading the value at `<output>.extracts["YourRuleName"]` in the output.

    import Json.Encode
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "Some.Rule.Name" initialContext
            -- visitors to collect information...
            |> Rule.withDataExtractor dataExtractor
            |> Rule.fromProjectRuleSchema

    dataExtractor : ProjectContext -> Encode.Value
    dataExtractor projectContext =
        Json.Encode.list
            (\thing ->
                Json.Encode.object
                    [ ( "name", Json.Encode.string thing.name )
                    , ( "value", Json.Encode.int thing.value )
                    ]
            )
            projectContext.things

-}
withDataExtractor :
    (projectContext -> Encode.Value)
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema schemaState projectContext moduleContext
withDataExtractor dataExtractor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | dataExtractor = Just (\context -> Extract (dataExtractor context)) }


removeErrorPhantomTypeFromVisitor : (element -> projectContext -> ( List (Error b), projectContext )) -> (element -> projectContext -> ( List (Error {}), projectContext ))
removeErrorPhantomTypeFromVisitor function =
    \element projectContext ->
        function element projectContext
            |> Tuple.mapFirst removeErrorPhantomTypes


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


setFilePathIfUnset : String -> Error scope -> Error scope
setFilePathIfUnset filePath ((Error err) as rawError) =
    if err.filePath == "" then
        Error { err | filePath = filePath }

    else
        rawError


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's [module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`) and report patterns.

The following example forbids having `_` in any part of a module name.

    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnderscoreInModuleName" ()
            |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> List (Rule.Error {})
    moduleDefinitionVisitor node =
        if List.any (String.contains "_") (Node.value node |> Module.moduleName) then
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

This visitor will give you access to the list of comments (in source order) in
the module all at once. Note that comments that are parsed as documentation comments by
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
are not included in this list.

As such, the following comments are included (✅) / excluded (❌):

  - ✅ Module documentation (`{-| -}`)
  - ✅ Port documentation comments (`{-| -}`)
  - ✅ Top-level comments not internal to a function/type/etc.
  - ✅ Comments internal to a function/type/etc.
  - ❌ Function/type/type alias documentation comments (`{-| -}`)

The following example forbids words like "TODO" appearing in a comment.

    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoTodoComment" ()
            |> Rule.withSimpleCommentsVisitor commentsVisitor
            |> Rule.fromModuleRuleSchema

    commentsVisitor : List (Node String) -> List (Rule.Error {})
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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoCoreHtml" ()
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> List (Rule.Error {})
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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingTypeAnnotation" ()
            |> Rule.withSimpleDeclarationVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    declarationVisitor : Node Declaration -> List (Rule.Error {})
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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    expressionVisitor : Node Expression -> List (Rule.Error {})
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
    import Review.Rule as Rule exposing (Rule)

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

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | elmJsonVisitor = Just (combineContextOnlyVisitor visitor schema.elmJsonVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit
the project's `README.md` file.
-}
withReadmeModuleVisitor :
    (Maybe String -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withReadmeModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | readmeVisitor = Just (combineContextOnlyVisitor visitor schema.readmeVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will examine the project's
[dependencies](./Review-Project-Dependency).

You can use this look at the modules contained in dependencies, which can make the rule very precise when it targets
specific functions.

-}
withDependenciesModuleVisitor :
    (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withDependenciesModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | dependenciesVisitor = Just (combineContextOnlyVisitor visitor schema.dependenciesVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will examine the project's
direct [dependencies](./Review-Project-Dependency).

You can use this look at the modules contained in dependencies, which can make the rule very precise when it targets
specific functions.

-}
withDirectDependenciesModuleVisitor :
    (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withDirectDependenciesModuleVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | directDependenciesVisitor = Just (combineContextOnlyVisitor visitor schema.directDependenciesVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

The following example forbids the use of `Html.button` except in the "Button" module.
The example is simplified to only forbid the use of the `Html.button` expression.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type Context
        = HtmlButtonIsAllowed
        | HtmlButtonIsForbidden

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHtmlButton" HtmlButtonIsForbidden
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Rule.Error {}), Context )
    moduleDefinitionVisitor node context =
        if (Node.value node |> Module.moduleName) == [ "Button" ] then
            ( [], HtmlButtonIsAllowed )

        else
            ( [], HtmlButtonIsForbidden )

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | moduleDefinitionVisitor = Just (combineVisitors visitor schema.moduleDefinitionVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's comments, collect data in
the `context` and/or report patterns.

This visitor will give you access to the list of comments (in source order) in
the module all at once. Note that comments that are parsed as documentation comments by
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
are not included in this list.

As such, the following comments are included (✅) / excluded (❌):

  - ✅ Module documentation (`{-| -}`)
  - ✅ Port documentation comments (`{-| -}`)
  - ✅ Top-level comments not internal to a function/type/etc.
  - ✅ Comments internal to a function/type/etc.
  - ❌ Function/type/type alias documentation comments (`{-| -}`)

Tip: If you do not need to collect data in this visitor, you may wish to use the
simpler [`withSimpleCommentsVisitor`](#withSimpleCommentsVisitor) function.

Tip: If you only need to access the module documentation, you should use
[`withModuleDocumentationVisitor`](#withModuleDocumentationVisitor) instead.

-}
withCommentsVisitor : (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCommentsVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | commentsVisitor = Just (combineVisitors visitor schema.commentsVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's documentation, collect data in
the `context` and/or report patterns.

This visitor will give you access to the module documentation comment. Modules don't always have a documentation.
When that is the case, the visitor will be called with the `Nothing` as the module documentation.

-}
withModuleDocumentationVisitor : (Maybe (Node String) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withModuleDocumentationVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | moduleDocumentationVisitor = Just (combineVisitors visitor schema.moduleDocumentationVisitor) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition, collect data
in the `context` and/or report patterns.

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`).

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

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

    importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | importVisitor = Just (combineVisitors visitor schema.importVisitor) }


{-| **@deprecated**

Use [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) and [`withDeclarationExitVisitor`](#withDeclarationExitVisitor) instead.
In the next major version, this function will be removed and [`withDeclarationEnterVisitor`](#withDeclarationEnterVisitor) will be renamed to `withDeclarationVisitor`.

**/@deprecated**

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
    import Review.Rule as Rule exposing (Rule)

    type ExposedFunctions
        = All
        | OnlySome (List String)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingDocumentationForExposedFunctions" (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List (Rule.Error {}), ExposedFunctions )
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

    declarationVisitor : Node Declaration -> Rule.Direction -> ExposedFunctions -> ( List (Rule.Error {}), ExposedFunctions )
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
            | declarationVisitorOnEnter = Just (combineVisitors (\node ctx -> visitor node OnEnter ctx) schema.declarationVisitorOnEnter)
            , declarationVisitorOnExit = Just (combineExitVisitors (\node ctx -> visitor node OnExit ctx) schema.declarationVisitorOnExit)
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
    import Review.Rule as Rule exposing (Rule)

    type ExposedFunctions
        = All
        | OnlySome (List String)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingDocumentationForExposedFunctions" (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationEnterVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List (Rule.Error {}), ExposedFunctions )
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

    declarationVisitor : Node Declaration -> ExposedFunctions -> ( List (Rule.Error {}), ExposedFunctions )
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
    ModuleRuleSchema { schema | declarationVisitorOnEnter = Just (combineVisitors visitor schema.declarationVisitorOnEnter) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

The following example reports unused parameters from top-level declarations.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
            |> Rule.withDeclarationExitVisitor declarationExitVisitor
            -- Omitted, but this marks parameters as used
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    declarationEnterVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
    declarationEnterVisitor node context =
        case Node.value node of
            Declaration.FunctionDeclaration function ->
                ( [], registerArguments context function )

            _ ->
                ( [], context )

    declarationExitVisitor : Node Declaration -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | declarationVisitorOnExit = Just (combineExitVisitors visitor schema.declarationVisitorOnExit) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
to collect data and/or report patterns. The declarations will be in the same
order that they appear in the source code.

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
    ModuleRuleSchema { schema | declarationListVisitor = Just (combineVisitors visitor schema.declarationListVisitor) }


{-| **@deprecated**

Use [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) and [`withExpressionExitVisitor`](#withExpressionExitVisitor) instead.
In the next major version, this function will be removed and [`withExpressionEnterVisitor`](#withExpressionEnterVisitor) will be renamed to `withExpressionVisitor`.

**/@deprecated**

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
    import Review.Rule as Rule exposing (Rule)

    type Context
        = DebugLogWasNotImported
        | DebugLogWasImported

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
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

    expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
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
            | expressionVisitorsOnEnter = Just (combineVisitors (\node ctx -> visitor node OnEnter ctx) schema.expressionVisitorsOnEnter)
            , expressionVisitorsOnExit = Just (combineExitVisitors (\node ctx -> visitor node OnExit ctx) schema.expressionVisitorsOnExit)
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
    import Review.Rule as Rule exposing (Rule)

    type Context
        = DebugLogWasNotImported
        | DebugLogWasImported

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
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

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | expressionVisitorsOnEnter = Just (combineVisitors visitor schema.expressionVisitorsOnEnter) }


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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withExpressionEnterVisitor expressionEnterVisitor
            |> Rule.withExpressionExitVisitor expressionExitVisitor
            |> Rule.fromModuleRuleSchema

    expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
    expressionEnterVisitor node context =
        case Node.value node of
            Expression.FunctionOrValue moduleName name ->
                ( [], markVariableAsUsed context name )

            -- Find variables declared in let expression
            Expression.LetExpression letBlock ->
                ( [], registerVariables context letBlock )

            _ ->
                ( [], context )

    expressionExitVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
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
    ModuleRuleSchema { schema | expressionVisitorsOnExit = Just (combineExitVisitors visitor schema.expressionVisitorsOnExit) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
case branches when entering the branch.

The visitor can be very useful if you need to change the context when inside a case branch.

The visitors would be called in the following order (ignore the expression visitor if you don't have one):

    x =
        case evaluated of
            Pattern1 ->
                expression1

            Pattern2 ->
                expression2

1.  Expression visitor (enter) for the entire case expression.
2.  Expression visitor (enter then exit) for `evaluated`
3.  Case branch visitor (enter) for `( Pattern1, expression1 )`
4.  Expression visitor (enter then exit) for `expression1`
5.  Case branch visitor (exit) for `( Pattern1, expression1 )`
6.  Case branch visitor (enter) for `( Pattern2, expression2 )`
7.  Expression visitor (enter then exit) for `expression2`
8.  Case branch visitor (exit) for `( Pattern2, expression2 )`
9.  Expression visitor (exit) for the entire case expression.

You can use [`withCaseBranchExitVisitor`](#withCaseBranchExitVisitor) to visit the node on exit.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Pattern exposing (Pattern)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnusedCaseVariables" ( [], [] )
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.withCaseBranchEnterVisitor caseBranchEnterVisitor
            |> Rule.withCaseBranchExitVisitor caseBranchExitVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        ( List String, List (List String) )

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
    expressionVisitor node (( scope, parentScopes ) as context) =
        case context of
            Expression.FunctionOrValue [] name ->
                ( [], ( name :: used, parentScopes ) )

            _ ->
                ( [], context )

    caseBranchEnterVisitor : Node Expression.LetBlock -> ( Node Pattern, Node Expression ) -> Context -> List ( Rule.Error {}, Context )
    caseBranchEnterVisitor _ _ ( scope, parentScopes ) =
        -- Entering a new scope every time we enter a new branch
        ( [], ( [], scope :: parentScopes ) )

    caseBranchExitVisitor : Node Expression.LetBlock -> ( Node Pattern, Node Expression ) -> Context -> List ( Rule.Error {}, Context )
    caseBranchExitVisitor _ ( pattern, _ ) ( scope, parentScopes ) =
        -- Exiting the current scope every time we enter a new branch, and reporting the patterns that weren't used
        let
            namesFromPattern =
                findNamesFromPattern pattern

            ( unusedPatterns, unmatchedUsed ) =
                findUnused namesFromPattern scope

            newScopes =
                case parentScopes of
                    head :: tail ->
                        ( unmatchedUsed ++ head, tail )

                    [] ->
                        ( unmatched, [] )
        in
        ( List.map errorForUnused unusedPatterns, newScopes )

For convenience, the entire case expression is passed as the first argument.

-}
withCaseBranchEnterVisitor : (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCaseBranchEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | caseBranchVisitorOnEnter = Just (combineVisitors2 visitor schema.caseBranchVisitorOnEnter) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
case branches when exiting the branch.

See the documentation for [`withCaseBranchEnterVisitor`](#withCaseBranchEnterVisitor) for explanations and an example.

-}
withCaseBranchExitVisitor : (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withCaseBranchExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | caseBranchVisitorOnExit = Just (combineExitVisitors2 visitor schema.caseBranchVisitorOnExit) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
let declarations branches when entering the declaration.

The visitor can be very useful if you need to change the context when inside a let declaration.

The visitors would be called in the following order (ignore the expression visitor if you don't have one):

    x =
        let
            declaration1 =
                expression1

            declaration2 =
                expression2
        in
        letInValue

1.  Expression visitor (enter) for the entire let expression.
2.  Let declaration visitor (enter) for `( declaration1, expression1 )`
3.  Expression visitor (enter then exit) for `expression1`
4.  Let declaration visitor (exit) for `( declaration1, expression1 )`
5.  Let declaration visitor (enter) for `( declaration2, expression2 )`
6.  Expression visitor (enter then exit) for `expression2`
7.  Let declaration visitor (exit) for `( declaration2, expression2 )`
8.  Expression visitor (enter then exit) for `letInValue`
9.  Expression visitor (exit) for the entire let expression.

You can use [`withLetDeclarationExitVisitor`](#withLetDeclarationExitVisitor) to visit the node on exit.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnusedLetFunctionParameters" ( [], [] )
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.withLetDeclarationEnterVisitor letDeclarationEnterVisitor
            |> Rule.withLetDeclarationExitVisitor letDeclarationExitVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        ( List String, List (List String) )

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
    expressionVisitor node (( scope, parentScopes ) as context) =
        case context of
            Expression.FunctionOrValue [] name ->
                ( [], ( name :: used, parentScopes ) )

            _ ->
                ( [], context )

    letDeclarationEnterVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> Context -> List ( Rule.Error {}, Context )
    letDeclarationEnterVisitor _ letDeclaration (( scope, parentScopes ) as context) =
        case Node.value letDeclaration of
            Expression.LetFunction _ ->
                ( [], ( [], scope :: parentScopes ) )

            Expression.LetDestructuring _ ->
                ( [], context )

    letDeclarationExitVisitor : Node Expression.LetBlock -> Node Expression.LetDeclaration -> Context -> List ( Rule.Error {}, Context )
    letDeclarationExitVisitor _ letDeclaration (( scope, parentScopes ) as context) =
        case Node.value letDeclaration of
            Expression.LetFunction _ ->
                let
                    namesFromPattern =
                        findNamesFromArguments letFunction

                    ( unusedArguments, unmatchedUsed ) =
                        findUnused namesFromPattern scope

                    newScopes =
                        case parentScopes of
                            head :: tail ->
                                ( unmatchedUsed ++ head, tail )

                            [] ->
                                ( unmatched, [] )
                in
                ( List.map errorForUnused unusedArguments, newScopes )

            Expression.LetDestructuring _ ->
                ( [], context )

For convenience, the entire let expression is passed as the first argument.

-}
withLetDeclarationEnterVisitor : (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withLetDeclarationEnterVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | letDeclarationVisitorOnEnter = Just (combineVisitors2 visitor schema.letDeclarationVisitorOnEnter) }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
let declarations branches when entering the declaration.

See the documentation for [`withLetDeclarationEnterVisitor`](#withLetDeclarationEnterVisitor) for explanations and an example.

-}
withLetDeclarationExitVisitor : (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withLetDeclarationExitVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | letDeclarationVisitorOnExit = Just (combineExitVisitors2 visitor schema.letDeclarationVisitorOnExit) }


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
    import Review.Rule as Rule exposing (Rule)

    type alias Context =
        Dict (List String) Range

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUsingBothHtmlAndHtmlStyled" Dict.empty
            |> Rule.withImportVisitor importVisitor
            |> Rule.withFinalModuleEvaluation finalEvaluation
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List (Rule.Error {}), Context )
    importVisitor node context =
        ( [], Dict.insert (Node.value node |> .moduleName |> Node.value) (Node.range node) context )

    finalEvaluation : Context -> List (Rule.Error {})
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
    let
        combinedVisitor : moduleContext -> List (Error {})
        combinedVisitor =
            case schema.finalEvaluationFn of
                Nothing ->
                    visitor

                Just previousVisitor ->
                    \context -> List.append (visitor context) (previousVisitor context)
    in
    ModuleRuleSchema { schema | finalEvaluationFn = Just combinedVisitor }


combineVisitors :
    (a -> context -> ( List error, context ))
    -> Maybe (a -> context -> ( List error, context ))
    -> a
    -> context
    -> ( List error, context )
combineVisitors newVisitor maybePreviousVisitor =
    case maybePreviousVisitor of
        Nothing ->
            newVisitor

        Just previousVisitor ->
            \node moduleContext ->
                let
                    ( errorsAfterFirstVisit, contextAfterFirstVisit ) =
                        previousVisitor node moduleContext

                    ( errorsAfterSecondVisit, contextAfterSecondVisit ) =
                        newVisitor node contextAfterFirstVisit
                in
                ( List.append errorsAfterFirstVisit errorsAfterSecondVisit, contextAfterSecondVisit )


combineContextOnlyVisitor :
    (a -> context -> context)
    -> Maybe (a -> context -> context)
    -> a
    -> context
    -> context
combineContextOnlyVisitor newVisitor maybePreviousVisitor =
    case maybePreviousVisitor of
        Nothing ->
            newVisitor

        Just previousVisitor ->
            \a moduleContext ->
                moduleContext
                    |> previousVisitor a
                    |> newVisitor a


combineVisitors2 :
    (a -> b -> context -> ( List error, context ))
    -> Maybe (a -> b -> context -> ( List error, context ))
    -> a
    -> b
    -> context
    -> ( List error, context )
combineVisitors2 newVisitor maybePreviousVisitor =
    case maybePreviousVisitor of
        Nothing ->
            newVisitor

        Just previousVisitor ->
            \a b moduleContext ->
                let
                    ( errorsAfterFirstVisit, contextAfterFirstVisit ) =
                        previousVisitor a b moduleContext

                    ( errorsAfterSecondVisit, contextAfterSecondVisit ) =
                        newVisitor a b contextAfterFirstVisit
                in
                ( List.append errorsAfterFirstVisit errorsAfterSecondVisit, contextAfterSecondVisit )


combineExitVisitors :
    (a -> context -> ( List error, context ))
    -> Maybe (a -> context -> ( List error, context ))
    -> a
    -> context
    -> ( List error, context )
combineExitVisitors newVisitor maybePreviousVisitor =
    case maybePreviousVisitor of
        Nothing ->
            newVisitor

        Just previousVisitor ->
            \node moduleContext ->
                let
                    ( errorsAfterFirstVisit, contextAfterFirstVisit ) =
                        newVisitor node moduleContext

                    ( errorsAfterSecondVisit, contextAfterSecondVisit ) =
                        previousVisitor node contextAfterFirstVisit
                in
                ( List.append errorsAfterFirstVisit errorsAfterSecondVisit, contextAfterSecondVisit )


combineExitVisitors2 :
    (a -> b -> context -> ( List error, context ))
    -> Maybe (a -> b -> context -> ( List error, context ))
    -> a
    -> b
    -> context
    -> ( List error, context )
combineExitVisitors2 newVisitor maybePreviousVisitor =
    case maybePreviousVisitor of
        Nothing ->
            newVisitor

        Just previousVisitor ->
            \a b moduleContext ->
                let
                    ( errorsAfterFirstVisit, contextAfterFirstVisit ) =
                        newVisitor a b moduleContext

                    ( errorsAfterSecondVisit, contextAfterSecondVisit ) =
                        previousVisitor a b contextAfterFirstVisit
                in
                ( List.append errorsAfterFirstVisit errorsAfterSecondVisit, contextAfterSecondVisit )



-- ERRORS


{-| Represents an error found by a [`Rule`](#Rule). These are created by the rules.
-}
type Error scope
    = Error InternalError


{-| Make this error prevent extracting data using [`withDataExtractor`](#withDataExtractor).

Use this if the rule extracts data and an issue is discovered that would make the extraction
output incorrect data.

    Rule.error
        { message = "..."
        , details = [ "..." ]
        }
        (Node.range node)
        |> Rule.preventExtract

-}
preventExtract : Error a -> Error a
preventExtract (Error err) =
    Error (Review.Error.preventExtract err)


doesPreventExtract : Error a -> Bool
doesPreventExtract (Error err) =
    Review.Error.doesPreventExtract err


removeErrorPhantomTypes : List (Error something) -> List (Error {})
removeErrorPhantomTypes list =
    -- This function could be replaced by `identity` at runtime
    List.map (\(Error err) -> Error err) list


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

    error : Node a -> Error {}
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
    Error
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = Nothing
        , target = Review.Error.Module
        , preventsExtract = False
        }


{-| Creates an [`Error`](#Error), just like the [`error`](#error) function, but
provides an automatic fix that the user can apply.

    import Review.Fix as Fix

    error : Node a -> Error {}
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
    Error
        { message = message
        , ruleName = ""
        , details = details
        , range = range
        , filePath = path
        , fixes = Nothing
        , target = Review.Error.Module
        , preventsExtract = False
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
    Error
        { message = errorInfo.message
        , ruleName = ""
        , details = errorInfo.details
        , range = errorInfo.range
        , filePath = path
        , fixes = Nothing
        , target = Review.Error.ElmJson
        , preventsExtract = False
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
    Error
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
        , preventsExtract = False
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
    Error
        { message = message
        , ruleName = ""
        , filePath = path
        , details = details
        , range = range
        , fixes = Nothing
        , target = Review.Error.Readme
        , preventsExtract = False
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
    Error
        { filePath = "GLOBAL ERROR"
        , ruleName = ""
        , message = message
        , details = details
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.Global
        , preventsExtract = False
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
    Error
        { filePath = "GLOBAL ERROR"
        , ruleName = ""
        , message = message
        , details = details
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.UserGlobal
        , preventsExtract = False
        }


parsingError : String -> Review.Error.ReviewError
parsingError path =
    Review.Error.ReviewError
        { filePath = path
        , ruleName = "ParsingError"
        , message = path ++ " is not a correct Elm module"
        , details =
            [ "I could not understand the content of this file, and this prevents me from analyzing it. It is highly likely that the contents of the file is not correct Elm code."
            , "I need this file to be fixed before analyzing the rest of the project. If I didn't, I would potentially report incorrect things."
            , "Hint: Try running `elm make`. The compiler should give you better hints on how to resolve the problem."
            ]
        , range = Range.emptyRange
        , fixes = Nothing
        , target = Review.Error.Module
        , preventsExtract = False
        }


{-| Give a list of fixes to automatically fix the error.

    import Review.Fix as Fix

    error : Node a -> Error {}
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
errorToReviewError (Error err) =
    Review.Error.ReviewError err


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
mapInternalError fn (Error err) =
    Error (fn err)



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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForDirectories [ "tests/" ]

    expressionVisitor : Node Expression -> List (Rule.Error {})
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
        , id = rule.id
        , exceptions = Exceptions.addDirectories directories rule.exceptions
        , requestedData = rule.requestedData
        , extractsData = rule.extractsData
        , providesFixes = rule.providesFixes
        , ruleImplementation = rule.ruleImplementation
        , ruleProjectVisitor = rule.ruleProjectVisitor
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
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHtmlButton"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForFiles [ "src/Button.elm" ]

    expressionVisitor : Node Expression -> List (Rule.Error {})
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
        , id = rule.id
        , exceptions = Exceptions.addFiles files rule.exceptions
        , requestedData = rule.requestedData
        , extractsData = rule.extractsData
        , providesFixes = rule.providesFixes
        , ruleImplementation = rule.ruleImplementation
        , ruleProjectVisitor = rule.ruleProjectVisitor
        }


{-| Filter the files to report errors for.

Use it to control precisely which files the rule applies or does not apply to. For example, you
might have written a rule that should only be applied to one specific file.

    config : List Rule
    config =
        [ Some.Rule.rule
            |> Rule.filterErrorsForFiles (\path -> path == "src/Some/File.elm")
        , Some.Other.Rule.rule
        ]

If you want to specify a condition for all of your rules, you can apply
`filterErrorsForFiles` like this:

     config : List Rule
     config =
         [ Some.Rule.For.Tests.rule
         , Some.Other.Rule.rule
         ]
             |> List.map (Rule.filterErrorsForFiles (String.startsWith "tests/"))

The received paths will be relative to the `elm.json` file, just like the ones for the
`elm.json`'s `source-directories`, and will be formatted in the Unix style `src/Some/File.elm`.

You can apply `filterErrorsForFiles` several times for a rule, the conditions will get
compounded, following the behavior of `List.filter`.

When [`ignoreErrorsForFiles`](#ignoreErrorsForFiles) or [`ignoreErrorsForDirectories`](#ignoreErrorsForDirectories)
are used in combination with this function, all constraints are observed.

You can also use it when writing a rule. We can hardcode in the rule that a rule
is only applicable to a folder, like `src/Api/` for instance. The following example
forbids using strings with hardcoded URLs, but only in the `src/Api/` folder.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHardcodedURLs" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.filterErrorsForFiles (String.startsWith "src/Api/")

    expressionVisitor : Node Expression -> List (Rule.Error {})
    expressionVisitor node =
        case Node.value node of
            Expression.Literal string ->
                if isUrl string then
                    [ Rule.error
                        { message = "Do not use hardcoded URLs in the API modules"
                        , details = [ "Hardcoded URLs should never make it to production. Please refer to the documentation of the `Endpoint` module." ]
                        }
                        (Node.range node)
                    ]

                else
                    []

            _ ->
                []

-}
filterErrorsForFiles : (String -> Bool) -> Rule -> Rule
filterErrorsForFiles condition (Rule rule) =
    Rule
        { name = rule.name
        , id = rule.id
        , exceptions = Exceptions.addFilter condition rule.exceptions
        , requestedData = rule.requestedData
        , extractsData = rule.extractsData
        , providesFixes = rule.providesFixes
        , ruleImplementation = rule.ruleImplementation
        , ruleProjectVisitor = rule.ruleProjectVisitor
        }



-- VISITOR
-- TODO BREAKING CHANGE Move this into a separate module later on


type alias RunnableProjectVisitor projectContext moduleContext =
    { name : String
    , initialProjectContext : projectContext
    , elmJsonVisitor : Maybe (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
    , readmeVisitor : Maybe (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
    , directDependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , dependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
    , moduleVisitor : Maybe ( RunnableModuleVisitor moduleContext, ContextCreator projectContext moduleContext )
    , traversalAndFolder : TraversalAndFolder projectContext moduleContext
    , finalEvaluationFn : Maybe (projectContext -> List (Error {}))
    , requestedData : RequestedData
    , providesFixes : Bool
    }


type alias RunnableModuleVisitor moduleContext =
    { moduleDefinitionVisitor : Maybe (Node Module -> moduleContext -> ( List (Error {}), moduleContext ))
    , moduleDocumentationVisitor : Maybe (Maybe (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
    , commentsVisitor : Maybe (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
    , importVisitor : Maybe (Node Import -> moduleContext -> ( List (Error {}), moduleContext ))
    , declarationListVisitor : Maybe (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext ))
    , declarationAndExpressionVisitor : List (Node Declaration) -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext )
    , finalEvaluationFn : Maybe (moduleContext -> List (Error {}))
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


type alias GraphModule =
    Graph.NodeContext FilePath ()


type alias ModuleCacheEntry projectContext =
    Cache.ModuleEntry (Error {}) projectContext


type alias CacheEntryMaybe projectContext =
    Cache.EntryMaybe (Error {}) projectContext


type alias FinalProjectEvaluationCache projectContext =
    Cache.EntryNoOutputContext (List (Error {})) projectContext


type alias ExtractCache projectContext =
    { inputContext : ContextHash projectContext
    , extract : Extract
    }


qualifyErrors : { ruleName : String, exceptions : Exceptions, filePath : String } -> List (Error {}) -> List (Error {}) -> List (Error {})
qualifyErrors params errors acc =
    List.foldl (\err -> qualifyError params err) acc errors


qualifyError : { ruleName : String, exceptions : Exceptions, filePath : String } -> Error {} -> List (Error {}) -> List (Error {})
qualifyError params err acc =
    let
        newError : Error {}
        newError =
            setFilePathIfUnset params.filePath err
    in
    if Exceptions.isFileWeWantReportsFor params.exceptions (errorFilePathInternal newError) then
        setRuleName params.ruleName newError :: acc

    else
        acc


runProjectVisitor :
    DataToComputeProject projectContext moduleContext
    -> List RuleProjectVisitor
    -> ProjectRuleCache projectContext
    -> FixedErrors
    -> ValidProject
    -> { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
runProjectVisitor ({ projectVisitor, exceptions } as dataToComputeProject) initialRuleProjectVisitors initialCache initialFixedErrors initialProject =
    let
        { project, ruleProjectVisitors, fixedErrors } =
            computeStepsForProject
                dataToComputeProject
                { step = ElmJson { initial = projectVisitor.initialProjectContext }
                , project = initialProject
                , cache = initialCache
                , ruleProjectVisitors = initialRuleProjectVisitors
                , fixedErrors = initialFixedErrors
                }
    in
    { fixedErrors = fixedErrors
    , ruleProjectVisitors = ruleProjectVisitors
    , project = project
    }


finalCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
finalCacheMarker _ _ cache =
    cache


computeFinalContext : RunnableProjectVisitor projectContext moduleContext -> ProjectRuleCache projectContext -> projectContext -> projectContext
computeFinalContext projectVisitor cache projectContext =
    case getFolderFromTraversal projectVisitor.traversalAndFolder of
        Just { foldProjectContexts } ->
            Dict.foldl
                (\_ cacheEntry acc -> foldProjectContexts (Cache.outputContext cacheEntry) acc)
                projectContext
                cache.moduleContexts

        Nothing ->
            projectContext


computeFinalContext2 : ProjectRuleSchemaData projectContext moduleContext -> ProjectRuleCache projectContext -> projectContext
computeFinalContext2 schema cache =
    let
        projectContext : projectContext
        projectContext =
            List.filterMap identity [ cache.dependencies, cache.readme, cache.elmJson ]
                |> List.head
                |> Maybe.map Cache.outputContextMaybe
                |> Maybe.withDefault schema.initialProjectContext

        traversalAndFolder : TraversalAndFolder projectContext moduleContext
        traversalAndFolder =
            case ( schema.traversalType, schema.folder ) of
                ( AllModulesInParallel, _ ) ->
                    TraverseAllModulesInParallel schema.folder

                ( ImportedModulesFirst, Just folder ) ->
                    TraverseImportedModulesFirst folder

                ( ImportedModulesFirst, Nothing ) ->
                    TraverseAllModulesInParallel Nothing
    in
    case getFolderFromTraversal traversalAndFolder of
        Just { foldProjectContexts } ->
            Dict.foldl
                (\_ cacheEntry acc -> foldProjectContexts (Cache.outputContext cacheEntry) acc)
                projectContext
                cache.moduleContexts

        Nothing ->
            projectContext


setRuleName : String -> Error scope -> Error scope
setRuleName ruleName_ error_ =
    mapInternalError (\err -> { err | ruleName = ruleName_ }) error_


errorsFromCache : ProjectRuleCache projectContext -> List (Error {})
errorsFromCache cache =
    List.concat
        [ Dict.foldl (\_ cacheEntry acc -> List.append (Cache.errors cacheEntry) acc) [] cache.moduleContexts
        , Cache.errorsMaybe cache.elmJson
        , Cache.errorsMaybe cache.readme
        , Cache.errorsMaybe cache.dependencies
        , Maybe.map Cache.outputForNoOutput cache.finalEvaluationErrors |> Maybe.withDefault []
        ]



-- VISIT PROJECT


type alias ProjectRuleCache projectContext =
    { elmJson : Maybe (CacheEntryMaybe projectContext)
    , readme : Maybe (CacheEntryMaybe projectContext)
    , dependencies : Maybe (CacheEntryMaybe projectContext)
    , moduleContexts : Dict String (ModuleCacheEntry projectContext)
    , finalEvaluationErrors : Maybe (FinalProjectEvaluationCache projectContext)
    , extract : Maybe (ExtractCache projectContext)
    }


type alias DataToComputeProject projectContext moduleContext =
    { reviewOptions : ReviewOptionsData
    , projectVisitor : RunnableProjectVisitor projectContext moduleContext
    , exceptions : Exceptions
    }


computeStepsForProject :
    DataToComputeProject projectContext moduleContext
    -> { project : ValidProject, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors, step : Step projectContext }
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeStepsForProject dataToComputeProject { project, cache, ruleProjectVisitors, fixedErrors, step } =
    case step of
        ElmJson contexts ->
            computeStepsForProject
                dataToComputeProject
                (computeElmJson dataToComputeProject project contexts.initial cache ruleProjectVisitors fixedErrors)

        Readme contexts ->
            computeStepsForProject
                dataToComputeProject
                (computeReadme dataToComputeProject project contexts cache ruleProjectVisitors fixedErrors)

        Dependencies contexts ->
            computeStepsForProject
                dataToComputeProject
                (computeDependencies dataToComputeProject project contexts cache ruleProjectVisitors fixedErrors)

        Modules contexts moduleZipper ->
            case dataToComputeProject.projectVisitor.moduleVisitor of
                Nothing ->
                    computeStepsForProject
                        dataToComputeProject
                        { project = project
                        , cache = cache
                        , ruleProjectVisitors = ruleProjectVisitors
                        , fixedErrors = fixedErrors
                        , step = FinalProjectEvaluation contexts
                        }

                Just ( moduleVisitor, moduleContextCreator ) ->
                    let
                        result : { project : ValidProject, moduleContexts : Dict String (ModuleCacheEntry projectContext), ruleProjectVisitors : List RuleProjectVisitor, step : Step projectContext, fixedErrors : FixedErrors }
                        result =
                            computeModules
                                { reviewOptions = dataToComputeProject.reviewOptions
                                , projectVisitor = dataToComputeProject.projectVisitor
                                , moduleVisitor = moduleVisitor
                                , moduleContextCreator = moduleContextCreator
                                , exceptions = dataToComputeProject.exceptions
                                }
                                contexts
                                (Just moduleZipper)
                                project
                                cache.moduleContexts
                                ruleProjectVisitors
                                fixedErrors
                    in
                    computeStepsForProject
                        dataToComputeProject
                        { project = result.project
                        , cache = { cache | moduleContexts = result.moduleContexts }
                        , ruleProjectVisitors = result.ruleProjectVisitors
                        , fixedErrors = result.fixedErrors
                        , step = result.step
                        }

        FinalProjectEvaluation contexts ->
            computeStepsForProject
                dataToComputeProject
                (computeFinalProjectEvaluation dataToComputeProject project contexts cache ruleProjectVisitors fixedErrors)

        DataExtract ->
            let
                newRuleProjectVisitors : List RuleProjectVisitor
                newRuleProjectVisitors =
                    List.map
                        (\((RuleProjectVisitor rule) as untouched) ->
                            case rule.dataExtractVisitor of
                                Just dataExtract ->
                                    dataExtract dataToComputeProject.reviewOptions

                                Nothing ->
                                    untouched
                        )
                        ruleProjectVisitors
            in
            { project = project
            , ruleProjectVisitors = newRuleProjectVisitors
            , fixedErrors = fixedErrors
            }

        Abort ->
            { project = project
            , ruleProjectVisitors = ruleProjectVisitors
            , fixedErrors = fixedErrors
            }


type Step projectContext
    = ElmJson { initial : projectContext }
    | Readme { initial : projectContext, elmJson : projectContext }
    | Dependencies { initial : projectContext, elmJson : projectContext, readme : projectContext }
    | Modules (ProjectContextAfterProjectFiles projectContext) (Zipper GraphModule)
    | FinalProjectEvaluation (ProjectContextAfterProjectFiles projectContext)
    | DataExtract
    | Abort


type alias ProjectContextAfterProjectFiles projectContext =
    { initial : projectContext
    , elmJson : projectContext
    , readme : projectContext
    , deps : projectContext
    }


type NextStep
    = ModuleVisitStep (Maybe (Zipper GraphModule))
    | BackToElmJson
    | BackToReadme
    | NextStepAbort


computeElmJson :
    DataToComputeProject projectContext moduleContext
    -> ValidProject
    -> projectContext
    -> ProjectRuleCache projectContext
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, step : Step projectContext, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeElmJson ({ reviewOptions, projectVisitor, exceptions } as dataToComputeProject) project inputContext cache ruleProjectVisitors fixedErrors =
    let
        cachePredicate : CacheEntryMaybe projectContext -> Bool
        cachePredicate elmJson =
            Cache.matchMaybe (ValidProject.elmJsonHash project) (ContextHash.create inputContext) elmJson
    in
    case reuseProjectRuleCache cachePredicate .elmJson cache of
        Just entry ->
            { project = project, step = Readme { initial = inputContext, elmJson = Cache.outputContextMaybe entry }, cache = cache, ruleProjectVisitors = ruleProjectVisitors, fixedErrors = fixedErrors }

        Nothing ->
            case projectVisitor.elmJsonVisitor of
                Nothing ->
                    let
                        -- TODO Maybe we can check whether there is a visitor before looking at whether there is a cache entry
                        -- TODO Can we maybe keep the cache entry empty?
                        elmJsonEntry : CacheEntryMaybe projectContext
                        elmJsonEntry =
                            Cache.createEntryMaybe
                                { contentHash = ValidProject.elmJsonHash project
                                , errors = []
                                , inputContext = inputContext
                                , outputContext = inputContext
                                }

                        newCache : ProjectRuleCache projectContext
                        newCache =
                            { cache | elmJson = Just elmJsonEntry }
                    in
                    { project = project, step = Readme { initial = inputContext, elmJson = inputContext }, ruleProjectVisitors = ruleProjectVisitors, cache = newCache, fixedErrors = fixedErrors }

                Just elmJsonVisitor ->
                    let
                        projectElmJson : Maybe { path : String, raw : String, project : Elm.Project.Project }
                        projectElmJson =
                            ValidProject.elmJson project

                        elmJsonData : Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
                        elmJsonData =
                            Maybe.map
                                (\elmJson ->
                                    { elmJsonKey = ElmJsonKey elmJson
                                    , project = elmJson.project
                                    }
                                )
                                projectElmJson

                        ( errorsForVisitor, outputContext ) =
                            elmJsonVisitor elmJsonData inputContext

                        ( errors, newRuleProjectVisitors ) =
                            List.foldl
                                (\((RuleProjectVisitor rule) as untouched) ( accErrors, accRules ) ->
                                    case rule.elmJsonVisitor of
                                        Just visitor ->
                                            let
                                                ( newErrors, updatedRule ) =
                                                    visitor project elmJsonData
                                            in
                                            ( List.append newErrors accErrors, updatedRule :: accRules )

                                        Nothing ->
                                            ( accErrors, untouched :: accRules )
                                )
                                ( [], [] )
                                ruleProjectVisitors

                        updateCache : () -> ProjectRuleCache projectContext
                        updateCache () =
                            let
                                elmJsonEntry : CacheEntryMaybe projectContext
                                elmJsonEntry =
                                    Cache.createEntryMaybe
                                        { contentHash = ValidProject.elmJsonHash project
                                        , errors = errors
                                        , inputContext = inputContext
                                        , outputContext = outputContext
                                        }
                            in
                            { cache | elmJson = Just elmJsonEntry }
                    in
                    case findFix reviewOptions projectVisitor project errors fixedErrors Nothing of
                        Just ( postFixStatus, fixResult ) ->
                            case postFixStatus of
                                ShouldContinue newFixedErrors ->
                                    -- The only possible thing we can fix here is the `elm.json` file, so we don't need to check
                                    -- what the fixed file was.
                                    computeElmJson dataToComputeProject fixResult.project inputContext cache ruleProjectVisitors newFixedErrors

                                ShouldAbort newFixedErrors ->
                                    { project = fixResult.project
                                    , step = Abort
                                    , cache = updateCache ()
                                    , ruleProjectVisitors = newRuleProjectVisitors
                                    , fixedErrors = newFixedErrors
                                    }

                        Nothing ->
                            { project = project
                            , step = Readme { initial = inputContext, elmJson = outputContext }
                            , cache = updateCache ()
                            , ruleProjectVisitors = newRuleProjectVisitors
                            , fixedErrors = fixedErrors
                            }


computeReadme :
    DataToComputeProject projectContext moduleContext
    -> ValidProject
    -> { initial : projectContext, elmJson : projectContext }
    -> ProjectRuleCache projectContext
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, step : Step projectContext, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeReadme ({ reviewOptions, projectVisitor, exceptions } as dataToComputeProject) project contexts cache ruleProjectVisitors fixedErrors =
    let
        inputContext : projectContext
        inputContext =
            contexts.elmJson

        cachePredicate : CacheEntryMaybe projectContext -> Bool
        cachePredicate entry =
            Cache.matchMaybe (ValidProject.readmeHash project) (ContextHash.create inputContext) entry
    in
    case reuseProjectRuleCache cachePredicate .readme cache of
        Just entry ->
            { project = project
            , step = Dependencies { initial = contexts.initial, elmJson = contexts.elmJson, readme = Cache.outputContextMaybe entry }
            , cache = cache
            , ruleProjectVisitors = ruleProjectVisitors
            , fixedErrors = fixedErrors
            }

        Nothing ->
            let
                projectReadme : Maybe { path : String, content : String }
                projectReadme =
                    ValidProject.readme project

                readmeData : Maybe { readmeKey : ReadmeKey, content : String }
                readmeData =
                    Maybe.map
                        (\readme ->
                            { readmeKey = ReadmeKey { path = readme.path, content = readme.content }
                            , content = readme.content
                            }
                        )
                        projectReadme

                ( errorsForVisitor, outputContext ) =
                    ( [], inputContext )
                        |> accumulateWithMaybe projectVisitor.readmeVisitor readmeData

                ( errors, newRuleProjectVisitors ) =
                    List.foldl
                        (\((RuleProjectVisitor rule) as untouched) ( accErrors, accRules ) ->
                            case rule.readmeVisitor of
                                Just visitor ->
                                    let
                                        ( newErrors, updatedRule ) =
                                            visitor project readmeData
                                    in
                                    ( List.append newErrors accErrors, updatedRule :: accRules )

                                Nothing ->
                                    ( accErrors, untouched :: accRules )
                        )
                        ( [], [] )
                        ruleProjectVisitors

                resultWhenNoFix : () -> { project : ValidProject, step : Step projectContext, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
                resultWhenNoFix () =
                    { project = project
                    , step = Dependencies { initial = contexts.initial, elmJson = contexts.elmJson, readme = outputContext }
                    , cache = updateCache ()
                    , ruleProjectVisitors = newRuleProjectVisitors
                    , fixedErrors = fixedErrors
                    }

                updateCache : () -> ProjectRuleCache projectContext
                updateCache () =
                    let
                        readmeEntry : CacheEntryMaybe projectContext
                        readmeEntry =
                            Cache.createEntryMaybe
                                { contentHash = ValidProject.readmeHash project
                                , errors = errors
                                , inputContext = inputContext
                                , outputContext = outputContext
                                }
                    in
                    { cache | readme = Just readmeEntry }
            in
            case findFix reviewOptions projectVisitor project errors fixedErrors Nothing of
                Just ( postFixStatus, fixResult ) ->
                    case postFixStatus of
                        ShouldAbort newFixedErrors ->
                            { project = fixResult.project, step = Abort, cache = updateCache (), ruleProjectVisitors = newRuleProjectVisitors, fixedErrors = newFixedErrors }

                        ShouldContinue newFixedErrors ->
                            case fixResult.fixedFile of
                                FixedElmJson ->
                                    { project = fixResult.project
                                    , step = ElmJson { initial = contexts.initial }
                                    , cache = updateCache ()
                                    , ruleProjectVisitors = newRuleProjectVisitors
                                    , fixedErrors = newFixedErrors
                                    }

                                FixedReadme ->
                                    computeReadme dataToComputeProject fixResult.project contexts (updateCache ()) ruleProjectVisitors newFixedErrors

                                FixedElmModule _ _ ->
                                    -- Not possible, users don't have the module key to provide fixes for an Elm module
                                    resultWhenNoFix ()

                Nothing ->
                    resultWhenNoFix ()


computeDependencies :
    DataToComputeProject projectContext moduleContext
    -> ValidProject
    -> { initial : projectContext, elmJson : projectContext, readme : projectContext }
    -> ProjectRuleCache projectContext
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, step : Step projectContext, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeDependencies { reviewOptions, projectVisitor, exceptions } project contexts cache ruleProjectVisitors fixedErrors =
    let
        inputContext : projectContext
        inputContext =
            contexts.readme

        cachePredicate : CacheEntryMaybe projectContext -> Bool
        cachePredicate entry =
            Cache.matchMaybe (ValidProject.dependenciesHash project) (ContextHash.create inputContext) entry

        modulesAsNextStep : projectContext -> Step projectContext
        modulesAsNextStep projectContext =
            Modules
                { initial = contexts.initial, elmJson = contexts.elmJson, readme = contexts.readme, deps = projectContext }
                (ValidProject.moduleZipper project)
    in
    case reuseProjectRuleCache cachePredicate .dependencies cache of
        Just entry ->
            { project = project, step = modulesAsNextStep (Cache.outputContextMaybe entry), cache = cache, ruleProjectVisitors = ruleProjectVisitors, fixedErrors = fixedErrors }

        Nothing ->
            let
                dependencies : Dict String Review.Project.Dependency.Dependency
                dependencies =
                    ValidProject.dependencies project

                directDependencies : Dict String Review.Project.Dependency.Dependency
                directDependencies =
                    ValidProject.directDependencies project

                accumulateWithDirectDependencies : ( List (Error {}), projectContext ) -> ( List (Error {}), projectContext )
                accumulateWithDirectDependencies =
                    case projectVisitor.directDependenciesVisitor of
                        Nothing ->
                            identity

                        Just visitor ->
                            \acc -> accumulate (\context -> visitor directDependencies context) acc

                ( errorsForVisitor, outputContext ) =
                    ( [], inputContext )
                        |> accumulateWithDirectDependencies
                        |> accumulateWithMaybe projectVisitor.dependenciesVisitor dependencies

                ( errors, newRuleProjectVisitors ) =
                    List.foldl
                        (\((RuleProjectVisitor rule) as untouched) ( accErrors, accRules ) ->
                            case rule.dependenciesVisitor of
                                Just visitor ->
                                    let
                                        ( newErrors, updatedRule ) =
                                            visitor project { all = dependencies, direct = directDependencies }
                                    in
                                    ( List.append newErrors accErrors, updatedRule :: accRules )

                                Nothing ->
                                    ( accErrors, untouched :: accRules )
                        )
                        ( [], [] )
                        ruleProjectVisitors

                resultWhenNoFix : () -> { project : ValidProject, step : Step projectContext, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
                resultWhenNoFix () =
                    { project = project
                    , step = modulesAsNextStep outputContext
                    , cache = updateCache ()
                    , ruleProjectVisitors = newRuleProjectVisitors
                    , fixedErrors = fixedErrors
                    }

                updateCache : () -> ProjectRuleCache projectContext
                updateCache () =
                    let
                        dependenciesEntry : CacheEntryMaybe projectContext
                        dependenciesEntry =
                            Cache.createEntryMaybe
                                { contentHash = ValidProject.dependenciesHash project
                                , errors = errors
                                , inputContext = inputContext
                                , outputContext = outputContext
                                }
                    in
                    { cache | dependencies = Just dependenciesEntry }
            in
            case findFix reviewOptions projectVisitor project errors fixedErrors Nothing of
                Just ( postFixStatus, fixResult ) ->
                    case postFixStatus of
                        ShouldAbort newFixedErrors ->
                            { project = fixResult.project, step = Abort, cache = updateCache (), ruleProjectVisitors = newRuleProjectVisitors, fixedErrors = newFixedErrors }

                        ShouldContinue newFixedErrors ->
                            case fixResult.fixedFile of
                                FixedElmJson ->
                                    { project = fixResult.project
                                    , step = ElmJson { initial = contexts.initial }
                                    , cache = updateCache ()
                                    , ruleProjectVisitors = newRuleProjectVisitors
                                    , fixedErrors = newFixedErrors
                                    }

                                FixedReadme ->
                                    { project = fixResult.project
                                    , step = Readme { initial = contexts.initial, elmJson = contexts.elmJson }
                                    , cache = updateCache ()
                                    , ruleProjectVisitors = newRuleProjectVisitors
                                    , fixedErrors = newFixedErrors
                                    }

                                FixedElmModule _ _ ->
                                    -- Not possible, users don't have the module key to provide fixes for an Elm module
                                    resultWhenNoFix ()

                Nothing ->
                    resultWhenNoFix ()


computeFinalProjectEvaluation :
    DataToComputeProject projectContext moduleContext
    -> ValidProject
    -> ProjectContextAfterProjectFiles projectContext
    -> ProjectRuleCache projectContext
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, cache : ProjectRuleCache projectContext, ruleProjectVisitors : List RuleProjectVisitor, step : Step projectContext, fixedErrors : FixedErrors }
computeFinalProjectEvaluation { reviewOptions, projectVisitor, exceptions } project projectContexts cache ruleProjectVisitors fixedErrors =
    case projectVisitor.finalEvaluationFn of
        Nothing ->
            { project = project, cache = cache, ruleProjectVisitors = ruleProjectVisitors, step = DataExtract, fixedErrors = fixedErrors }

        Just finalEvaluationFn ->
            let
                finalContext : projectContext
                finalContext =
                    computeFinalContext projectVisitor cache projectContexts.deps

                cachePredicate : FinalProjectEvaluationCache projectContext -> Bool
                cachePredicate entry =
                    Cache.matchNoOutput (ContextHash.create finalContext) entry
            in
            case reuseProjectRuleCache cachePredicate .finalEvaluationErrors cache of
                Just _ ->
                    { project = project, cache = cache, ruleProjectVisitors = ruleProjectVisitors, step = DataExtract, fixedErrors = fixedErrors }

                Nothing ->
                    let
                        ( errors, newRuleProjectVisitors ) =
                            List.foldl
                                (\((RuleProjectVisitor rule) as untouched) ( accErrors, accRules ) ->
                                    case rule.finalProjectEvaluation of
                                        Just visitor ->
                                            let
                                                ( newErrors, updatedRule ) =
                                                    visitor ()
                                            in
                                            ( List.append newErrors accErrors, updatedRule :: accRules )

                                        Nothing ->
                                            ( accErrors, untouched :: accRules )
                                )
                                ( [], [] )
                                ruleProjectVisitors
                    in
                    case findFix reviewOptions projectVisitor project errors fixedErrors Nothing of
                        Just ( postFixStatus, fixResult ) ->
                            let
                                ( newFixedErrors, step ) =
                                    case postFixStatus of
                                        ShouldAbort newFixedErrors_ ->
                                            ( newFixedErrors_, Abort )

                                        ShouldContinue newFixedErrors_ ->
                                            ( newFixedErrors_
                                            , case fixResult.fixedFile of
                                                FixedElmModule _ moduleZipper ->
                                                    Modules projectContexts moduleZipper

                                                FixedElmJson ->
                                                    ElmJson { initial = projectContexts.initial }

                                                FixedReadme ->
                                                    Readme { initial = projectContexts.initial, elmJson = projectContexts.elmJson }
                                            )
                            in
                            { project = fixResult.project
                            , cache = { cache | finalEvaluationErrors = Just (Cache.createNoOutput finalContext errors) }
                            , ruleProjectVisitors = newRuleProjectVisitors
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        Nothing ->
                            { project = project
                            , cache = { cache | finalEvaluationErrors = Just (Cache.createNoOutput finalContext errors) }
                            , ruleProjectVisitors = newRuleProjectVisitors
                            , step = DataExtract
                            , fixedErrors = fixedErrors
                            }


reuseProjectRuleCache : (b -> Bool) -> (ProjectRuleCache a -> Maybe b) -> ProjectRuleCache a -> Maybe b
reuseProjectRuleCache predicate getter cache =
    case getter cache of
        Nothing ->
            Nothing

        Just value ->
            if predicate value then
                Just value

            else
                Nothing


filterExceptionsAndSetName : Exceptions -> String -> List (Error scope) -> List (Error scope)
filterExceptionsAndSetName exceptions name errors =
    List.foldl
        (\error_ acc ->
            if Exceptions.isFileWeWantReportsFor exceptions (errorFilePathInternal error_) then
                setRuleName name error_ :: acc

            else
                acc
        )
        []
        errors


errorFilePathInternal : Error scope -> String
errorFilePathInternal (Error err) =
    err.filePath



-- VISIT MODULES


type alias DataToComputeModules projectContext moduleContext =
    { reviewOptions : ReviewOptionsData
    , projectVisitor : RunnableProjectVisitor projectContext moduleContext
    , moduleVisitor : RunnableModuleVisitor moduleContext
    , moduleContextCreator : ContextCreator projectContext moduleContext
    , exceptions : Exceptions
    }


type alias DataToComputeSingleModule projectContext moduleContext =
    { dataToComputeModules : DataToComputeModules projectContext moduleContext
    , ruleProjectVisitors : List RuleProjectVisitor
    , module_ : OpaqueProjectModule
    , isFileIgnored : Bool
    , projectContext : projectContext
    , project : ValidProject
    , moduleZipper : Zipper GraphModule
    , fixedErrors : FixedErrors
    , incoming : Graph.Adjacency ()
    }


computeModule :
    Exceptions
    -> DataToComputeSingleModule projectContext moduleContext
    -> { project : ValidProject, analysis : ModuleCacheEntry projectContext, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
computeModule exceptions ({ dataToComputeModules, ruleProjectVisitors, module_, isFileIgnored, projectContext, project, incoming } as params) =
    let
        (RequestedData requestedData) =
            dataToComputeModules.projectVisitor.requestedData

        moduleName : ModuleName
        moduleName =
            Node.value (moduleNameNode (ProjectModule.ast module_).moduleDefinition)

        ( moduleNameLookupTable, newProject ) =
            if requestedData.moduleNameLookupTable then
                Review.ModuleNameLookupTable.Compute.compute moduleName module_ project

            else
                ( ModuleNameLookupTableInternal.empty moduleName, project )

        availableData : AvailableData
        availableData =
            { ast = ProjectModule.ast module_
            , moduleKey = ModuleKey (ProjectModule.path module_)
            , moduleNameLookupTable = moduleNameLookupTable
            , extractSourceCode =
                if requestedData.sourceCodeExtractor then
                    let
                        lines : List String
                        lines =
                            String.lines (ProjectModule.source module_)
                    in
                    \range -> extractSourceCode lines range

                else
                    always ""
            , filePath = ProjectModule.path module_
            , isInSourceDirectories = ProjectModule.isInSourceDirectories module_
            , isFileIgnored = isFileIgnored
            }

        ( inputRuleModuleVisitors, rulesWithoutModuleVisitors ) =
            -- We can probably compute this in computeModules or above.
            List.foldl
                (\((RuleProjectVisitor ruleProjectVisitor) as rule) ( with, without ) ->
                    case ruleProjectVisitor.createModuleVisitorFromProjectVisitor of
                        Just moduleVisitor ->
                            ( moduleVisitor project availableData (ProjectModule.contentHash module_) incoming :: with, without )

                        Nothing ->
                            ( with, rule :: without )
                )
                ( [], [] )
                ruleProjectVisitors

        initialModuleContext : moduleContext
        initialModuleContext =
            applyContextCreator availableData dataToComputeModules.moduleContextCreator projectContext

        modulePath : String
        modulePath =
            ProjectModule.path module_

        ( errors, outputRuleProjectVisitors ) =
            List.foldl
                (\(RuleModuleVisitor ruleModuleVisitor) ( accErrors, rules ) ->
                    ( List.append (ruleModuleVisitor.getErrors ()) accErrors
                    , ruleModuleVisitor.toProjectVisitor () :: rules
                    )
                )
                ( [], [] )
                (visitModuleForProjectRule2 module_ inputRuleModuleVisitors)

        ( _, resultModuleContext ) =
            visitModuleForProjectRule
                dataToComputeModules.moduleVisitor
                initialModuleContext
                module_

        outputProjectContext : projectContext
        outputProjectContext =
            case getFolderFromTraversal dataToComputeModules.projectVisitor.traversalAndFolder of
                Just { fromModuleToProject } ->
                    applyContextCreator availableData fromModuleToProject resultModuleContext

                Nothing ->
                    projectContext
    in
    case findFixInComputeModuleResults { params | project = newProject } outputProjectContext (List.append rulesWithoutModuleVisitors outputRuleProjectVisitors) errors of
        ContinueWithNextStep nextStepResult ->
            nextStepResult

        ReComputeModule newParams ->
            computeModule exceptions newParams


type ComputeModuleFindFixResult projectContext moduleContext
    = ContinueWithNextStep { project : ValidProject, analysis : ModuleCacheEntry projectContext, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
    | ReComputeModule (DataToComputeSingleModule projectContext moduleContext)


findFixInComputeModuleResults :
    DataToComputeSingleModule projectContext moduleContext
    -> projectContext
    -> List RuleProjectVisitor
    -> List (Error {})
    -> ComputeModuleFindFixResult projectContext moduleContext
findFixInComputeModuleResults ({ dataToComputeModules, module_, isFileIgnored, projectContext, project, moduleZipper, fixedErrors } as params) outputContext outputRuleProjectVisitors errors =
    let
        analysis : ModuleCacheEntry projectContext
        analysis =
            Cache.createModuleEntry
                { contentHash = ProjectModule.contentHash module_
                , errors = errors
                , inputContext = projectContext
                , isFileIgnored = isFileIgnored
                , outputContext = outputContext
                }

        resultWhenNoFix : () -> ComputeModuleFindFixResult projectContext moduleContext
        resultWhenNoFix () =
            ContinueWithNextStep
                { project = project
                , ruleProjectVisitors = outputRuleProjectVisitors
                , analysis = analysis
                , nextStep = ModuleVisitStep (Zipper.next moduleZipper)
                , fixedErrors = fixedErrors
                }
    in
    case findFix dataToComputeModules.reviewOptions dataToComputeModules.projectVisitor project errors fixedErrors (Just moduleZipper) of
        Just ( postFixStatus, fixResult ) ->
            case postFixStatus of
                ShouldAbort newFixedErrors ->
                    ContinueWithNextStep
                        { project = fixResult.project
                        , ruleProjectVisitors = outputRuleProjectVisitors
                        , analysis = analysis
                        , nextStep = NextStepAbort
                        , fixedErrors = newFixedErrors
                        }

                ShouldContinue newFixedErrors ->
                    case fixResult.fixedFile of
                        FixedElmModule { source, ast } newModuleZipper_ ->
                            let
                                filePath : FilePath
                                filePath =
                                    errorFilePath fixResult.error
                            in
                            if ProjectModule.path module_ == filePath then
                                ReComputeModule
                                    { params
                                        | module_ =
                                            ProjectModule.create
                                                { path = filePath
                                                , source = source
                                                , ast = ast
                                                , isInSourceDirectories = ProjectModule.isInSourceDirectories module_
                                                }
                                        , project = fixResult.project
                                        , moduleZipper = newModuleZipper_
                                        , fixedErrors = newFixedErrors
                                    }

                            else
                                case Zipper.focusl (\mod -> mod.node.label == filePath) moduleZipper of
                                    Just newModuleZipper ->
                                        Logger.log
                                            dataToComputeModules.reviewOptions.logger
                                            (fixedError newFixedErrors { ruleName = dataToComputeModules.projectVisitor.name, filePath = filePath })
                                            (ContinueWithNextStep
                                                { project = fixResult.project
                                                , ruleProjectVisitors = outputRuleProjectVisitors
                                                , analysis = analysis
                                                , nextStep = ModuleVisitStep (Just newModuleZipper)
                                                , fixedErrors = newFixedErrors
                                                }
                                            )

                                    Nothing ->
                                        resultWhenNoFix ()

                        FixedElmJson ->
                            ContinueWithNextStep
                                { project = fixResult.project
                                , ruleProjectVisitors = outputRuleProjectVisitors
                                , analysis = analysis
                                , nextStep = BackToElmJson
                                , fixedErrors = FixedErrors.insert fixResult.error fixedErrors
                                }

                        FixedReadme ->
                            ContinueWithNextStep
                                { project = fixResult.project
                                , ruleProjectVisitors = outputRuleProjectVisitors
                                , analysis = analysis
                                , nextStep = BackToReadme
                                , fixedErrors = FixedErrors.insert fixResult.error fixedErrors
                                }

        Nothing ->
            resultWhenNoFix ()


computeModules :
    DataToComputeModules projectContext moduleContext
    -> ProjectContextAfterProjectFiles projectContext
    -> Maybe (Zipper GraphModule)
    -> ValidProject
    -> Dict String (ModuleCacheEntry projectContext)
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, moduleContexts : Dict String (ModuleCacheEntry projectContext), ruleProjectVisitors : List RuleProjectVisitor, step : Step projectContext, fixedErrors : FixedErrors }
computeModules dataToComputeModules projectContexts maybeModuleZipper initialProject initialModuleContexts ruleProjectVisitors fixedErrors =
    case maybeModuleZipper of
        Nothing ->
            { project = initialProject, moduleContexts = initialModuleContexts, ruleProjectVisitors = ruleProjectVisitors, step = FinalProjectEvaluation projectContexts, fixedErrors = fixedErrors }

        Just moduleZipper ->
            let
                result : { project : ValidProject, moduleContexts : Dict String (ModuleCacheEntry projectContext), ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
                result =
                    computeModuleAndCacheResult
                        dataToComputeModules
                        projectContexts.deps
                        moduleZipper
                        initialProject
                        initialModuleContexts
                        ruleProjectVisitors
                        fixedErrors
            in
            case result.nextStep of
                ModuleVisitStep newModuleZipper ->
                    computeModules
                        dataToComputeModules
                        projectContexts
                        newModuleZipper
                        result.project
                        result.moduleContexts
                        result.ruleProjectVisitors
                        result.fixedErrors

                BackToElmJson ->
                    { project = result.project
                    , moduleContexts = result.moduleContexts
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = ElmJson { initial = projectContexts.initial }
                    , fixedErrors = result.fixedErrors
                    }

                BackToReadme ->
                    { project = result.project
                    , moduleContexts = result.moduleContexts
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = Readme { initial = projectContexts.initial, elmJson = projectContexts.elmJson }
                    , fixedErrors = result.fixedErrors
                    }

                NextStepAbort ->
                    { project = result.project
                    , moduleContexts = result.moduleContexts
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = Abort
                    , fixedErrors = result.fixedErrors
                    }


computeProjectContext :
    TraversalAndFolder projectContext moduleContext
    -> ValidProject
    -> Dict String (ModuleCacheEntry projectContext)
    -> Graph.Adjacency ()
    -> projectContext
    -> projectContext
computeProjectContext traversalAndFolder project cache incoming initial =
    case traversalAndFolder of
        TraverseAllModulesInParallel _ ->
            initial

        TraverseImportedModulesFirst { foldProjectContexts } ->
            let
                graph : Graph FilePath ()
                graph =
                    ValidProject.moduleGraph project
            in
            IntDict.foldl
                (\key _ accContext ->
                    case
                        Graph.get key graph
                            |> Maybe.andThen (\graphModule -> Dict.get graphModule.node.label cache)
                    of
                        Just importedModuleCache ->
                            foldProjectContexts (Cache.outputContext importedModuleCache) accContext

                        Nothing ->
                            accContext
                )
                initial
                incoming


computeModuleAndCacheResult :
    DataToComputeModules projectContext moduleContext
    -> projectContext
    -> Zipper GraphModule
    -> ValidProject
    -> Dict String (ModuleCacheEntry projectContext)
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, moduleContexts : Dict String (ModuleCacheEntry projectContext), ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
computeModuleAndCacheResult dataToComputeModules inputProjectContext moduleZipper project moduleContexts ruleProjectVisitors fixedErrors =
    let
        { node, incoming } =
            Zipper.current moduleZipper

        ignoreModule : () -> { project : ValidProject, moduleContexts : Dict String (ModuleCacheEntry projectContext), ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
        ignoreModule () =
            { project = project, moduleContexts = moduleContexts, ruleProjectVisitors = ruleProjectVisitors, nextStep = ModuleVisitStep (Zipper.next moduleZipper), fixedErrors = fixedErrors }
    in
    case ValidProject.getModuleByPath node.label project of
        Nothing ->
            ignoreModule ()

        Just module_ ->
            let
                modulePath : String
                modulePath =
                    ProjectModule.path module_
            in
            if shouldIgnoreModule dataToComputeModules modulePath then
                ignoreModule ()

            else
                let
                    projectContext : projectContext
                    projectContext =
                        computeProjectContext dataToComputeModules.projectVisitor.traversalAndFolder project moduleContexts incoming inputProjectContext

                    (RequestedData requestedData) =
                        dataToComputeModules.projectVisitor.requestedData

                    isFileIgnored : Bool
                    isFileIgnored =
                        not (Exceptions.isFileWeWantReportsFor dataToComputeModules.exceptions modulePath)

                    shouldReuseCache : Cache.ModuleEntry error projectContext -> Bool
                    shouldReuseCache cacheEntry =
                        Cache.match
                            (ProjectModule.contentHash module_)
                            (ContextHash.create projectContext)
                            cacheEntry
                            { isFileIgnored = isFileIgnored
                            , rulesCareAboutIgnoredFiles = requestedData.ignoredFiles
                            }

                    maybeCacheEntry : Maybe (ModuleCacheEntry projectContext)
                    maybeCacheEntry =
                        Dict.get modulePath moduleContexts
                in
                case reuseCache shouldReuseCache maybeCacheEntry of
                    Just cacheEntry ->
                        -- Find if some cached errors contain fixes.
                        -- Useful because (but only because) we might not have tried to apply them
                        -- if they come directly from the file-system cache (or if the rule was first ran in non-fix mode).
                        -- This is not ideal because this could be quite slow.
                        -- TODO Find a way to avoid doing this when possible
                        case
                            findFix
                                dataToComputeModules.reviewOptions
                                dataToComputeModules.projectVisitor
                                project
                                (Cache.errors cacheEntry)
                                fixedErrors
                                (Just moduleZipper)
                        of
                            Just ( ShouldAbort newFixedErrors, fixResult ) ->
                                { project = fixResult.project
                                , moduleContexts = moduleContexts
                                , ruleProjectVisitors = ruleProjectVisitors
                                , nextStep = NextStepAbort
                                , fixedErrors = newFixedErrors
                                }

                            Just ( ShouldContinue newFixedErrors, fixResult ) ->
                                let
                                    nextStep : NextStep
                                    nextStep =
                                        case fixResult.fixedFile of
                                            FixedElmModule _ newModuleZipper ->
                                                ModuleVisitStep (Just newModuleZipper)

                                            FixedElmJson ->
                                                BackToElmJson

                                            FixedReadme ->
                                                BackToReadme
                                in
                                { project = fixResult.project
                                , moduleContexts = moduleContexts
                                , ruleProjectVisitors = ruleProjectVisitors
                                , nextStep = nextStep
                                , fixedErrors = newFixedErrors
                                }

                            Nothing ->
                                ignoreModule ()

                    Nothing ->
                        let
                            result : { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, analysis : ModuleCacheEntry projectContext, nextStep : NextStep, fixedErrors : FixedErrors }
                            result =
                                computeModule
                                    dataToComputeModules.exceptions
                                    { dataToComputeModules = dataToComputeModules
                                    , ruleProjectVisitors = ruleProjectVisitors
                                    , module_ = module_
                                    , isFileIgnored = isFileIgnored
                                    , projectContext = projectContext
                                    , project = project
                                    , moduleZipper = moduleZipper
                                    , fixedErrors = fixedErrors
                                    , incoming = incoming
                                    }
                        in
                        { project = result.project
                        , moduleContexts = Dict.insert modulePath result.analysis moduleContexts
                        , ruleProjectVisitors = result.ruleProjectVisitors
                        , nextStep = result.nextStep
                        , fixedErrors = result.fixedErrors
                        }


shouldIgnoreModule : DataToComputeModules projectContext moduleContext -> String -> Bool
shouldIgnoreModule dataToComputeModules path =
    case dataToComputeModules.projectVisitor.traversalAndFolder of
        TraverseAllModulesInParallel Nothing ->
            not (Exceptions.isFileWeWantReportsFor dataToComputeModules.exceptions path)

        TraverseAllModulesInParallel (Just _) ->
            False

        TraverseImportedModulesFirst _ ->
            False


reuseCache : (ModuleCacheEntry v -> Bool) -> Maybe (ModuleCacheEntry v) -> Maybe (ModuleCacheEntry v)
reuseCache predicate maybeCacheEntry =
    case maybeCacheEntry of
        Nothing ->
            Nothing

        Just cacheEntry ->
            if predicate cacheEntry then
                maybeCacheEntry

            else
                Nothing


getFolderFromTraversal : TraversalAndFolder projectContext moduleContext -> Maybe (Folder projectContext moduleContext)
getFolderFromTraversal traversalAndFolder =
    case traversalAndFolder of
        TraverseAllModulesInParallel maybeFolder ->
            maybeFolder

        TraverseImportedModulesFirst folder ->
            Just folder


type FixedFile
    = FixedElmModule { source : String, ast : File } (Zipper (Graph.NodeContext FilePath ()))
    | FixedElmJson
    | FixedReadme


type PostFixStatus
    = ShouldAbort FixedErrors
    | ShouldContinue FixedErrors


findFix : ReviewOptionsData -> RunnableProjectVisitor projectContext moduleContext -> ValidProject -> List (Error a) -> FixedErrors -> Maybe (Zipper (Graph.NodeContext FilePath ())) -> Maybe ( PostFixStatus, { project : ValidProject, fixedFile : FixedFile, error : ReviewError } )
findFix reviewOptions projectVisitor project errors fixedErrors maybeModuleZipper =
    InternalOptions.shouldApplyFix projectVisitor reviewOptions
        |> Maybe.andThen (\fixablePredicate -> findFixHelp project fixablePredicate errors maybeModuleZipper)
        |> Maybe.map
            (\fixResult ->
                let
                    newFixedErrors : FixedErrors
                    newFixedErrors =
                        FixedErrors.insert fixResult.error fixedErrors
                in
                if InternalOptions.shouldAbort reviewOptions newFixedErrors then
                    ( ShouldAbort newFixedErrors, fixResult )

                else
                    ( ShouldContinue newFixedErrors, fixResult )
                        |> Logger.log
                            reviewOptions.logger
                            (fixedError newFixedErrors { ruleName = projectVisitor.name, filePath = errorFilePath fixResult.error })
            )


findFixHelp :
    ValidProject
    -> ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool)
    -> List (Error a)
    -> Maybe (Zipper (Graph.NodeContext FilePath ()))
    -> Maybe { project : ValidProject, fixedFile : FixedFile, error : ReviewError }
findFixHelp project fixablePredicate errors maybeModuleZipper =
    case errors of
        [] ->
            Nothing

        (Error headError) :: restOfErrors ->
            case isFixable fixablePredicate headError of
                Nothing ->
                    findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                Just fixes ->
                    case headError.target of
                        Review.Error.Module ->
                            case ValidProject.getModuleByPath headError.filePath project of
                                Nothing ->
                                    findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                Just file ->
                                    case
                                        InternalFix.fixModule fixes (ProjectModule.source file)
                                            |> Maybe.andThen
                                                (\fixResult ->
                                                    ValidProject.addParsedModule { path = headError.filePath, source = fixResult.source, ast = fixResult.ast } maybeModuleZipper project
                                                        |> Maybe.map
                                                            (\( newProject, newModuleZipper ) ->
                                                                { project = newProject
                                                                , fixedFile = FixedElmModule fixResult newModuleZipper
                                                                , error = errorToReviewError (Error headError)
                                                                }
                                                            )
                                                )
                                    of
                                        Nothing ->
                                            findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                        Just fixResult ->
                                            Just fixResult

                        Review.Error.ElmJson ->
                            case ValidProject.elmJson project of
                                Nothing ->
                                    findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                Just elmJson ->
                                    case
                                        InternalFix.fixElmJson fixes elmJson.raw
                                            |> Maybe.andThen
                                                (\fixResult ->
                                                    ValidProject.addElmJson { path = elmJson.path, raw = fixResult.raw, project = fixResult.project } project
                                                )
                                    of
                                        Nothing ->
                                            findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                        Just newProject ->
                                            Just
                                                { project = newProject
                                                , fixedFile = FixedElmJson
                                                , error = errorToReviewError (Error headError)
                                                }

                        Review.Error.Readme ->
                            case ValidProject.readme project of
                                Nothing ->
                                    findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                Just readme ->
                                    case InternalFix.fixReadme fixes readme.content of
                                        Nothing ->
                                            findFixHelp project fixablePredicate restOfErrors maybeModuleZipper

                                        Just content ->
                                            Just
                                                { project = ValidProject.addReadme { path = readme.path, content = content } project
                                                , fixedFile = FixedReadme
                                                , error = errorToReviewError (Error headError)
                                                }

                        _ ->
                            findFixHelp project fixablePredicate restOfErrors maybeModuleZipper


isFixable : ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool) -> InternalError -> Maybe (List Fix)
isFixable predicate err =
    case err.fixes of
        Just _ ->
            -- It's cheaper to check for fixes first and also quite likely to return Nothing
            -- so we do the fixes check first.
            if predicate { ruleName = err.ruleName, filePath = err.filePath, message = err.message, details = err.details, range = err.range } then
                err.fixes

            else
                Nothing

        Nothing ->
            Nothing


visitModuleForProjectRule : RunnableModuleVisitor moduleContext -> moduleContext -> OpaqueProjectModule -> ( List (Error {}), moduleContext )
visitModuleForProjectRule schema initialContext module_ =
    let
        ast : File
        ast =
            ProjectModule.ast module_
    in
    ( [], initialContext )
        |> accumulateWithMaybe schema.moduleDefinitionVisitor ast.moduleDefinition
        -- TODO When `elm-syntax` integrates the module documentation by default, then we should use that instead of this.
        |> accumulateModuleDocumentationVisitor schema.moduleDocumentationVisitor ast
        |> accumulateWithMaybe schema.commentsVisitor ast.comments
        |> accumulateListWithMaybe schema.importVisitor ast.imports
        |> accumulateWithMaybe schema.declarationListVisitor ast.declarations
        |> schema.declarationAndExpressionVisitor ast.declarations
        |> accumulateFinalEvaluation schema.finalEvaluationFn


accumulateFinalEvaluation :
    Maybe (context -> List (Error {}))
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateFinalEvaluation maybeFinalEvaluationFn (( errors, moduleContext ) as untouched) =
    case maybeFinalEvaluationFn of
        Just finalEvaluationFn ->
            ( List.append (finalEvaluationFn moduleContext) errors, moduleContext )

        Nothing ->
            untouched


visitModuleForProjectRule2 : OpaqueProjectModule -> List RuleModuleVisitor -> List RuleModuleVisitor
visitModuleForProjectRule2 module_ ruleModuleVisitors =
    let
        ast : File
        ast =
            ProjectModule.ast module_

        moduleDocumentation : Maybe (Node String)
        moduleDocumentation =
            findModuleDocumentation ast
    in
    ruleModuleVisitors
        |> List.map (\acc -> runVisitor .moduleDefinitionVisitor ast.moduleDefinition acc)
        |> List.map (\acc -> runVisitor .moduleDocumentationVisitor moduleDocumentation acc)
        |> List.map (\acc -> runVisitor .commentVisitor ast.comments acc)
        |> List.map (\acc -> runVisitor .importsVisitor ast.imports acc)
        |> List.map (\acc -> runVisitor .declarationListVisitor ast.declarations acc)
        |> visitDeclarationsAndExpressions ast.declarations
        |> List.map (\acc -> runVisitor .finalModuleEvaluation () acc)


visitDeclarationsAndExpressions : List (Node Declaration) -> List RuleModuleVisitor -> List RuleModuleVisitor
visitDeclarationsAndExpressions declarations rules =
    List.foldl visitDeclarationAndExpressions rules declarations


visitDeclarationAndExpressions : Node Declaration -> List RuleModuleVisitor -> List RuleModuleVisitor
visitDeclarationAndExpressions declaration rules =
    rules
        |> List.map (\acc -> runVisitor .declarationVisitorOnEnter declaration acc)
        |> (\updatedRules ->
                case Node.value declaration of
                    Declaration.FunctionDeclaration function ->
                        visitExpression2 (Node.value function.declaration |> .expression) updatedRules

                    _ ->
                        updatedRules
           )
        |> List.map (\acc -> runVisitor .declarationVisitorOnExit declaration acc)


visitExpression2 : Node Expression -> List RuleModuleVisitor -> List RuleModuleVisitor
visitExpression2 node rules =
    case Node.value node of
        Expression.LetExpression letBlock ->
            rules
                |> List.map (\acc -> runVisitor .expressionVisitorOnEnter node acc)
                |> (\updatedRules ->
                        List.foldl
                            (visitLetDeclaration2 (Node (Node.range node) letBlock))
                            updatedRules
                            letBlock.declarations
                   )
                |> visitExpression2 letBlock.expression
                |> List.map (\acc -> runVisitor .expressionVisitorOnExit node acc)

        Expression.CaseExpression caseBlock ->
            rules
                |> List.map (\acc -> runVisitor .expressionVisitorOnEnter node acc)
                |> visitExpression2 caseBlock.expression
                |> (\updatedRules ->
                        List.foldl
                            (\case_ acc -> visitCaseBranch2 (Node (Node.range node) caseBlock) case_ acc)
                            updatedRules
                            caseBlock.cases
                   )
                |> List.map (\acc -> runVisitor .expressionVisitorOnExit node acc)

        _ ->
            rules
                |> List.map (\acc -> runVisitor .expressionVisitorOnEnter node acc)
                |> (\updatedRules ->
                        List.foldl
                            visitExpression2
                            updatedRules
                            (expressionChildren node)
                   )
                |> List.map (\acc -> runVisitor .expressionVisitorOnExit node acc)


visitLetDeclaration2 :
    Node Expression.LetBlock
    -> Node Expression.LetDeclaration
    -> List RuleModuleVisitor
    -> List RuleModuleVisitor
visitLetDeclaration2 letBlockWithRange ((Node _ letDeclaration) as letDeclarationWithRange) rules =
    let
        expressionNode : Node Expression
        expressionNode =
            case letDeclaration of
                Expression.LetFunction function ->
                    functionToExpression function

                Expression.LetDestructuring _ expr ->
                    expr
    in
    rules
        |> List.map (\acc -> runVisitor2 .letDeclarationVisitorOnEnter letBlockWithRange letDeclarationWithRange acc)
        |> visitExpression2 expressionNode
        |> List.map (\acc -> runVisitor2 .letDeclarationVisitorOnExit letBlockWithRange letDeclarationWithRange acc)


visitCaseBranch2 :
    Node Expression.CaseBlock
    -> ( Node Pattern, Node Expression )
    -> List RuleModuleVisitor
    -> List RuleModuleVisitor
visitCaseBranch2 caseBlockWithRange (( _, caseExpression ) as caseBranch) rules =
    rules
        |> List.map (\acc -> runVisitor2 .caseBranchVisitorOnEnter caseBlockWithRange caseBranch acc)
        |> visitExpression2 caseExpression
        |> List.map (\acc -> runVisitor2 .caseBranchVisitorOnExit caseBlockWithRange caseBranch acc)


shouldVisitDeclarations : ModuleRuleSchemaData moduleContext -> Bool
shouldVisitDeclarations schema =
    (schema.declarationVisitorOnEnter /= Nothing)
        || (schema.declarationVisitorOnExit /= Nothing)


createExpressionVisitor :
    ModuleRuleSchemaData moduleContext
    -> Maybe (Node Expression -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext ))
createExpressionVisitor schema =
    if
        (schema.letDeclarationVisitorOnEnter /= Nothing)
            || (schema.letDeclarationVisitorOnExit /= Nothing)
            || (schema.caseBranchVisitorOnEnter /= Nothing)
            || (schema.caseBranchVisitorOnExit /= Nothing)
    then
        let
            expressionRelatedVisitors : ExpressionRelatedVisitors moduleContext
            expressionRelatedVisitors =
                { expressionVisitorsOnEnter = schema.expressionVisitorsOnEnter
                , expressionVisitorsOnExit = schema.expressionVisitorsOnExit
                , letDeclarationVisitorOnEnter = schema.letDeclarationVisitorOnEnter
                , letDeclarationVisitorOnExit = schema.letDeclarationVisitorOnExit
                , caseBranchVisitorOnEnter = schema.caseBranchVisitorOnEnter
                , caseBranchVisitorOnExit = schema.caseBranchVisitorOnExit
                }
        in
        Just (\expr acc -> visitExpression expressionRelatedVisitors expr acc)

    else if schema.expressionVisitorsOnExit /= Nothing then
        Just (\expr acc -> visitOnlyExpressions schema.expressionVisitorsOnEnter schema.expressionVisitorsOnExit expr acc)

    else if schema.expressionVisitorsOnEnter /= Nothing then
        Just (\expr acc -> visitOnlyExpressionsOnlyOnEnter schema.expressionVisitorsOnEnter expr acc)

    else
        Nothing


type alias ExpressionRelatedVisitors moduleContext =
    { expressionVisitorsOnEnter : Maybe (Visitor Expression moduleContext)
    , expressionVisitorsOnExit : Maybe (Visitor Expression moduleContext)
    , letDeclarationVisitorOnEnter : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext ))
    , letDeclarationVisitorOnExit : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> moduleContext -> ( List (Error {}), moduleContext ))
    , caseBranchVisitorOnEnter : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext ))
    , caseBranchVisitorOnExit : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> moduleContext -> ( List (Error {}), moduleContext ))
    }


extractSourceCode : List String -> Range -> String
extractSourceCode lines range =
    lines
        |> List.drop (range.start.row - 1)
        |> List.take (range.end.row - range.start.row + 1)
        |> mapLast (String.slice 0 (range.end.column - 1))
        |> String.join "\n"
        |> String.dropLeft (range.start.column - 1)


mapLast : (a -> a) -> List a -> List a
mapLast mapper lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (mapper first :: rest)


visitDeclaration :
    Maybe (Visitor Declaration moduleContext)
    -> Maybe (Visitor Declaration moduleContext)
    -> (Node Expression -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext ))
    -> Node Declaration
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitDeclaration declarationVisitorOnEnter declarationVisitorOnExit expressionVisitor node errorsAndContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            errorsAndContext
                |> accumulateWithMaybe declarationVisitorOnEnter node
                |> expressionVisitor (Node.value function.declaration).expression
                |> accumulateWithMaybe declarationVisitorOnExit node

        _ ->
            visitOnlyDeclaration declarationVisitorOnEnter declarationVisitorOnExit node errorsAndContext


visitDeclarationButOnlyExpressions :
    (Node Expression -> ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext ))
    -> Node Declaration
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitDeclarationButOnlyExpressions expressionVisitor node errorsAndContext =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            expressionVisitor (Node.value function.declaration).expression errorsAndContext

        _ ->
            errorsAndContext


visitOnlyDeclaration :
    Maybe (Visitor Declaration moduleContext)
    -> Maybe (Visitor Declaration moduleContext)
    -> Node Declaration
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitOnlyDeclaration declarationVisitorOnEnter declarationVisitorOnExit node errorsAndContext =
    errorsAndContext
        |> accumulateWithMaybe declarationVisitorOnEnter node
        |> accumulateWithMaybe declarationVisitorOnExit node


type RuleProjectVisitor
    = RuleProjectVisitor (RuleProjectVisitorOperations RuleProjectVisitor)


type alias RuleProjectVisitorHidden projectContext =
    { cache : ProjectRuleCache projectContext
    , ruleData : ChangeableRuleData
    }


type alias ChangeableRuleData =
    { exceptions : Exceptions
    , ruleId : Int
    , requestedData : RequestedData
    }


{-| Object to analyze projects with a rule.

`projectContext` is the hidden type variable.
The hidden state is `{ cache : ProjectRuleCache projectContext, ruleData : ChangeableRuleData }`

-}
type alias RuleProjectVisitorOperations t =
    { elmJsonVisitor : Maybe (ValidProject -> Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> ( List (Error {}), t ))
    , readmeVisitor : Maybe (ValidProject -> Maybe { readmeKey : ReadmeKey, content : String } -> ( List (Error {}), t ))
    , dependenciesVisitor : Maybe (ValidProject -> { all : Dict String Review.Project.Dependency.Dependency, direct : Dict String Review.Project.Dependency.Dependency } -> ( List (Error {}), t ))
    , createModuleVisitorFromProjectVisitor : Maybe (ValidProject -> AvailableData -> ContentHash -> Graph.Adjacency () -> RuleModuleVisitor)
    , finalProjectEvaluation : Maybe (() -> ( List (Error {}), t ))
    , dataExtractVisitor : Maybe (ReviewOptionsData -> t)
    , addDataExtract : Dict String Encode.Value -> Dict String Encode.Value
    , getErrors : () -> List (Error {})
    , backToRule : () -> Rule
    }


createRuleProjectVisitor : ProjectRuleSchemaData projectContext moduleContext -> ChangeableRuleData -> RuleProjectVisitor
createRuleProjectVisitor schema ruleData =
    If.create RuleProjectVisitor
        (\raise hidden -> projectRuleImplementation schema raise hidden)
        { cache = initialCacheMarker schema.name ruleData.ruleId emptyCache
        , ruleData = ruleData
        }


projectRuleImplementation :
    ProjectRuleSchemaData projectContext moduleContext
    -> (RuleProjectVisitorHidden projectContext -> RuleProjectVisitor)
    -> RuleProjectVisitorHidden projectContext
    -> RuleProjectVisitorOperations RuleProjectVisitor
projectRuleImplementation schema baseRaise ({ cache } as hidden) =
    let
        raiseCache : ProjectRuleCache projectContext -> RuleProjectVisitor
        raiseCache newCache =
            baseRaise { cache = newCache, ruleData = hidden.ruleData }
    in
    { elmJsonVisitor = addProjectVisitor schema hidden.ruleData schema.elmJsonVisitor [] ValidProject.elmJsonHash (\entry -> raiseCache { cache | elmJson = entry })
    , readmeVisitor = addProjectVisitor schema hidden.ruleData schema.readmeVisitor [ cache.elmJson ] ValidProject.readmeHash (\entry -> raiseCache { cache | readme = entry })
    , dependenciesVisitor = addDependenciesVisitor schema hidden.ruleData raiseCache cache { allVisitor = schema.dependenciesVisitor, directVisitor = schema.directDependenciesVisitor }
    , createModuleVisitorFromProjectVisitor = createModuleVisitorFromProjectVisitor schema hidden.ruleData.exceptions raiseCache cache
    , finalProjectEvaluation = addFinalProjectEvaluationVisitor schema hidden.ruleData raiseCache cache
    , dataExtractVisitor = addDataExtract schema raiseCache cache
    , addDataExtract =
        \extracts ->
            case Maybe.map .extract (finalCacheMarker schema.name hidden.ruleData.ruleId cache).extract of
                Just (Extract extract) ->
                    Dict.insert schema.name extract extracts

                Nothing ->
                    extracts
    , getErrors = \() -> errorsFromCache cache
    , backToRule =
        \() ->
            Rule
                { name = schema.name
                , id = hidden.ruleData.ruleId
                , exceptions = hidden.ruleData.exceptions
                , requestedData = hidden.ruleData.requestedData
                , providesFixes = schema.providesFixes
                , extractsData = schema.dataExtractor /= Nothing
                , ruleImplementation = \_ -> Debug.todo "ruleImplementation in projectRuleImplementation"
                , ruleProjectVisitor = Ok (\_ -> Debug.todo "ruleProjectVisitor in projectRuleImplementation")
                }
    }


addProjectVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> ChangeableRuleData
    -> Maybe (data -> projectContext -> ( List (Error {}), projectContext ))
    -> List (Maybe (Cache.EntryMaybe error projectContext))
    -> (ValidProject -> Maybe ContentHash)
    -> (Maybe (CacheEntryMaybe projectContext) -> RuleProjectVisitor)
    ->
        Maybe
            (ValidProject
             -> data
             -> ( List (Error {}), RuleProjectVisitor )
            )
addProjectVisitor schema { exceptions } maybeVisitor possibleInputContexts contentHash toRuleProjectVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just
                (\project data ->
                    let
                        inputContext : projectContext
                        inputContext =
                            List.filterMap identity possibleInputContexts
                                |> List.head
                                |> Maybe.map Cache.outputContextMaybe
                                |> Maybe.withDefault schema.initialProjectContext

                        ( errorsForVisitor, outputContext ) =
                            visitor data inputContext

                        errors : List (Error {})
                        errors =
                            filterExceptionsAndSetName exceptions schema.name errorsForVisitor
                    in
                    ( errors
                    , Cache.createEntryMaybe
                        { contentHash = contentHash project
                        , errors = errors
                        , inputContext = inputContext
                        , outputContext = outputContext
                        }
                        |> Just
                        |> toRuleProjectVisitor
                    )
                )


addDependenciesVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> ChangeableRuleData
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    ->
        { allVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , directVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        }
    ->
        Maybe
            (ValidProject
             -> { all : Dict String Review.Project.Dependency.Dependency, direct : Dict String Review.Project.Dependency.Dependency }
             -> ( List (Error {}), RuleProjectVisitor )
            )
addDependenciesVisitor schema { exceptions } raise cache { allVisitor, directVisitor } =
    case ( allVisitor, directVisitor ) of
        ( Nothing, Nothing ) ->
            Nothing

        _ ->
            Just
                (\project { all, direct } ->
                    let
                        inputContext : projectContext
                        inputContext =
                            List.filterMap identity [ cache.readme, cache.elmJson ]
                                |> List.head
                                |> Maybe.map Cache.outputContextMaybe
                                |> Maybe.withDefault schema.initialProjectContext

                        ( errorsForDirect, outputContextForDirect ) =
                            case directVisitor of
                                Just directVisitor_ ->
                                    directVisitor_ direct inputContext

                                Nothing ->
                                    ( [], inputContext )

                        ( errorsForIndirect, finalOutputContext ) =
                            case allVisitor of
                                Just allVisitor_ ->
                                    allVisitor_ all outputContextForDirect

                                Nothing ->
                                    ( [], outputContextForDirect )

                        errors : List (Error {})
                        errors =
                            filterExceptionsAndSetName exceptions schema.name (List.append errorsForIndirect errorsForDirect)

                        dependenciesEntry : CacheEntryMaybe projectContext
                        dependenciesEntry =
                            Cache.createEntryMaybe
                                { contentHash = ValidProject.dependenciesHash project
                                , errors = errors
                                , inputContext = inputContext
                                , outputContext = finalOutputContext
                                }
                    in
                    ( errors
                    , raise { cache | dependencies = Just dependenciesEntry }
                    )
                )


addFinalProjectEvaluationVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> ChangeableRuleData
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> Maybe (() -> ( List (Error {}), RuleProjectVisitor ))
addFinalProjectEvaluationVisitor schema { exceptions } raise cache =
    case schema.finalEvaluationFn of
        Nothing ->
            Nothing

        Just finalEvaluationFn ->
            Just
                (\() ->
                    let
                        inputContext : projectContext
                        inputContext =
                            computeFinalContext2 schema cache

                        errors : List (Error {})
                        errors =
                            filterExceptionsAndSetName exceptions schema.name (finalEvaluationFn inputContext)
                    in
                    ( errors
                    , raise { cache | finalEvaluationErrors = Just (Cache.createNoOutput inputContext errors) }
                    )
                )


addDataExtract :
    ProjectRuleSchemaData projectContext moduleContext
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> Maybe (ReviewOptionsData -> RuleProjectVisitor)
addDataExtract schema raise cache =
    case schema.dataExtractor of
        Nothing ->
            Nothing

        Just dataExtractor ->
            Just
                (\reviewOptions ->
                    let
                        errors : List (Error {})
                        errors =
                            -- TODO Can we compute this as we traverse the file?
                            errorsFromCache cache
                    in
                    if reviewOptions.extract && not (List.any doesPreventExtract errors) then
                        let
                            inputContext : projectContext
                            inputContext =
                                computeFinalContext2 schema cache

                            cachePredicate : ExtractCache projectContext -> Bool
                            cachePredicate extract =
                                extract.inputContext == ContextHash.create inputContext
                        in
                        case reuseProjectRuleCache cachePredicate .extract cache of
                            Just _ ->
                                raise cache

                            Nothing ->
                                raise { cache | extract = Just { inputContext = ContextHash.create inputContext, extract = dataExtractor inputContext } }

                    else
                        raise cache
                )


createModuleVisitorFromProjectVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> Exceptions
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> Maybe (ValidProject -> AvailableData -> ContentHash -> Graph.Adjacency () -> RuleModuleVisitor)
createModuleVisitorFromProjectVisitor schema exceptions raise cache =
    case mergeModuleVisitors schema.name schema.initialProjectContext schema.moduleContextCreator schema.moduleVisitors of
        Nothing ->
            Nothing

        Just moduleRuleSchema ->
            let
                traversalAndFolder : TraversalAndFolder projectContext moduleContext
                traversalAndFolder =
                    case ( schema.traversalType, schema.folder ) of
                        ( AllModulesInParallel, _ ) ->
                            TraverseAllModulesInParallel schema.folder

                        ( ImportedModulesFirst, Just folder ) ->
                            TraverseImportedModulesFirst folder

                        ( ImportedModulesFirst, Nothing ) ->
                            TraverseAllModulesInParallel Nothing
            in
            Just (createModuleVisitorFromProjectVisitorHelp schema exceptions raise cache traversalAndFolder moduleRuleSchema)


createModuleVisitorFromProjectVisitorHelp :
    ProjectRuleSchemaData projectContext moduleContext
    -> Exceptions
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> TraversalAndFolder projectContext moduleContext
    -> ( ModuleRuleSchema schemaState moduleContext, ContextCreator projectContext moduleContext )
    -> ValidProject
    -> AvailableData
    -> ContentHash
    -> Graph.Adjacency ()
    -> RuleModuleVisitor
createModuleVisitorFromProjectVisitorHelp schema exceptions raise cache traversalAndFolder ( ModuleRuleSchema moduleRuleSchema, moduleContextCreator ) =
    \project availableData moduleContentHash incoming ->
        let
            initialProjectContext : projectContext
            initialProjectContext =
                List.filterMap identity [ cache.dependencies, cache.readme, cache.elmJson ]
                    |> List.head
                    |> Maybe.map Cache.outputContextMaybe
                    |> Maybe.withDefault schema.initialProjectContext

            inputProjectContext : projectContext
            inputProjectContext =
                computeProjectContext traversalAndFolder project cache.moduleContexts incoming initialProjectContext

            initialContext : moduleContext
            initialContext =
                applyContextCreator availableData moduleContextCreator inputProjectContext

            toRuleProjectVisitor : ( List (Error {}), moduleContext ) -> RuleProjectVisitor
            toRuleProjectVisitor ( errors, resultModuleContext ) =
                let
                    outputProjectContext : projectContext
                    outputProjectContext =
                        case getFolderFromTraversal traversalAndFolder of
                            Just { fromModuleToProject } ->
                                applyContextCreator availableData fromModuleToProject resultModuleContext

                            Nothing ->
                                schema.initialProjectContext

                    cacheEntry : Cache.ModuleEntry (Error {}) projectContext
                    cacheEntry =
                        Cache.createModuleEntry
                            { contentHash = moduleContentHash
                            , errors = errors
                            , inputContext = inputProjectContext
                            , isFileIgnored = availableData.isFileIgnored
                            , outputContext = outputProjectContext
                            }
                in
                raise { cache | moduleContexts = Dict.insert availableData.filePath cacheEntry cache.moduleContexts }

            ruleData : { ruleName : String, exceptions : Exceptions, filePath : String }
            ruleData =
                { ruleName = schema.name, exceptions = exceptions, filePath = availableData.filePath }
        in
        If.create RuleModuleVisitor
            (\ruleModuleVisitorRaise hidden ->
                moduleRuleImplementation moduleRuleSchema ruleData toRuleProjectVisitor ruleModuleVisitorRaise hidden
            )
            ( [], initialContext )


type RuleModuleVisitor
    = RuleModuleVisitor (RuleModuleVisitorOperations RuleModuleVisitor)


type alias RuleModuleVisitorOperations t =
    -- `moduleContext` is the hidden type variable
    -- The hidden state is `( List (Error {}), moduleContext )`
    { moduleDefinitionVisitor : Maybe (Node Module -> t)
    , moduleDocumentationVisitor : Maybe (Maybe (Node String) -> t)
    , commentVisitor : Maybe (List (Node String) -> t)
    , importsVisitor : Maybe (List (Node Import) -> t)
    , declarationListVisitor : Maybe (List (Node Declaration) -> t)
    , declarationVisitorOnEnter : Maybe (Node Declaration -> t)
    , declarationVisitorOnExit : Maybe (Node Declaration -> t)
    , expressionVisitorOnEnter : Maybe (Node Expression -> t)
    , expressionVisitorOnExit : Maybe (Node Expression -> t)
    , letDeclarationVisitorOnEnter : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> t)
    , letDeclarationVisitorOnExit : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> t)
    , caseBranchVisitorOnEnter : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> t)
    , caseBranchVisitorOnExit : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> t)
    , finalModuleEvaluation : Maybe (() -> t)
    , getErrors : () -> List (Error {})
    , toProjectVisitor : () -> RuleProjectVisitor
    }


moduleRuleImplementation :
    ModuleRuleSchemaData moduleContext
    -> { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), moduleContext ) -> RuleProjectVisitor)
    -> (( List (Error {}), moduleContext ) -> RuleModuleVisitor)
    -> ( List (Error {}), moduleContext )
    -> RuleModuleVisitorOperations RuleModuleVisitor
moduleRuleImplementation schema params toRuleProjectVisitor raise (( errors, _ ) as errorsAndContext) =
    { moduleDefinitionVisitor = createVisitor params raise errorsAndContext schema.moduleDefinitionVisitor
    , moduleDocumentationVisitor = createVisitor params raise errorsAndContext schema.moduleDocumentationVisitor
    , commentVisitor = createVisitor params raise errorsAndContext schema.commentsVisitor
    , importsVisitor = createImportsVisitor params raise errorsAndContext schema.importVisitor
    , declarationListVisitor = createVisitor params raise errorsAndContext schema.declarationListVisitor
    , declarationVisitorOnEnter = createVisitor params raise errorsAndContext schema.declarationVisitorOnEnter
    , declarationVisitorOnExit = createVisitor params raise errorsAndContext schema.declarationVisitorOnExit
    , expressionVisitorOnEnter = createVisitor params raise errorsAndContext schema.expressionVisitorsOnEnter
    , expressionVisitorOnExit = createVisitor params raise errorsAndContext schema.expressionVisitorsOnExit
    , letDeclarationVisitorOnEnter = createVisitor2 params raise errorsAndContext schema.letDeclarationVisitorOnEnter
    , letDeclarationVisitorOnExit = createVisitor2 params raise errorsAndContext schema.letDeclarationVisitorOnExit
    , caseBranchVisitorOnEnter = createVisitor2 params raise errorsAndContext schema.caseBranchVisitorOnEnter
    , caseBranchVisitorOnExit = createVisitor2 params raise errorsAndContext schema.caseBranchVisitorOnExit
    , finalModuleEvaluation = createFinalModuleEvaluationVisitor params raise errorsAndContext schema.finalEvaluationFn
    , getErrors = \() -> errors
    , toProjectVisitor = \() -> toRuleProjectVisitor errorsAndContext
    }


createVisitor :
    { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), context ) -> a)
    -> ( List (Error {}), context )
    -> Maybe (b -> context -> ( List (Error {}), context ))
    -> Maybe (b -> a)
createVisitor params raise errorsAndContext maybeVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just (\node -> raise (accumulateWithParams params (visitor node) errorsAndContext))


createVisitor2 :
    { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), context ) -> t)
    -> ( List (Error {}), context )
    -> Maybe (a -> b -> context -> ( List (Error {}), context ))
    -> Maybe (a -> b -> t)
createVisitor2 params raise errorsAndContext maybeVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just (\a b -> raise (accumulateWithParams params (visitor a b) errorsAndContext))


createImportsVisitor :
    { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), context ) -> a)
    -> ( List (Error {}), context )
    -> Maybe (b -> context -> ( List (Error {}), context ))
    -> Maybe (List b -> a)
createImportsVisitor params raise errorsAndContext maybeImportVisitors =
    case maybeImportVisitors of
        Nothing ->
            Nothing

        Just visitor ->
            Just
                (\imports ->
                    raise
                        (List.foldl
                            (\import_ initialErrorsAndContext ->
                                accumulateWithParams params (visitor import_) initialErrorsAndContext
                            )
                            errorsAndContext
                            imports
                        )
                )


createFinalModuleEvaluationVisitor :
    { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), context ) -> RuleModuleVisitor)
    -> ( List (Error {}), context )
    -> Maybe (context -> List (Error {}))
    -> Maybe (() -> RuleModuleVisitor)
createFinalModuleEvaluationVisitor params raise ( errors, context ) maybeVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just (\() -> raise ( qualifyErrors params (visitor context) errors, context ))


runVisitor : (RuleModuleVisitorOperations RuleModuleVisitor -> Maybe (a -> RuleModuleVisitor)) -> a -> RuleModuleVisitor -> RuleModuleVisitor
runVisitor field node ((RuleModuleVisitor ruleModuleVisitor) as original) =
    case field ruleModuleVisitor of
        Just visitor ->
            visitor node

        Nothing ->
            original


runVisitor2 : (RuleModuleVisitorOperations RuleModuleVisitor -> Maybe (a -> b -> RuleModuleVisitor)) -> a -> b -> RuleModuleVisitor -> RuleModuleVisitor
runVisitor2 field a b ((RuleModuleVisitor ruleModuleVisitor) as original) =
    case field ruleModuleVisitor of
        Just visitor ->
            visitor a b

        Nothing ->
            original


visitExpression :
    ExpressionRelatedVisitors moduleContext
    -> Node Expression
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitExpression expressionRelatedVisitors node errorsAndContext =
    -- IGNORE TCO
    --   Is there a way to make this function TCO?
    case Node.value node of
        Expression.LetExpression letBlock ->
            errorsAndContext
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnEnter node
                |> ListExtra.foldlSwitched (\decl acc -> visitLetDeclaration expressionRelatedVisitors (Node (Node.range node) letBlock) decl acc) letBlock.declarations
                |> visitExpression expressionRelatedVisitors letBlock.expression
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnExit node

        Expression.CaseExpression caseBlock ->
            errorsAndContext
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnEnter node
                |> visitExpression expressionRelatedVisitors caseBlock.expression
                |> ListExtra.foldlSwitched (\case_ acc -> visitCaseBranch expressionRelatedVisitors (Node (Node.range node) caseBlock) case_ acc) caseBlock.cases
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnExit node

        _ ->
            errorsAndContext
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnEnter node
                |> ListExtra.foldlSwitched (\expr acc -> visitExpression expressionRelatedVisitors expr acc) (expressionChildren node)
                |> accumulateWithMaybe expressionRelatedVisitors.expressionVisitorsOnExit node


visitOnlyExpressions :
    Maybe (Visitor Expression moduleContext)
    -> Maybe (Visitor Expression moduleContext)
    -> Node Expression
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitOnlyExpressions expressionVisitorsOnEnter expressionVisitorsOnExit node errorsAndContext =
    -- IGNORE TCO
    errorsAndContext
        |> accumulateWithMaybe expressionVisitorsOnEnter node
        |> ListExtra.foldlSwitched (\expr acc -> visitOnlyExpressions expressionVisitorsOnEnter expressionVisitorsOnExit expr acc) (expressionChildren node)
        |> accumulateWithMaybe expressionVisitorsOnExit node


visitOnlyExpressionsOnlyOnEnter :
    Maybe (Visitor Expression moduleContext)
    -> Node Expression
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitOnlyExpressionsOnlyOnEnter expressionVisitorsOnEnter node errorsAndContext =
    List.foldl
        (\exprNode acc -> accumulateWithMaybe expressionVisitorsOnEnter exprNode acc)
        errorsAndContext
        (expressionChildrenTCO [ node ] [])


expressionChildrenTCO : List (Node Expression) -> List (Node Expression) -> List (Node Expression)
expressionChildrenTCO nodesToVisit acc =
    case nodesToVisit of
        [] ->
            List.reverse acc

        head :: rest ->
            case Node.value head of
                Expression.Application expressions ->
                    expressionChildrenTCO (List.append expressions rest) (head :: acc)

                Expression.ListExpr expressions ->
                    expressionChildrenTCO (List.append expressions rest) (head :: acc)

                Expression.RecordExpr fields ->
                    expressionChildrenTCO
                        (List.foldl (\(Node _ ( _, expr )) toVisitAcc -> expr :: toVisitAcc) rest fields)
                        (head :: acc)

                Expression.RecordUpdateExpression _ setters ->
                    expressionChildrenTCO
                        (List.foldl (\(Node _ ( _, expr )) toVisitAcc -> expr :: toVisitAcc) rest setters)
                        (head :: acc)

                Expression.ParenthesizedExpression expr ->
                    expressionChildrenTCO (expr :: rest) (head :: acc)

                Expression.OperatorApplication _ direction left right ->
                    let
                        nodeStack : List (Node Expression)
                        nodeStack =
                            case direction of
                                Infix.Left ->
                                    left :: right :: rest

                                Infix.Right ->
                                    right :: left :: rest

                                Infix.Non ->
                                    left :: right :: rest
                    in
                    expressionChildrenTCO nodeStack (head :: acc)

                Expression.IfBlock cond then_ else_ ->
                    expressionChildrenTCO
                        (cond :: then_ :: else_ :: rest)
                        (head :: acc)

                Expression.LetExpression { expression, declarations } ->
                    expressionChildrenTCO
                        (List.foldl
                            (\declaration toVisitAcc ->
                                case Node.value declaration of
                                    Expression.LetFunction function ->
                                        functionToExpression function :: toVisitAcc

                                    Expression.LetDestructuring _ expr ->
                                        expr :: toVisitAcc
                            )
                            (expression :: rest)
                            declarations
                        )
                        (head :: acc)

                Expression.CaseExpression { expression, cases } ->
                    expressionChildrenTCO
                        (expression
                            :: List.foldl (\( _, caseExpression ) toVisitAcc -> caseExpression :: toVisitAcc) rest cases
                        )
                        (head :: acc)

                Expression.LambdaExpression { expression } ->
                    expressionChildrenTCO (expression :: rest) (head :: acc)

                Expression.TupledExpression expressions ->
                    expressionChildrenTCO (List.append expressions rest) (head :: acc)

                Expression.Negation expression ->
                    expressionChildrenTCO (expression :: rest) (head :: acc)

                Expression.RecordAccess expression _ ->
                    expressionChildrenTCO (expression :: rest) (head :: acc)

                _ ->
                    expressionChildrenTCO rest (head :: acc)


visitLetDeclaration :
    ExpressionRelatedVisitors moduleContext
    -> Node Expression.LetBlock
    -> Node Expression.LetDeclaration
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitLetDeclaration expressionRelatedVisitors letBlockWithRange ((Node _ letDeclaration) as letDeclarationWithRange) errorsAndContext =
    let
        expressionNode : Node Expression
        expressionNode =
            case letDeclaration of
                Expression.LetFunction function ->
                    functionToExpression function

                Expression.LetDestructuring _ expr ->
                    expr
    in
    errorsAndContext
        |> accumulateWithMaybe2 expressionRelatedVisitors.letDeclarationVisitorOnEnter letBlockWithRange letDeclarationWithRange
        |> visitExpression expressionRelatedVisitors expressionNode
        |> accumulateWithMaybe2 expressionRelatedVisitors.letDeclarationVisitorOnExit letBlockWithRange letDeclarationWithRange


visitCaseBranch :
    ExpressionRelatedVisitors moduleContext
    -> Node Expression.CaseBlock
    -> ( Node Pattern, Node Expression )
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitCaseBranch expressionRelatedVisitors caseBlockWithRange (( _, caseExpression ) as caseBranch) errorsAndContext =
    errorsAndContext
        |> accumulateWithMaybe2 expressionRelatedVisitors.caseBranchVisitorOnEnter caseBlockWithRange caseBranch
        |> visitExpression expressionRelatedVisitors caseExpression
        |> accumulateWithMaybe2 expressionRelatedVisitors.caseBranchVisitorOnExit caseBlockWithRange caseBranch


expressionChildren : Node Expression -> List (Node Expression)
expressionChildren node =
    case Node.value node of
        Expression.Application expressions ->
            expressions

        Expression.ListExpr elements ->
            elements

        Expression.RecordExpr fields ->
            List.map (\(Node _ ( _, expr )) -> expr) fields

        Expression.RecordUpdateExpression _ setters ->
            List.map (\(Node _ ( _, expr )) -> expr) setters

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


functionToExpression : Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression


moduleNameNode : Node Module -> Node ModuleName
moduleNameNode node =
    case Node.value node of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName


accumulateWithMaybe :
    Maybe (a -> context -> ( List (Error {}), context ))
    -> a
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateWithMaybe maybeVisitor element errorsAndContext =
    case maybeVisitor of
        Just visitor ->
            accumulate (\context -> visitor element context) errorsAndContext

        Nothing ->
            errorsAndContext


accumulateWithMaybe2 :
    Maybe (a -> b -> context -> ( List (Error {}), context ))
    -> a
    -> b
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateWithMaybe2 maybeVisitor a b errorsAndContext =
    case maybeVisitor of
        Just visitor ->
            accumulate (\context -> visitor a b context) errorsAndContext

        Nothing ->
            errorsAndContext


accumulateModuleDocumentationVisitor :
    Maybe (Maybe (Node String) -> context -> ( List (Error {}), context ))
    -> Elm.Syntax.File.File
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateModuleDocumentationVisitor maybeVisitor ast errorsAndContext =
    case maybeVisitor of
        Just visitor ->
            let
                moduleDocumentation : Maybe (Node String)
                moduleDocumentation =
                    findModuleDocumentation ast
            in
            accumulate (\context -> visitor moduleDocumentation context) errorsAndContext

        Nothing ->
            errorsAndContext


findModuleDocumentation : Elm.Syntax.File.File -> Maybe (Node String)
findModuleDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                firstImport :: _ ->
                    (Node.range firstImport).start.row

                [] ->
                    case ast.declarations of
                        firstDeclaration :: _ ->
                            (Node.range firstDeclaration).start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    findModuleDocumentationBeforeCutOffLine cutOffLine ast.comments


findModuleDocumentationBeforeCutOffLine : Int -> List (Node String) -> Maybe (Node String)
findModuleDocumentationBeforeCutOffLine cutOffLine comments =
    case comments of
        [] ->
            Nothing

        ((Node range content) as comment) :: restOfComments ->
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just comment

            else
                findModuleDocumentationBeforeCutOffLine cutOffLine restOfComments


accumulateListWithMaybe :
    Maybe (a -> context -> ( List (Error {}), context ))
    -> List a
    -> ( List (Error {}), context )
    -> ( List (Error {}), context )
accumulateListWithMaybe maybeVisitor elements errorsAndContext =
    case maybeVisitor of
        Just visitor ->
            List.foldl (\element acc -> accumulate (\context -> visitor element context) acc) errorsAndContext elements

        Nothing ->
            errorsAndContext


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulate : (context -> ( List (Error {}), context )) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulate visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( List.append newErrors previousErrors, newContext )


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulateWithParams : { ruleName : String, exceptions : Exceptions, filePath : String } -> (context -> ( List (Error {}), context )) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulateWithParams params visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( qualifyErrors params newErrors previousErrors, newContext )



-- INITIALIZING WITH CONTEXT
-- TODO Move this to a different module later on


{-| Create a module context from a project context or the other way around.
Use functions like [`withModuleName`](#withModuleName) to request more information.
-}
type ContextCreator from to
    = ContextCreator (AvailableData -> from -> to) RequestedData


requestedDataFromContextCreator : ContextCreator from to -> RequestedData
requestedDataFromContextCreator (ContextCreator _ requestedData) =
    requestedData


{-| Initialize a new context creator.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\moduleName () ->
                { moduleName = moduleName

                -- ...other fields
                }
            )
            |> Rule.withModuleName

-}
initContextCreator : (from -> to) -> ContextCreator from to
initContextCreator fromProjectToModule =
    -- TODO Try to get rid of the ()/from when using in a module rule
    ContextCreator
        (always fromProjectToModule)
        RequestedData.none


applyContextCreator : AvailableData -> ContextCreator from to -> from -> to
applyContextCreator data (ContextCreator fn _) from =
    fn data from


{-| Request metadata about the module.

**@deprecated**: Use more practical functions like

  - [`withModuleName`](#withModuleName)
  - [`withModuleNameNode`](#withModuleNameNode)
  - [`withIsInSourceDirectories`](#withIsInSourceDirectories)

-}
withMetadata : ContextCreator Metadata (from -> to) -> ContextCreator from to
withMetadata (ContextCreator fn requestedData) =
    ContextCreator
        (\data ->
            fn data
                (createMetadata
                    { moduleNameNode = moduleNameNode data.ast.moduleDefinition
                    , isInSourceDirectories = data.isInSourceDirectories
                    }
                )
        )
        requestedData


{-| Request the name of the module.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\moduleName () ->
                { moduleName = moduleName

                -- ...other fields
                }
            )
            |> Rule.withModuleName

-}
withModuleName : ContextCreator ModuleName (from -> to) -> ContextCreator from to
withModuleName (ContextCreator fn requestedData) =
    ContextCreator
        (\data -> fn data (moduleNameNode data.ast.moduleDefinition |> Node.value))
        requestedData


{-| Request the node corresponding to the name of the module.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\moduleNameNode () ->
                { moduleNameNode = moduleNameNode

                -- ...other fields
                }
            )
            |> Rule.withModuleNameNode

-}
withModuleNameNode : ContextCreator (Node ModuleName) (from -> to) -> ContextCreator from to
withModuleNameNode (ContextCreator fn requestedData) =
    ContextCreator (\data -> fn data (moduleNameNode data.ast.moduleDefinition))
        requestedData


{-| Request to know whether the current module is in the "source-directories" of the project. You can use this information to
know whether the module is part of the tests or of the production code.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\isInSourceDirectories () ->
                { isInSourceDirectories = isInSourceDirectories

                -- ...other fields
                }
            )
            |> Rule.withIsInSourceDirectories

-}
withIsInSourceDirectories : ContextCreator Bool (from -> to) -> ContextCreator from to
withIsInSourceDirectories (ContextCreator fn requestedData) =
    ContextCreator
        (\data -> fn data data.isInSourceDirectories)
        requestedData


{-| Request to know whether the errors for the current module has been ignored for this particular rule.
This may be useful to reduce the amount of work related to ignored files — like collecting unnecessary data or reporting
errors — when that will ignored anyway.

Note that for module rules, ignored files will be skipped automatically anyway.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\isFileIgnored () ->
                { isFileIgnored = isFileIgnored

                -- ...other fields
                }
            )
            |> Rule.withIsFileIgnored

-}
withIsFileIgnored : ContextCreator Bool (from -> to) -> ContextCreator from to
withIsFileIgnored (ContextCreator fn (RequestedData requested)) =
    ContextCreator
        (\data -> fn data data.isFileIgnored)
        (RequestedData { requested | ignoredFiles = True })


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
        Rule.newModuleRuleSchemaUsingContextCreator "NoHtmlButton" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema
            |> Rule.ignoreErrorsForFiles [ "src/Colors.elm" ]

    initialContext : Rule.ContextCreator () Context
    initialContext =
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


{-| Request the full [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) for the current module.

This can be useful if you wish to avoid initializing the module context with dummy data future node visits can replace them.

For instance, if you wish to know what is exposed from a module, you may need to visit the module definition and then
the list of declarations. If you need this information earlier on, you will have to provide dummy data at context
initialization and store some intermediary data.

Using the full AST, you can simplify the implementation by computing the data in the context creator, without the use of visitors.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\ast () ->
                { exposed = collectExposed ast.moduleDefinition ast.declarations

                -- ...other fields
                }
            )
            |> Rule.withFullAst

-}
withFullAst : ContextCreator Elm.Syntax.File.File (from -> to) -> ContextCreator from to
withFullAst (ContextCreator fn requested) =
    ContextCreator
        (\data -> fn data data.ast)
        requested


{-| Request the module documentation. Modules don't always have a documentation.
When that is the case, the module documentation will be `Nothing`.

    contextCreator : Rule.ContextCreator () Context
    contextCreator =
        Rule.initContextCreator
            (\moduleDocumentation () ->
                { moduleDocumentation = moduleDocumentation

                -- ...other fields
                }
            )
            |> Rule.withModuleDocumentation

-}
withModuleDocumentation : ContextCreator (Maybe (Node String)) (from -> to) -> ContextCreator from to
withModuleDocumentation (ContextCreator fn requested) =
    ContextCreator
        (\data -> fn data (findModuleDocumentation data.ast))
        requested


{-| Request the [module key](#ModuleKey) for this module.

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "NoMissingSubscriptionsCall" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextUsingContextCreator
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }

    fromModuleToProject : Rule.ContextCreator () Context
    fromModuleToProject =
        Rule.initContextCreator
            (\moduleKey () -> { moduleKey = moduleKey })
            |> Rule.withModuleKey

-}
withModuleKey : ContextCreator ModuleKey (from -> to) -> ContextCreator from to
withModuleKey (ContextCreator fn requestedData) =
    ContextCreator
        (\data -> fn data data.moduleKey)
        requestedData


{-| Request the file path for this module, relative to the project's `elm.json`.

Using [`newModuleRuleSchemaUsingContextCreator`](#newModuleRuleSchemaUsingContextCreator):

    rule : Rule
    rule =
        Rule.newModuleRuleSchemaUsingContextCreator "YourRuleName" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    initialContext : Rule.ContextCreator () Context
    initialContext =
        Rule.initContextCreator
            (\filePath () -> { filePath = filePath })
            |> Rule.withFilePath

Using [`withModuleContextUsingContextCreator`](#withModuleContextUsingContextCreator) in a project rule:

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "YourRuleName" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextUsingContextCreator
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }

    fromModuleToProject : Rule.ContextCreator () Context
    fromModuleToProject =
        Rule.initContextCreator
            (\filePath () -> { filePath = filePath })
            |> Rule.withFilePath

-}
withFilePath : ContextCreator String (from -> to) -> ContextCreator from to
withFilePath (ContextCreator fn requestedData) =
    ContextCreator
        (\data -> fn data data.filePath)
        requestedData


{-| Requests access to a function that gives you the source code at a given range.

    rule : Rule
    rule =
        Rule.newModuleRuleSchemaUsingContextCreator "YourRuleName" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        { extractSourceCode : Range -> String
        }

    initialContext : Rule.ContextCreator () Context
    initialContext =
        Rule.initContextCreator
            (\extractSourceCode () -> { extractSourceCode = extractSourceCode })
            |> Rule.withSourceCodeExtractor

The motivation for this capability was for allowing to provide higher-quality fixes, especially where you'd need to **move** or **copy**
code from one place to another (example: [when switching the branches of an if expression](https://github.com/jfmengels/elm-review/blob/master/tests/NoNegationInIfCondition.elm)).

I discourage using this functionality to explore the source code, as the different visitor functions make for a nicer
experience.

-}
withSourceCodeExtractor : ContextCreator (Range -> String) (from -> to) -> ContextCreator from to
withSourceCodeExtractor (ContextCreator fn (RequestedData requested)) =
    ContextCreator
        (\data -> fn data data.extractSourceCode)
        (RequestedData { requested | sourceCodeExtractor = True })


type alias AvailableData =
    { ast : Elm.Syntax.File.File
    , moduleKey : ModuleKey
    , moduleNameLookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , filePath : String
    , isInSourceDirectories : Bool
    , isFileIgnored : Bool
    }



-- METADATA


{-| Metadata for the module being visited.

**@deprecated**: More practical functions have been made available since the introduction of this type.

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

**@deprecated**: Use the more practical [`withModuleName`](#withModuleName) instead.

-}
moduleNameFromMetadata : Metadata -> ModuleName
moduleNameFromMetadata (Metadata metadata) =
    Node.value metadata.moduleNameNode


{-| Get the [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Node#Node) to the module name of the current module.

**@deprecated**: Use the more practical [`withModuleNameNode`](#withModuleNameNode) instead.

-}
moduleNameNodeFromMetadata : Metadata -> Node ModuleName
moduleNameNodeFromMetadata (Metadata metadata) =
    metadata.moduleNameNode


{-| Learn whether the current module is in the "source-directories" of the project. You can use this information to
know whether the module is part of the tests or of the production code.

**@deprecated**: Use the more practical [`withIsInSourceDirectories`](#withIsInSourceDirectories) instead.

-}
isInSourceDirectories : Metadata -> Bool
isInSourceDirectories (Metadata metadata) =
    metadata.isInSourceDirectories



-- LOGS


startedRule : String -> List ( String, Encode.Value )
startedRule name =
    [ ( "type", Encode.string "timer-start" )
    , ( "metric", Encode.string ("Running " ++ name) )
    ]


endedRule : String -> List ( String, Encode.Value )
endedRule name =
    [ ( "type", Encode.string "timer-end" )
    , ( "metric", Encode.string ("Running " ++ name) )
    ]


fixedError : FixedErrors -> { ruleName : String, filePath : String } -> List ( String, Encode.Value )
fixedError fixedErrors data =
    [ ( "type", Encode.string "apply-fix" )
    , ( "ruleName", Encode.string data.ruleName )
    , ( "filePath", Encode.string data.filePath )
    , ( "count", Encode.int (FixedErrors.count fixedErrors) )
    ]
