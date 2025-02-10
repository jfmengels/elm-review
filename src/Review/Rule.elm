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
    , withExtraFilesModuleVisitor
    , ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withModuleContextWithErrors, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDirectDependenciesProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withExtraFilesProjectVisitor, withContextFromImportedModules
    , providesFixesForProjectRule
    , ContextCreator, initContextCreator, withModuleName, withModuleNameNode, withIsInSourceDirectories, withFilePath, withIsFileIgnored, withModuleNameLookupTable, withModuleKey, withSourceCodeExtractor, withFullAst, withModuleDocumentation
    , Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix
    , ElmJsonKey, errorForElmJson, errorForElmJsonWithFix
    , ReadmeKey, errorForReadme, errorForReadmeWithFix
    , ExtraFileKey, errorForExtraFile, errorForExtraFileWithFix
    , globalError, configurationError
    , FixV2, withFixesV2, editModule, removeModule, editElmJson, editReadme, editExtraFile, removeExtraFile
    , ignoreErrorsForDirectories, ignoreErrorsForFiles, filterErrorsForFiles
    , withDataExtractor, preventExtract
    , reviewV3, reviewV2, review, ProjectData, ruleName, ruleProvidesFixes, ruleKnowsAboutIgnoredFiles, ruleRequestedFiles, withRuleId, getConfigurationError
    , ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFilePath, errorTarget, errorFixesV2, errorFixProblem
    , Required, Forbidden
    , errorFixes, errorFixFailure
    , Metadata, withMetadata, moduleNameFromMetadata, moduleNameNodeFromMetadata, isInSourceDirectories
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
      - Extra files that are not analyzed by default, visited by [`withExtraFilesModuleVisitor`](#withExtraFilesModuleVisitor)
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
@docs withExtraFilesModuleVisitor


## Creating a project rule

Project rules can look at the global picture of an Elm project. Contrary to module
rules, which forget everything about the module they were looking at when going from
one module to another, project rules can retain information about previously
analyzed modules, and use it to report errors when analyzing a different module or
after all modules have been visited.

Project rules can also report errors in the `elm.json` or the `README.md` files.

If you are new to writing rules, I would recommend learning [how to build a module rule](#creating-a-module-rule)
first, as they are in practice a simpler version of project rules.

@docs ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withModuleContextUsingContextCreator, withModuleContextWithErrors, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDirectDependenciesProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withExtraFilesProjectVisitor, withContextFromImportedModules
@docs providesFixesForProjectRule


## Requesting more information

@docs ContextCreator, initContextCreator, withModuleName, withModuleNameNode, withIsInSourceDirectories, withFilePath, withIsFileIgnored, withModuleNameLookupTable, withModuleKey, withSourceCodeExtractor, withFullAst, withModuleDocumentation


## Errors

@docs Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix
@docs ElmJsonKey, errorForElmJson, errorForElmJsonWithFix
@docs ReadmeKey, errorForReadme, errorForReadmeWithFix
@docs ExtraFileKey, errorForExtraFile, errorForExtraFileWithFix
@docs globalError, configurationError


## Multi-file automatic fixes

If you wish to provide automatic fixes for the file the error was reported for, you can use the previously listed functions
([`errorWithFix`](#errorWithFix), [`errorForModuleWithFix`](#errorForModuleWithFix), [`errorForElmJsonWithFix`](#errorForElmJsonWithFix),
[`errorForReadmeWithFix`](#errorForReadmeWithFix) and [`errorForExtraFile`](#errorForExtraFile)).

If you wish to provide an automatic fix that spawns multiple files or removes files, then use the following functions.

I highly recommend reading at the guidelines in [`Review.Fix`](./Review-Fix) to understand how to provide good fixes.

**NOTE**: The type names are a bit confusing, and will be improved in the next major version.
The [`Review.Fix.Fix`](./Review-Fix#Fix) type will likely be renamed to `Edit` as it represents edits inside of a specific file,
and [`FixV2`](#FixV2) will likely be renamed to `Fix` and moved to another module.
In the meantime, some functions still refer to edits as "fixes", sorry about the confusion!

@docs FixV2, withFixesV2, editModule, removeModule, editElmJson, editReadme, editExtraFile, removeExtraFile


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

@docs reviewV3, reviewV2, review, ProjectData, ruleName, ruleProvidesFixes, ruleKnowsAboutIgnoredFiles, ruleRequestedFiles, withRuleId, getConfigurationError

@docs ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFilePath, errorTarget, errorFixesV2, errorFixProblem


# Internals

@docs Required, Forbidden


# Deprecated

These types and functions are deprecated and should not be used, as there are now better alternatives.

@docs errorFixes, errorFixFailure
@docs Metadata, withMetadata, moduleNameFromMetadata, moduleNameNodeFromMetadata, isInSourceDirectories

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
import Json.Decode as Decode
import Json.Encode as Encode
import Review.Cache.ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.Cache.EndAnalysis as EndAnalysisCache
import Review.Cache.ExtraFile as ExtraFile
import Review.Cache.Module as ModuleCache
import Review.Cache.ProjectFile as ProjectFileCache
import Review.ElmProjectEncoder
import Review.Error.FileTarget as FileTarget exposing (FileTarget)
import Review.Error.Fixes as ErrorFixes exposing (ErrorFixes, FixKind)
import Review.Error.ReviewError
import Review.Error.Target as Target exposing (Target)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.FilePath exposing (FilePath)
import Review.FilePattern as FilePattern exposing (FilePattern)
import Review.Fix as Fix exposing (Fix)
import Review.Fix.Edit exposing (Edit)
import Review.Fix.FixProblem as FixProblem exposing (FixProblem)
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
import Unicode
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict
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
        , ruleProjectVisitor : Result { message : String, details : List String } (ValidProject -> ChangeableRuleData -> RuleProjectVisitor)
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
    , extraFileRequest : ExtraFileRequest
    , readmeVisitor : Maybe (Maybe String -> moduleContext -> moduleContext)
    , extraFilesVisitor : Maybe (ExtraFileData -> moduleContext -> moduleContext)
    , dependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    , directDependenciesVisitor : Maybe (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
    }


type alias ExtraFileRequest =
    Result (List String) (List { files : List { pattern : String, included : Bool }, excludedDirectories : List String })



-- REVIEWING


{-| **DEPRECATED:** Use [`reviewV3`](#reviewV3) instead.

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

        Ok validProject ->
            case checkForConfigurationErrors validProject rules [] of
                Err configurationErrors ->
                    ( configurationErrors, rules )

                Ok ruleProjectVisitors ->
                    let
                        runRulesResult : { errors : List ReviewError, fixedErrors : Dict String (List ReviewError), rules : List Rule, project : Project, extracts : Dict String Encode.Value }
                        runRulesResult =
                            runRules ReviewOptions.defaults ruleProjectVisitors validProject
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
    case getValidProjectAndRules project rules of
        Ok ( validProject, ruleProjectVisitors ) ->
            runReviewForV2 ReviewOptions.defaults validProject ruleProjectVisitors

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
            { errors, rules, projectData, extracts, fixedErrors } =
                -- Replace `config` by `rules` next time you call reviewV3
                Rule.reviewV3
                    ReviewOptions.defaults
                    config
                    project
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
        , rules : List Rule
        , project : Project
        , extracts : Dict String Encode.Value
        , fixedErrors : Dict String (List ReviewError)
        }
reviewV3 reviewOptions rules project =
    case getValidProjectAndRules project rules of
        Ok ( validProject, ruleProjectVisitors ) ->
            runRules reviewOptions ruleProjectVisitors validProject

        Err errors ->
            { errors = errors
            , rules = rules
            , project = project
            , extracts = Dict.empty
            , fixedErrors = Dict.empty
            }


getValidProjectAndRules : Project -> List Rule -> Result (List ReviewError) ( ValidProject, List RuleProjectVisitor )
getValidProjectAndRules project rules =
    getModulesSortedByImport project
        |> Result.andThen
            (\validProject ->
                checkForConfigurationErrors validProject rules []
                    |> Result.map (Tuple.pair validProject)
            )


checkForConfigurationErrors : ValidProject -> List Rule -> List RuleProjectVisitor -> Result (List ReviewError) (List RuleProjectVisitor)
checkForConfigurationErrors project rules rulesToRunAcc =
    case rules of
        [] ->
            Ok rulesToRunAcc

        (Rule rule) :: remainingRules ->
            case rule.ruleProjectVisitor of
                Ok ruleProjectVisitor ->
                    checkForConfigurationErrors
                        project
                        remainingRules
                        (ruleProjectVisitor
                            project
                            { exceptions = rule.exceptions
                            , ruleId = rule.id
                            , requestedData = rule.requestedData
                            }
                            :: rulesToRunAcc
                        )

                Err _ ->
                    Err (collectConfigurationErrors rules)


collectConfigurationErrors : List Rule -> List ReviewError
collectConfigurationErrors rules =
    List.filterMap
        (\(Rule rule) ->
            case rule.ruleProjectVisitor of
                Err { message, details } ->
                    Just
                        (Review.Error.ReviewError.fromBaseError
                            { filePath = "CONFIGURATION ERROR"
                            , ruleName = rule.name
                            , message = message
                            , details = details
                            , range = Range.emptyRange
                            , fixes = ErrorFixes.none
                            , fixProblem = Nothing
                            , target = Target.Global
                            , preventsExtract = False
                            }
                        )

                Ok _ ->
                    Nothing
        )
        rules


getModulesSortedByImport : Project -> Result (List ReviewError) ValidProject
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


importCycleError : List String -> ReviewError
importCycleError cycle =
    ImportCycle.error cycle
        |> elmReviewGlobalError
        |> setRuleName "Incorrect project"
        |> errorToReviewError


runReviewForV2 : ReviewOptions -> ValidProject -> List RuleProjectVisitor -> { errors : List ReviewError, rules : List Rule, projectData : Maybe ProjectData }
runReviewForV2 reviewOptions project ruleProjectVisitors =
    let
        runResult : { errors : List ReviewError, fixedErrors : Dict String (List ReviewError), rules : List Rule, project : Project, extracts : Dict String Encode.Value }
        runResult =
            runRules reviewOptions ruleProjectVisitors project
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
    -> List RuleProjectVisitor
    -> ValidProject
    -> { errors : List ReviewError, fixedErrors : Dict String (List ReviewError), rules : List Rule, project : Project, extracts : Dict String Encode.Value }
runRules (ReviewOptionsInternal reviewOptions) ruleProjectVisitors project =
    let
        result : { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
        result =
            runProjectVisitor
                reviewOptions
                ruleProjectVisitors
                FixedErrors.empty
                project

        { errors, rules, extracts } =
            computeErrorsAndRulesAndExtracts reviewOptions result.ruleProjectVisitors
    in
    { errors = errors
    , rules = rules
    , extracts = extracts
    , fixedErrors = FixedErrors.toDict result.fixedErrors
    , project = ValidProject.toRegularProject result.project
    }


computeErrorsAndRulesAndExtracts : ReviewOptionsData -> List RuleProjectVisitor -> { errors : List ReviewError, rules : List Rule, extracts : Dict String Encode.Value }
computeErrorsAndRulesAndExtracts reviewOptions ruleProjectVisitors =
    if reviewOptions.extract then
        List.foldl
            (\(RuleProjectVisitor rule) { errors, rules, extracts } ->
                let
                    ( newErrors, canComputeExtract ) =
                        List.foldl
                            (\(Error err) ( accErrors, canComputeExtract_ ) ->
                                ( Review.Error.ReviewError.fromBaseError err :: accErrors
                                , canComputeExtract_ && not err.preventsExtract
                                )
                            )
                            ( errors, True )
                            (rule.getErrors ())

                    ( newExtracts, RuleProjectVisitor newRule ) =
                        if canComputeExtract then
                            rule.dataExtractVisitor reviewOptions extracts

                        else
                            ( extracts, RuleProjectVisitor rule )
                in
                { errors = newErrors
                , rules = newRule.backToRule () :: rules
                , extracts = newExtracts
                }
            )
            { errors = [], rules = [], extracts = Dict.empty }
            ruleProjectVisitors

    else
        { errors =
            List.concatMap
                (\(RuleProjectVisitor rule) -> rule.getErrors () |> List.map errorToReviewError)
                ruleProjectVisitors
        , rules = List.map (\(RuleProjectVisitor rule) -> rule.backToRule ()) ruleProjectVisitors
        , extracts = Dict.empty
        }


{-| Let `elm-review` know that this rule may provide fixes in the reported errors.

**@deprecated**: This information is not necessary anymore.

This information is hard for `elm-review` to deduce on its own, but can be very useful for improving the performance of
the tool while running in fix mode.

If your rule is a project rule, then you should use [`providesFixesForProjectRule`](#providesFixesForProjectRule) instead.

-}
providesFixesForModuleRule : ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema schemaState moduleContext
providesFixesForModuleRule (ModuleRuleSchema moduleRuleSchema) =
    -- TODO Breaking change: Remove providesFixes field, unless we find a new use for it.
    ModuleRuleSchema { moduleRuleSchema | providesFixes = True }


{-| Let `elm-review` know that this rule may provide fixes in the reported errors.
**@deprecated**: This information is not necessary anymore.

This information is hard for `elm-review` to deduce on its own, but can be very useful for improving the performance of
the tool while running in fix mode.

If your rule is a module rule, then you should use [`providesFixesForModuleRule`](#providesFixesForModuleRule) instead.

-}
providesFixesForProjectRule : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleSchema schemaState projectContext moduleContext
providesFixesForProjectRule (ProjectRuleSchema projectRuleSchema) =
    -- TODO Breaking change: Remove providesFixes field, unless we find a new use for it.
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


{-| Get the patterns for extra files that this rule requested.

You should not have to use this when writing a rule.

-}
ruleRequestedFiles : Rule -> List { files : List { pattern : String, included : Bool }, excludedDirectories : List String }
ruleRequestedFiles (Rule rule) =
    let
        (RequestedData requestedData) =
            rule.requestedData
    in
    requestedData.files


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
        , extraFilesVisitor = Nothing
        , extraFileRequest = Ok []
        , dependenciesVisitor = Nothing
        , directDependenciesVisitor = Nothing
        , providesFixes = False
        }


{-| Same as [`newModuleRuleSchema`](#newModuleRuleSchema), except that you can request for data to help initialize the context.

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
        , extraFilesVisitor = Nothing
        , extraFileRequest = Ok []
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
                , extraFilesVisitor = compactExtraFilesVisitor schema.extraFilesVisitor
                , extraFileRequest = schema.extraFileRequest
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
                , extraFilesVisitor = Nothing
                , extraFileRequest = Ok []
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


compactExtraFilesVisitor :
    Maybe (ExtraFileData -> moduleContext -> moduleContext)
    -> Maybe (ExtraFileData -> moduleContext -> ( List nothing, moduleContext ))
compactExtraFilesVisitor maybeExtraFilesVisitor =
    case maybeExtraFilesVisitor of
        Just extraFilesVisitor ->
            Just (\files moduleContext -> ( [], extraFilesVisitor files moduleContext ))

        Nothing ->
            Nothing



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
    , extraFilesVisitor : Maybe (ExtraFileData -> projectContext -> ( List (Error {}), projectContext ))
    , extraFileRequest : ExtraFileRequest
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
    | ImportedModulesFirst


{-| Creates a schema for a project rule. Will require adding project visitors and calling
[`fromProjectRuleSchema`](#fromProjectRuleSchema) to create a usable [`Rule`](#Rule).

The first argument is the rule name. I _highly_ recommend naming it just like the
module name (including all the `.` there may be).

The second argument is the initial `projectContext`, i.e. the data that the rule will
accumulate as the project will be traversed, and allows the rule to know/remember
what happens in other parts of the project.

**NOTE**: Do not store functions, JSON values or regular expressions in your project context, as they will be
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

Project rules traverse the project in the following order:

  - Read and/or report errors in project files
      - The `elm.json` file, visited by [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
      - The `README.md` file, visited by [`withReadmeProjectVisitor`](#withReadmeProjectVisitor)
      - Extra files that are not analyzed by default, visited by [`withExtraFilesProjectVisitor`](#withExtraFilesProjectVisitor)
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
        , extraFilesVisitor = Nothing
        , extraFileRequest = Ok []
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
fromProjectRuleSchema (ProjectRuleSchema schema) =
    case schema.extraFileRequest of
        Ok extraFileGlobs ->
            Rule
                { name = schema.name
                , id = 0
                , exceptions = Exceptions.init
                , requestedData =
                    RequestedData.combine
                        (Maybe.map requestedDataFromContextCreator schema.moduleContextCreator)
                        (Maybe.map (.fromModuleToProject >> requestedDataFromContextCreator) schema.folder)
                        |> RequestedData.withFiles extraFileGlobs
                , providesFixes = schema.providesFixes
                , ruleProjectVisitor =
                    Ok
                        (\project ruleData ->
                            createRuleProjectVisitor
                                schema
                                project
                                ruleData
                                (initialCacheMarker schema.name ruleData.ruleId emptyCache)
                        )
                }

        Err faultyGlobs ->
            configurationError schema.name
                { message = "Invalid globs provided when requesting extra files"
                , details =
                    [ "This rule requested additional files, but did so by specifying globs that I could not make sense of:"
                    , faultyGlobs
                        |> List.indexedMap (\index glob -> "  " ++ String.fromInt (index + 1) ++ ". " ++ glob)
                        |> String.join "\n"
                    ]
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
    , extraFiles = Nothing
    , dependencies = Nothing
    , moduleContexts = Dict.empty
    , finalEvaluationErrors = Nothing
    , extract = Nothing
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
            , moduleDocumentation = Nothing
            , moduleNameLookupTable = ModuleNameLookupTableInternal.empty []
            , extractSourceCode = always "dummy"
            , filePath = "dummy file path"
            , isInSourceDirectories = True
            }

        initialModuleContext : moduleContext
        initialModuleContext =
            applyContextCreator dummyAvailableData False moduleContextCreator initialProjectContext

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
                , extraFilesVisitor = Nothing
                , extraFileRequest = Ok []
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
        |> removeExtensibleRecordFromModuleRuleSchema
    , moduleContextCreator
    )


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
    function >> removeExtensibleRecordFromModuleRuleSchema


removeExtensibleRecordFromModuleRuleSchema : ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema a moduleContext
removeExtensibleRecordFromModuleRuleSchema (ModuleRuleSchema param) =
    ModuleRuleSchema param


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
        , providesFixes = False
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
                        initContextCreator
                            (\moduleKey moduleNameNode_ moduleContext ->
                                ( [], functions.fromModuleToProject moduleKey moduleNameNode_ moduleContext )
                            )
                            |> withModuleKey
                            |> withModuleNameNode
                    , foldProjectContexts = functions.foldProjectContexts
                    }
        }


{-| Use a [`ContextCreator`](#ContextCreator) to initialize your `moduleContext` and `projectContext`. This will allow
you to request more information.

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
                    { fromModuleToProject = mapContextCreator (Tuple.pair []) functions.fromModuleToProject
                    , foldProjectContexts = functions.foldProjectContexts
                    }
        }


{-| Use a [`ContextCreator`](#ContextCreator) to initialize your `moduleContext` and `projectContext`. This will allow
you to request more information.

This is similar to [`withModuleContextUsingContextCreator`](#withModuleContextUsingContextCreator) but it allows you
to return errors in the `fromModuleToProject` function. In the case where you would need to report errors in a
[final module evaluation](#withFinalModuleEvaluation) and then do similar computations in `fromModuleToProject`, you can
use this variant to do both in a single function (bypassing the need for the final module evaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "Rule.Name" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextWithErrors
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.fromProjectRuleSchema

    fromModuleToProject : Rule.ContextCreator ModuleContext ( List (Rule.Error {}), ProjectContext )
    fromModuleToProject =
        Rule.initContextCreator
            (\moduleContext ->
                let
                    ( errors, someData ) =
                        someComputation moduleContext
                in
                ( errors, { someData = someData } )
            )

-}
withModuleContextWithErrors :
    { fromProjectToModule : ContextCreator projectContext moduleContext
    , fromModuleToProject : ContextCreator moduleContext ( List (Error {}), projectContext )
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }
    -> ProjectRuleSchema { schemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : (), withModuleContext : Forbidden } projectContext moduleContext
withModuleContextWithErrors functions (ProjectRuleSchema schema) =
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


{-| Add a visitor to the [`ProjectRuleSchema`](#ProjectRuleSchema) to visit files that `elm-review`
doesn't analyze by default.

The visitor function will be called with all the files matching the file patterns.

The following example rule reads a project's `.css` files to extract all the mentioned CSS classes,
then finds calls to `Html.Attributes.class` in the Elm code (such as `Html.Attributes.class "big-red-button"`)
and reports errors when the classes given as argument are unknown.

    import Dict exposing (Dict)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Regex exposing (Regex)
    import Review.FilePattern as FilePattern
    import Review.Rule as Rule exposing (Rule)
    import Set exposing (Set)

    rule : Rule
    rule =
        Rule.newProjectRuleSchema "NoUnusedCssClasses" initialProjectContext
            |> Rule.withExtraFilesProjectVisitor cssFilesVisitor
                [ FilePattern.include "**/*.css" ]
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContextUsingContextCreator
                { fromProjectToModule = fromProjectToModule
                , fromModuleToProject = fromModuleToProject
                , foldProjectContexts = foldProjectContexts
                }
            |> Rule.withFinalProjectEvaluation finalEvaluation
            |> Rule.fromProjectRuleSchema

    moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
    moduleVisitor schema =
        schema
            |> Rule.withExpressionEnterVisitor expressionVisitor

    type alias ProjectContext =
        { cssFiles :
            Dict
                String
                { fileKey : Rule.ExtraFileKey
                , classes : Set String
                }
        , usedCssClasses : Set String
        }

    type alias ModuleContext =
        { usedCssClasses : Set String
        }

    initialProjectContext : ProjectContext
    initialProjectContext =
        { cssFiles = Dict.empty
        , usedCssClasses = Set.empty
        }

    fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
    fromProjectToModule =
        Rule.initContextCreator (\_ -> { usedCssClasses = Set.empty })

    fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
    fromModuleToProject =
        Rule.initContextCreator
            (\{ usedCssClasses } ->
                { cssFiles = Dict.empty
                , usedCssClasses = usedCssClasses
                }
            )

    foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
    foldProjectContexts newContext previousContext =
        { cssFiles = previousContext.cssFiles
        , usedCssClasses = Set.union newContext.usedCssClasses previousContext.usedCssClasses
        }

    cssClassRegex : Regex
    cssClassRegex =
        Regex.fromString "\\.([\\w-_]+)"
            |> Maybe.withDefault Regex.never

    cssFilesVisitor : Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
    cssFilesVisitor files context =
        ( []
        , { context
            | cssFiles =
                Dict.map
                    (\_ { fileKey, content } ->
                        { fileKey = fileKey
                        , classes =
                            Regex.find cssClassRegex content
                                |> List.map (\m -> String.dropLeft 1 m.match)
                                |> Set.fromList
                        }
                    )
                    files
          }
        )

    expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
    expressionVisitor node context =
        case Node.value node of
            Expression.Application [ function, firstArg ] ->
                case Node.value function of
                    Expression.FunctionOrValue [ "Html", "Attributes" ] "class" ->
                        case Node.value firstArg of
                            Expression.Literal stringLiteral ->
                                let
                                    usedCssClasses : List String
                                    usedCssClasses =
                                        String.split " " stringLiteral
                                in
                                ( []
                                , { context | usedCssClasses = List.foldl Set.insert context.usedCssClasses usedCssClasses }
                                )

                            _ ->
                                ( [], context )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

    finalEvaluation : ProjectContext -> List (Rule.Error { useErrorForModule : () })
    finalEvaluation context =
        context.cssFiles
            |> Dict.toList
            |> List.filterMap (\( filePath, file ) -> reportUnusedClasses context.usedCssClasses filePath file)

    reportUnusedClasses : Set String -> String -> { a | fileKey : Rule.ExtraFileKey, classes : Set String } -> Maybe (Rule.Error { useErrorForModule : () })
    reportUnusedClasses usedCssClasses filePath { fileKey, classes } =
        let
            unusedClasses : Set String
            unusedClasses =
                Set.diff classes usedCssClasses
        in
        if Set.isEmpty unusedClasses then
            Nothing

        else
            Just
                (Rule.errorForExtraFile fileKey
                    { message = "Found unused CSS classes in " ++ filePath
                    , details =
                        [ "This file declared the usage of some CSS classes for which I could not any usage in the Elm codebase. Please check that no typo was made in the name of the classes, and remove them if they still seem unused."
                        , "Here are the classes that seem unused: " ++ String.join " " (Set.toList unusedClasses)
                        ]
                    }
                    { start = { row = 1, column = 1 }, end = { row = 1, column = 100000 } }
                )

-}
withExtraFilesProjectVisitor :
    (Dict String { fileKey : ExtraFileKey, content : String } -> projectContext -> ( List (Error { useErrorForModule : () }), projectContext ))
    -> List FilePattern
    -> ProjectRuleSchema schemaState projectContext moduleContext
    -> ProjectRuleSchema { schemaState | hasAtLeastOneVisitor : () } projectContext moduleContext
withExtraFilesProjectVisitor baseVisitor filePatterns (ProjectRuleSchema schema) =
    case FilePattern.compact filePatterns of
        Ok filePatternSummary ->
            let
                visitor : ExtraFileData -> projectContext -> ( List (Error {}), projectContext )
                visitor files context =
                    baseVisitor (Dict.filter (\path _ -> FilePattern.match { includeByDefault = False } filePatternSummary path) files.withFileKeys) context
                        |> Tuple.mapFirst removeErrorPhantomTypes
            in
            ProjectRuleSchema
                { schema
                    | extraFilesVisitor = Just (combineVisitors visitor schema.extraFilesVisitor)
                    , extraFileRequest =
                        case schema.extraFileRequest of
                            Ok previous ->
                                Ok (FilePattern.toStrings filePatternSummary :: previous)

                            Err _ ->
                                schema.extraFileRequest
                }

        Err globErrors ->
            ProjectRuleSchema
                { schema
                    | extraFileRequest =
                        case schema.extraFileRequest of
                            Err previous ->
                                Err (previous ++ globErrors)

                            Ok _ ->
                                Err globErrors
                }


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

    dataExtractor : ProjectContext -> Json.Encode.Value
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


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) to visit files that `elm-review`
doesn't analyze by default.

The visitor function will be called with all the files matching the file patterns.

The following example rule reads a project's `.css` files to extract all the mentioned CSS classes,
then finds calls to `Html.Attributes.class` in the Elm code (such as `Html.Attributes.class "big-red-button"`)
and reports errors when the classes given as argument are unknown.

    import Dict exposing (Dict)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Regex exposing (Regex)
    import Review.FilePattern as FilePattern
    import Review.Rule as Rule exposing (Rule)
    import Set exposing (Set)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "Css.NoUnknownCssClasses" initialContext
            |> Rule.withExtraFilesModuleVisitor cssFilesVisitor
                [ FilePattern.include "**/*.css" ]
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        { knownCssClasses : Set String
        }

    initialContext : Context
    initialContext =
        { knownCssClasses = Set.empty
        }

    cssClassRegex : Regex
    cssClassRegex =
        Regex.fromString "\\.([\\w-_]+)"
            |> Maybe.withDefault Regex.never

    cssFilesVisitor : Dict String String -> Context -> Context
    cssFilesVisitor files context =
        { knownCssClasses =
            files
                |> Dict.values
                |> List.concatMap (\cssSource -> Regex.find cssClassRegex cssSource)
                |> List.map (\m -> String.dropLeft 1 m.match)
                |> Set.fromList
        }

    expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
    expressionVisitor node context =
        case Node.value node of
            Expression.Application [ function, firstArg ] ->
                case Node.value function of
                    Expression.FunctionOrValue [ "Html", "Attributes" ] "class" ->
                        case Node.value firstArg of
                            Expression.Literal stringLiteral ->
                                ( stringLiteral
                                    |> String.split " "
                                    |> List.filterMap (checkForUnknownCssClass context.knownCssClasses (Node.range firstArg))
                                , context
                                )

                            _ ->
                                ( [], context )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

    checkForUnknownCssClass : Set String -> Range -> String -> Maybe (Rule.Error {})
    checkForUnknownCssClass knownCssClasses range class =
        if Set.member class knownCssClasses then
            Nothing

        else
            Just
                (Rule.error
                    { message = "Unknown CSS class " ++ class
                    , details =
                        [ "This CSS class does not appear in the project's `.css` files."
                        , "Could it be that you misspelled the name of the class, or that the class recently got removed?"
                        ]
                    }
                    range
                )

-}
withExtraFilesModuleVisitor :
    (Dict String String -> moduleContext -> moduleContext)
    -> List FilePattern
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
    -> ModuleRuleSchema { schemaState | canCollectProjectData : () } moduleContext
withExtraFilesModuleVisitor baseVisitor filePatterns (ModuleRuleSchema schema) =
    case FilePattern.compact filePatterns of
        Ok filePatternSummary ->
            let
                visitor : ExtraFileData -> moduleContext -> moduleContext
                visitor files context =
                    baseVisitor (Dict.filter (\path _ -> FilePattern.match { includeByDefault = False } filePatternSummary path) files.withoutFileKeys) context
            in
            ModuleRuleSchema
                { schema
                    | extraFilesVisitor = Just (combineContextOnlyVisitor visitor schema.extraFilesVisitor)
                    , extraFileRequest =
                        case schema.extraFileRequest of
                            Ok previous ->
                                Ok (FilePattern.toStrings filePatternSummary :: previous)

                            Err _ ->
                                schema.extraFileRequest
                }

        Err globErrors ->
            ModuleRuleSchema
                { schema
                    | extraFileRequest =
                        case schema.extraFileRequest of
                            Err previous ->
                                Err (previous ++ globErrors)

                            Ok _ ->
                                Err globErrors
                }


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

    import Dict exposing (Dict)
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
    = Error BaseError


type alias BaseError =
    { message : String
    , ruleName : String
    , filePath : String
    , details : List String
    , range : Range
    , fixes : ErrorFixes
    , fixProblem : Maybe FixProblem
    , target : Target.Target
    , preventsExtract : Bool
    }


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
    Error { err | preventsExtract = True }


removeErrorPhantomTypes : List (Error something) -> List (Error {})
removeErrorPhantomTypes list =
    -- This function could be replaced by `identity` at runtime
    List.map (\(Error err) -> Error err) list


{-| Represents an error found by a [`Rule`](#Rule). These are the ones that will
be reported to the user.

If you are building a [`Rule`](#Rule), you shouldn't have to use this.

-}
type alias ReviewError =
    Review.Error.ReviewError.ReviewError


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
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.module_ ""
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
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.module_ path
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
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.elmJson
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
            case getFix elmJson.project of
                Just updatedProject ->
                    let
                        encoded : String
                        encoded =
                            updatedProject
                                |> Review.ElmProjectEncoder.encode
                                |> Encode.encode 4
                    in
                    ErrorFixes.edit FileTarget.ElmJson
                        [ Fix.replaceRangeBy
                            { start = { row = 1, column = 1 }, end = { row = 100000000, column = 1 } }
                            (encoded ++ "\n")
                        ]

                Nothing ->
                    ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.elmJson
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
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.readme
        , preventsExtract = False
        }


{-| Just like [`errorForReadme`](#errorForReadme), create an [`Error`](#Error) for the `README.md` file, but
provides an automatic fix that the user can apply.

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then it will give the same error as if you had
called [`errorForReadme`](#errorForReadme) instead.

**Note**: Each fix applies on a location in the file, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
errorForReadmeWithFix : ReadmeKey -> { message : String, details : List String } -> Range -> List Fix -> Error scope
errorForReadmeWithFix readmeKey info range fixes =
    errorForReadme readmeKey info range
        |> withFixes fixes


{-| A key to be able to report an error for an extra file. You need this
key in order to use the [`errorForReadmeWithFix`](#errorForReadmeWithFix) function. This is
to prevent creating errors for it if you have not visited it.

You can get a `ExtraFileKey` using the [`withExtraFilesProjectVisitor`](#withExtraFilesProjectVisitor) function.

-}
type ExtraFileKey
    = ExtraFileKey
        { path : String
        , content : String
        }


type alias ExtraFileData =
    ValidProject.ExtraFileData ExtraFileKey


{-| Create an [`Error`](#Error) for an extra file.

You will need an [`ExtraFileKey`](#ExtraFileKey), which you can get from the [`withExtraFilesProjectVisitor`](#withExtraFilesProjectVisitor)
function.

-}
errorForExtraFile : ExtraFileKey -> { message : String, details : List String } -> Range -> Error scope
errorForExtraFile (ExtraFileKey { path }) { message, details } range =
    Error
        { message = message
        , ruleName = ""
        , filePath = path
        , details = details
        , range = range
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.extraFile path
        , preventsExtract = False
        }


{-| Just like [`errorForExtraFile`](#errorForExtraFile), create an [`Error`](#Error) for an extra file, but
provides an automatic fix that the user can apply.

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then it will give the same error as if you had
called [`errorForExtraFile`](#errorForExtraFile) instead.

**Note**: Each fix applies on a location in the file, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
errorForExtraFileWithFix : ExtraFileKey -> { message : String, details : List String } -> Range -> List Fix -> Error scope
errorForExtraFileWithFix extraFileKey info range fixes =
    errorForExtraFile extraFileKey info range
        |> withFixes fixes


elmReviewGlobalError : { message : String, details : List String } -> Error scope
elmReviewGlobalError { message, details } =
    Error
        { filePath = "GLOBAL ERROR"
        , ruleName = ""
        , message = message
        , details = details
        , range = Range.emptyRange
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.Global
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
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.UserGlobal
        , preventsExtract = False
        }


parsingError : String -> Review.Error.ReviewError.ReviewError
parsingError path =
    Review.Error.ReviewError.fromBaseError
        { filePath = path
        , ruleName = "ParsingError"
        , message = path ++ " is not a correct Elm module"
        , details =
            [ "I could not understand the content of this file, and this prevents me from analyzing it. It is highly likely that the contents of the file is not correct Elm code."
            , "I need this file to be fixed before analyzing the rest of the project. If I didn't, I would potentially report incorrect things."
            , "Hint: Try running `elm make`. The compiler should give you better hints on how to resolve the problem."
            ]
        , range = Range.emptyRange
        , fixes = ErrorFixes.none
        , fixProblem = Nothing
        , target = Target.module_ path
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
                { err | fixes = ErrorFixes.none }

            else
                case err.target of
                    Target.FileTarget FileTarget.ElmJson ->
                        err

                    Target.FileTarget fileTarget ->
                        { err | fixes = ErrorFixes.edit fileTarget fixes }

                    Target.Global ->
                        err

                    Target.UserGlobal ->
                        err
        )
        error_


{-| Represents (part of a) fix that will edit files and/or remove modules.
-}
type alias FixV2 =
    ErrorFixes.FixV2


{-| Provide automatic fixes for an error that the user can apply.

To be used along with functions listed below, such as [`editModule`](#editModule).

    import Review.Fix as Fix

    error : Rule.ModuleKey -> Node a -> Error {}
    error moduleKey node =
        Rule.error
            { message = "Remove the use of `Debug` before shipping to production"
            , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
            }
            (Node.range node)
            [ Fix.editModule
                moduleKey
                [ Fix.removeRange (Node.range node) ]
            ]

Take a look at [`Review.Fix`](./Review-Fix) to know more on how to makes fixes.

If the list of fixes is empty, then the error will be considered as not providing a fix.

-}
withFixesV2 : List FixV2 -> Error scope -> Error scope
withFixesV2 providedFixes error_ =
    mapInternalError
        (\err ->
            { err
                | fixes =
                    List.foldl
                        (List.singleton >> ErrorFixes.add)
                        err.fixes
                        (List.map (\(ErrorFixes.FixV2 target fixes) -> ( target, fixes )) providedFixes)
            }
        )
        error_


{-| Provide an automatic fix for a specific Elm module, by using edit functions from [`Review.Fix`](./Review-Fix) module.

You will need a [`ModuleKey`](#ModuleKey), which you can get from the `fromProjectToModule` and `fromModuleToProject`
functions that you define when using [`newProjectRuleSchema`](#newProjectRuleSchema).

-}
editModule : ModuleKey -> List Fix -> FixV2
editModule (ModuleKey path) fixes =
    ErrorFixes.FixV2 (FileTarget.Module path) (ErrorFixes.Edit fixes)


{-| Provide an automatic fix that removes an Elm module.

When using the CLI, this will only be enabled when running with the `--allow-remove-files` flag. When the flag is absent,
then the entire fix will be ignored.

You will need a [`ModuleKey`](#ModuleKey), which you can get from the `fromProjectToModule` and `fromModuleToProject`
functions that you define when using [`newProjectRuleSchema`](#newProjectRuleSchema).

-}
removeModule : ModuleKey -> FixV2
removeModule (ModuleKey path) =
    ErrorFixes.FixV2 (FileTarget.Module path) ErrorFixes.Remove


{-| Provide an automatic fix for a specific extra file, by using edit functions from [`Review.Fix`](./Review-Fix) module.

You will need a [`ExtraFileKey`](#ExtraFileKey), which you can get from the [`withExtraFilesProjectVisitor`](#withExtraFilesProjectVisitor).

-}
editExtraFile : ExtraFileKey -> List Fix -> FixV2
editExtraFile (ExtraFileKey { path }) fixes =
    ErrorFixes.FixV2 (FileTarget.ExtraFile path) (ErrorFixes.Edit fixes)


{-| Provide an automatic fix that removes an extra file.

You will need a [`ExtraFileKey`](#ExtraFileKey), which you can get from the [`withExtraFilesProjectVisitor`](#withExtraFilesProjectVisitor).

When using the CLI, this will only be enabled when running with the `--allow-remove-files` flag. When the flag is absent,
then the entire fix will be ignored.

-}
removeExtraFile : ExtraFileKey -> FixV2
removeExtraFile (ExtraFileKey { path }) =
    ErrorFixes.FixV2 (FileTarget.ExtraFile path) ErrorFixes.Remove


{-| Provide an automatic fix for the `README.md` file, by using edit functions from [`Review.Fix`](./Review-Fix) module.

You will need an [`ReadmeKey`](#ReadmeKey), which you can get from the [`withReadmeProjectVisitor`](#withReadmeProjectVisitor)
function.

-}
editReadme : ReadmeKey -> List Fix -> FixV2
editReadme (ReadmeKey _) fixes =
    ErrorFixes.FixV2 FileTarget.Readme (ErrorFixes.Edit fixes)


{-| Provide an automatic fix for the `elm.json` file.

You will need an [`ElmJsonKey`](#ElmJsonKey), which you can get from the [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
function.

-}
editElmJson :
    ElmJsonKey
    -> (Elm.Project.Project -> Maybe Elm.Project.Project)
    -> FixV2
editElmJson (ElmJsonKey elmJson) fixer =
    let
        fixes : List Fix
        fixes =
            case fixer elmJson.project of
                Just updatedProject ->
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

                Nothing ->
                    []
    in
    ErrorFixes.FixV2 FileTarget.ElmJson (ErrorFixes.Edit fixes)


errorToReviewError : Error scope -> ReviewError
errorToReviewError (Error err) =
    Review.Error.ReviewError.fromBaseError err


{-| Get the name of the rule that triggered this [`Error`](#Error).
-}
errorRuleName : ReviewError -> String
errorRuleName (Review.Error.ReviewError.ReviewError err) =
    err.ruleName


{-| Get the error message of an [`Error`](#Error).
-}
errorMessage : ReviewError -> String
errorMessage (Review.Error.ReviewError.ReviewError err) =
    err.message


{-| Get the error details of an [`Error`](#Error).
-}
errorDetails : ReviewError -> List String
errorDetails (Review.Error.ReviewError.ReviewError err) =
    err.details


{-| Get the [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Range)
of an [`Error`](#Error).
-}
errorRange : ReviewError -> Range
errorRange (Review.Error.ReviewError.ReviewError err) =
    err.range


{-| Get the automatic [`fixes`](./Review-Fix#Fix) of an [`Error`](#Error), if it
defined any.

An error can provide fixes for multiple files. For each file, the fix consists of
either a `Just` list of edits (named `Fix` until the next breaking change) or of `Nothing` for a file removal.

-}
errorFixesV2 : Review.Error.ReviewError.ReviewError -> Result FixProblem (Maybe (List ( String, Maybe (List Fix) )))
errorFixesV2 (Review.Error.ReviewError.ReviewError err) =
    -- The type for this function would be better described through a custom type.
    -- It is however purposefully low-level in order to keep it possible to introduce
    -- new fix kinds (most likely file creations) without a breaking change
    -- (but through a new `errorFixesV3` function).
    err.fixes
        |> Result.map
            (Maybe.map
                (List.map
                    (\( fileTarget, fixKind ) ->
                        ( FileTarget.filePath fileTarget
                        , case fixKind of
                            ErrorFixes.Edit edits ->
                                Just edits

                            ErrorFixes.Remove ->
                                Nothing
                        )
                    )
                )
            )


{-| Get the automatic [`fixes`](./Review-Fix#Fix) of an [`Error`](#Error), if it
defined any.

**@deprecated**: Use [`errorFixesV2`](#errorFixesV2) which supports file removals.
`errorFixes` will consider any error with a file removal fix as not having a fix.

-}
errorFixes : ReviewError -> Maybe (List Fix)
errorFixes (Review.Error.ReviewError.ReviewError err) =
    case err.fixes of
        Err _ ->
            Nothing

        Ok Nothing ->
            Nothing

        Ok (Just fixes) ->
            case fixes of
                [ ( target, ErrorFixes.Edit fileFixes ) ] ->
                    if FileTarget.filePath target == err.filePath then
                        Just fileFixes

                    else
                        Nothing

                _ ->
                    Nothing


{-| Get the reason why the fix for an error failed when its available automatic fix was attempted and deemed incorrect.

Note that if the review process was not run in fix mode previously, then this may return `Nothing`.

-}
errorFixProblem : ReviewError -> Maybe FixProblem
errorFixProblem (Review.Error.ReviewError.ReviewError err) =
    case err.fixes of
        Ok _ ->
            Nothing

        Err fixProblem ->
            Just fixProblem


{-| Get the reason why the fix for an error failed when its available automatic fix was attempted and deemed incorrect.

Note that if the review process was not run in fix mode previously, then this will return `Nothing`.

**@deprecated**: Use [`errorFixProblem`](#errorFixProblem) which returns a different type that better
describes fixing problems that have been checked for.

-}
errorFixFailure : ReviewError -> Maybe Fix.Problem
errorFixFailure err =
    case errorFixProblem err of
        Just fixProblem ->
            case fixProblem of
                FixProblem.Unchanged _ ->
                    Just Fix.Unchanged

                FixProblem.InvalidElm { source } ->
                    Just (Fix.SourceCodeIsNotValid source)

                FixProblem.InvalidJson { source } ->
                    Just (Fix.SourceCodeIsNotValid source)

                FixProblem.HasCollisionsInEditRanges _ ->
                    Just Fix.HasCollisionsInFixRanges

                FixProblem.EditWithNegativeRange _ ->
                    Just Fix.Unchanged

                FixProblem.CreatesImportCycle _ ->
                    Just Fix.Unchanged

                FixProblem.RemovesUnknownFile _ ->
                    Just Fix.Unchanged

                FixProblem.Other _ ->
                    Just Fix.Unchanged

        Nothing ->
            Nothing


{-| Get the file path of an [`Error`](#Error).
-}
errorFilePath : ReviewError -> String
errorFilePath (Review.Error.ReviewError.ReviewError err) =
    err.filePath


{-| Get the target of an [`Error`](#Error).
-}
errorTarget : ReviewError -> Target
errorTarget (Review.Error.ReviewError.ReviewError err) =
    err.target


mapInternalError : (BaseError -> BaseError) -> Error scope -> Error scope
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
        , providesFixes = rule.providesFixes
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
        , providesFixes = rule.providesFixes
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
        , providesFixes = rule.providesFixes
        , ruleProjectVisitor = rule.ruleProjectVisitor
        }



-- VISITOR
-- TODO BREAKING CHANGE Move this into a separate module later on


type alias Visitor nodeType context =
    Node nodeType -> context -> ( List (Error {}), context )


type TraversalAndFolder projectContext moduleContext
    = TraverseAllModulesInParallel (Maybe (Folder projectContext moduleContext))
    | TraverseImportedModulesFirst (Folder projectContext moduleContext)


type alias Folder projectContext moduleContext =
    { fromModuleToProject : ContextCreator moduleContext ( List (Error {}), projectContext )
    , foldProjectContexts : projectContext -> projectContext -> projectContext
    }


type alias GraphModule =
    Graph.NodeContext FilePath ()


{-| TODO Breaking change: Remove the type variable and hardcode `Error {}` when Error has been moved to a separate module in v3.
-}
type alias ModuleCacheEntry projectContext =
    ModuleCache.Entry (Error {}) projectContext


{-| TODO Breaking change: Remove the type variable and hardcode `Error {}` when Error has been moved to a separate module in v3.
-}
type alias ProjectFileCache projectContext =
    ProjectFileCache.Entry (Error {}) projectContext


type alias ExtraFilesCache projectContext =
    ExtraFile.Entry (Error {}) projectContext


type alias FinalProjectEvaluationCache projectContext =
    EndAnalysisCache.Entry (List (Error {})) projectContext


type alias ExtractCache projectContext =
    EndAnalysisCache.Entry Extract projectContext


qualifyErrors : { ruleName : String, exceptions : Exceptions, filePath : String } -> List (Error {}) -> List (Error {}) -> List (Error {})
qualifyErrors params errors acc =
    List.foldl (\err subAcc -> qualifyError params err subAcc) acc errors


qualifyError : { ruleName : String, exceptions : Exceptions, filePath : String } -> Error {} -> List (Error {}) -> List (Error {})
qualifyError params (Error err) acc =
    let
        newError : BaseError
        newError =
            if err.filePath == "" then
                { err
                    | filePath = params.filePath
                    , target = Target.setCurrentFilePathOnTargetIfNeeded params.filePath err.target
                    , fixes = ErrorFixes.qualify params.filePath err.fixes
                }

            else
                err
    in
    if Exceptions.isFileWeWantReportsFor params.exceptions newError.filePath then
        setRuleName params.ruleName (Error newError) :: acc

    else
        acc


runProjectVisitor :
    ReviewOptionsData
    -> List RuleProjectVisitor
    -> FixedErrors
    -> ValidProject
    -> { fixedErrors : FixedErrors, ruleProjectVisitors : List RuleProjectVisitor, project : ValidProject }
runProjectVisitor reviewOptions initialRuleProjectVisitors initialFixedErrors initialProject =
    let
        { project, ruleProjectVisitors, fixedErrors } =
            computeStepsForProject
                reviewOptions
                { step = ElmJson
                , project = initialProject
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


computeFinalContextHashes : ProjectRuleSchemaData projectContext moduleContext -> ProjectRuleCache projectContext -> ComparableContextHash projectContext
computeFinalContextHashes schema cache =
    let
        ( projectContextHash, _ ) =
            findInitialInputContext cache AfterProjectFilesStep schema.initialProjectContext

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
        Just _ ->
            Dict.foldl
                (\_ cacheEntry acc -> ModuleCache.outputContextHash cacheEntry :: acc)
                projectContextHash
                cache.moduleContexts
                |> ContextHash.toComparable

        Nothing ->
            ContextHash.toComparable projectContextHash


computeFinalContext : ProjectRuleSchemaData projectContext moduleContext -> ProjectRuleCache projectContext -> projectContext
computeFinalContext schema cache =
    let
        ( _, projectContext ) =
            findInitialInputContext cache AfterProjectFilesStep schema.initialProjectContext

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
                (\_ cacheEntry acc -> foldProjectContexts (ModuleCache.outputContext cacheEntry) acc)
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
        [ Dict.foldl (\_ cacheEntry acc -> List.append (ModuleCache.errors cacheEntry) acc) [] cache.moduleContexts
        , ProjectFileCache.errorsForMaybe cache.elmJson
        , ProjectFileCache.errorsForMaybe cache.readme
        , ExtraFile.errorsForMaybe cache.extraFiles
        , ProjectFileCache.errorsForMaybe cache.dependencies
        , Maybe.map EndAnalysisCache.output cache.finalEvaluationErrors |> Maybe.withDefault []
        ]



-- VISIT PROJECT


type alias ProjectRuleCache projectContext =
    { elmJson : Maybe (ProjectFileCache projectContext)
    , readme : Maybe (ProjectFileCache projectContext)
    , extraFiles : Maybe (ExtraFilesCache projectContext)
    , dependencies : Maybe (ProjectFileCache projectContext)
    , moduleContexts : Dict String (ModuleCacheEntry projectContext)
    , finalEvaluationErrors : Maybe (FinalProjectEvaluationCache projectContext)
    , extract : Maybe (ExtractCache projectContext)
    }


computeStepsForProject :
    ReviewOptionsData
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors, step : Step }
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeStepsForProject reviewOptions { project, ruleProjectVisitors, fixedErrors, step } =
    case step of
        ElmJson ->
            let
                elmJsonData : Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
                elmJsonData =
                    Maybe.map
                        (\elmJson ->
                            { elmJsonKey = ElmJsonKey elmJson
                            , project = elmJson.project
                            }
                        )
                        (ValidProject.elmJson project)
            in
            computeStepsForProject
                reviewOptions
                (computeElmJson reviewOptions project fixedErrors elmJsonData ruleProjectVisitors [])

        Readme ->
            let
                readmeData : Maybe { readmeKey : ReadmeKey, content : String }
                readmeData =
                    Maybe.map
                        (\readme ->
                            { readmeKey = ReadmeKey { path = readme.path, content = readme.content }
                            , content = readme.content
                            }
                        )
                        (ValidProject.readme project)
            in
            computeStepsForProject
                reviewOptions
                (computeReadme reviewOptions project fixedErrors readmeData ruleProjectVisitors [])

        ExtraFiles ->
            let
                extraFiles : ExtraFileData
                extraFiles =
                    ValidProject.extraFiles ExtraFileKey project
            in
            computeStepsForProject
                reviewOptions
                (computeExtraFiles reviewOptions project fixedErrors extraFiles ruleProjectVisitors [])

        Dependencies ->
            let
                dependenciesData : { all : Dict String Review.Project.Dependency.Dependency, direct : Dict String Review.Project.Dependency.Dependency }
                dependenciesData =
                    { all = ValidProject.dependencies project
                    , direct = ValidProject.directDependencies project
                    }
            in
            computeStepsForProject
                reviewOptions
                (computeDependencies reviewOptions project fixedErrors dependenciesData ruleProjectVisitors [])

        Modules moduleZipper ->
            computeStepsForProject
                reviewOptions
                (computeModules
                    reviewOptions
                    moduleZipper
                    project
                    ruleProjectVisitors
                    fixedErrors
                )

        FinalProjectEvaluation ->
            computeStepsForProject
                reviewOptions
                (computeFinalProjectEvaluation reviewOptions project fixedErrors ruleProjectVisitors [])

        EndAnalysis ->
            { project = project
            , ruleProjectVisitors = ruleProjectVisitors
            , fixedErrors = fixedErrors
            }


type Step
    = ElmJson
    | Readme
    | ExtraFiles
    | Dependencies
    | Modules (Maybe (Zipper GraphModule))
    | FinalProjectEvaluation
    | EndAnalysis


type StepToComputeContext
    = ElmJsonStep
    | ReadmeStep
    | ExtraFilesStep
    | DependenciesStep
    | AfterProjectFilesStep


type NextStep
    = ModuleVisitStep (Maybe (Zipper GraphModule))
    | BackToElmJson
    | BackToReadme
    | BackToExtraFiles
    | NextStepAbort


computeElmJson :
    ReviewOptionsData
    -> ValidProject
    -> FixedErrors
    -> Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project }
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> { project : ValidProject, step : Step, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeElmJson reviewOptions project fixedErrors elmJsonData remainingRules accRules =
    case remainingRules of
        [] ->
            { project = project
            , step = Readme
            , ruleProjectVisitors = accRules
            , fixedErrors = fixedErrors
            }

        ((RuleProjectVisitor rule) as untouched) :: rest ->
            case rule.elmJsonVisitor of
                Just visitor ->
                    let
                        ( errors, RuleProjectVisitor updatedRule ) =
                            visitor project elmJsonData
                    in
                    case standardFindFix reviewOptions project fixedErrors updatedRule.setErrorsForElmJson errors of
                        FoundFixStandard { newProject, newRule, newFixedErrors, step } ->
                            { project = newProject
                            , ruleProjectVisitors = newRule :: (rest ++ accRules)
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        FoundNoFixesStandard newRule ->
                            computeElmJson
                                reviewOptions
                                project
                                fixedErrors
                                elmJsonData
                                rest
                                (newRule :: accRules)

                Nothing ->
                    computeElmJson
                        reviewOptions
                        project
                        fixedErrors
                        elmJsonData
                        rest
                        (untouched :: accRules)


computeReadme :
    ReviewOptionsData
    -> ValidProject
    -> FixedErrors
    -> Maybe { readmeKey : ReadmeKey, content : String }
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, step : Step, fixedErrors : FixedErrors }
computeReadme reviewOptions project fixedErrors readmeData remainingRules accRules =
    case remainingRules of
        [] ->
            { project = project
            , step = ExtraFiles
            , ruleProjectVisitors = accRules
            , fixedErrors = fixedErrors
            }

        ((RuleProjectVisitor rule) as untouched) :: rest ->
            case rule.readmeVisitor of
                Just visitor ->
                    let
                        ( errors, RuleProjectVisitor updatedRule ) =
                            visitor project readmeData
                    in
                    case standardFindFix reviewOptions project fixedErrors updatedRule.setErrorsForReadme errors of
                        FoundFixStandard { newProject, newRule, newFixedErrors, step } ->
                            { project = newProject
                            , ruleProjectVisitors = newRule :: (rest ++ accRules)
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        FoundNoFixesStandard newRule ->
                            computeReadme
                                reviewOptions
                                project
                                fixedErrors
                                readmeData
                                rest
                                (newRule :: accRules)

                Nothing ->
                    computeReadme
                        reviewOptions
                        project
                        fixedErrors
                        readmeData
                        rest
                        (untouched :: accRules)


computeExtraFiles :
    ReviewOptionsData
    -> ValidProject
    -> FixedErrors
    -> ExtraFileData
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, step : Step, fixedErrors : FixedErrors }
computeExtraFiles reviewOptions project fixedErrors extraFiles remainingRules accRules =
    case remainingRules of
        [] ->
            { project = project
            , step = Dependencies
            , ruleProjectVisitors = accRules
            , fixedErrors = fixedErrors
            }

        ((RuleProjectVisitor rule) as untouched) :: rest ->
            case rule.extraFilesVisitor of
                Just visitor ->
                    let
                        ( errors, RuleProjectVisitor updatedRule ) =
                            visitor project extraFiles
                    in
                    case standardFindFix reviewOptions project fixedErrors updatedRule.setErrorsForExtraFiles errors of
                        FoundFixStandard { newProject, newRule, newFixedErrors, step } ->
                            { project = newProject
                            , ruleProjectVisitors = newRule :: (rest ++ accRules)
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        FoundNoFixesStandard newRule ->
                            computeExtraFiles
                                reviewOptions
                                project
                                fixedErrors
                                extraFiles
                                rest
                                (newRule :: accRules)

                Nothing ->
                    computeExtraFiles
                        reviewOptions
                        project
                        fixedErrors
                        extraFiles
                        rest
                        (untouched :: accRules)


computeDependencies :
    ReviewOptionsData
    -> ValidProject
    -> FixedErrors
    -> { all : Dict String Review.Project.Dependency.Dependency, direct : Dict String Review.Project.Dependency.Dependency }
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> { project : ValidProject, step : Step, ruleProjectVisitors : List RuleProjectVisitor, fixedErrors : FixedErrors }
computeDependencies reviewOptions project fixedErrors dependenciesData remainingRules accRules =
    case remainingRules of
        [] ->
            { project = project
            , ruleProjectVisitors = accRules
            , step = Modules (Just (ValidProject.moduleZipper project))
            , fixedErrors = fixedErrors
            }

        ((RuleProjectVisitor rule) as untouched) :: rest ->
            case rule.dependenciesVisitor of
                Just visitor ->
                    let
                        ( errors, RuleProjectVisitor updatedRule ) =
                            visitor project dependenciesData
                    in
                    case standardFindFix reviewOptions project fixedErrors updatedRule.setErrorsForDependencies errors of
                        FoundFixStandard { newProject, newRule, newFixedErrors, step } ->
                            { project = newProject
                            , ruleProjectVisitors = newRule :: (rest ++ accRules)
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        FoundNoFixesStandard newRule ->
                            computeDependencies
                                reviewOptions
                                project
                                fixedErrors
                                dependenciesData
                                rest
                                (newRule :: accRules)

                Nothing ->
                    computeDependencies
                        reviewOptions
                        project
                        fixedErrors
                        dependenciesData
                        rest
                        (untouched :: accRules)


computeFinalProjectEvaluation :
    ReviewOptionsData
    -> ValidProject
    -> FixedErrors
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, step : Step, fixedErrors : FixedErrors }
computeFinalProjectEvaluation reviewOptions project fixedErrors remainingRules accRules =
    case remainingRules of
        [] ->
            { project = project
            , ruleProjectVisitors = accRules
            , step = EndAnalysis
            , fixedErrors = fixedErrors
            }

        ((RuleProjectVisitor rule) as untouched) :: rest ->
            case rule.finalProjectEvaluation of
                Just visitor ->
                    let
                        ( errors, RuleProjectVisitor updatedRule ) =
                            visitor ()
                    in
                    case standardFindFix reviewOptions project fixedErrors updatedRule.setErrorsForFinalEvaluation errors of
                        FoundFixStandard { newProject, newRule, newFixedErrors, step } ->
                            { project = newProject
                            , ruleProjectVisitors = newRule :: (rest ++ accRules)
                            , step = step
                            , fixedErrors = newFixedErrors
                            }

                        FoundNoFixesStandard newRule ->
                            computeFinalProjectEvaluation
                                reviewOptions
                                project
                                fixedErrors
                                rest
                                (newRule :: accRules)

                Nothing ->
                    computeFinalProjectEvaluation
                        reviewOptions
                        project
                        fixedErrors
                        rest
                        (untouched :: accRules)


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


type alias DataToComputeSingleModule =
    { reviewOptions : ReviewOptionsData
    , ruleProjectVisitors : List RuleProjectVisitor
    , module_ : OpaqueProjectModule
    , project : ValidProject
    , moduleZipper : Zipper GraphModule
    , fixedErrors : FixedErrors
    , incoming : Graph.Adjacency ()
    }


computeModule :
    DataToComputeSingleModule
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
computeModule params =
    let
        ( inputRuleModuleVisitors, requestedData, rulesNotToRun ) =
            computeWhatsRequiredToAnalyze params.project params.module_ params.incoming params.ruleProjectVisitors

        paramsAfterVisit : DataToComputeSingleModule
        paramsAfterVisit =
            if List.isEmpty inputRuleModuleVisitors then
                params

            else
                let
                    ( newProject, newRules ) =
                        computeModuleWithRuleVisitors params.project params.module_ inputRuleModuleVisitors requestedData rulesNotToRun
                in
                { params | project = newProject, ruleProjectVisitors = newRules }
    in
    case findFixInComputeModuleResults paramsAfterVisit paramsAfterVisit.ruleProjectVisitors [] of
        ContinueWithNextStep nextStepResult ->
            nextStepResult

        ReComputeModule newParams ->
            computeModule newParams


computeWhatsRequiredToAnalyze : ValidProject -> OpaqueProjectModule -> Graph.Adjacency () -> List RuleProjectVisitor -> ( List (AvailableData -> RuleModuleVisitor), RequestedData, List RuleProjectVisitor )
computeWhatsRequiredToAnalyze project module_ incoming ruleProjectVisitors =
    let
        filePath : String
        filePath =
            ProjectModule.path module_
    in
    List.foldl
        (\((RuleProjectVisitor ruleProjectVisitor) as rule) ( with, requestedAcc, without ) ->
            case ruleProjectVisitor.createModuleVisitorFromProjectVisitor of
                Just moduleVisitorCreator ->
                    case moduleVisitorCreator project filePath (ProjectModule.contentHash module_) incoming of
                        Just moduleVisitor ->
                            ( moduleVisitor :: with
                            , RequestedData.combineJust ruleProjectVisitor.requestedData requestedAcc
                            , without
                            )

                        Nothing ->
                            ( with, requestedAcc, rule :: without )

                Nothing ->
                    ( with, requestedAcc, rule :: without )
        )
        ( [], RequestedData.none, [] )
        ruleProjectVisitors


computeModuleWithRuleVisitors : ValidProject -> OpaqueProjectModule -> List (AvailableData -> RuleModuleVisitor) -> RequestedData -> List RuleProjectVisitor -> ( ValidProject, List RuleProjectVisitor )
computeModuleWithRuleVisitors project module_ inputRuleModuleVisitors (RequestedData requestedData) rulesNotToRun =
    let
        ( moduleNameLookupTable, newProject ) =
            computeModuleNameLookupTable requestedData project module_

        ast : File
        ast =
            ProjectModule.ast module_

        availableData : AvailableData
        availableData =
            { ast = ast
            , moduleKey = ModuleKey (ProjectModule.path module_)
            , moduleNameLookupTable = moduleNameLookupTable
            , moduleDocumentation = findModuleDocumentation ast
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
            }

        outputRuleProjectVisitors : List RuleProjectVisitor
        outputRuleProjectVisitors =
            inputRuleModuleVisitors
                |> visitModuleForProjectRule availableData
                |> List.map (\(RuleModuleVisitor ruleModuleVisitor) -> ruleModuleVisitor.toProjectVisitor ())
    in
    ( newProject, List.append rulesNotToRun outputRuleProjectVisitors )


computeModuleNameLookupTable : { a | moduleNameLookupTable : Bool } -> ValidProject -> OpaqueProjectModule -> ( ModuleNameLookupTableInternal.ModuleNameLookupTable, ValidProject )
computeModuleNameLookupTable requestedData project module_ =
    let
        moduleName : ModuleName
        moduleName =
            ProjectModule.moduleName module_
    in
    -- TODO If the file has changed, then compute the module docs anyway.
    if requestedData.moduleNameLookupTable then
        Review.ModuleNameLookupTable.Compute.compute moduleName module_ project

    else
        ( ModuleNameLookupTableInternal.empty moduleName, project )


type ComputeModuleFindFixResult projectContext moduleContext
    = ContinueWithNextStep { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
    | ReComputeModule DataToComputeSingleModule


findFixInComputeModuleResults :
    DataToComputeSingleModule
    -> List RuleProjectVisitor
    -> List RuleProjectVisitor
    -> ComputeModuleFindFixResult projectContext moduleContext
findFixInComputeModuleResults ({ reviewOptions, module_, project, moduleZipper, fixedErrors, incoming } as params) remainingRules rulesSoFar =
    case remainingRules of
        [] ->
            ContinueWithNextStep
                { project = project
                , ruleProjectVisitors = rulesSoFar
                , nextStep = ModuleVisitStep (Zipper.next moduleZipper)
                , fixedErrors = fixedErrors
                }

        (RuleProjectVisitor ruleProjectVisitor) :: rest ->
            let
                modulePath : String
                modulePath =
                    ProjectModule.path module_

                errors : List (Error {})
                errors =
                    ruleProjectVisitor.getErrorsForModule modulePath
            in
            case findFix reviewOptions project (\newErrors -> ruleProjectVisitor.setErrorsForModule modulePath newErrors) errors fixedErrors (Just moduleZipper) of
                FoundFix newRule ( postFixStatus, fixResult ) ->
                    case postFixStatus of
                        ShouldAbort newFixedErrors ->
                            ContinueWithNextStep
                                { project = fixResult.project
                                , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
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
                                            { reviewOptions = reviewOptions
                                            , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                            , module_ =
                                                ProjectModule.create
                                                    { path = filePath
                                                    , source = source
                                                    , ast = ast
                                                    , isInSourceDirectories = ProjectModule.isInSourceDirectories module_
                                                    }
                                            , project = fixResult.project
                                            , moduleZipper = newModuleZipper_
                                            , fixedErrors = newFixedErrors
                                            , incoming = incoming
                                            }

                                    else
                                        case Zipper.focusl (\mod -> mod.node.label == filePath) moduleZipper of
                                            Just newModuleZipper ->
                                                ContinueWithNextStep
                                                    { project = fixResult.project
                                                    , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                                    , nextStep = ModuleVisitStep (Just newModuleZipper)
                                                    , fixedErrors = newFixedErrors
                                                    }

                                            Nothing ->
                                                ContinueWithNextStep
                                                    { project = project
                                                    , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                                    , nextStep = ModuleVisitStep (Zipper.next moduleZipper)
                                                    , fixedErrors = fixedErrors
                                                    }

                                RemovedElmModule ->
                                    ContinueWithNextStep
                                        { project = project
                                        , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)

                                        -- TODO MULTIFILE-FIXES Move to a more optimal starting position than at the very beginning.
                                        , nextStep = ModuleVisitStep Nothing
                                        , fixedErrors = fixedErrors
                                        }

                                FixedElmJson ->
                                    ContinueWithNextStep
                                        { project = fixResult.project
                                        , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                        , nextStep = BackToElmJson
                                        , fixedErrors = FixedErrors.insert fixResult.error fixedErrors
                                        }

                                FixedReadme ->
                                    ContinueWithNextStep
                                        { project = fixResult.project
                                        , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                        , nextStep = BackToReadme
                                        , fixedErrors = FixedErrors.insert fixResult.error fixedErrors
                                        }

                                FixedExtraFile ->
                                    ContinueWithNextStep
                                        { project = fixResult.project
                                        , ruleProjectVisitors = newRule :: (rest ++ rulesSoFar)
                                        , nextStep = BackToExtraFiles
                                        , fixedErrors = FixedErrors.insert fixResult.error fixedErrors
                                        }

                FoundNoFixes newRule ->
                    findFixInComputeModuleResults
                        params
                        rest
                        (newRule :: rulesSoFar)


computeModules :
    ReviewOptionsData
    -> Maybe (Zipper GraphModule)
    -> ValidProject
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, step : Step, fixedErrors : FixedErrors }
computeModules reviewOptions maybeModuleZipper initialProject ruleProjectVisitors fixedErrors =
    case maybeModuleZipper of
        Nothing ->
            { project = initialProject, ruleProjectVisitors = ruleProjectVisitors, step = FinalProjectEvaluation, fixedErrors = fixedErrors }

        Just moduleZipper ->
            let
                result : { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
                result =
                    computeModuleAndCacheResult
                        reviewOptions
                        moduleZipper
                        initialProject
                        ruleProjectVisitors
                        fixedErrors
            in
            case result.nextStep of
                ModuleVisitStep newModuleZipper ->
                    computeModules
                        reviewOptions
                        newModuleZipper
                        result.project
                        result.ruleProjectVisitors
                        result.fixedErrors

                BackToElmJson ->
                    { project = result.project
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = ElmJson
                    , fixedErrors = result.fixedErrors
                    }

                BackToReadme ->
                    { project = result.project
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = Readme
                    , fixedErrors = result.fixedErrors
                    }

                BackToExtraFiles ->
                    { project = result.project
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = ExtraFiles
                    , fixedErrors = result.fixedErrors
                    }

                NextStepAbort ->
                    { project = result.project
                    , ruleProjectVisitors = result.ruleProjectVisitors
                    , step = EndAnalysis
                    , fixedErrors = result.fixedErrors
                    }


computeProjectContextHashes :
    TraversalAndFolder projectContext moduleContext
    -> ValidProject
    -> Dict String (ModuleCacheEntry projectContext)
    -> Graph.Adjacency ()
    -> List (ContextHash projectContext)
    -> ComparableContextHash projectContext
computeProjectContextHashes traversalAndFolder project cache incoming initial =
    case traversalAndFolder of
        TraverseAllModulesInParallel _ ->
            ContextHash.toComparable initial

        TraverseImportedModulesFirst _ ->
            let
                graph : Graph FilePath ()
                graph =
                    ValidProject.moduleGraph project
            in
            IntDict.foldl
                (\key _ acc ->
                    case
                        Graph.get key graph
                            |> Maybe.andThen (\graphModule -> Dict.get graphModule.node.label cache)
                    of
                        Just importedModuleCache ->
                            ModuleCache.outputContextHash importedModuleCache :: acc

                        Nothing ->
                            acc
                )
                initial
                incoming
                |> ContextHash.toComparable


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
                            foldProjectContexts (ModuleCache.outputContext importedModuleCache) accContext

                        Nothing ->
                            accContext
                )
                initial
                incoming


computeModuleAndCacheResult :
    ReviewOptionsData
    -> Zipper GraphModule
    -> ValidProject
    -> List RuleProjectVisitor
    -> FixedErrors
    -> { project : ValidProject, ruleProjectVisitors : List RuleProjectVisitor, nextStep : NextStep, fixedErrors : FixedErrors }
computeModuleAndCacheResult reviewOptions moduleZipper project ruleProjectVisitors fixedErrors =
    let
        { node, incoming } =
            Zipper.current moduleZipper
    in
    case ValidProject.getModuleByPath node.label project of
        Nothing ->
            { project = project
            , ruleProjectVisitors = ruleProjectVisitors
            , nextStep = ModuleVisitStep (Zipper.next moduleZipper)
            , fixedErrors = fixedErrors
            }

        Just module_ ->
            computeModule
                { reviewOptions = reviewOptions
                , ruleProjectVisitors = ruleProjectVisitors
                , module_ = module_
                , project = project
                , moduleZipper = moduleZipper
                , fixedErrors = fixedErrors
                , incoming = incoming
                }


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
    | RemovedElmModule
    | FixedElmJson
    | FixedReadme
    | FixedExtraFile


type PostFixStatus
    = ShouldAbort FixedErrors
    | ShouldContinue FixedErrors


type StandardFindFixResult
    = FoundNoFixesStandard RuleProjectVisitor
    | FoundFixStandard { newProject : ValidProject, newRule : RuleProjectVisitor, step : Step, newFixedErrors : FixedErrors }


standardFindFix : ReviewOptionsData -> ValidProject -> FixedErrors -> (List (Error {}) -> RuleProjectVisitor) -> List (Error {}) -> StandardFindFixResult
standardFindFix reviewOptions project fixedErrors updateErrors errors =
    case findFix reviewOptions project updateErrors errors fixedErrors Nothing of
        FoundNoFixes newRule ->
            FoundNoFixesStandard newRule

        FoundFix newRule ( postFixStatus, fixResult ) ->
            let
                ( newFixedErrors, step ) =
                    case postFixStatus of
                        ShouldAbort newFixedErrors_ ->
                            ( newFixedErrors_, EndAnalysis )

                        ShouldContinue newFixedErrors_ ->
                            ( newFixedErrors_
                            , case fixResult.fixedFile of
                                FixedElmJson ->
                                    ElmJson

                                FixedReadme ->
                                    Readme

                                FixedExtraFile ->
                                    ExtraFiles

                                FixedElmModule _ zipper ->
                                    Modules (Just zipper)

                                RemovedElmModule ->
                                    -- TODO MULTIFILE-FIXES Move to a more optimal starting position.
                                    Modules Nothing
                            )
            in
            FoundFixStandard { newProject = fixResult.project, newRule = newRule, newFixedErrors = newFixedErrors, step = step }


type FindFixResult
    = FoundNoFixes RuleProjectVisitor
    | FoundFix RuleProjectVisitor ( PostFixStatus, { project : ValidProject, fixedFile : FixedFile, error : ReviewError } )


findFix : ReviewOptionsData -> ValidProject -> (List (Error {}) -> RuleProjectVisitor) -> List (Error {}) -> FixedErrors -> Maybe (Zipper (Graph.NodeContext FilePath ())) -> FindFixResult
findFix reviewOptions project updateErrors errors fixedErrors maybeModuleZipper =
    case InternalOptions.shouldApplyFix reviewOptions of
        Nothing ->
            FoundNoFixes (updateErrors errors)

        Just fixablePredicate ->
            case findFixHelp project reviewOptions.supportFileRemoval fixablePredicate errors [] maybeModuleZipper of
                FoundNoFixesHelp errorsWithFailedFixes ->
                    FoundNoFixes (updateErrors errorsWithFailedFixes)

                FoundFixHelp errorsWithFailedFixes fixResult ->
                    let
                        newFixedErrors : FixedErrors
                        newFixedErrors =
                            FixedErrors.insert fixResult.error fixedErrors

                        nextStep : PostFixStatus
                        nextStep =
                            if InternalOptions.shouldContinueLookingForFixes reviewOptions newFixedErrors then
                                ShouldContinue newFixedErrors

                            else
                                ShouldAbort newFixedErrors
                    in
                    FoundFix (updateErrors errorsWithFailedFixes) ( nextStep, fixResult )
                        |> Logger.log
                            reviewOptions.logger
                            (fixedError newFixedErrors { ruleName = errorRuleName fixResult.error, filePath = errorFilePath fixResult.error })


type FindFixHelpResult
    = FoundNoFixesHelp (List (Error {}))
    | FoundFixHelp (List (Error {})) { project : ValidProject, fixedFile : FixedFile, error : ReviewError }


findFixHelp :
    ValidProject
    -> Bool
    -> ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool)
    -> List (Error {})
    -> List (Error {})
    -> Maybe (Zipper (Graph.NodeContext FilePath ()))
    -> FindFixHelpResult
findFixHelp project supportsFileDeletion fixablePredicate errors accErrors maybeModuleZipper =
    case errors of
        [] ->
            FoundNoFixesHelp accErrors

        err :: restOfErrors ->
            case isFixable supportsFileDeletion fixablePredicate err of
                Err updatedError ->
                    findFixHelp project supportsFileDeletion fixablePredicate restOfErrors (updatedError :: accErrors) maybeModuleZipper

                Ok fixes ->
                    case fixes of
                        [] ->
                            findFixHelp project supportsFileDeletion fixablePredicate restOfErrors (err :: accErrors) maybeModuleZipper

                        firstFix :: restOfFixes ->
                            case
                                applyFix project maybeModuleZipper err firstFix
                                    |> Result.andThen (\fixResult -> applyFixes maybeModuleZipper err restOfFixes fixResult)
                            of
                                Ok fixResult ->
                                    FoundFixHelp (errors ++ accErrors) { project = fixResult.project, fixedFile = fixResult.fixedFile, error = errorToReviewError err }

                                Err nonAppliedError ->
                                    findFixHelp project supportsFileDeletion fixablePredicate restOfErrors (nonAppliedError :: accErrors) maybeModuleZipper


applyFixes : Maybe (Zipper (Graph.NodeContext FilePath ())) -> Error {} -> List ( FileTarget, FixKind ) -> { project : ValidProject, fixedFile : FixedFile } -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyFixes maybeModuleZipper err fixes acc =
    case fixes of
        [] ->
            Ok acc

        fix :: rest ->
            case applyFix acc.project maybeModuleZipper err fix of
                Ok fixResult ->
                    applyFixes maybeModuleZipper err rest { project = fixResult.project, fixedFile = earlierFixedFile fixResult.fixedFile acc.fixedFile }

                fixResultErr ->
                    fixResultErr


earlierFixedFile : FixedFile -> FixedFile -> FixedFile
earlierFixedFile a b =
    case ( a, b ) of
        ( FixedElmJson, _ ) ->
            FixedElmJson

        ( _, FixedElmJson ) ->
            FixedElmJson

        ( FixedReadme, _ ) ->
            FixedReadme

        ( _, FixedReadme ) ->
            FixedReadme

        ( FixedExtraFile, _ ) ->
            FixedExtraFile

        ( _, FixedExtraFile ) ->
            FixedExtraFile

        ( RemovedElmModule, _ ) ->
            RemovedElmModule

        ( _, RemovedElmModule ) ->
            RemovedElmModule

        ( FixedElmModule _ zipperA, FixedElmModule _ zipperB ) ->
            if Zipper.position zipperA <= Zipper.position zipperB then
                a

            else
                b


applyFix : ValidProject -> Maybe (Zipper (Graph.NodeContext FilePath ())) -> Error {} -> ( FileTarget, FixKind ) -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyFix project maybeModuleZipper err ( target, fixes ) =
    case fixes of
        ErrorFixes.Edit edits ->
            applyEditFix project maybeModuleZipper err target edits

        ErrorFixes.Remove ->
            applyFileDeletionFix project err target


applyEditFix : ValidProject -> Maybe (Zipper (Graph.NodeContext FilePath ())) -> Error {} -> FileTarget -> List Fix -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyEditFix project maybeModuleZipper err target fixes =
    case target of
        FileTarget.Module targetPath ->
            applySingleModuleFix project maybeModuleZipper err targetPath fixes

        FileTarget.ElmJson ->
            applyElmJsonFix project err fixes

        FileTarget.Readme ->
            applyReadmeFix project err fixes

        FileTarget.ExtraFile targetPath ->
            applyExtraFileFix project err targetPath fixes


applyFileDeletionFix : ValidProject -> Error {} -> FileTarget -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyFileDeletionFix project (Error err) target =
    case target of
        FileTarget.Module targetPath ->
            case ValidProject.removeModule targetPath project of
                Ok newProject ->
                    Ok { project = newProject, fixedFile = RemovedElmModule }

                Err fixProblem ->
                    Err (Error (markFixesAsProblem fixProblem err))

        FileTarget.ExtraFile targetPath ->
            Ok { project = ValidProject.removeExtraFile targetPath project, fixedFile = FixedExtraFile }

        FileTarget.ElmJson ->
            -- Not supported
            Ok { project = project, fixedFile = FixedElmJson }

        FileTarget.Readme ->
            -- Not supported
            Ok { project = project, fixedFile = FixedReadme }


markFixesAsProblem : FixProblem -> BaseError -> BaseError
markFixesAsProblem fixProblem error_ =
    { error_ | fixProblem = Just fixProblem }


isFixable : Bool -> ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool) -> Error {} -> Result (Error {}) (List ( FileTarget, FixKind ))
isFixable supportsFileDeletion predicate ((Error err) as untouchedError) =
    case err.fixProblem of
        Just _ ->
            Err untouchedError

        Nothing ->
            -- It's cheaper to check for fixes first and also quite likely to return Nothing
            -- so we do the fixes check first.
            if predicate { ruleName = err.ruleName, filePath = err.filePath, message = err.message, details = err.details, range = err.range } then
                case Review.Error.ReviewError.compileFixes err.fixes err.fixProblem of
                    Ok (Just list) ->
                        if not supportsFileDeletion && fixTriesToDeleteFiles list then
                            Err untouchedError

                        else
                            Ok list

                    Ok Nothing ->
                        Err untouchedError

                    Err fixProblem ->
                        Err (Error (markFixesAsProblem fixProblem err))

            else
                Err untouchedError


fixTriesToDeleteFiles : List ( a, FixKind ) -> Bool
fixTriesToDeleteFiles list =
    List.any
        (\( _, fix ) ->
            case fix of
                ErrorFixes.Edit _ ->
                    False

                ErrorFixes.Remove ->
                    True
        )
        list


applySingleModuleFix : ValidProject -> Maybe (Zipper (Graph.NodeContext FilePath ())) -> Error {} -> String -> List Edit -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applySingleModuleFix project maybeModuleZipper ((Error headError) as err) targetPath edits =
    case ValidProject.getModuleByPath targetPath project of
        Nothing ->
            Err err

        Just file ->
            case
                InternalFix.editModule edits (ProjectModule.path file) (ProjectModule.source file)
                    |> Result.andThen
                        (\fixResult ->
                            ValidProject.addParsedModule { path = targetPath, source = fixResult.source, ast = fixResult.ast } maybeModuleZipper project
                                |> Result.map
                                    (\( newProject, newModuleZipper ) ->
                                        { project = newProject
                                        , fixedFile = FixedElmModule fixResult newModuleZipper
                                        }
                                    )
                        )
            of
                Err fixProblem ->
                    Err (Error (markFixesAsProblem fixProblem headError))

                Ok fixResult ->
                    Ok fixResult


applyElmJsonFix : ValidProject -> Error {} -> List Edit -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyElmJsonFix project ((Error headError) as err) fixes =
    case ValidProject.elmJson project of
        Nothing ->
            Err err

        Just elmJson ->
            case
                InternalFix.editElmJson fixes elmJson.raw
                    |> Result.map
                        (\fixResult ->
                            ValidProject.addElmJson { path = elmJson.path, raw = fixResult.raw, project = fixResult.project } project
                        )
            of
                Err fixProblem ->
                    Err (Error (markFixesAsProblem fixProblem headError))

                Ok newProject ->
                    Ok
                        { project = newProject
                        , fixedFile = FixedElmJson
                        }


applyReadmeFix : ValidProject -> Error {} -> List Edit -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyReadmeFix project ((Error headError) as err) fixes =
    case ValidProject.readme project of
        Nothing ->
            Err err

        Just readme ->
            case InternalFix.applyEdits "README.md" fixes readme.content of
                Err fixProblem ->
                    Err (Error (markFixesAsProblem fixProblem headError))

                Ok content ->
                    Ok
                        { project = ValidProject.addReadme { path = readme.path, content = content } project
                        , fixedFile = FixedReadme
                        }


applyExtraFileFix : ValidProject -> Error {} -> String -> List Edit -> Result (Error {}) { project : ValidProject, fixedFile : FixedFile }
applyExtraFileFix project ((Error headError) as err) targetPath edits =
    case Dict.get targetPath (ValidProject.extraFilesWithoutKeys project) of
        Nothing ->
            Err err

        Just content ->
            case InternalFix.applyEdits targetPath edits content of
                Err fixProblem ->
                    Err (Error (markFixesAsProblem fixProblem headError))

                Ok newFileContent ->
                    if String.endsWith ".json" targetPath then
                        case Decode.decodeString Decode.value newFileContent of
                            Ok _ ->
                                Ok
                                    { project = ValidProject.addExtraFile { path = targetPath, content = newFileContent } project
                                    , fixedFile = FixedExtraFile
                                    }

                            Err decodingError ->
                                let
                                    fixProblem : FixProblem
                                    fixProblem =
                                        FixProblem.InvalidJson
                                            { filePath = targetPath
                                            , source = newFileContent
                                            , edits = edits
                                            , decodingError = decodingError
                                            }
                                in
                                Err (Error (markFixesAsProblem fixProblem headError))

                    else
                        Ok
                            { project = ValidProject.addExtraFile { path = targetPath, content = newFileContent } project
                            , fixedFile = FixedExtraFile
                            }


visitModuleForProjectRule : AvailableData -> List (AvailableData -> RuleModuleVisitor) -> List RuleModuleVisitor
visitModuleForProjectRule availableData ruleModuleVisitors =
    ruleModuleVisitors
        |> List.map (\createRuleVisitor -> createRuleVisitor availableData)
        |> fromListToJsArray
        |> mutatingMap (\acc -> runVisitor .moduleDefinitionVisitor availableData.ast.moduleDefinition acc)
        |> mutatingMap (\acc -> runVisitor .moduleDocumentationVisitor availableData.moduleDocumentation acc)
        |> mutatingMap (\acc -> runVisitor .commentVisitor availableData.ast.comments acc)
        |> mutatingMap (\acc -> runVisitor .importsVisitor availableData.ast.imports acc)
        |> mutatingMap (\acc -> runVisitor .declarationListVisitor availableData.ast.declarations acc)
        |> visitDeclarationsAndExpressions availableData.ast.declarations
        |> mutatingMap (\acc -> runVisitor .finalModuleEvaluation () acc)
        |> fromJsArrayToList


type JsArray a
    = JsArray (List a)


fromJsArrayToList : JsArray a -> List a
fromJsArrayToList (JsArray list) =
    list


fromListToJsArray : List a -> JsArray a
fromListToJsArray =
    JsArray


mutatingMap : (a -> b) -> JsArray a -> JsArray b
mutatingMap mapper (JsArray list) =
    JsArray (List.map mapper list)


visitDeclarationsAndExpressions : List (Node Declaration) -> JsArray RuleModuleVisitor -> JsArray RuleModuleVisitor
visitDeclarationsAndExpressions declarations rules =
    List.foldl visitDeclarationAndExpressions rules declarations


visitDeclarationAndExpressions : Node Declaration -> JsArray RuleModuleVisitor -> JsArray RuleModuleVisitor
visitDeclarationAndExpressions declaration rules =
    let
        updatedRules : JsArray RuleModuleVisitor
        updatedRules =
            rules
                |> mutatingMap (\acc -> runVisitor .declarationVisitorOnEnter declaration acc)
    in
    (case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            visitExpression (Node.value function.declaration |> .expression) updatedRules

        _ ->
            updatedRules
    )
        |> mutatingMap (\acc -> runVisitor .declarationVisitorOnExit declaration acc)


visitExpression : Node Expression -> JsArray RuleModuleVisitor -> JsArray RuleModuleVisitor
visitExpression node rules =
    case Node.value node of
        Expression.LetExpression letBlock ->
            let
                updatedRules : JsArray RuleModuleVisitor
                updatedRules =
                    rules
                        |> mutatingMap (\acc -> runVisitor .expressionVisitorOnEnter node acc)
            in
            List.foldl
                (visitLetDeclaration (Node (Node.range node) letBlock))
                updatedRules
                letBlock.declarations
                |> visitExpression letBlock.expression
                |> mutatingMap (\acc -> runVisitor .expressionVisitorOnExit node acc)

        Expression.CaseExpression caseBlock ->
            let
                updatedRules : JsArray RuleModuleVisitor
                updatedRules =
                    rules
                        |> mutatingMap (\acc -> runVisitor .expressionVisitorOnEnter node acc)
                        |> visitExpression caseBlock.expression
            in
            List.foldl
                (\case_ acc -> visitCaseBranch (Node (Node.range node) caseBlock) case_ acc)
                updatedRules
                caseBlock.cases
                |> mutatingMap (\acc -> runVisitor .expressionVisitorOnExit node acc)

        _ ->
            let
                updatedRules : JsArray RuleModuleVisitor
                updatedRules =
                    rules
                        |> mutatingMap (\acc -> runVisitor .expressionVisitorOnEnter node acc)
            in
            List.foldl
                visitExpression
                updatedRules
                (expressionChildren node)
                |> mutatingMap (\acc -> runVisitor .expressionVisitorOnExit node acc)


visitLetDeclaration :
    Node Expression.LetBlock
    -> Node Expression.LetDeclaration
    -> JsArray RuleModuleVisitor
    -> JsArray RuleModuleVisitor
visitLetDeclaration letBlockWithRange ((Node _ letDeclaration) as letDeclarationWithRange) rules =
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
        |> mutatingMap (\acc -> runVisitor2 .letDeclarationVisitorOnEnter letBlockWithRange letDeclarationWithRange acc)
        |> visitExpression expressionNode
        |> mutatingMap (\acc -> runVisitor2 .letDeclarationVisitorOnExit letBlockWithRange letDeclarationWithRange acc)


visitCaseBranch :
    Node Expression.CaseBlock
    -> ( Node Pattern, Node Expression )
    -> JsArray RuleModuleVisitor
    -> JsArray RuleModuleVisitor
visitCaseBranch caseBlockWithRange (( _, caseExpression ) as caseBranch) rules =
    rules
        |> mutatingMap (\acc -> runVisitor2 .caseBranchVisitorOnEnter caseBlockWithRange caseBranch acc)
        |> visitExpression caseExpression
        |> mutatingMap (\acc -> runVisitor2 .caseBranchVisitorOnExit caseBlockWithRange caseBranch acc)


extractSourceCode : List String -> Range -> String
extractSourceCode lines { start, end } =
    case List.drop (start.row - 1) lines of
        [] ->
            ""

        firstLine :: rest ->
            if start.row == end.row then
                Unicode.slice
                    (start.column - 1)
                    (end.column - 1)
                    firstLine

            else
                let
                    { linesTaken, lastLine } =
                        takeNLines (end.row - start.row - 1) rest ""
                in
                Unicode.dropLeft (start.column - 1) firstLine
                    ++ linesTaken
                    ++ (case lastLine of
                            Just lastLine_ ->
                                "\n" ++ Unicode.left (end.column - 1) lastLine_

                            Nothing ->
                                ""
                       )


takeNLines : Int -> List String -> String -> { linesTaken : String, lastLine : Maybe String }
takeNLines n lines linesTaken =
    if n <= 0 then
        { linesTaken = linesTaken, lastLine = List.head lines }

    else
        case lines of
            [] ->
                { linesTaken = linesTaken, lastLine = Nothing }

            line :: rest ->
                takeNLines (n - 1)
                    rest
                    (linesTaken ++ "\n" ++ line)


type RuleProjectVisitor
    = RuleProjectVisitor RuleProjectVisitorOperations


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
type alias RuleProjectVisitorOperations =
    { elmJsonVisitor : Maybe (ValidProject -> Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> ( List (Error {}), RuleProjectVisitor ))
    , readmeVisitor : Maybe (ValidProject -> Maybe { readmeKey : ReadmeKey, content : String } -> ( List (Error {}), RuleProjectVisitor ))
    , extraFilesVisitor : Maybe (ValidProject -> ExtraFileData -> ( List (Error {}), RuleProjectVisitor ))
    , dependenciesVisitor : Maybe (ValidProject -> { all : Dict String Review.Project.Dependency.Dependency, direct : Dict String Review.Project.Dependency.Dependency } -> ( List (Error {}), RuleProjectVisitor ))
    , createModuleVisitorFromProjectVisitor : Maybe (ValidProject -> String -> ContentHash -> Graph.Adjacency () -> Maybe (AvailableData -> RuleModuleVisitor))
    , finalProjectEvaluation : Maybe (() -> ( List (Error {}), RuleProjectVisitor ))
    , dataExtractVisitor : ReviewOptionsData -> Dict String Encode.Value -> ( Dict String Encode.Value, RuleProjectVisitor )
    , getErrorsForModule : String -> List (Error {})
    , getErrors : () -> List (Error {})
    , setErrorsForModule : String -> List (Error {}) -> RuleProjectVisitor
    , setErrorsForElmJson : List (Error {}) -> RuleProjectVisitor
    , setErrorsForExtraFiles : List (Error {}) -> RuleProjectVisitor
    , setErrorsForReadme : List (Error {}) -> RuleProjectVisitor
    , setErrorsForDependencies : List (Error {}) -> RuleProjectVisitor
    , setErrorsForFinalEvaluation : List (Error {}) -> RuleProjectVisitor
    , backToRule : () -> Rule
    , requestedData : RequestedData
    }


createRuleProjectVisitor : ProjectRuleSchemaData projectContext moduleContext -> ValidProject -> ChangeableRuleData -> ProjectRuleCache projectContext -> RuleProjectVisitor
createRuleProjectVisitor schema initialProject ruleData initialCache =
    let
        raise : { cache : ProjectRuleCache projectContext, ruleData : ChangeableRuleData } -> RuleProjectVisitor
        raise ({ cache } as hidden) =
            let
                raiseCache : ProjectRuleCache projectContext -> RuleProjectVisitor
                raiseCache newCache =
                    raise { cache = newCache, ruleData = hidden.ruleData }
            in
            RuleProjectVisitor
                { elmJsonVisitor = createProjectVisitor schema hidden schema.elmJsonVisitor ElmJsonStep ValidProject.elmJsonHash .elmJson (\entry -> raiseCache { cache | elmJson = Just entry }) (\() -> raise hidden)
                , readmeVisitor = createProjectVisitor schema hidden schema.readmeVisitor ReadmeStep ValidProject.readmeHash .readme (\entry -> raiseCache { cache | readme = Just entry }) (\() -> raise hidden)
                , extraFilesVisitor = createExtraFilesVisitor schema hidden raise raiseCache
                , dependenciesVisitor = createDependenciesVisitor schema hidden.ruleData raiseCache cache { allVisitor = schema.dependenciesVisitor, directVisitor = schema.directDependenciesVisitor }
                , createModuleVisitorFromProjectVisitor = createModuleVisitorFromProjectVisitor schema raiseCache hidden
                , finalProjectEvaluation = createFinalProjectEvaluationVisitor schema hidden.ruleData raiseCache cache
                , dataExtractVisitor = createDataExtractVisitor schema raiseCache cache
                , getErrorsForModule = \filePath -> getErrorsForModule cache filePath
                , getErrors = \() -> errorsFromCache (finalCacheMarker schema.name hidden.ruleData.ruleId cache)
                , setErrorsForModule = \filePath newErrors -> raiseCache { cache | moduleContexts = Dict.update filePath (Maybe.map (\entry -> ModuleCache.setErrors newErrors entry)) cache.moduleContexts }
                , setErrorsForElmJson = \newErrors -> raiseCache { cache | elmJson = ProjectFileCache.setErrors newErrors cache.elmJson }
                , setErrorsForReadme = \newErrors -> raiseCache { cache | readme = ProjectFileCache.setErrors newErrors cache.readme }
                , setErrorsForExtraFiles = \newErrors -> raiseCache { cache | extraFiles = ExtraFile.setErrors newErrors cache.extraFiles }
                , setErrorsForDependencies = \newErrors -> raiseCache { cache | dependencies = ProjectFileCache.setErrors newErrors cache.dependencies }
                , setErrorsForFinalEvaluation = \newErrors -> raiseCache { cache | finalEvaluationErrors = EndAnalysisCache.setOutput newErrors cache.finalEvaluationErrors }
                , backToRule =
                    \() ->
                        Rule
                            { name = schema.name
                            , id = hidden.ruleData.ruleId
                            , exceptions = hidden.ruleData.exceptions
                            , requestedData = hidden.ruleData.requestedData
                            , providesFixes = schema.providesFixes
                            , ruleProjectVisitor = Ok (\newProject newRuleData -> createRuleProjectVisitor schema newProject newRuleData cache)
                            }
                , requestedData = hidden.ruleData.requestedData
                }
    in
    raise
        { cache = removeUnknownModulesFromInitialCache initialProject initialCache
        , ruleData = ruleData
        }


createProjectVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> RuleProjectVisitorHidden projectContext
    -> Maybe (data -> projectContext -> ( List (Error {}), projectContext ))
    -> StepToComputeContext
    -> (ValidProject -> Maybe ContentHash)
    -> (ProjectRuleCache projectContext -> Maybe (ProjectFileCache projectContext))
    -> (ProjectFileCache projectContext -> RuleProjectVisitor)
    -> (() -> RuleProjectVisitor)
    ->
        Maybe
            (ValidProject
             -> data
             -> ( List (Error {}), RuleProjectVisitor )
            )
createProjectVisitor schema hidden maybeVisitor step computeContentHash cacheGetter toRuleProjectVisitor toRuleProjectVisitorWithoutChangingCache =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just
                (\project data ->
                    let
                        ( baseInputContextHash, inputContext ) =
                            findInitialInputContext hidden.cache step schema.initialProjectContext

                        inputContextHash : ComparableContextHash projectContext
                        inputContextHash =
                            ContextHash.toComparable baseInputContextHash

                        contentHash : Maybe ContentHash
                        contentHash =
                            computeContentHash project

                        cachePredicate : ProjectFileCache projectContext -> Bool
                        cachePredicate cacheData =
                            ProjectFileCache.match contentHash inputContextHash cacheData
                    in
                    case reuseProjectRuleCache cachePredicate cacheGetter hidden.cache of
                        Just entry ->
                            ( ProjectFileCache.errors entry, toRuleProjectVisitorWithoutChangingCache () )

                        Nothing ->
                            let
                                ( errorsForVisitor, outputContext ) =
                                    visitor data inputContext

                                errors : List (Error {})
                                errors =
                                    filterExceptionsAndSetName hidden.ruleData.exceptions schema.name errorsForVisitor
                            in
                            ( errors
                            , ProjectFileCache.create
                                { contentHash = contentHash
                                , errors = errors
                                , inputContextHash = inputContextHash
                                , outputContext = outputContext
                                }
                                |> toRuleProjectVisitor
                            )
                )


createExtraFilesVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> RuleProjectVisitorHidden projectContext
    -> ({ cache : ProjectRuleCache projectContext, ruleData : ChangeableRuleData } -> RuleProjectVisitor)
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    ->
        Maybe
            (ValidProject
             -> ExtraFileData
             -> ( List (Error {}), RuleProjectVisitor )
            )
createExtraFilesVisitor schema ({ cache } as hidden) raise raiseCache =
    case schema.extraFilesVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just
                (\project data ->
                    let
                        ( baseInputContextHash, inputContext ) =
                            findInitialInputContext hidden.cache ExtraFilesStep schema.initialProjectContext

                        inputContextHash : ComparableContextHash projectContext
                        inputContextHash =
                            ContextHash.toComparable baseInputContextHash

                        contentHash : ContentHash
                        contentHash =
                            ValidProject.extraFilesHash project

                        cachePredicate : ExtraFilesCache projectContext -> Bool
                        cachePredicate extraFiles =
                            ExtraFile.match contentHash inputContextHash extraFiles
                    in
                    case reuseProjectRuleCache cachePredicate .extraFiles hidden.cache of
                        Just entry ->
                            ( ExtraFile.errors entry, raise hidden )

                        Nothing ->
                            let
                                ( errorsForVisitor, outputContext ) =
                                    visitor data inputContext

                                errors : List (Error {})
                                errors =
                                    filterExceptionsAndSetName hidden.ruleData.exceptions schema.name errorsForVisitor

                                entry : ExtraFilesCache projectContext
                                entry =
                                    ExtraFile.create
                                        { contentHash = contentHash
                                        , errors = errors
                                        , inputContextHash = inputContextHash
                                        , outputContext = outputContext
                                        }
                            in
                            ( errors
                            , raiseCache { cache | extraFiles = Just entry }
                            )
                )


createDependenciesVisitor :
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
createDependenciesVisitor schema { exceptions } raise cache { allVisitor, directVisitor } =
    case ( allVisitor, directVisitor ) of
        ( Nothing, Nothing ) ->
            Nothing

        _ ->
            Just
                (\project { all, direct } ->
                    let
                        ( baseInputContextHash, inputContext ) =
                            findInitialInputContext cache DependenciesStep schema.initialProjectContext

                        inputContextHash : ComparableContextHash projectContext
                        inputContextHash =
                            ContextHash.toComparable baseInputContextHash

                        dependenciesHash : Maybe ContentHash
                        dependenciesHash =
                            ValidProject.dependenciesHash project

                        cachePredicate : ProjectFileCache projectContext -> Bool
                        cachePredicate entry =
                            ProjectFileCache.match dependenciesHash inputContextHash entry
                    in
                    case reuseProjectRuleCache cachePredicate .dependencies cache of
                        Just entry ->
                            ( ProjectFileCache.errors entry, raise cache )

                        Nothing ->
                            let
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

                                dependenciesEntry : ProjectFileCache projectContext
                                dependenciesEntry =
                                    ProjectFileCache.create
                                        { contentHash = dependenciesHash
                                        , errors = errors
                                        , inputContextHash = inputContextHash
                                        , outputContext = finalOutputContext
                                        }
                            in
                            ( errors
                            , raise { cache | dependencies = Just dependenciesEntry }
                            )
                )


findInitialInputContext :
    ProjectRuleCache projectContext
    -> StepToComputeContext
    -> projectContext
    -> ( List (ContextHash projectContext), projectContext )
findInitialInputContext cache step defaultContext =
    case step of
        ElmJsonStep ->
            ( [], defaultContext )

        ReadmeStep ->
            case cache.elmJson of
                Just entry ->
                    ( [ ProjectFileCache.outputContextHash entry ], ProjectFileCache.outputContext entry )

                Nothing ->
                    findInitialInputContext cache ElmJsonStep defaultContext

        ExtraFilesStep ->
            case cache.readme of
                Just entry ->
                    ( [ ProjectFileCache.outputContextHash entry ], ProjectFileCache.outputContext entry )

                Nothing ->
                    findInitialInputContext cache ReadmeStep defaultContext

        DependenciesStep ->
            case cache.extraFiles of
                Just entry ->
                    ( [ ExtraFile.outputContextHash entry ], ExtraFile.outputContext entry )

                Nothing ->
                    findInitialInputContext cache ExtraFilesStep defaultContext

        AfterProjectFilesStep ->
            case cache.dependencies of
                Just entry ->
                    ( [ ProjectFileCache.outputContextHash entry ], ProjectFileCache.outputContext entry )

                Nothing ->
                    findInitialInputContext cache DependenciesStep defaultContext


createFinalProjectEvaluationVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> ChangeableRuleData
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> Maybe (() -> ( List (Error {}), RuleProjectVisitor ))
createFinalProjectEvaluationVisitor schema { exceptions } raise cache =
    case schema.finalEvaluationFn of
        Nothing ->
            Nothing

        Just finalEvaluationFn ->
            Just
                (\() ->
                    let
                        inputContextHashes : ComparableContextHash projectContext
                        inputContextHashes =
                            computeFinalContextHashes schema cache

                        cachePredicate : FinalProjectEvaluationCache projectContext -> Bool
                        cachePredicate entry =
                            EndAnalysisCache.match inputContextHashes entry
                    in
                    case reuseProjectRuleCache cachePredicate .finalEvaluationErrors cache of
                        Just entry ->
                            ( EndAnalysisCache.output entry, raise cache )

                        Nothing ->
                            let
                                errors : List (Error {})
                                errors =
                                    cache
                                        |> computeFinalContext schema
                                        |> finalEvaluationFn
                                        |> filterExceptionsAndSetName exceptions schema.name
                            in
                            ( errors
                            , raise { cache | finalEvaluationErrors = Just (EndAnalysisCache.create inputContextHashes errors) }
                            )
                )


createDataExtractVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> ProjectRuleCache projectContext
    -> ReviewOptionsData
    -> Dict String Encode.Value
    -> ( Dict String Encode.Value, RuleProjectVisitor )
createDataExtractVisitor schema raise cache =
    case schema.dataExtractor of
        Nothing ->
            \_ extracts -> ( extracts, raise cache )

        Just dataExtractor ->
            \reviewOptions extracts ->
                if reviewOptions.extract then
                    let
                        inputContextHashes : ComparableContextHash projectContext
                        inputContextHashes =
                            computeFinalContextHashes schema cache

                        cachePredicate : ExtractCache projectContext -> Bool
                        cachePredicate extract =
                            EndAnalysisCache.match inputContextHashes extract

                        ( Extract extractData, newCache ) =
                            case reuseProjectRuleCache cachePredicate .extract cache of
                                Just entry ->
                                    ( EndAnalysisCache.output entry, cache )

                                Nothing ->
                                    let
                                        inputContext : projectContext
                                        inputContext =
                                            computeFinalContext schema cache

                                        extract : Extract
                                        extract =
                                            dataExtractor inputContext
                                    in
                                    ( extract
                                    , { cache | extract = Just (EndAnalysisCache.create inputContextHashes extract) }
                                    )
                    in
                    ( Dict.insert schema.name extractData extracts
                    , raise newCache
                    )

                else
                    ( extracts, raise cache )


createModuleVisitorFromProjectVisitor :
    ProjectRuleSchemaData projectContext moduleContext
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> RuleProjectVisitorHidden projectContext
    -> Maybe (ValidProject -> String -> ContentHash -> Graph.Adjacency () -> Maybe (AvailableData -> RuleModuleVisitor))
createModuleVisitorFromProjectVisitor schema raise hidden =
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
            Just (createModuleVisitorFromProjectVisitorHelp schema raise hidden traversalAndFolder moduleRuleSchema)


createModuleVisitorFromProjectVisitorHelp :
    ProjectRuleSchemaData projectContext moduleContext
    -> (ProjectRuleCache projectContext -> RuleProjectVisitor)
    -> RuleProjectVisitorHidden projectContext
    -> TraversalAndFolder projectContext moduleContext
    -> ( ModuleRuleSchema schemaState moduleContext, ContextCreator projectContext moduleContext )
    -> ValidProject
    -> String
    -> ContentHash
    -> Graph.Adjacency ()
    -> Maybe (AvailableData -> RuleModuleVisitor)
createModuleVisitorFromProjectVisitorHelp schema raise hidden traversalAndFolder ( ModuleRuleSchema moduleRuleSchema, moduleContextCreator ) =
    \project filePath moduleContentHash incoming ->
        let
            ( initialProjectContextHash, initialProjectContext ) =
                findInitialInputContext hidden.cache AfterProjectFilesStep schema.initialProjectContext

            inputContextHashes : ComparableContextHash projectContext
            inputContextHashes =
                computeProjectContextHashes traversalAndFolder project hidden.cache.moduleContexts incoming initialProjectContextHash

            isFileIgnored : Bool
            isFileIgnored =
                not (Exceptions.isFileWeWantReportsFor hidden.ruleData.exceptions filePath)

            shouldReuseCache : ModuleCacheEntry projectContext -> Bool
            shouldReuseCache cacheEntry =
                ModuleCache.match
                    moduleContentHash
                    inputContextHashes
                    cacheEntry
                    { isFileIgnored = isFileIgnored
                    , requestedData = hidden.ruleData.requestedData
                    }

            maybeCacheEntry : Maybe (ModuleCacheEntry projectContext)
            maybeCacheEntry =
                Dict.get filePath hidden.cache.moduleContexts
        in
        case reuseCache shouldReuseCache maybeCacheEntry of
            Just _ ->
                Nothing

            Nothing ->
                let
                    inputProjectContext : projectContext
                    inputProjectContext =
                        computeProjectContext traversalAndFolder project hidden.cache.moduleContexts incoming initialProjectContext
                in
                Just
                    (\availableData ->
                        let
                            initialContext : moduleContext
                            initialContext =
                                applyContextCreator availableData isFileIgnored moduleContextCreator inputProjectContext

                            ruleData : { ruleName : String, exceptions : Exceptions, filePath : String }
                            ruleData =
                                { ruleName = schema.name, exceptions = hidden.ruleData.exceptions, filePath = availableData.filePath }

                            toRuleProjectVisitor : ( List (Error {}), moduleContext ) -> RuleProjectVisitor
                            toRuleProjectVisitor ( errors, resultModuleContext ) =
                                let
                                    ( fromModuleToProjectErrors, outputProjectContext ) =
                                        case getFolderFromTraversal traversalAndFolder of
                                            Just { fromModuleToProject } ->
                                                applyContextCreator availableData isFileIgnored fromModuleToProject resultModuleContext

                                            Nothing ->
                                                ( [], schema.initialProjectContext )

                                    cacheEntry : ModuleCacheEntry projectContext
                                    cacheEntry =
                                        ModuleCache.create
                                            { contentHash = moduleContentHash
                                            , errors = qualifyErrors ruleData fromModuleToProjectErrors errors
                                            , inputContextHashes = inputContextHashes
                                            , isFileIgnored = isFileIgnored
                                            , outputContext = outputProjectContext
                                            }

                                    cache : ProjectRuleCache projectContext
                                    cache =
                                        hidden.cache
                                in
                                raise { cache | moduleContexts = Dict.insert availableData.filePath cacheEntry cache.moduleContexts }
                        in
                        createRuleModuleVisitor moduleRuleSchema ruleData toRuleProjectVisitor initialContext
                    )


getErrorsForModule : ProjectRuleCache projectContext -> String -> List (Error {})
getErrorsForModule cache filePath =
    case Dict.get filePath cache.moduleContexts of
        Just cacheEntry ->
            ModuleCache.errors cacheEntry

        Nothing ->
            []


type RuleModuleVisitor
    = RuleModuleVisitor RuleModuleVisitorOperations


type alias RuleModuleVisitorOperations =
    -- `moduleContext` is the hidden type variable
    -- The hidden state is `( List (Error {}), moduleContext )`
    { moduleDefinitionVisitor : Maybe (Node Module -> RuleModuleVisitor)
    , moduleDocumentationVisitor : Maybe (Maybe (Node String) -> RuleModuleVisitor)
    , commentVisitor : Maybe (List (Node String) -> RuleModuleVisitor)
    , importsVisitor : Maybe (List (Node Import) -> RuleModuleVisitor)
    , declarationListVisitor : Maybe (List (Node Declaration) -> RuleModuleVisitor)
    , declarationVisitorOnEnter : Maybe (Node Declaration -> RuleModuleVisitor)
    , declarationVisitorOnExit : Maybe (Node Declaration -> RuleModuleVisitor)
    , expressionVisitorOnEnter : Maybe (Node Expression -> RuleModuleVisitor)
    , expressionVisitorOnExit : Maybe (Node Expression -> RuleModuleVisitor)
    , letDeclarationVisitorOnEnter : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> RuleModuleVisitor)
    , letDeclarationVisitorOnExit : Maybe (Node Expression.LetBlock -> Node Expression.LetDeclaration -> RuleModuleVisitor)
    , caseBranchVisitorOnEnter : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> RuleModuleVisitor)
    , caseBranchVisitorOnExit : Maybe (Node Expression.CaseBlock -> ( Node Pattern, Node Expression ) -> RuleModuleVisitor)
    , finalModuleEvaluation : Maybe (() -> RuleModuleVisitor)
    , toProjectVisitor : () -> RuleProjectVisitor
    }


createRuleModuleVisitor :
    ModuleRuleSchemaData moduleContext
    -> { ruleName : String, exceptions : Exceptions, filePath : String }
    -> (( List (Error {}), moduleContext ) -> RuleProjectVisitor)
    -> moduleContext
    -> RuleModuleVisitor
createRuleModuleVisitor schema params toRuleProjectVisitor initialContext =
    let
        raise : ( List (Error {}), moduleContext ) -> RuleModuleVisitor
        raise errorsAndContext =
            RuleModuleVisitor
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
                , toProjectVisitor = \() -> toRuleProjectVisitor errorsAndContext
                }
    in
    raise ( [], initialContext )


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
            Just (\node -> raise (accumulate params (visitor node) errorsAndContext))


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
            Just (\a b -> raise (accumulate params (visitor a b) errorsAndContext))


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
                                accumulate params (visitor import_) initialErrorsAndContext
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
createFinalModuleEvaluationVisitor params raise errorsAndContext maybeVisitor =
    case maybeVisitor of
        Nothing ->
            Nothing

        Just visitor ->
            Just
                (\() ->
                    let
                        -- We don't want to destructure the tuple in the arguments,
                        -- because we want to mutate `errorsAndContext` in `moduleRuleImplementation`.
                        -- See the transformation we do in `optimize-js.js` in `node-elm-review`.
                        -- Destructuring earlier would mean we would reference older values of `errorsAndContext`.
                        ( errors, context ) =
                            errorsAndContext
                    in
                    raise ( qualifyErrors params (visitor context) errors, context )
                )


runVisitor : (RuleModuleVisitorOperations -> Maybe (a -> RuleModuleVisitor)) -> a -> RuleModuleVisitor -> RuleModuleVisitor
runVisitor field a ((RuleModuleVisitor ruleModuleVisitor) as original) =
    case field ruleModuleVisitor of
        Just visitor ->
            visitor a

        Nothing ->
            original


runVisitor2 : (RuleModuleVisitorOperations -> Maybe (a -> b -> RuleModuleVisitor)) -> a -> b -> RuleModuleVisitor -> RuleModuleVisitor
runVisitor2 field a b ((RuleModuleVisitor ruleModuleVisitor) as original) =
    case field ruleModuleVisitor of
        Just visitor ->
            visitor a b

        Nothing ->
            original


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


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulate : { ruleName : String, exceptions : Exceptions, filePath : String } -> (context -> ( List (Error {}), context )) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulate params visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( qualifyErrors params newErrors previousErrors, newContext )



-- INITIALIZING WITH CONTEXT
-- TODO Breaking change: Move this to a different module later on


{-| Create a module context from a project context or the other way around.
Use functions like [`withModuleName`](#withModuleName) to request more information.
-}
type ContextCreator from to
    = ContextCreator (AvailableData -> Bool -> from -> to) RequestedData


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
initContextCreator fn =
    ContextCreator
        (\_ _ -> fn)
        RequestedData.none


mapContextCreator : (a -> b) -> ContextCreator from a -> ContextCreator from b
mapContextCreator mapper (ContextCreator fn requestedData) =
    ContextCreator (\availableData bool from -> mapper (fn availableData bool from)) requestedData


applyContextCreator : AvailableData -> Bool -> ContextCreator from to -> from -> to
applyContextCreator data isFileIgnored (ContextCreator fn _) from =
    fn data isFileIgnored from


{-| Request metadata about the module.

**@deprecated**: Use more practical functions like

  - [`withModuleName`](#withModuleName)
  - [`withModuleNameNode`](#withModuleNameNode)
  - [`withIsInSourceDirectories`](#withIsInSourceDirectories)

-}
withMetadata : ContextCreator Metadata (from -> to) -> ContextCreator from to
withMetadata (ContextCreator fn requestedData) =
    ContextCreator
        (\data isFileIgnored ->
            fn data
                isFileIgnored
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
        (\data isFileIgnored -> fn data isFileIgnored (moduleNameNode data.ast.moduleDefinition |> Node.value))
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
    ContextCreator (\data isFileIgnored -> fn data isFileIgnored (moduleNameNode data.ast.moduleDefinition))
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
        (\data isFileIgnored -> fn data isFileIgnored data.isInSourceDirectories)
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
        (\data isFileIgnored -> fn data isFileIgnored isFileIgnored)
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
    import Review.Rule as Rule exposing (Rule)

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
        (\data isFileIgnored -> fn data isFileIgnored data.moduleNameLookupTable)
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
        (\data isFileIgnored -> fn data isFileIgnored data.ast)
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
        (\data isFileIgnored -> fn data isFileIgnored data.moduleDocumentation)
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
        (\data isFileIgnored -> fn data isFileIgnored data.moduleKey)
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
        (\data isFileIgnored -> fn data isFileIgnored data.filePath)
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
        (\data isFileIgnored -> fn data isFileIgnored data.extractSourceCode)
        (RequestedData { requested | sourceCodeExtractor = True })


type alias AvailableData =
    { ast : Elm.Syntax.File.File
    , moduleKey : ModuleKey
    , moduleDocumentation : Maybe (Node String)
    , moduleNameLookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , filePath : String
    , isInSourceDirectories : Bool
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


fixedError : FixedErrors -> { ruleName : String, filePath : String } -> List ( String, Encode.Value )
fixedError fixedErrors data =
    [ ( "type", Encode.string "apply-fix" )
    , ( "ruleName", Encode.string data.ruleName )
    , ( "filePath", Encode.string data.filePath )
    , ( "count", Encode.int (FixedErrors.count fixedErrors) )
    ]
