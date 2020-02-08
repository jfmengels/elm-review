module Review.Rule exposing
    ( Rule
    , review
    , ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema
    , withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , withModuleDefinitionVisitor, withCommentsVisitor, withImportVisitor, Direction(..), withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalModuleEvaluation
    , withModuleElmJsonVisitor, withModuleDependenciesVisitor
    , ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema
    , Error, error, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath
    , withFixes
    , traversingImportedModulesFirst, withProjectElmJsonVisitor, withProjectDependenciesVisitor, withFinalProjectEvaluation
    , FileKey, errorForFile
    )

{-| This module contains functions that are used for writing rules.


# How does it work?

`elm-review` turns the code of the analyzed module into an [Abstract Syntax Tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
(a tree-like structure which represents your source code) using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
Then, for each module and rule, it will give the details of your project (like the `elm.json` file) and the
contents of the file to analyze to the rule. The order in which things get passed to the rule is the following:

  - Read project-related info (only collect data in these steps)
      - The `elm.json` file, visited by [`withModuleElmJsonVisitor`](#withModuleElmJsonVisitor)
      - The definition for dependencies, visited by [`withModuleDependenciesVisitor`](#withModuleDependenciesVisitor)
  - Visit the file (in the following order)
      - The module definition, visited by [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) and [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor)
      - The module's list of comments, visited by [`withSimpleCommentsVisitor`](#withSimpleCommentsVisitor) and [`withCommentsVisitor`](#withCommentsVisitor)
      - Each import, visited by [`withSimpleImportVisitor`](#withSimpleImportVisitor) and [`withImportVisitor`](#withImportVisitor)
      - The list of declarations, visited by [`withDeclarationListVisitor`](#withDeclarationListVisitor)
      - Each declaration, visited by [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) and [`withDeclarationVisitor`](#withDeclarationVisitor).
        Before evaluating the next declaration, the expression contained in the declaration
        will be visited recursively using by [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) and [`withExpressionVisitor`](#withExpressionVisitor)
      - A final evaluation is made when the whole AST has been traversed, using [`withFinalModuleEvaluation`](#withFinalModuleEvaluation)

Evaluating a node means two things:

  - Detecting patterns and reporting errors
  - Collecting data in a `context` to have more information available in a later
    node evaluation. This is only available using "non-simple with\*" visitors.
    I recommend using the "simple with\*" visitors if you don't need to collect
    data, as they are simpler to use

`elm-review` relies on the [`elm-syntax`package](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/),
and all the node types you'll see will be coming from there. You are likely to
need to have the documentation for that package open when writing a rule.

There are plenty of examples in the documentation for each visitor function,
and you can also look at the source code for existing rules to better grasp how
rules work.


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
application, then you should not use this rule.".

Additionally, it could give a few examples of patterns that will be reported and
of patterns that will not be reported, so that users can have a better grasp of
what to expect.


# Strategies for writing rules effectively


## Use Test-Driven Development

This package comes with [`Review.Test`](./Review-Test), which works with [`elm-test`](https://github.com/elm-explorations/test).
I recommend reading through [`the strategies for effective testing`](./Review-Test#strategies-for-effective-testing) before
starting writing a rule.


## Look at the documentation for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)

`elm-review` is heavily dependent on the types that [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
provides. If you don't understand the AST it provides, you will have a hard time
implementing the rule you wish to create.


# Writing a Rule

**NOTE**: There are a lot of rule examples in the documentation of the functions
below. They are only here to showcase how to write rules and how a function can
be used. The rule examples are not necessarily good rules to enforce. See the
[section on whether to write a rule](./#when-to-write-or-enable-a-rule) for more on that. Even if you think
they are good ideas to enforce, they are often not complete, as there are other
patterns you would want to forbid, but that are not handled by the example.

There are several ways to write rules, depending on what information you need to collect to write the rule.


## Definition

@docs Rule


## Running the rule

@docs review


## Creating a module rule

A "module rule" looks at modules (i.e. files) once at a time. When it finishes looking at a file and reporting errors,
it forgets all about the file it just analyzed before looking at a different file. You should create one of these if you
do not need to know the contents of a different module in the project, such as what functions are exposed.
If you do need that information, you should create a [project rule](#creating-a-project-rule).

If you are new to writing rules, I would recommend learning how to build a module rule first, as they are in practice a
simpler version of project rules.

@docs ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema


## Builder functions without context

@docs withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor


## Builder functions with context

@docs withModuleDefinitionVisitor, withCommentsVisitor, withImportVisitor, Direction, withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalModuleEvaluation


## Builder functions to analyze the project's data

@docs withModuleElmJsonVisitor, withModuleDependenciesVisitor


## Creating a project rule

@docs ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema


## Errors

@docs Error, error, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath


## Automatic fixing

For more information on automatic fixing, read the documentation for [`Review.Fix`](./Review-Fix).

@docs withFixes


# TODO

@docs traversingImportedModulesFirst, withProjectElmJsonVisitor, withProjectDependenciesVisitor, withFinalProjectEvaluation
@docs FileKey, errorForFile

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Graph exposing (Graph)
import IntDict
import Review.Fix exposing (Fix)
import Review.Project exposing (Project, ProjectModule)
import Set exposing (Set)


{-| Represents a construct able to analyze modules from a project and report
unwanted patterns.
TODO Link to "creating a module rule" and project rule instead
See [`newModuleRuleSchema`](#newModuleRuleSchema), and [`newProjectRuleSchema`](#newProjectRuleSchema) for how to create one.
-}
type Rule
    = Rule String (Project -> ( List Error, Rule ))


{-| Represents a schema for a module [`Rule`](#Rule).

Start by using [`newModuleRuleSchema`](#newModuleRuleSchema), then add visitors to look at the parts of the code you are interested in.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

-}
type ModuleRuleSchema configuration context
    = ModuleRuleSchema
        { name : String
        , initialContext : context
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> context -> context)
        , dependenciesVisitors : List (Dict String Elm.Docs.Module -> context -> context)
        , moduleDefinitionVisitors : List (Node Module -> context -> ( List Error, context ))
        , commentsVisitors : List (List (Node String) -> context -> ( List Error, context ))
        , importVisitors : List (Node Import -> context -> ( List Error, context ))
        , declarationListVisitors : List (List (Node Declaration) -> context -> ( List Error, context ))
        , declarationVisitors : List (DirectedVisitor Declaration context)
        , expressionVisitors : List (DirectedVisitor Expression context)
        , finalEvaluationFns : List (context -> List Error)
        }


type alias DirectedVisitor nodeType context =
    Node nodeType -> Direction -> context -> ( List Error, context )


type alias InAndOut visitor =
    { onEnter : List visitor
    , onExit : List visitor
    }



-- REVIEWING


{-| Review a project and gives back the errors raised by the given rules.

Note that you won't need to use this function when writing a function. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review.File exposing (ProjectModule)
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
internal cache to make it faster to re-run the rules on the same project. If you
plan on re-reviewing with the same rules and project, for instance to review the
project after a file has changed, you may want to store the rules in your `Model`.

The rules are functions, so doing so will make your model unable to be exported/imported
with `elm/browser`'s debugger.

-}
review : List Rule -> Project -> ( List Error, List Rule )
review rules project =
    let
        ( ruleErrors, rulesWithCache ) =
            runRules rules project
    in
    ( List.concat
        [ ruleErrors
        , project
            |> Review.Project.filesThatFailedToParse
            |> List.map parsingError
        ]
    , rulesWithCache
    )


runRules : List Rule -> Project -> ( List Error, List Rule )
runRules rules project =
    List.foldl
        (\rule ( errors, previousRules ) ->
            let
                ( ruleErrors, ruleWithCache ) =
                    run rule project
            in
            ( List.concat [ ruleErrors, errors ], ruleWithCache :: previousRules )
        )
        ( [], [] )
        rules


run : Rule -> Project -> ( List Error, Rule )
run (Rule _ fn) project =
    fn project


{-| Represents whether a Node is being traversed before having seen its children (`OnEnter`ing the Node), or after (`OnExit`ing the Node).

When visiting the AST, nodes are visited twice: once on `OnEnter`, before the
children of the node will be visited, and once on `OnExit`, after the children of
the node have been visited.

In most cases, you'll only want to handle the `OnEnter` case, but in some cases,
you'll want to visit a `Node` after having seen its children. For instance, if
you're trying to detect the unused variables defined inside of a `let in` expression,
you'll want to collect the declaration of variables, note which ones are used,
and at the end of the block, report the ones that weren't used.

    expressionVisitor : Context -> Direction -> Node Expression -> ( List Error, Context )
    expressionVisitor context direction node =
        case ( direction, node ) of
            ( Rule.OnEnter, Expression.FunctionOrValue moduleName name ) ->
                ( [], markVariableAsUsed context name )

            -- Find variables declared in `let in` expression
            ( Rule.OnEnter, LetExpression letBlock ) ->
                ( [], registerVariables context letBlock )

            -- When exiting the `let in expression, report the variables that were not used.
            ( Rule.OnExit, LetExpression _ ) ->
                ( unusedVariables context |> List.map createError, context )

-}
type Direction
    = OnEnter
    | OnExit


{-| Creates a new schema for a rule. Will require calling [`fromModuleRuleSchema`](#fromModuleRuleSchema)
to create a usable [`Rule`](#Rule). Use "with\*" functions from this module, like
[`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) or [`withSimpleImportVisitor`](#withSimpleImportVisitor)
to make it report something.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

TODO Update this text
If you wish to build a [`Rule`](#Rule) that collects data as the file gets traversed,
take a look at [`withInitialContext`](#withInitialContext) and "with\*" functions without
"Simple" in their name, like [`withExpressionVisitor`](#withExpressionVisitor),
[`withImportVisitor`](#withImportVisitor) or [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnusedVariables" initialContext
            |> Rule.withExpressionVisitor expressionVisitor
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
newModuleRuleSchema :
    String
    -> context
    -> ModuleRuleSchema { withModuleElmJsonVisitor : (), withModuleDependenciesVisitor : () } context
newModuleRuleSchema name_ context =
    emptySchema name_ context


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema : ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context -> Rule
fromModuleRuleSchema ((ModuleRuleSchema { name }) as schema) =
    Rule name (runModuleRule (reverseVisitors schema) Dict.empty)


reverseVisitors : ModuleRuleSchema anything context -> ModuleRuleSchema anything context
reverseVisitors (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | elmJsonVisitors = List.reverse schema.elmJsonVisitors
            , dependenciesVisitors = List.reverse schema.dependenciesVisitors
            , moduleDefinitionVisitors = List.reverse schema.moduleDefinitionVisitors
            , commentsVisitors = List.reverse schema.commentsVisitors
            , importVisitors = List.reverse schema.importVisitors
            , declarationListVisitors = List.reverse schema.declarationListVisitors
            , finalEvaluationFns = List.reverse schema.finalEvaluationFns
        }


type alias ModuleRuleCache =
    Dict String
        { source : String
        , errors : List Error
        }


runModuleRule : ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context -> ModuleRuleCache -> Project -> ( List Error, Rule )
runModuleRule ((ModuleRuleSchema { name }) as schema) startCache project =
    let
        computeErrors_ : ProjectModule -> List Error
        computeErrors_ =
            computeErrors schema project

        newCache : ModuleRuleCache
        newCache =
            List.foldl
                (\module_ cache ->
                    case Dict.get module_.path cache of
                        Nothing ->
                            Dict.insert module_.path { source = module_.source, errors = computeErrors_ module_ } cache

                        Just cacheEntry ->
                            if cacheEntry.source == module_.source then
                                -- File is unchanged, we will later return the cached errors
                                cache

                            else
                                Dict.insert module_.path { source = module_.source, errors = computeErrors_ module_ } cache
                )
                startCache
                (Review.Project.modules project)

        errors : List Error
        errors =
            newCache
                |> Dict.values
                |> List.concatMap .errors
    in
    ( errors, Rule name (runModuleRule schema newCache) )


computeErrors : ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context -> Project -> ProjectModule -> List Error
computeErrors (ModuleRuleSchema schema) project =
    let
        initialContext : context
        initialContext =
            schema.initialContext
                |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project)
                |> accumulateContext schema.dependenciesVisitors (Review.Project.dependencyModules project)

        declarationVisitors : InAndOut (DirectedVisitor Declaration context)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression context)
        expressionVisitors =
            inAndOut schema.expressionVisitors
    in
    \module_ ->
        ( [], initialContext )
            |> accumulateWithListOfVisitors schema.moduleDefinitionVisitors module_.ast.moduleDefinition
            |> accumulateWithListOfVisitors schema.commentsVisitors module_.ast.comments
            |> accumulateList (visitImport schema.importVisitors) module_.ast.imports
            |> accumulateWithListOfVisitors schema.declarationListVisitors module_.ast.declarations
            |> accumulateList (visitDeclaration declarationVisitors expressionVisitors) module_.ast.declarations
            |> makeFinalEvaluation schema.finalEvaluationFns
            |> List.map (\(Error err) -> Error { err | ruleName = schema.name, filePath = module_.path })
            |> List.reverse


inAndOut : List (DirectedVisitor nodeType context) -> InAndOut (DirectedVisitor nodeType context)
inAndOut visitors =
    { onEnter = List.reverse visitors
    , onExit = visitors
    }


accumulateContext : List (element -> context -> context) -> element -> context -> context
accumulateContext visitors element context =
    List.foldl (\visitor -> visitor element) context visitors


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluation : List (context -> List Error) -> ( List Error, context ) -> List Error
makeFinalEvaluation finalEvaluationFns ( previousErrors, context ) =
    List.concat
        [ List.concatMap
            (\visitor -> visitor context)
            finalEvaluationFns
        , previousErrors
        ]



-- PROJECT RULES


{-| TODO
-}
type ProjectRuleSchema projectContext moduleContext
    = ProjectRuleSchema
        { name : String
        , context :
            { initProjectContext : projectContext
            , fromProjectToModule : FileKey -> Node ModuleName -> projectContext -> moduleContext
            , fromModuleToProject : FileKey -> Node ModuleName -> moduleContext -> projectContext
            , foldProjectContexts : projectContext -> projectContext -> projectContext
            }
        , moduleVisitorSchema : ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> projectContext -> projectContext)
        , dependenciesVisitors : List (Dict String Elm.Docs.Module -> projectContext -> projectContext)
        , finalEvaluationFns : List (projectContext -> List Error)
        , traversalType : TraversalType
        }


type TraversalType
    = AllModulesInParallel
    | ImportedModulesFirst


{-| TODO
-}
newProjectRuleSchema :
    String
    ->
        { moduleVisitorSchema : ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
        , initProjectContext : projectContext
        , fromProjectToModule : FileKey -> Node ModuleName -> projectContext -> moduleContext
        , fromModuleToProject : FileKey -> Node ModuleName -> moduleContext -> projectContext
        , foldProjectContexts : projectContext -> projectContext -> projectContext
        }
    -> ProjectRuleSchema projectContext moduleContext
newProjectRuleSchema name_ { moduleVisitorSchema, initProjectContext, fromProjectToModule, fromModuleToProject, foldProjectContexts } =
    ProjectRuleSchema
        { name = name_
        , context =
            { initProjectContext = initProjectContext
            , fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        , moduleVisitorSchema = moduleVisitorSchema
        , elmJsonVisitors = []
        , dependenciesVisitors = []
        , finalEvaluationFns = []
        , traversalType = AllModulesInParallel
        }


{-| TODO documentation
-}
fromProjectRuleSchema : ProjectRuleSchema projectContext moduleContext -> Rule
fromProjectRuleSchema (ProjectRuleSchema schema) =
    Rule schema.name
        (runProjectRule
            (ProjectRuleSchema
                { schema
                    | elmJsonVisitors = List.reverse schema.elmJsonVisitors
                    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
                    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
                }
            )
            Dict.empty
        )


type alias ProjectRuleCache context =
    Dict String
        { source : String
        , errors : List Error
        , context : context
        }


runProjectRule : ProjectRuleSchema projectContext moduleContext -> ProjectRuleCache projectContext -> Project -> ( List Error, Rule )
runProjectRule ((ProjectRuleSchema { traversalType }) as schema) =
    case traversalType of
        AllModulesInParallel ->
            allModulesInParallelTraversal schema

        ImportedModulesFirst ->
            importedModulesFirst schema


allModulesInParallelTraversal : ProjectRuleSchema projectContext moduleContext -> ProjectRuleCache projectContext -> Project -> ( List Error, Rule )
allModulesInParallelTraversal (ProjectRuleSchema schema) startCache project =
    let
        initialContext : projectContext
        initialContext =
            schema.context.initProjectContext
                |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project)
                |> accumulateContext schema.dependenciesVisitors (Review.Project.dependencyModules project)

        computeModule : ProjectModule -> { source : String, errors : List Error, context : projectContext }
        computeModule module_ =
            let
                fileKey : FileKey
                fileKey =
                    FileKey module_.path

                moduleNameNode_ : Node ModuleName
                moduleNameNode_ =
                    moduleNameNode module_.ast.moduleDefinition

                initialModuleContext : moduleContext
                initialModuleContext =
                    schema.context.fromProjectToModule
                        fileKey
                        moduleNameNode_
                        initialContext

                moduleVisitor : ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
                moduleVisitor =
                    emptySchema "" initialModuleContext
                        |> schema.moduleVisitorSchema
                        |> reverseVisitors

                ( fileErrors, context ) =
                    visitModuleForProjectRule
                        moduleVisitor
                        initialModuleContext
                        module_
            in
            { source = module_.source
            , errors = List.map (\(Error err) -> Error { err | filePath = module_.path }) fileErrors
            , context =
                schema.context.fromModuleToProject
                    fileKey
                    moduleNameNode_
                    context
            }

        newCache : ProjectRuleCache projectContext
        newCache =
            List.foldl
                (\module_ cache ->
                    case Dict.get module_.path startCache of
                        Nothing ->
                            Dict.insert module_.path (computeModule module_) cache

                        Just cacheEntry ->
                            if cacheEntry.source == module_.source then
                                -- File is unchanged, we will later return the cached errors and context
                                Dict.insert module_.path cacheEntry cache

                            else
                                Dict.insert module_.path (computeModule module_) cache
                )
                Dict.empty
                (Review.Project.modules project)

        contextsAndErrorsPerFile : List ( List Error, projectContext )
        contextsAndErrorsPerFile =
            newCache
                |> Dict.values
                |> List.map (\cacheEntry -> ( cacheEntry.errors, cacheEntry.context ))

        errors : List Error
        errors =
            List.concat
                [ List.concatMap Tuple.first contextsAndErrorsPerFile
                , contextsAndErrorsPerFile
                    |> List.map Tuple.second
                    |> List.foldl schema.context.foldProjectContexts initialContext
                    |> makeFinalEvaluationForProject schema.finalEvaluationFns
                    |> List.map (\(Error err) -> Error { err | ruleName = schema.name })
                ]
    in
    ( errors, Rule schema.name (runProjectRule (ProjectRuleSchema schema) newCache) )


importedModulesFirst : ProjectRuleSchema projectContext moduleContext -> ProjectRuleCache projectContext -> Project -> ( List Error, Rule )
importedModulesFirst (ProjectRuleSchema schema) startCache project =
    let
        graph : Graph ModuleName ()
        graph =
            project
                |> Review.Project.moduleGraph
    in
    case Graph.checkAcyclic graph |> Result.map Graph.topologicalSort of
        Ok nodeContexts ->
            let
                initialContext : projectContext
                initialContext =
                    schema.context.initProjectContext
                        |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project)
                        |> accumulateContext schema.dependenciesVisitors (Review.Project.dependencyModules project)

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

                projectModulePaths : Set String
                projectModulePaths =
                    project
                        |> Review.Project.modules
                        |> List.map .path
                        |> Set.fromList

                computeModule : ProjectRuleCache projectContext -> List ProjectModule -> ProjectModule -> { source : String, errors : List Error, context : projectContext }
                computeModule cache importedModules module_ =
                    let
                        fileKey : FileKey
                        fileKey =
                            FileKey module_.path

                        moduleNameNode_ : Node ModuleName
                        moduleNameNode_ =
                            moduleNameNode module_.ast.moduleDefinition

                        initialModuleContext : moduleContext
                        initialModuleContext =
                            importedModules
                                |> List.filterMap
                                    (\importedModule ->
                                        Dict.get importedModule.path cache
                                            |> Maybe.map .context
                                    )
                                -- TODO Remove contexts from parents already handled by other parents
                                |> List.foldl schema.context.foldProjectContexts initialContext
                                |> schema.context.fromProjectToModule fileKey moduleNameNode_

                        moduleVisitor : ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
                        moduleVisitor =
                            emptySchema "" initialModuleContext
                                |> schema.moduleVisitorSchema
                                |> reverseVisitors

                        ( fileErrors, context ) =
                            visitModuleForProjectRule
                                moduleVisitor
                                initialModuleContext
                                module_
                    in
                    { source = module_.source
                    , errors = List.map (\(Error err) -> Error { err | filePath = module_.path }) fileErrors
                    , context =
                        schema.context.fromModuleToProject
                            fileKey
                            moduleNameNode_
                            context
                    }

                newStartCache : ProjectRuleCache projectContext
                newStartCache =
                    startCache
                        |> Dict.filter (\path _ -> Set.member path projectModulePaths)
                        |> Dict.insert "#INITIAL_CONTEXT#" { source = "", errors = [], context = initialContext }

                newCache : ProjectRuleCache projectContext
                newCache =
                    List.foldl
                        (computeModuleAndCacheResult modules graph computeModule)
                        ( newStartCache, Set.empty )
                        nodeContexts
                        |> Tuple.first

                contextsAndErrorsPerFile : List ( List Error, projectContext )
                contextsAndErrorsPerFile =
                    -- TODO select leaf nodes and fold their contexts only
                    newCache
                        |> Dict.remove "#INITIAL_CONTEXT#"
                        |> Dict.values
                        |> List.map (\cacheEntry -> ( cacheEntry.errors, cacheEntry.context ))

                errors : List Error
                errors =
                    List.concat
                        [ List.concatMap Tuple.first contextsAndErrorsPerFile
                        , contextsAndErrorsPerFile
                            |> List.map Tuple.second
                            |> List.foldl schema.context.foldProjectContexts initialContext
                            |> makeFinalEvaluationForProject schema.finalEvaluationFns
                            |> List.map (\(Error err) -> Error { err | ruleName = schema.name })
                        ]
            in
            ( errors, Rule schema.name (runProjectRule (ProjectRuleSchema schema) newCache) )

        Err _ ->
            -- TODO return some kind of global error?
            ( [], Rule schema.name (runProjectRule (ProjectRuleSchema schema) startCache) )


computeModuleAndCacheResult :
    Dict ModuleName ProjectModule
    -> Graph ModuleName ()
    -> (ProjectRuleCache projectContext -> List ProjectModule -> ProjectModule -> { source : String, errors : List Error, context : projectContext })
    -> Graph.NodeContext ModuleName ()
    -> ( ProjectRuleCache projectContext, Set ModuleName )
    -> ( ProjectRuleCache projectContext, Set ModuleName )
computeModuleAndCacheResult modules graph computeModule { node, incoming } ( cache, invalidatedModules ) =
    case Dict.get node.label modules of
        Nothing ->
            ( cache, invalidatedModules )

        Just module_ ->
            let
                importedModules : List ProjectModule
                importedModules =
                    incoming
                        |> IntDict.keys
                        |> List.filterMap
                            (\key ->
                                Graph.get key graph
                                    |> Maybe.andThen (\nodeContext -> Dict.get nodeContext.node.label modules)
                            )

                compute previousResult =
                    let
                        result : { source : String, errors : List Error, context : projectContext }
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
                    let
                        noImportedModulesHaveANewContext : Bool
                        noImportedModulesHaveANewContext =
                            importedModules
                                |> List.map getModuleName
                                |> Set.fromList
                                |> Set.intersect invalidatedModules
                                |> Set.isEmpty
                    in
                    if cacheEntry.source == module_.source && noImportedModulesHaveANewContext then
                        -- File and its imported modules' context are unchanged, we will later return the cached errors and context
                        ( cache, invalidatedModules )

                    else
                        compute (Just cacheEntry)


visitModuleForProjectRule : ModuleRuleSchema { hasAtLeastOneVisitor : () } context -> context -> ProjectModule -> ( List Error, context )
visitModuleForProjectRule (ModuleRuleSchema schema) =
    let
        declarationVisitors : InAndOut (DirectedVisitor Declaration context)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression context)
        expressionVisitors =
            inAndOut schema.expressionVisitors
    in
    \initialContext module_ ->
        ( [], initialContext )
            |> accumulateWithListOfVisitors schema.moduleDefinitionVisitors module_.ast.moduleDefinition
            |> accumulateWithListOfVisitors schema.commentsVisitors module_.ast.comments
            |> accumulateList (visitImport schema.importVisitors) module_.ast.imports
            |> accumulateWithListOfVisitors schema.declarationListVisitors module_.ast.declarations
            |> accumulateList (visitDeclaration declarationVisitors expressionVisitors) module_.ast.declarations
            |> (\( errors, context ) -> ( makeFinalEvaluation schema.finalEvaluationFns ( errors, context ), context ))


getModuleName : ProjectModule -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Module.moduleName


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluationForProject : List (context -> List Error) -> context -> List Error
makeFinalEvaluationForProject finalEvaluationFns context =
    List.concatMap
        (\visitor -> visitor context)
        finalEvaluationFns


moduleNameNode : Node Module -> Node ModuleName
moduleNameNode node =
    case Node.value node of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName


{-| TODO documentation
-}
traversingImportedModulesFirst : ProjectRuleSchema projectContext moduleContext -> ProjectRuleSchema projectContext moduleContext
traversingImportedModulesFirst (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | traversalType = ImportedModulesFirst }


{-| TODO documentation
-}
withProjectElmJsonVisitor :
    (Maybe Elm.Project.Project -> projectContext -> projectContext)
    -> ProjectRuleSchema projectContext moduleContext
    -> ProjectRuleSchema projectContext moduleContext
withProjectElmJsonVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | elmJsonVisitors = visitor :: schema.elmJsonVisitors }


{-| TODO documentation
-}
withProjectDependenciesVisitor :
    (Dict String Elm.Docs.Module -> projectContext -> projectContext)
    -> ProjectRuleSchema projectContext moduleContext
    -> ProjectRuleSchema projectContext moduleContext
withProjectDependenciesVisitor visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | dependenciesVisitors = visitor :: schema.dependenciesVisitors }


{-| TODO documentation
-}
withFinalProjectEvaluation :
    (projectContext -> List Error)
    -> ProjectRuleSchema projectContext moduleContext
    -> ProjectRuleSchema projectContext moduleContext
withFinalProjectEvaluation visitor (ProjectRuleSchema schema) =
    ProjectRuleSchema { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s [module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`) and report patterns.

The following example forbids having `_` in any part of a module name.

    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUnderscoreInModuleName" ()
            |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> List Error
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
withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withSimpleModuleDefinitionVisitor visitor schema =
    withModuleDefinitionVisitor (\node context -> ( visitor node, context )) schema


{-| TODO documentation
-}
withSimpleCommentsVisitor : (List (Node String) -> List Error) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withSimpleCommentsVisitor visitor schema =
    withCommentsVisitor (\node context -> ( visitor node, context )) schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s [import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import) (`import Html as H exposing (div)`) in order of their definition and report patterns.

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

    importVisitor : Node Import -> List Error
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
withSimpleImportVisitor : (Node Import -> List Error) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withSimpleImportVisitor visitor schema =
    withImportVisitor (\node context -> ( visitor node, context )) schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`)
and report patterns. The declarations will be visited in the order of their definition.

The following example forbids declaring a function or a value without a type
annotation.

    import Elm.Syntax.Declaration exposing (Declaration(..))
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingTypeAnnotation" ()
            |> Rule.withSimpleDeclarationVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    declarationVisitor : Node Declaration -> List Error
    declarationVisitor node =
        case Node.value node of
            FunctionDeclaration { signature, declaration } ->
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

Note: `withSimpleDeclarationVisitor` is a simplified version of [`withDeclarationVisitor`](#withDeclarationVisitor),
which isn't passed a [`Direction`](#Direction) (it will only be called `OnEnter`ing the node) and a `context` and doesn't return a context. You can use `withSimpleDeclarationVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withSimpleDeclarationVisitor visitor schema =
    withDeclarationVisitor
        (\node direction context ->
            case direction of
                OnEnter ->
                    ( visitor node, context )

                OnExit ->
                    ( [], context )
        )
        schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`). The expressions are visited in pre-order
depth-first search, meaning that an expression will be visited, then its first
child, the first child's children (and so on), then the second child (and so on).

The following example forbids using the Debug module.

    import Elm.Syntax.Expression exposing (Expression(..))
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebug" ()
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    expressionVisitor : Node Expression -> List Error
    expressionVisitor node =
        case Node.value node of
            FunctionOrValue moduleName fnName ->
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

Note: `withSimpleExpressionVisitor` is a simplified version of [`withExpressionVisitor`](#withExpressionVisitor),
which isn't passed a [`Direction`](#Direction) (it will only be called `OnEnter`ing the node) and a `context` and doesn't return a context. You can use `withSimpleExpressionVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleExpressionVisitor : (Node Expression -> List Error) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withSimpleExpressionVisitor visitor schema =
    withExpressionVisitor
        (\node direction context ->
            case direction of
                OnEnter ->
                    ( visitor node, context )

                OnExit ->
                    ( [], context )
        )
        schema


{-| TODO Have this explanation somewhere.
Adds an initial `context` to start collecting data during your traversal.

In some cases, you can't just report a pattern when you see it, but you want to
not report or report differently depending on information located in a different
part of the file. In that case, you collect data as the nodes in the file get
traversed and store it in what we'll call a `context`. This `context` will be
available and updated by non-"simple" "with\*" functions, like
[`withExpressionVisitor`](#withExpressionVisitor) or [`withImportVisitor`](#withImportVisitor).

Once the file has been traversed and you have collected all the data available
from the file, you can report some final errors using [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

A few use examples:

  - You want to report the use of `Debug.log`: and if you see a call using a `log`
    function, you need to check whether `log` was defined in the file, or imported
    using `import Debug exposing (log)` or `import Debug exposing (..)`.
  - You wish to report unused variables, so you need to register the declared and
    imported variables, and note when they get used.
  - You noticed plenty of bad or inconsistent uses of the `Html.button` function,
    so you built a nice `Button` module. You now want to forbid all uses of
    `Html.button`, except in the `Button` module ([`See simplified example`](#withModuleDefinitionVisitor)).

The `context` you choose needs to be of the same type for all visitors. In practice,
it is similar to a `Model` for a rule.

The following example forbids calling `Rule.newModuleRuleSchema` with a name that is not
the same as the module's name (forbidding `Rule.newModuleRuleSchema "NoSomething"` when the
module name is `Review.Rule.NoSomethingElse`).

    import Elm.Syntax.Expression exposing (Expression(..))
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Direction, Error, Rule)

    type alias Context =
        -- Contains the module name's last part
        Maybe String

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDifferentNameForRuleAndModuleName" Nothing
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
    moduleDefinitionVisitor node context =
        let
            moduleLastName : Maybe String
            moduleLastName =
                node
                    |> Node.value
                    |> Module.moduleName
                    |> List.reverse
                    |> List.head
        in
        ( [], moduleLastName )

    expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
    expressionVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Application (function :: ruleNameNode :: _) ) ->
                case ( Node.value function, Node.value ruleNameNode ) of
                    ( FunctionOrValue [ "Rule" ] "newModuleRuleSchema", Literal ruleName ) ->
                        if Just ruleName /= context then
                            let
                                suggestedName : String
                                suggestedName =
                                    case context of
                                        Just name ->
                                            " (`" ++ name ++ "`)"

                                        Nothing ->
                                            ""
                            in
                            ( [ Rule.error
                                    { message = "Rule name should be the same as the module name" ++ suggestedName
                                    , details = [ "This makes it easier to find the documentation for a rule or to find the rule in the configuration." ]
                                    }
                                    (Node.range ruleNameNode)
                              ]
                            , context
                            )

                        else
                            ( [], context )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

-}
emptySchema : String -> context -> ModuleRuleSchema anything context
emptySchema name_ initialContext =
    ModuleRuleSchema
        { name = name_
        , initialContext = initialContext
        , elmJsonVisitors = []
        , dependenciesVisitors = []
        , moduleDefinitionVisitors = []
        , commentsVisitors = []
        , importVisitors = []
        , declarationListVisitors = []
        , declarationVisitors = []
        , expressionVisitors = []
        , finalEvaluationFns = []
        }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the project's
[`elm.json`](https://package.elm-lang.org/packages/elm/project-metadata-utils/latest/Elm-Project) file.
information, such as the contents of the `elm.json` file, to collect data (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

The following example forbids exposing a file in an "Internal" directory in your `elm.json` file.

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
            |> Rule.withModuleElmJsonVisitor elmJsonVisitor
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromModuleRuleSchema

    elmJsonVisitor : Maybe Elm.Project.Project -> Context -> Context
    elmJsonVisitor elmJson context =
        elmJson

    moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
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
withModuleElmJsonVisitor :
    (Maybe Elm.Project.Project -> context -> context)
    -> ModuleRuleSchema { anything | withModuleElmJsonVisitor : () } context
    -> ModuleRuleSchema { anything | withModuleElmJsonVisitor : () } context
withModuleElmJsonVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | elmJsonVisitors = visitor :: schema.elmJsonVisitors }


{-| TODO
-}
withModuleDependenciesVisitor :
    (Dict String Elm.Docs.Module -> context -> context)
    -> ModuleRuleSchema { anything | withModuleDependenciesVisitor : () } context
    -> ModuleRuleSchema { anything | withModuleDependenciesVisitor : () } context
withModuleDependenciesVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | dependenciesVisitors = visitor :: schema.dependenciesVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

The following example forbids the use of `Html.button` except in the "Button" file.
The example is simplified to only forbid the use of the `Html.button` expression.

    import Elm.Syntax.Expression exposing (Expression(..))
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
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List Error, Context )
    moduleDefinitionVisitor node context =
        if (Node.value node |> Module.moduleName) == [ "Button" ] then
            ( [], HtmlButtonIsAllowed )

        else
            ( [], HtmlButtonIsForbidden )

    expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
    expressionVisitor node direction context =
        case ( direction, context ) of
            ( Rule.OnEnter, HtmlButtonIsAllowed ) ->
                ( [], context )

            ( Rule.OnEnter, HtmlButtonIsForbidden ) ->
                case Node.value node of
                    FunctionOrValue [ "Html" ] "button" ->
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

            ( _, _ ) ->
                ( [], context )

Tip: If you do not need to collect data in this visitor, you may wish to use the
simpler [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) function.

-}
withModuleDefinitionVisitor : (Node Module -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withModuleDefinitionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


{-| TODO documentation
-}
withCommentsVisitor : (List (Node String) -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withCommentsVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | commentsVisitors = visitor :: schema.commentsVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import)
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

    error : Node Import -> Error
    error node =
        Rule.error
            { message = "Do not use both `elm-ui` and `elm-css`"
            , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
            }
            (Node.range node)

    importVisitor : Node Import -> Context -> ( List Error, Context )
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
withImportVisitor : (Node Import -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withImportVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | importVisitors = visitor :: schema.importVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

The following example forbids exposing a function or a value without it having a
type annotation.

    import Elm.Syntax.Declaration exposing (Declaration(..))
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

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List Error, ExposedFunctions )
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

    declarationVisitor : Node Declaration -> Direction -> ExposedFunctions -> ( List Error, ExposedFunctions )
    declarationVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, FunctionDeclaration { documentation, declaration } ) ->
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
withDeclarationVisitor : (Node Declaration -> Direction -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withDeclarationVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationVisitors = visitor :: schema.declarationVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns.

It is similar to [withDeclarationVisitor](#withDeclarationVisitor), but the
visitor used with this function is called before the visitor added with
[withDeclarationVisitor](#withDeclarationVisitor). You can use this visitor in
order to look ahead and add the file's types and variables into your context,
before visiting the contents of the file using [withDeclarationVisitor](#withDeclarationVisitor)
and [withExpressionVisitor](#withExpressionVisitor). Otherwise, using
[withDeclarationVisitor](#withDeclarationVisitor) is probably a simpler choice.

-}
withDeclarationListVisitor : (List (Node Declaration) -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withDeclarationListVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the `File`'s
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`), collect data in the `context` and/or report patterns.
The expressions are visited in pre-order depth-first search, meaning that an
expression will be visited, then its first child, the first child's children
(and so on), then the second child (and so on).

The following example forbids the use of `Debug.log` even when it is imported like
`import Debug exposing (log)`.

    import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose(..))
    import Elm.Syntax.Expression exposing (Expression(..))
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

    importVisitor : Node Import -> Context -> ( List Error, Context )
    importVisitor node context =
        case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
            ( [ "Debug" ], Just (Exposing.All _) ) ->
                ( [], DebugLogWasImported )

            ( [ "Debug" ], Just (Exposing.Explicit exposedFunctions) ) ->
                let
                    isLogFunction : Node Exposing.TopLevelExpose -> Bool
                    isLogFunction exposeNode =
                        case Node.value exposeNode of
                            FunctionExpose "log" ->
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

    expressionVisitor : Node Expression -> Direction -> Context -> ( List Error, Context )
    expressionVisitor node direction context =
        case context of
            DebugLogWasNotImported ->
                ( [], context )

            DebugLogWasImported ->
                case ( direction, Node.value node ) of
                    ( Rule.OnEnter, FunctionOrValue [] "log" ) ->
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
withExpressionVisitor : (Node Expression -> Direction -> context -> ( List Error, context )) -> ModuleRuleSchema anything context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withExpressionVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | expressionVisitors = visitor :: schema.expressionVisitors }


{-| Add a function that makes a final evaluation of the module based only on the
data that was collected in the `context`. This can be useful if you can't or if
it is hard to determine something as you traverse the file.

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

    importVisitor : Node Import -> Context -> ( List Error, Context )
    importVisitor node context =
        ( [], Dict.insert (Node.value node |> .moduleName |> Node.value) (Node.range node) context )

    finalEvaluation : Context -> List Error
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
withFinalModuleEvaluation : (context -> List Error) -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context -> ModuleRuleSchema { anything | hasAtLeastOneVisitor : () } context
withFinalModuleEvaluation visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }



-- ERRORS


{-| Represents an error found by a [`Rule`](#Rule). These are created by the
rules, and will be reported to the user.
-}
type Error
    = Error
        { message : String
        , ruleName : String
        , filePath : String
        , details : List String
        , range : Range
        , fixes : Maybe (List Fix)
        }


{-| Creates an [`Error`](#Error). Use it when you find a pattern that the rule should forbid.

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

[`Range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range
[`Node.range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Node#range

-}
error : { message : String, details : List String } -> Range -> Error
error { message, details } range =
    Error
        { message = message
        , ruleName = ""
        , filePath = ""
        , details = details
        , range = range
        , fixes = Nothing
        }


{-| Creates an [`Error`](#Error). Use it when you find a pattern that the rule should forbid.
It takes the [message you want to display to the user](#a-helpful-error-message-and-details), and a [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range),
which is the location where the error should be shown (under which to put the squiggly lines in an editor).
In most cases, you can get it using [`Node.range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Node#range).

The `details` is a list of strings, and each item will be visually separated
when shown to the user. The details may not be empty, and this will be enforced
by the tests automatically.

    error : Node a -> Error
    error node =
        Rule.error
            { message = "Remove the use of `Debug` before shipping to production"
            , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
            }
            (Node.range node)

-}
errorForFile : FileKey -> { message : String, details : List String } -> Range -> Error
errorForFile (FileKey path) { message, details } range =
    Error
        { message = message
        , ruleName = ""
        , details = details
        , range = range
        , filePath = path
        , fixes = Nothing
        }


{-| A key to be able to report an error for a specific module. You need such a
key in order to use the [`errorForFile`](#errorForFile) function. This is to
prevent creating errors for modules you have not visited, or files that do not exist.

You can get a `FileKey` from the `fromProjectToModule` and `fromModuleToProject`
functions that you define when using [`newProjectRuleSchema`](#newProjectRuleSchema).

-}
type FileKey
    = FileKey String


parsingError : { path : String, source : String } -> Error
parsingError rawFile =
    Error
        { filePath = rawFile.path
        , ruleName = "ParsingError"
        , message = rawFile.path ++ " is not a correct Elm file"
        , details =
            [ "I could not understand the content of this file, and this prevents me from analyzing it. It is highly likely that the content of the file is not correct Elm code."
            , "Hint: Try running `elm make`. The compiler should give you better hints on how to resolve the problem."
            ]
        , range = { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
        , fixes = Nothing
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

**Note**: Each fix applies on a location in the code, defined by a range. To avoid an
unpredictable result, those ranges may not overlap. The order of the fixes does
not matter.

-}
withFixes : List Fix -> Error -> Error
withFixes fixes (Error err) =
    if List.isEmpty fixes then
        Error { err | fixes = Nothing }

    else
        Error { err | fixes = Just fixes }


{-| Get the name of the rule that triggered this [`Error`](#Error).
-}
errorRuleName : Error -> String
errorRuleName (Error err) =
    err.ruleName


{-| Get the error message of an [`Error`](#Error).
-}
errorMessage : Error -> String
errorMessage (Error err) =
    err.message


{-| Get the error details of an [`Error`](#Error).
-}
errorDetails : Error -> List String
errorDetails (Error err) =
    err.details


{-| Get the [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range)
of an [`Error`](#Error).
-}
errorRange : Error -> Range
errorRange (Error err) =
    err.range


{-| Get the [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range)
of an [`Error`](#Error).
-}
errorFixes : Error -> Maybe (List Fix)
errorFixes (Error err) =
    err.fixes


{-| Get the file path of an [`Error`](#Error).
-}
errorFilePath : Error -> String
errorFilePath (Error err) =
    err.filePath



-- TREE TRAVERSAL


visitImport :
    List (Node Import -> context -> ( List Error, context ))
    -> Node Import
    -> context
    -> ( List Error, context )
visitImport importVisitors node context =
    visitNodeWithListOfVisitors importVisitors node ( [], context )


visitDeclaration :
    InAndOut (DirectedVisitor Declaration context)
    -> InAndOut (DirectedVisitor Expression context)
    -> Node Declaration
    -> context
    -> ( List Error, context )
visitDeclaration declarationVisitors expressionVisitors node context =
    let
        accumulateExpressionNodes : ( List Error, context ) -> ( List Error, context )
        accumulateExpressionNodes =
            if List.isEmpty expressionVisitors.onEnter then
                identity

            else
                accumulateList
                    (visitExpression expressionVisitors)
                    (expressionsInDeclaration node)
    in
    ( [], context )
        |> visitNodeWithListOfVisitorsAndDirection OnEnter declarationVisitors.onEnter node
        |> accumulateExpressionNodes
        |> visitNodeWithListOfVisitorsAndDirection OnExit declarationVisitors.onExit node


visitNodeWithListOfVisitors :
    List (Node a -> context -> ( List Error, context ))
    -> Node a
    -> ( List Error, context )
    -> ( List Error, context )
visitNodeWithListOfVisitors visitors node initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor node))
        initialErrorsAndContext
        visitors


visitNodeWithListOfVisitorsAndDirection :
    Direction
    -> List (Node a -> Direction -> context -> ( List Error, context ))
    -> Node a
    -> ( List Error, context )
    -> ( List Error, context )
visitNodeWithListOfVisitorsAndDirection direction visitors node initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor node direction))
        initialErrorsAndContext
        visitors


accumulateWithListOfVisitors :
    List (a -> context -> ( List Error, context ))
    -> a
    -> ( List Error, context )
    -> ( List Error, context )
accumulateWithListOfVisitors visitors element initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor element))
        initialErrorsAndContext
        visitors


expressionsInDeclaration : Node Declaration -> List (Node Expression)
expressionsInDeclaration node =
    case Node.value node of
        FunctionDeclaration function ->
            [ functionToExpression function ]

        CustomTypeDeclaration _ ->
            []

        AliasDeclaration { typeAnnotation } ->
            []

        Destructuring pattern expr ->
            [ expr ]

        PortDeclaration _ ->
            []

        InfixDeclaration _ ->
            []


visitExpression :
    InAndOut (DirectedVisitor Expression context)
    -> Node Expression
    -> context
    -> ( List Error, context )
visitExpression visitors node context =
    ( [], context )
        |> visitNodeWithListOfVisitorsAndDirection OnEnter visitors.onEnter node
        |> accumulateList (visitExpression visitors) (expressionChildren node)
        |> visitNodeWithListOfVisitorsAndDirection OnExit visitors.onExit node


expressionChildren : Node Expression -> List (Node Expression)
expressionChildren node =
    case Node.value node of
        Application expressions ->
            expressions

        Literal _ ->
            []

        Integer _ ->
            []

        Floatable _ ->
            []

        UnitExpr ->
            []

        ListExpr elements ->
            elements

        FunctionOrValue _ _ ->
            []

        RecordExpr fields ->
            List.map (Node.value >> (\( _, expr ) -> expr)) fields

        RecordUpdateExpression _ setters ->
            List.map (Node.value >> (\( field, expr ) -> expr)) setters

        ParenthesizedExpression expr ->
            [ expr ]

        Operator _ ->
            []

        OperatorApplication operator direction left right ->
            case direction of
                Left ->
                    [ left, right ]

                Right ->
                    [ right, left ]

                Non ->
                    [ left, right ]

        IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        LetExpression { expression, declarations } ->
            List.map
                (\declaration ->
                    case Node.value declaration of
                        LetFunction function ->
                            functionToExpression function

                        LetDestructuring pattern expr ->
                            expr
                )
                declarations
                ++ [ expression ]

        CaseExpression { expression, cases } ->
            expression
                :: List.map (\( pattern, caseExpression ) -> caseExpression) cases

        LambdaExpression { args, expression } ->
            [ expression ]

        TupledExpression expressions ->
            expressions

        PrefixOperator _ ->
            []

        Hex _ ->
            []

        Negation expr ->
            [ expr ]

        CharLiteral _ ->
            []

        RecordAccess expr property ->
            [ expr ]

        RecordAccessFunction _ ->
            []

        GLSLExpression _ ->
            []


functionToExpression : Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression


accumulateList : (Node a -> context -> ( List Error, context )) -> List (Node a) -> ( List Error, context ) -> ( List Error, context )
accumulateList visitor nodes ( previousErrors, previousContext ) =
    List.foldl
        (\node -> accumulate (visitor node))
        ( previousErrors, previousContext )
        nodes


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulate : (context -> ( List Error, context )) -> ( List Error, context ) -> ( List Error, context )
accumulate visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( newErrors ++ previousErrors, newContext )
