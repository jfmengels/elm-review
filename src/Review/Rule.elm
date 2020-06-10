module Review.Rule exposing
    ( Rule
    , ModuleRuleSchema, newModuleRuleSchema, fromModuleRuleSchema
    , withSimpleModuleDefinitionVisitor, withSimpleCommentsVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , withModuleDefinitionVisitor, withCommentsVisitor, withImportVisitor, Direction(..), withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalModuleEvaluation
    , withElmJsonModuleVisitor, withReadmeModuleVisitor, withDependenciesModuleVisitor
    , ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules
    , Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, ReadmeKey, errorForReadme, errorForReadmeWithFix
    , ReviewError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath, errorTarget
    , ignoreErrorsForDirectories, ignoreErrorsForFiles
    , review
    , Required, Forbidden
    )

{-| This module contains functions that are used for writing rules.


# How does it work?

`elm-review` reads the modules, `elm.json`, dependencies and `README.md` from your project,
and turns each module into an [Abstract Syntax Tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree),
a tree-like structure which represents your source code, using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/).

`elm-review` then feeds all this data into `review rules`, that traverse them to report problems.
The way that review rules go through the data depends on whether it is a [module rule](#creating-a-module-rule) or a [project rule](#creating-a-project-rule).

`elm-review` relies on the [`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/),
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


## Look at the documentation for [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/)

`elm-review` is heavily dependent on the types that [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/)
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
      - Each declaration, visited by [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) and [`withDeclarationVisitor`](#withDeclarationVisitor).
        Before evaluating the next declaration, the expression contained in the declaration
        will be visited recursively by [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) and [`withExpressionVisitor`](#withExpressionVisitor)
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

@docs withModuleDefinitionVisitor, withCommentsVisitor, withImportVisitor, Direction, withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalModuleEvaluation


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

@docs ProjectRuleSchema, newProjectRuleSchema, fromProjectRuleSchema, withModuleVisitor, withModuleContext, withElmJsonProjectVisitor, withReadmeProjectVisitor, withDependenciesProjectVisitor, withFinalProjectEvaluation, withContextFromImportedModules


## Errors

@docs Error, error, errorWithFix, ModuleKey, errorForModule, errorForModuleWithFix, ElmJsonKey, errorForElmJson, ReadmeKey, errorForReadme, errorForReadmeWithFix
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

@docs review


# Internals

@docs Required, Forbidden

-}

import Dict exposing (Dict)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range exposing (Range)
import Review.Error exposing (InternalError)
import Review.Exceptions as Exceptions exposing (Exceptions)
import Review.Fix exposing (Fix)
import Review.Project exposing (Project, ProjectModule)
import Review.Project.Dependency
import Review.Project.Internal
import Set exposing (Set)
import Vendor.Graph as Graph exposing (Graph)
import Vendor.IntDict as IntDict


{-| Represents a construct able to analyze a project and report
unwanted patterns.

You can create [module rules](#creating-a-module-rule) or [project rules](#creating-a-project-rule).

-}
type Rule
    = Rule String Exceptions (Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule ))


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
        , initialContext : moduleContext
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> moduleContext -> moduleContext)
        , readmeVisitors : List (Maybe String -> moduleContext -> moduleContext)
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> moduleContext -> moduleContext)
        , moduleDefinitionVisitors : List (Node Module -> moduleContext -> ( List (Error {}), moduleContext ))
        , commentsVisitors : List (List (Node String) -> moduleContext -> ( List (Error {}), moduleContext ))
        , importVisitors : List (Node Import -> moduleContext -> ( List (Error {}), moduleContext ))
        , declarationListVisitors : List (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext ))
        , declarationVisitors : List (DirectedVisitor Declaration moduleContext)
        , expressionVisitors : List (DirectedVisitor Expression moduleContext)
        , finalEvaluationFns : List (moduleContext -> List (Error {}))
        }


type alias DirectedVisitor nodeType context =
    Node nodeType -> Direction -> context -> ( List (Error {}), context )


type alias InAndOut visitor =
    { onEnter : List visitor
    , onExit : List visitor
    }



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
                    in
                    ( [ Review.Error.ReviewError
                            { filePath = "GLOBAL ERROR"
                            , ruleName = "Incorrect project"
                            , message = "Found several modules named `" ++ String.join "." duplicate.moduleName ++ "`"
                            , details =
                                [ "I found several modules with the name `" ++ String.join "." duplicate.moduleName ++ "`. Depending on how I choose to resolve this, I might give you different reports. Since this is a compiler error anyway, I require this problem to be solved. Please fix this then try running `elm-review` again."
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
        (\(Rule _ exceptions fn) ( errors, previousRules ) ->
            let
                ( ruleErrors, ruleWithCache ) =
                    fn exceptions project nodeContexts
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


{-| Represents whether a node is being traversed before having seen its children (`OnEnter`ing the node), or after (`OnExit`ing the node).

When visiting the AST, declaration and expression nodes are visited twice: once
with `OnEnter`, before the children of the node are visited, and once with
`OnExit`, after the children of the node have been visited.
In most cases, you'll only want to handle the `OnEnter` case, but there are cases
where you'll want to visit a [`Node`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Node#Node)
after having seen its children.

For instance, if you are trying to detect the unused variables defined inside
of a `let in` expression, you will want to collect the declaration of variables,
note which ones are used, and at the end of the block report the ones that weren't used.

    expressionVisitor : Context -> Direction -> Node Expression -> ( List (Error {}), Context )
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
their name, like [`withExpressionVisitor`](#withExpressionVisitor),
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
    -> moduleContext
    -> ModuleRuleSchema { canCollectProjectData : () } moduleContext
newModuleRuleSchema name_ moduleContext =
    emptySchema name_ moduleContext


{-| Create a [`Rule`](#Rule) from a configured [`ModuleRuleSchema`](#ModuleRuleSchema).
-}
fromModuleRuleSchema : ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> Rule
fromModuleRuleSchema ((ModuleRuleSchema { name }) as schema) =
    runModuleRule
        (reverseVisitors schema)
        newModuleRuleCache
        |> Rule name Exceptions.init


reverseVisitors : ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema schemaState moduleContext
reverseVisitors (ModuleRuleSchema schema) =
    ModuleRuleSchema
        { schema
            | elmJsonVisitors = List.reverse schema.elmJsonVisitors
            , readmeVisitors = List.reverse schema.readmeVisitors
            , dependenciesVisitors = List.reverse schema.dependenciesVisitors
            , moduleDefinitionVisitors = List.reverse schema.moduleDefinitionVisitors
            , commentsVisitors = List.reverse schema.commentsVisitors
            , importVisitors = List.reverse schema.importVisitors
            , declarationListVisitors = List.reverse schema.declarationListVisitors
            , finalEvaluationFns = List.reverse schema.finalEvaluationFns
        }


type alias ModuleRuleCache moduleContext =
    { initialContext : Maybe moduleContext
    , moduleResults : ModuleRuleResultCache
    }


type alias ModuleRuleResultCache =
    Dict String
        { source : String
        , errors : List (Error {})
        }


newModuleRuleCache : ModuleRuleCache moduleContext
newModuleRuleCache =
    { initialContext = Nothing
    , moduleResults = Dict.empty
    }


runModuleRule : ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> ModuleRuleCache moduleContext -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
runModuleRule ((ModuleRuleSchema schema) as moduleRuleSchema) previousCache exceptions project _ =
    let
        initialContext : moduleContext
        initialContext =
            schema.initialContext
                |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project |> Maybe.map .project)
                |> accumulateContext schema.readmeVisitors (Review.Project.readme project |> Maybe.map .content)
                |> accumulateContext schema.dependenciesVisitors (Review.Project.dependencies project)

        startCache : ModuleRuleCache moduleContext
        startCache =
            case previousCache.initialContext of
                Just previousInitialContext ->
                    if previousInitialContext == initialContext then
                        previousCache

                    else
                        { initialContext = Just initialContext
                        , moduleResults = newModuleRuleCache.moduleResults
                        }

                Nothing ->
                    { initialContext = Just initialContext
                    , moduleResults = newModuleRuleCache.moduleResults
                    }

        computeErrors_ : ProjectModule -> List (Error {})
        computeErrors_ =
            computeErrors moduleRuleSchema initialContext

        modulesToAnalyze : List ProjectModule
        modulesToAnalyze =
            project
                |> Review.Project.modules
                |> Exceptions.apply exceptions .path

        moduleResults : ModuleRuleResultCache
        moduleResults =
            List.foldl
                (\module_ cache ->
                    case Dict.get module_.path cache of
                        Nothing ->
                            Dict.insert module_.path { source = module_.source, errors = computeErrors_ module_ } cache

                        Just cacheEntry ->
                            if cacheEntry.source == module_.source then
                                -- Module is unchanged, we will later return the cached errors
                                cache

                            else
                                Dict.insert module_.path { source = module_.source, errors = computeErrors_ module_ } cache
                )
                startCache.moduleResults
                modulesToAnalyze

        errors : List (Error {})
        errors =
            moduleResults
                |> Dict.values
                |> List.concatMap .errors
    in
    ( errors
    , runModuleRule
        moduleRuleSchema
        { initialContext = startCache.initialContext, moduleResults = moduleResults }
        |> Rule schema.name exceptions
    )


computeErrors : ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext -> moduleContext -> ProjectModule -> List (Error {})
computeErrors (ModuleRuleSchema schema) initialContext =
    let
        declarationVisitors : InAndOut (DirectedVisitor Declaration moduleContext)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression moduleContext)
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
            |> List.map (setRuleName schema.name >> setFilePathIfUnset module_)
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
makeFinalEvaluation : List (context -> List (Error {})) -> ( List (Error {}), context ) -> List (Error {})
makeFinalEvaluation finalEvaluationFns ( previousErrors, context ) =
    List.concat
        [ List.concatMap
            (\visitor -> visitor context)
            finalEvaluationFns
        , previousErrors
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
        , moduleVisitor : ModuleVisitor projectContext moduleContext
        , elmJsonVisitors : List (Maybe { elmJsonKey : ElmJsonKey, project : Elm.Project.Project } -> projectContext -> ( List (Error {}), projectContext ))
        , readmeVisitors : List (Maybe { readmeKey : ReadmeKey, content : String } -> projectContext -> ( List (Error {}), projectContext ))
        , dependenciesVisitors : List (Dict String Review.Project.Dependency.Dependency -> projectContext -> ( List (Error {}), projectContext ))
        , finalEvaluationFns : List (projectContext -> List (Error {}))
        , traversalType : TraversalType
        }


type ModuleVisitor projectContext moduleContext
    = NoModuleVisitor
    | HasVisitors (List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext))
    | IsPrepared
        { visitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        , moduleContext : ModuleContextFunctions projectContext moduleContext
        }


type alias ModuleContextFunctions projectContext moduleContext =
    { fromProjectToModule : ModuleKey -> Node ModuleName -> projectContext -> moduleContext
    , fromModuleToProject : ModuleKey -> Node ModuleName -> moduleContext -> projectContext
    , foldProjectContexts : projectContext -> projectContext -> projectContext
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
newProjectRuleSchema name_ initialProjectContext =
    ProjectRuleSchema
        { name = name_
        , initialProjectContext = initialProjectContext
        , moduleVisitor = NoModuleVisitor
        , elmJsonVisitors = []
        , readmeVisitors = []
        , dependenciesVisitors = []
        , finalEvaluationFns = []
        , traversalType = AllModulesInParallel
        }


{-| Create a [`Rule`](#Rule) from a configured [`ProjectRuleSchema`](#ProjectRuleSchema).
-}
fromProjectRuleSchema : ProjectRuleSchema { schemaState | withModuleContext : Forbidden, hasAtLeastOneVisitor : () } projectContext moduleContext -> Rule
fromProjectRuleSchema (ProjectRuleSchema schema) =
    Rule schema.name
        Exceptions.init
        (runProjectRule
            (ProjectRuleSchema
                { schema
                    | elmJsonVisitors = List.reverse schema.elmJsonVisitors
                    , readmeVisitors = List.reverse schema.readmeVisitors
                    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
                    , finalEvaluationFns = List.reverse schema.finalEvaluationFns
                }
            )
            { elmJson = Nothing
            , initialContext = Nothing
            , moduleContexts = Dict.empty
            , finalEvaluationErrors = []
            }
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
    -> ProjectRuleSchema { projectSchemaState | canAddModuleVisitor : (), withModuleContext : Required } projectContext moduleContext
withModuleVisitor visitor (ProjectRuleSchema schema) =
    let
        previousModuleVisitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        previousModuleVisitors =
            case schema.moduleVisitor of
                NoModuleVisitor ->
                    []

                HasVisitors list ->
                    list

                IsPrepared _ ->
                    []
    in
    ProjectRuleSchema
        { schema
            | moduleVisitor =
                HasVisitors (removeExtensibleRecordTypeVariable visitor :: previousModuleVisitors)
        }


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
it is in [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-ModuleName),
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
define a dummy value in the `fromModuleToProject` function). if it helps, imagine
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
            |> Rule.withExpressionVisitor expressionVisitor

    type alias ProjectContext =
        { -- Modules exposed by the package, that we should not report
          exposedModules : Set ModuleName
        , exposedFunctions :
            -- An entry for each module
            Dict ModuleName
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
[`Node`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Node#Node
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
withModuleContext moduleContext (ProjectRuleSchema schema) =
    let
        visitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        visitors =
            case schema.moduleVisitor of
                NoModuleVisitor ->
                    []

                HasVisitors list ->
                    List.reverse list

                IsPrepared _ ->
                    []
    in
    ProjectRuleSchema { schema | moduleVisitor = IsPrepared { visitors = visitors, moduleContext = moduleContext } }


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


type alias ProjectRuleCache projectContext =
    { elmJson : Maybe (CacheEntryFor (Maybe { path : String, raw : String, project : Elm.Project.Project }) projectContext)
    , initialContext : Maybe projectContext
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


runProjectRule : ProjectRuleSchema schemaState projectContext moduleContext -> ProjectRuleCache projectContext -> Exceptions -> Project -> List (Graph.NodeContext ModuleName ()) -> ( List (Error {}), Rule )
runProjectRule ((ProjectRuleSchema schema) as wrappedSchema) startCache exceptions project nodeContexts =
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
                computeElmJson () =
                    let
                        ( errorsForVisitor, contextForVisitor ) =
                            ( [], schema.initialProjectContext )
                                |> accumulateWithListOfVisitors schema.elmJsonVisitors elmJsonData
                    in
                    { value = projectElmJson
                    , errors = errorsForVisitor
                    , context = contextForVisitor
                    }
            in
            case startCache.elmJson of
                Just cacheElmJson ->
                    if cacheElmJson.value == projectElmJson then
                        cacheElmJson

                    else
                        computeElmJson ()

                Nothing ->
                    computeElmJson ()

        ( projectRelatedErrors, initialContext ) =
            ( elmJsonCacheEntry.errors, elmJsonCacheEntry.context )
                |> accumulateWithListOfVisitors schema.readmeVisitors readmeData
                |> accumulateWithListOfVisitors schema.dependenciesVisitors (Review.Project.dependencies project)

        moduleVisitors :
            Maybe
                { visitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
                , moduleContext : ModuleContextFunctions projectContext moduleContext
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
                    computeModules wrappedSchema visitors project initialContext nodeContexts startCache.moduleContexts

                Nothing ->
                    startCache.moduleContexts

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
                    startCache.moduleContexts
                        |> Dict.values
                        |> List.map .context

                allModulesContext : List projectContext
                allModulesContext =
                    List.map Tuple.second contextsAndErrorsPerModule
            in
            if Just initialContext == startCache.initialContext && allModulesContext == previousAllModulesContext then
                startCache.finalEvaluationErrors

            else
                errorsFromFinalEvaluationForProject wrappedSchema initialContext allModulesContext

        errors : List (Error {})
        errors =
            [ projectRelatedErrors
            , List.concatMap Tuple.first contextsAndErrorsPerModule
            , errorsFromFinalEvaluation
            ]
                |> List.concat
                |> Exceptions.apply exceptions (accessInternalError >> .filePath)
                |> List.map (setRuleName schema.name)

        newCache : ProjectRuleCache projectContext
        newCache =
            { elmJson = Just elmJsonCacheEntry
            , initialContext = Just initialContext
            , moduleContexts = newCachedModuleContexts
            , finalEvaluationErrors = errorsFromFinalEvaluation
            }
    in
    ( errors, Rule schema.name exceptions (runProjectRule wrappedSchema newCache) )


computeModules :
    ProjectRuleSchema schemaState projectContext moduleContext
    ->
        { visitors : List (ModuleRuleSchema {} moduleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext)
        , moduleContext : ModuleContextFunctions projectContext moduleContext
        }
    -> Project
    -> projectContext
    -> List (Graph.NodeContext ModuleName ())
    -> Dict String (CacheEntry projectContext)
    -> Dict String (CacheEntry projectContext)
computeModules (ProjectRuleSchema schema) visitors project initialContext nodeContexts startCache =
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

        dummyInitialContext : moduleContext
        dummyInitialContext =
            visitors.moduleContext.fromProjectToModule
                (ModuleKey "dummy")
                (Node.Node Range.emptyRange [ "Dummy" ])
                initialContext

        moduleVisitor : ModuleRuleSchema { hasAtLeastOneVisitor : () } moduleContext
        moduleVisitor =
            List.foldl
                (\addVisitors (ModuleRuleSchema moduleVisitorSchema) ->
                    addVisitors (ModuleRuleSchema moduleVisitorSchema)
                )
                (emptySchema "" dummyInitialContext)
                visitors.visitors
                |> reverseVisitors

        computeModule : Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext
        computeModule cache importedModules module_ =
            let
                moduleKey : ModuleKey
                moduleKey =
                    ModuleKey module_.path

                moduleNameNode_ : Node ModuleName
                moduleNameNode_ =
                    moduleNameNode module_.ast.moduleDefinition

                initialModuleContext : moduleContext
                initialModuleContext =
                    case schema.traversalType of
                        AllModulesInParallel ->
                            visitors.moduleContext.fromProjectToModule
                                moduleKey
                                moduleNameNode_
                                initialContext

                        ImportedModulesFirst ->
                            importedModules
                                |> List.filterMap
                                    (\importedModule ->
                                        Dict.get importedModule.path cache
                                            |> Maybe.map .context
                                    )
                                |> List.foldl visitors.moduleContext.foldProjectContexts initialContext
                                |> visitors.moduleContext.fromProjectToModule moduleKey moduleNameNode_

                ( moduleErrors, context ) =
                    visitModuleForProjectRule
                        moduleVisitor
                        initialModuleContext
                        module_
            in
            { source = module_.source
            , errors = List.map (setFilePathIfUnset module_) moduleErrors
            , context =
                visitors.moduleContext.fromModuleToProject
                    moduleKey
                    moduleNameNode_
                    context
            }
    in
    List.foldl
        (computeModuleAndCacheResult schema.traversalType modules graph computeModule)
        ( newStartCache, Set.empty )
        nodeContexts
        |> Tuple.first


setRuleName : String -> Error scope -> Error scope
setRuleName ruleName error_ =
    mapInternalError (\err -> { err | ruleName = ruleName }) error_


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


computeModuleAndCacheResult :
    TraversalType
    -> Dict ModuleName ProjectModule
    -> Graph ModuleName ()
    -> (Dict String (CacheEntry projectContext) -> List ProjectModule -> ProjectModule -> CacheEntry projectContext)
    -> Graph.NodeContext ModuleName ()
    -> ( Dict String (CacheEntry projectContext), Set ModuleName )
    -> ( Dict String (CacheEntry projectContext), Set ModuleName )
computeModuleAndCacheResult traversalType modules graph computeModule { node, incoming } ( cache, invalidatedModules ) =
    case Dict.get node.label modules of
        Nothing ->
            ( cache, invalidatedModules )

        Just module_ ->
            let
                importedModules : List ProjectModule
                importedModules =
                    case traversalType of
                        AllModulesInParallel ->
                            []

                        ImportedModulesFirst ->
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
                    if cacheEntry.source == module_.source && (traversalType == AllModulesInParallel || noImportedModulesHaveANewContext importedModules invalidatedModules) then
                        -- The module's source and the module's imported modules' context are unchanged, we will later return the cached errors and context
                        ( cache, invalidatedModules )

                    else
                        compute (Just cacheEntry)


noImportedModulesHaveANewContext : List ProjectModule -> Set ModuleName -> Bool
noImportedModulesHaveANewContext importedModules invalidatedModules =
    importedModules
        |> List.map getModuleName
        |> Set.fromList
        |> Set.intersect invalidatedModules
        |> Set.isEmpty


visitModuleForProjectRule : ModuleRuleSchema a moduleContext -> moduleContext -> ProjectModule -> ( List (Error {}), moduleContext )
visitModuleForProjectRule (ModuleRuleSchema schema) =
    let
        declarationVisitors : InAndOut (DirectedVisitor Declaration moduleContext)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression moduleContext)
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
            |> (\( errors, moduleContext ) -> ( makeFinalEvaluation schema.finalEvaluationFns ( errors, moduleContext ), moduleContext ))


getModuleName : ProjectModule -> ModuleName
getModuleName module_ =
    module_.ast.moduleDefinition
        |> Node.value
        |> Module.moduleName


errorsFromFinalEvaluationForProject : ProjectRuleSchema schemaState projectContext moduleContext -> projectContext -> List projectContext -> List (Error {})
errorsFromFinalEvaluationForProject (ProjectRuleSchema schema) initialContext contextsPerModule =
    if List.isEmpty schema.finalEvaluationFns then
        []

    else
        let
            finalContext : projectContext
            finalContext =
                case schema.moduleVisitor of
                    NoModuleVisitor ->
                        initialContext

                    HasVisitors _ ->
                        initialContext

                    IsPrepared { moduleContext } ->
                        List.foldl moduleContext.foldProjectContexts initialContext contextsPerModule
        in
        makeFinalEvaluationForProject schema.finalEvaluationFns finalContext


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluationForProject : List (projectContext -> List (Error {})) -> projectContext -> List (Error {})
makeFinalEvaluationForProject finalEvaluationFns projectContext =
    List.concatMap
        (\visitor -> visitor projectContext)
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


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's [module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`) and report patterns.

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


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's [import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Import) (`import Html as H exposing (div)`) in order of their definition and report patterns.

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
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Declaration)
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

Note: `withSimpleDeclarationVisitor` is a simplified version of [`withDeclarationVisitor`](#withDeclarationVisitor),
which isn't passed a [`Direction`](#Direction) (it will only be called `OnEnter`ing the node) and a `context` and doesn't return a context. You can use `withSimpleDeclarationVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleDeclarationVisitor : (Node Declaration -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleDeclarationVisitor visitor schema =
    withDeclarationVisitor
        (\node direction moduleContext ->
            case direction of
                OnEnter ->
                    ( visitor node, moduleContext )

                OnExit ->
                    ( [], moduleContext )
        )
        schema


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Expression)
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

Note: `withSimpleExpressionVisitor` is a simplified version of [`withExpressionVisitor`](#withExpressionVisitor),
which isn't passed a [`Direction`](#Direction) (it will only be called `OnEnter`ing the node) and a `context` and doesn't return a context. You can use `withSimpleExpressionVisitor` even if you use "non-simple with\*" functions.

-}
withSimpleExpressionVisitor : (Node Expression -> List (Error {})) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withSimpleExpressionVisitor visitor schema =
    withExpressionVisitor
        (\node direction moduleContext ->
            case direction of
                OnEnter ->
                    ( visitor node, moduleContext )

                OnExit ->
                    ( [], moduleContext )
        )
        schema


{-| TODO Have this explanation somewhere.
Adds an initial `context` to start collecting data during your traversal.

In some cases, you can't just report a pattern when you see it, but you want to
not report or report differently depending on information located in a different
part of the module. In that case, you collect data as the nodes in the module get
traversed and store it in what we'll call a `context`. This `context` will be
available and updated by non-"simple" "with\*" functions, like
[`withExpressionVisitor`](#withExpressionVisitor) or [`withImportVisitor`](#withImportVisitor).

Once the module has been traversed and you have collected all the data available
from the module, you can report some final errors using [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).

A few use examples:

  - You want to report the use of `Debug.log`: and if you see a call using a `log`
    function, you need to check whether `log` was defined in the module, or imported
    using `import Debug exposing (log)` or `import Debug exposing (..)`.
  - You wish to report unused variables, so you need to register the declared and
    imported variables, and note when they get used.
  - You noticed plenty of bad or inconsistent uses of the `Html.button` function,
    so you built a nice `Button` module. You now want to forbid all uses of
    `Html.button`, except in the `Button` module ([`See simplified example`](#withModuleDefinitionVisitor)).

The `context` you choose needs to be of the same type for all visitors. In practice,
it is similar to a `Model` for a rule.

The following example forbids calling `Rule.newModuleRuleSchema` with a name that is not
the same as the module's name (forbidding `Rule.newModuleRuleSchema "OtherRuleName"` when the
module name is `RuleName`).

    import Elm.Syntax.Expression as Expression exposing (Expression)
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

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
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

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
    expressionVisitor node direction context =
        case ( direction, Node.value node ) of
            ( Rule.OnEnter, Expression.Application (function :: ruleNameNode :: _) ) ->
                case ( Node.value function, Node.value ruleNameNode ) of
                    ( Expression.FunctionOrValue [ "Rule" ] "newModuleRuleSchema", Expression.Literal ruleName ) ->
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
emptySchema : String -> moduleContext -> ModuleRuleSchema schemaState moduleContext
emptySchema name_ initialContext =
    ModuleRuleSchema
        { name = name_
        , initialContext = initialContext
        , elmJsonVisitors = []
        , readmeVisitors = []
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
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

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
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
    moduleDefinitionVisitor node context =
        if (Node.value node |> Module.moduleName) == [ "Button" ] then
            ( [], HtmlButtonIsAllowed )

        else
            ( [], HtmlButtonIsForbidden )

    expressionVisitor : Node Expression -> Direction -> Context -> ( List (Error {}), Context )
    expressionVisitor node direction context =
        case ( direction, context ) of
            ( Rule.OnEnter, HtmlButtonIsAllowed ) ->
                ( [], context )

            ( Rule.OnEnter, HtmlButtonIsForbidden ) ->
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

            ( _, _ ) ->
                ( [], context )

Tip: If you do not need to collect data in this visitor, you may wish to use the
simpler [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) function.

Tip: The rule above is very brittle. What if `button` was imported using `import Html exposing (button)` or `import Html exposing (..)`, or if `Html` was aliased (`import Html as H`)? Then the rule above would
not catch and report the use `Html.button`. I highly recommend checking out [`elm-review-scope`](https://github.com/jfmengels/elm-review-scope) to handle all these cases quite simply.

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
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Import)
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


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Declaration)
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
    ModuleRuleSchema { schema | declarationVisitors = visitor :: schema.declarationVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns.

It is similar to [withDeclarationVisitor](#withDeclarationVisitor), but the
visitor used with this function is called before the visitor added with
[withDeclarationVisitor](#withDeclarationVisitor). You can use this visitor in
order to look ahead and add the module's types and variables into your context,
before visiting the contents of the module using [withDeclarationVisitor](#withDeclarationVisitor)
and [withExpressionVisitor](#withExpressionVisitor). Otherwise, using
[withDeclarationVisitor](#withDeclarationVisitor) is probably a simpler choice.

-}
withDeclarationListVisitor : (List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )) -> ModuleRuleSchema schemaState moduleContext -> ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } moduleContext
withDeclarationListVisitor visitor (ModuleRuleSchema schema) =
    ModuleRuleSchema { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Expression)
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
    ModuleRuleSchema { schema | expressionVisitors = visitor :: schema.expressionVisitors }


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

[`Range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range
[`Node.range`]: https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Node#range

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
        }


{-| Create an [`Error`](#Error) for the `elm.json` file.

You will need an [`ElmJsonKey`](#ElmJsonKey), which you can get from the [`withElmJsonProjectVisitor`](#withElmJsonProjectVisitor)
function.

The second argument is a function that takes the `elm.json` content as a raw string,
and returns the error details. Using the raw string, you should try and find the
most fitting [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range)
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
        , range = { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
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
        )
        error_


errorToReviewError : Error scope -> ReviewError
errorToReviewError error_ =
    Review.Error.ReviewError (accessInternalError error_)


{-| Get the name of the rule that triggered this [`Error`](#Error).
-}
errorRuleName : Review.Error.ReviewError -> String
errorRuleName (Review.Error.ReviewError err) =
    err.ruleName


{-| Get the error message of an [`Error`](#Error).
-}
errorMessage : Review.Error.ReviewError -> String
errorMessage (Review.Error.ReviewError err) =
    err.message


{-| Get the error details of an [`Error`](#Error).
-}
errorDetails : Review.Error.ReviewError -> List String
errorDetails (Review.Error.ReviewError err) =
    err.details


{-| Get the [`Range`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.1.0/Elm-Syntax-Range)
of an [`Error`](#Error).
-}
errorRange : Review.Error.ReviewError -> Range
errorRange (Review.Error.ReviewError err) =
    err.range


{-| Get the automatic [`fixes`](./Review-Fix#Fix) of an [`Error`](#Error), if it
defined any.
-}
errorFixes : Review.Error.ReviewError -> Maybe (List Fix)
errorFixes (Review.Error.ReviewError err) =
    err.fixes


{-| Get the file path of an [`Error`](#Error).
-}
errorFilePath : Review.Error.ReviewError -> String
errorFilePath (Review.Error.ReviewError err) =
    err.filePath


{-| Get the file path of an [`Error`](#Error).
-}
errorTarget : Review.Error.ReviewError -> Review.Error.Target
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
            |> Rule.withExpressionVisitor expressionVisitor
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
ignoreErrorsForDirectories directories (Rule name exceptions fn) =
    Rule
        name
        (Exceptions.addDirectories directories exceptions)
        fn


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
ignoreErrorsForFiles files (Rule name exceptions fn) =
    Rule
        name
        (Exceptions.addFiles files exceptions)
        fn



-- TREE TRAVERSAL


visitImport :
    List (Node Import -> moduleContext -> ( List (Error {}), moduleContext ))
    -> Node Import
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitImport importVisitors node moduleContext =
    visitNodeWithListOfVisitors importVisitors node ( [], moduleContext )


visitDeclaration :
    InAndOut (DirectedVisitor Declaration moduleContext)
    -> InAndOut (DirectedVisitor Expression moduleContext)
    -> Node Declaration
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitDeclaration declarationVisitors expressionVisitors node moduleContext =
    let
        accumulateExpressionNodes : ( List (Error {}), moduleContext ) -> ( List (Error {}), moduleContext )
        accumulateExpressionNodes =
            if List.isEmpty expressionVisitors.onEnter then
                identity

            else
                accumulateList
                    (visitExpression expressionVisitors)
                    (expressionsInDeclaration node)
    in
    ( [], moduleContext )
        |> visitNodeWithListOfVisitorsAndDirection OnEnter declarationVisitors.onEnter node
        |> accumulateExpressionNodes
        |> visitNodeWithListOfVisitorsAndDirection OnExit declarationVisitors.onExit node


visitNodeWithListOfVisitors :
    List (Node a -> moduleContext -> ( List (Error {}), moduleContext ))
    -> Node a
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitNodeWithListOfVisitors visitors node initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor node))
        initialErrorsAndContext
        visitors


visitNodeWithListOfVisitorsAndDirection :
    Direction
    -> List (Node a -> Direction -> moduleContext -> ( List (Error {}), moduleContext ))
    -> Node a
    -> ( List (Error {}), moduleContext )
    -> ( List (Error {}), moduleContext )
visitNodeWithListOfVisitorsAndDirection direction visitors node initialErrorsAndContext =
    List.foldl
        (\visitor -> accumulate (visitor node direction))
        initialErrorsAndContext
        visitors


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


expressionsInDeclaration : Node Declaration -> List (Node Expression)
expressionsInDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            [ functionToExpression function ]

        Declaration.CustomTypeDeclaration _ ->
            []

        Declaration.AliasDeclaration { typeAnnotation } ->
            []

        Declaration.Destructuring _ expr ->
            [ expr ]

        Declaration.PortDeclaration _ ->
            []

        Declaration.InfixDeclaration _ ->
            []


visitExpression :
    InAndOut (DirectedVisitor Expression moduleContext)
    -> Node Expression
    -> moduleContext
    -> ( List (Error {}), moduleContext )
visitExpression visitors node moduleContext =
    ( [], moduleContext )
        |> visitNodeWithListOfVisitorsAndDirection OnEnter visitors.onEnter node
        |> accumulateList (visitExpression visitors) (expressionChildren node)
        |> visitNodeWithListOfVisitorsAndDirection OnExit visitors.onExit node


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

        Expression.LambdaExpression { args, expression } ->
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


functionToExpression : Function -> Node Expression
functionToExpression function =
    Node.value function.declaration
        |> .expression


accumulateList : (Node a -> context -> ( List (Error {}), context )) -> List (Node a) -> ( List (Error {}), context ) -> ( List (Error {}), context )
accumulateList visitor nodes ( previousErrors, previousContext ) =
    List.foldl
        (\node -> accumulate (visitor node))
        ( previousErrors, previousContext )
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
