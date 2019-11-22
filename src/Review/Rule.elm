module Review.Rule exposing
    ( Rule, Schema, MultiSchema, ForLookingAtASingleFile, ForLookingAtSeveralFiles
    , runRules
    , newSchema, fromSchema
    , withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , withInitialContext, withModuleDefinitionVisitor, withImportVisitor, Direction(..), withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalEvaluation
    , withElmJsonVisitor, withDependenciesVisitor
    , withFixes
    , Error, error, parsingError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath
    , newMultiSchema, fromMultiSchema, newFileVisitorSchema
    , FileKey, withFileKeyVisitor, errorForFile
    , ReviewResult(..)
    )

{-| This module contains functions that are used for writing rules.


# How does it work?

`elm-review` turns the code of the analyzed file into an [Abstract Syntax Tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
(a tree-like structure which represents your source code) using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
Then, for each file and rule, it will give the details of your project (like the `elm.json` file) and the
contents of the file to analyze to the rule. The order in which things get passed to the rule is the following:

  - Read project-related info (only collect data in these steps)
      - The `elm.json` file, visited by [`withElmJsonVisitor`](#withElmJsonVisitor)
      - The definition for dependencies, visited by [`withDependenciesVisitor`](#withDependenciesVisitor)
  - Visit the file (in the following order)
      - The module definition, visited by [`withSimpleModuleDefinitionVisitor`](#withSimpleModuleDefinitionVisitor) and [`withModuleDefinitionVisitor`](#withModuleDefinitionVisitor)
      - Each import, visited by [`withSimpleImportVisitor`](#withSimpleImportVisitor) and [`withImportVisitor`](#withImportVisitor)
      - The list of declarations, visited by [`withDeclarationListVisitor`](#withDeclarationListVisitor).
      - Each declaration, visited by [`withSimpleDeclarationVisitor`](#withSimpleDeclarationVisitor) and [`withDeclarationVisitor`](#withDeclarationVisitor).
        Before evaluating the next declaration, the expression contained in the declaration
        will be visited recursively using by [`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) and [`withExpressionVisitor`](#withExpressionVisitor)
      - A final evaluation is made when the whole AST has been traversed, using [`withFinalEvaluation`](#withFinalEvaluation)

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

I recommend having the name of the file containing the rule be the same as the
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


## Definition

@docs Rule, Schema, MultiSchema, ForLookingAtASingleFile, ForLookingAtSeveralFiles


## Running the rule

@docs runRules


## Creating a Rule

@docs newSchema, fromSchema


## Builder functions without context

@docs withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor


## Builder functions with context

@docs withInitialContext, withModuleDefinitionVisitor, withImportVisitor, Direction, withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalEvaluation


## Builder functions to analyze the project's data

@docs withElmJsonVisitor, withDependenciesVisitor


## Automatic fixing

For more information on automatic fixing, read the documentation for [`Review.Fix`](./Review-Fix).

@docs withFixes


## Errors

@docs Error, error, parsingError, errorRuleName, errorMessage, errorDetails, errorRange, errorFixes, errorFilePath


# TODO

@docs newMultiSchema, fromMultiSchema, newFileVisitorSchema
@docs FileKey, withFileKeyVisitor, errorForFile
@docs ReviewResult

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.File exposing (ParsedFile, RawFile)
import Review.Fix exposing (Fix)
import Review.Project exposing (Project)


{-| Represents a construct able to analyze a `File` and report unwanted patterns.
See [`newSchema`](#newSchema), and [`fromSchema`](#fromSchema) for how to create one.
TODO Explain about single and multi-file rules
-}
type Rule
    = Single String (Project -> List ParsedFile -> ( List Error, Rule ))
    | Multi String (Project -> List ParsedFile -> ( List Error, Rule ))


type ReviewResult
    = ReviewResult ( List Error, Project -> List ParsedFile -> ReviewResult )


{-| Represents a Schema for a [`Rule`](#Rule). Create one using [`newSchema`](#newSchema).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoDebug"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromSchema

-}
type
    Schema ruleType configurationState context
    -- `configurationState` is a phantom type used to forbid using `withInitialContext`
    -- after having defined some visitors already. For `withInitialContext` to
    -- work and due to the change in `context` type value, all visitors need to be
    -- replaced by no-op functions. This means that if you did the following
    --   Rule.newSchema "RuleName"
    --     |> Rule.withSimpleExpressionVisitor expressionVisitor
    --     |> Rule.withInitialContext something
    --     |> Rule.withImportVisitor importVisitor
    --     |> Rule.fromSchema
    -- then the expression visitor would be erased and ignored. This phantom type
    -- should prevent that from working.
    -- It also prevents using
    --   Rule.newSchema "RuleName"
    --     |> Rule.withSimpleExpressionVisitor expressionVisitor
    --     |> Rule.withInitialContext something
    --     |> Rule.withImportVisitor importVisitor
    --     |> Rule.fromSchema
    -- Which is nice because this is a rule that does nothing. It doesn't yet
    -- prevent the following, but I haven't yet found a fitting phantom type.
    --   Rule.newSchema "RuleName"
    --     |> Rule.withInitialContext something
    --     |> Rule.fromSchema
    = Schema
        { name : String
        , initialContext : context
        , fileKeyVisitors : List (FileKey -> context -> context)
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> context -> context)
        , dependenciesVisitors : List (Dict String Elm.Docs.Module -> context -> context)
        , moduleDefinitionVisitors : List (Node Module -> context -> ( List Error, context ))
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



-- RULE TYPES


type ForLookingAtASingleFile
    = JustOneMore1 ForLookingAtASingleFile


type ForLookingAtSeveralFiles
    = JustOneMore2 ForLookingAtSeveralFiles


runRules : List Rule -> Project -> List ParsedFile -> ( List Error, List Rule )
runRules rules project files =
    List.foldl
        (\rule ( errors, previousRules ) ->
            let
                ( ruleErrors, ruleWithCache ) =
                    run rule project files
            in
            ( List.concat [ ruleErrors, errors ], ruleWithCache :: previousRules )
        )
        ( [], [] )
        rules


run : Rule -> Project -> List ParsedFile -> ( List Error, Rule )
run rule project files =
    case rule of
        Single _ fn ->
            fn project files

        Multi _ fn ->
            fn project files


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


{-| Creates a new schema for a rule. Will require calling [`fromSchema`](#fromSchema)
to create a usable [`Rule`](#Rule). Use "with\*" functions from this module, like
[`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) or [`withSimpleImportVisitor`](#withSimpleImportVisitor)
to make it report something.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoDebug"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromSchema

If you wish to build a [`Rule`](#Rule) that collects data as the file gets traversed,
take a look at [`withInitialContext`](#withInitialContext) and "with\*" functions without
"Simple" in their name, like [`withExpressionVisitor`](#withExpressionVisitor),
[`withImportVisitor`](#withImportVisitor) or [`withFinalEvaluation`](#withFinalEvaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoUnusedVariables"
            |> Rule.withInitialContext { declaredVariables = [], usedVariables = [] }
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromSchema

-}
newSchema : String -> Schema ForLookingAtASingleFile { hasNoVisitor : () } ()
newSchema name_ =
    emptySchema name_ ()


{-| Creates a new schema for a rule. Will require calling [`fromSchema`](#fromSchema)
to create a usable [`Rule`](#Rule). Use "with\*" functions from this module, like
[`withSimpleExpressionVisitor`](#withSimpleExpressionVisitor) or [`withSimpleImportVisitor`](#withSimpleImportVisitor)
to make it report something.

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoDebug"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromSchema

If you wish to build a [`Rule`](#Rule) that collects data as the file gets traversed,
take a look at [`withInitialContext`](#withInitialContext) and "with\*" functions without
"Simple" in their name, like [`withExpressionVisitor`](#withExpressionVisitor),
[`withImportVisitor`](#withImportVisitor) or [`withFinalEvaluation`](#withFinalEvaluation).

    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoUnusedVariables"
            |> Rule.withInitialContext { declaredVariables = [], usedVariables = [] }
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromSchema

-}
newFileVisitorSchema : context -> Schema ForLookingAtSeveralFiles { hasNoVisitor : () } context
newFileVisitorSchema initialContext =
    emptySchema "" initialContext


{-| Create a [`Rule`](#Rule) from a configured [`Schema`](#Schema).
-}
fromSchema : Schema ForLookingAtASingleFile { hasAtLeastOneVisitor : () } context -> Rule
fromSchema ((Schema { name }) as schema) =
    Single name (runSingle (reverseVisitors schema) Dict.empty)


reverseVisitors : Schema anyType { hasAtLeastOneVisitor : () } context -> Schema anyType { hasAtLeastOneVisitor : () } context
reverseVisitors (Schema schema) =
    Schema
        { schema
            | fileKeyVisitors = List.reverse schema.fileKeyVisitors
            , elmJsonVisitors = List.reverse schema.elmJsonVisitors
            , dependenciesVisitors = List.reverse schema.dependenciesVisitors
            , moduleDefinitionVisitors = List.reverse schema.moduleDefinitionVisitors
            , importVisitors = List.reverse schema.importVisitors
            , declarationListVisitors = List.reverse schema.declarationListVisitors
            , finalEvaluationFns = List.reverse schema.finalEvaluationFns
        }


type alias SingleRuleCache =
    Dict String
        { source : String
        , errors : List Error
        }


runSingle : Schema ForLookingAtASingleFile { hasAtLeastOneVisitor : () } context -> SingleRuleCache -> Project -> List ParsedFile -> ( List Error, Rule )
runSingle ((Schema { name }) as schema) startCache project files =
    let
        computeErrors_ : ParsedFile -> List Error
        computeErrors_ =
            computeErrors schema project

        newCache : SingleRuleCache
        newCache =
            List.foldl
                (\file cache ->
                    case Dict.get file.path cache of
                        Nothing ->
                            Dict.insert file.path { source = file.source, errors = computeErrors_ file } cache

                        Just cacheEntry ->
                            if cacheEntry.source == file.source then
                                -- File is unchanged, we will later return the cached errors
                                cache

                            else
                                Dict.insert file.path { source = file.source, errors = computeErrors_ file } cache
                )
                startCache
                files

        errors : List Error
        errors =
            newCache
                |> Dict.values
                |> List.concatMap .errors
    in
    ( errors, Single name (runSingle schema newCache) )


computeErrors : Schema ForLookingAtASingleFile { hasAtLeastOneVisitor : () } context -> Project -> ParsedFile -> List Error
computeErrors (Schema schema) project =
    let
        initialContext : context
        initialContext =
            schema.initialContext
                |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project)
                |> accumulateContext schema.dependenciesVisitors (Review.Project.modules project)

        declarationVisitors : InAndOut (DirectedVisitor Declaration context)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression context)
        expressionVisitors =
            inAndOut schema.expressionVisitors
    in
    \file ->
        ( [], initialContext )
            |> accumulateWithListOfVisitors schema.moduleDefinitionVisitors file.ast.moduleDefinition
            |> accumulateList (visitImport schema.importVisitors) file.ast.imports
            |> accumulateWithListOfVisitors schema.declarationListVisitors file.ast.declarations
            |> accumulateList (visitDeclaration declarationVisitors expressionVisitors) file.ast.declarations
            |> makeFinalEvaluation schema.finalEvaluationFns
            |> List.map (\(Error err) -> Error { err | ruleName = schema.name, filePath = file.path })
            |> List.reverse


inAndOut : List (DirectedVisitor nodeType context) -> InAndOut (DirectedVisitor nodeType context)
inAndOut visitors =
    { onEnter = List.reverse visitors
    , onExit = visitors
    }


accumulateContext : List (element -> context -> context) -> element -> context -> context
accumulateContext visitors element context =
    List.foldl (\visitor -> visitor element) context visitors


type MultiSchema context
    = MultiSchema
        { name : String
        , initialContext : context
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> context -> context)
        , dependenciesVisitors : List (Dict String Elm.Docs.Module -> context -> context)
        , mergeContexts : context -> context -> context
        , fileVisitor : context -> Schema ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } context
        , finalEvaluationFn : context -> List Error
        }


newMultiSchema :
    String
    ->
        { initialContext : context
        , elmJsonVisitors : List (Maybe Elm.Project.Project -> context -> context)
        , dependenciesVisitors : List (Dict String Elm.Docs.Module -> context -> context)
        , fileVisitor : context -> Schema ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } context
        , mergeContexts : context -> context -> context
        , finalEvaluation : context -> List Error
        }
    -> MultiSchema context
newMultiSchema name_ { initialContext, elmJsonVisitors, dependenciesVisitors, fileVisitor, mergeContexts, finalEvaluation } =
    MultiSchema
        { name = name_
        , initialContext = initialContext
        , elmJsonVisitors = elmJsonVisitors
        , dependenciesVisitors = dependenciesVisitors
        , mergeContexts = mergeContexts
        , fileVisitor = fileVisitor
        , finalEvaluationFn = finalEvaluation
        }


visitFileForMulti : Schema ForLookingAtSeveralFiles { hasAtLeastOneVisitor : () } context -> context -> ParsedFile -> ( List Error, context )
visitFileForMulti (Schema schema) =
    let
        declarationVisitors : InAndOut (DirectedVisitor Declaration context)
        declarationVisitors =
            inAndOut schema.declarationVisitors

        expressionVisitors : InAndOut (DirectedVisitor Expression context)
        expressionVisitors =
            inAndOut schema.expressionVisitors
    in
    \initialContext file ->
        ( []
        , initialContext
            |> accumulateContext schema.fileKeyVisitors (FileKey file.path)
        )
            |> accumulateWithListOfVisitors schema.moduleDefinitionVisitors file.ast.moduleDefinition
            |> accumulateList (visitImport schema.importVisitors) file.ast.imports
            |> accumulateWithListOfVisitors schema.declarationListVisitors file.ast.declarations
            |> accumulateList (visitDeclaration declarationVisitors expressionVisitors) file.ast.declarations


{-| TODO documentation
-}
fromMultiSchema : MultiSchema context -> Rule
fromMultiSchema (MultiSchema schema) =
    Multi schema.name
        (runMulti
            (MultiSchema
                { schema
                    | elmJsonVisitors = List.reverse schema.elmJsonVisitors
                    , dependenciesVisitors = List.reverse schema.dependenciesVisitors
                    , fileVisitor = schema.fileVisitor >> reverseVisitors
                }
            )
            Dict.empty
        )


type alias MultiRuleCache context =
    Dict String
        { source : String
        , errors : List Error
        , context : context
        }


runMulti : MultiSchema context -> MultiRuleCache context -> Project -> List ParsedFile -> ( List Error, Rule )
runMulti (MultiSchema schema) startCache project =
    let
        initialContext : context
        initialContext =
            schema.initialContext
                |> accumulateContext schema.elmJsonVisitors (Review.Project.elmJson project)
                |> accumulateContext schema.dependenciesVisitors (Review.Project.modules project)
    in
    \files ->
        let
            computeFile : ParsedFile -> { source : String, errors : List Error, context : context }
            computeFile file =
                let
                    ( fileErrors, context ) =
                        visitFileForMulti
                            (schema.fileVisitor initialContext)
                            initialContext
                            file
                in
                { source = file.source
                , errors = List.map (\(Error err) -> Error { err | filePath = file.path }) fileErrors
                , context = context
                }

            newCache : MultiRuleCache context
            newCache =
                List.foldl
                    (\file cache ->
                        case Dict.get file.path cache of
                            Nothing ->
                                Dict.insert file.path (computeFile file) cache

                            Just cacheEntry ->
                                if cacheEntry.source == file.source then
                                    -- File is unchanged, we will later return the cached errors and context
                                    cache

                                else
                                    Dict.insert file.path (computeFile file) cache
                    )
                    startCache
                    files

            contextsAndErrorsPerFile : List ( List Error, context )
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
                        |> List.foldl schema.mergeContexts schema.initialContext
                        |> schema.finalEvaluationFn
                        |> List.map (\(Error err) -> Error { err | ruleName = schema.name })
                    ]
        in
        ( errors, Multi schema.name (runMulti (MultiSchema schema) newCache) )


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


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s [module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`) and report patterns.

The following example forbids having `_` in any part of a module name.

    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoUnderscoreInModuleName"
            |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromSchema

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
withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withSimpleModuleDefinitionVisitor visitor schema =
    withModuleDefinitionVisitor (\node context -> ( visitor node, context )) schema


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s [import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Import) (`import Html as H exposing (div)`) in order of their definition and report patterns.

The following example forbids using the core Html package and suggests using
`elm-css` instead.

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Error, Rule)

    rule : Rule
    rule =
        Rule.newSchema "NoCoreHtml"
            |> Rule.withSimpleImportVisitor importVisitor
            |> Rule.fromSchema

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
withSimpleImportVisitor : (Node Import -> List Error) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withSimpleImportVisitor visitor schema =
    withImportVisitor (\node context -> ( visitor node, context )) schema


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoMissingTypeAnnotation"
            |> Rule.withSimpleDeclarationVisitor declarationVisitor
            |> Rule.fromSchema

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
withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
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


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoDebug"
            |> Rule.withSimpleExpressionVisitor expressionVisitor
            |> Rule.fromSchema

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
withSimpleExpressionVisitor : (Node Expression -> List Error) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
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


{-| Adds an initial `context` to start collecting data during your traversal.

In some cases, you can't just report a pattern when you see it, but you want to
not report or report differently depending on information located in a different
part of the file. In that case, you collect data as the nodes in the file get
traversed and store it in what we'll call a `context`. This `context` will be
available and updated by non-"simple" "with\*" functions, like
[`withExpressionVisitor`](#withExpressionVisitor) or [`withImportVisitor`](#withImportVisitor).

Once the file has been traversed and you have collected all the data available
from the file, you can report some final errors using [`withFinalEvaluation`](#withFinalEvaluation).

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

The following example forbids calling `Rule.newSchema` with a name that is not
the same as the module's name (forbidding `Rule.newSchema "NoSomething"` when the
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
        Rule.newSchema "NoDifferentNameForRuleAndModuleName"
            |> Rule.withInitialContext Nothing
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromSchema

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
                    ( FunctionOrValue [ "Rule" ] "newSchema", Literal ruleName ) ->
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

Note that due to implementation details, `withInitialContext` needs to be chained
right after [`newSchema`](#newSchema) just like in the example above, as previous
"with\*" functions will be ignored.

-}
withInitialContext : context -> Schema anyType { hasNoVisitor : () } () -> Schema anyType { hasNoVisitor : () } context
withInitialContext initialContext_ (Schema schema) =
    emptySchema schema.name initialContext_


emptySchema : String -> context -> Schema anyType anything context
emptySchema name_ initialContext =
    Schema
        { name = name_
        , initialContext = initialContext
        , fileKeyVisitors = []
        , elmJsonVisitors = []
        , dependenciesVisitors = []
        , moduleDefinitionVisitors = []
        , importVisitors = []
        , declarationListVisitors = []
        , declarationVisitors = []
        , expressionVisitors = []
        , finalEvaluationFns = []
        }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the project's
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
        Rule.newSchema "DoNoExposeInternalModules"
            |> Rule.withInitialContext Nothing
            |> Rule.withElmJsonVisitor elmJsonVisitor
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.fromSchema

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
withElmJsonVisitor : (Maybe Elm.Project.Project -> context -> context) -> Schema anyType anything context -> Schema anyType anything context
withElmJsonVisitor visitor (Schema schema) =
    Schema { schema | elmJsonVisitors = visitor :: schema.elmJsonVisitors }


withDependenciesVisitor : (Dict String Elm.Docs.Module -> context -> context) -> Schema anyType anything context -> Schema anyType anything context
withDependenciesVisitor visitor (Schema schema) =
    Schema { schema | dependenciesVisitors = visitor :: schema.dependenciesVisitors }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoHtmlButton"
            |> Rule.withInitialContext HtmlButtonIsForbidden
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromSchema

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
withModuleDefinitionVisitor : (Node Module -> context -> ( List Error, context )) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withModuleDefinitionVisitor visitor (Schema schema) =
    Schema { schema | moduleDefinitionVisitors = visitor :: schema.moduleDefinitionVisitors }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoUsingBothHtmlAndHtmlStyled"
            |> Rule.withInitialContext { elmUiWasImported = False, elmCssWasImported = False }
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromSchema

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

This example was written in a different way in the example for [`withFinalEvaluation`](#withFinalEvaluation).

Tip: If you do not need to collect or use the `context` in this visitor, you may wish to use the
simpler [`withSimpleImportVisitor`](#withSimpleImportVisitor) function.

-}
withImportVisitor : (Node Import -> context -> ( List Error, context )) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withImportVisitor visitor (Schema schema) =
    Schema { schema | importVisitors = visitor :: schema.importVisitors }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoMissingDocumentationForExposedFunctions"
            |> Rule.withInitialContext (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationVisitor declarationVisitor
            |> Rule.fromSchema

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
withDeclarationVisitor : (Node Declaration -> Direction -> context -> ( List Error, context )) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withDeclarationVisitor visitor (Schema schema) =
    Schema { schema | declarationVisitors = visitor :: schema.declarationVisitors }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
withDeclarationListVisitor : (List (Node Declaration) -> context -> ( List Error, context )) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withDeclarationListVisitor visitor (Schema schema) =
    Schema { schema | declarationListVisitors = visitor :: schema.declarationListVisitors }


{-| Add a visitor to the [`Schema`](#Schema) which will visit the `File`'s
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
        Rule.newSchema "NoDebugEvenIfImported"
            |> Rule.withInitialContext DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionVisitor expressionVisitor
            |> Rule.fromSchema

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
withExpressionVisitor : (Node Expression -> Direction -> context -> ( List Error, context )) -> Schema anyType anything context -> Schema anyType { hasAtLeastOneVisitor : () } context
withExpressionVisitor visitor (Schema schema) =
    Schema { schema | expressionVisitors = visitor :: schema.expressionVisitors }


{-| Add a function that makes a final evaluation based only on the data that was
collected in the `context`. This can be useful if you can't or if it is hard to
determine something as you traverse the file.

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`). Note that this is the same one written in the example
for [`withImportVisitor`](#withImportVisitor), but using [`withFinalEvaluation`](#withFinalEvaluation).

    import Dict as Dict exposing (Dict)
    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Elm.Syntax.Range exposing (Range)
    import Review.Rule as Rule exposing (Error, Rule)

    type alias Context =
        Dict (List String) Range

    rule : Rule
    rule =
        Rule.newSchema "NoUsingBothHtmlAndHtmlStyled"
            |> Rule.withInitialContext Dict.empty
            |> Rule.withImportVisitor importVisitor
            |> Rule.withFinalEvaluation finalEvaluation
            |> Rule.fromSchema

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
withFinalEvaluation : (context -> List Error) -> Schema anyType { hasAtLeastOneVisitor : () } context -> Schema anyType { hasAtLeastOneVisitor : () } context
withFinalEvaluation visitor (Schema schema) =
    Schema { schema | finalEvaluationFns = visitor :: schema.finalEvaluationFns }


{-| TODO
-}
withFileKeyVisitor : (FileKey -> context -> context) -> Schema ForLookingAtSeveralFiles { hasNoVisitor : () } context -> Schema ForLookingAtSeveralFiles { hasNoVisitor : () } context
withFileKeyVisitor visitor (Schema schema) =
    Schema { schema | fileKeyVisitors = visitor :: schema.fileKeyVisitors }



-- ERRORS


{-| Represents an error found by a [`Rule`](#Rule).

Note: This should not be confused with [`Review.Error`](./Review#Error) from the
[`Review`](./Review) module. [`Review.Error`](./Review#Error) is created from
this module's [`Error`](#Error) but contains additional information like the
name of the rule that emitted it and the file name.

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


type FileKey
    = FileKey String


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
    -- TODO Use fileKey
    Error
        { message = message
        , ruleName = ""
        , details = details
        , range = range
        , filePath = path
        , fixes = Nothing
        }


{-| TODO
-}
parsingError : RawFile -> Error
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


{-| TODO
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
    ( [], context )
        |> visitNodeWithListOfVisitorsAndDirection OnEnter declarationVisitors.onEnter node
        |> accumulateList (visitExpression expressionVisitors) (expressionsInDeclaration node)
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



-- |> accumulate (visitor node OnExit)


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
