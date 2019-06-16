module Lint.Rule exposing
    ( Implementation
    , create, createSimple
    , withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation
    , withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor
    , evaluateDeclaration, evaluateExpression, evaluateImport, evaluateModuleDefinition, finalEvaluation, initialContext
    , Visitor, LintResult
    )

{-| This module contains functions that are used for writing rules.


# Definition

@docs Implementation


# Writing rules

@docs create, createSimple
@docs withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation
@docs withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor


# ACCESS

@docs evaluateDeclaration, evaluateExpression, evaluateImport, evaluateModuleDefinition, finalEvaluation, initialContext


# Internal types

@docs Visitor, LintResult

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)


{-| A Implementation is the implementation of a rule. It is a record that contains:

  - initialContext: An initial context

  - expression: A LintImplementation for Expression nodes

  - visitEnd: A function that takes a context and returns a list of error. Similar to a LintImplementation, but will
    be called after visiting the whole AST.

        import Lint exposing (Rule)
        import Lint.Rule as Rule

        type alias Context =
            { numberOfImports : Int
            }

        rule : Rule
        rule input =
            lint input implementation

        implementation : Rule.Implementation Context
        implementation =
            Rule.create { numberOfImports = 0 }
                |> Rule.withImportVisitor importVisitor

-}
type Implementation context
    = Implementation
        { initialContext : context
        , moduleDefinitionVisitor : context -> Node Module -> ( List Error, context )
        , importVisitor : context -> Node Import -> ( List Error, context )
        , expressionVisitor : context -> Direction -> Node Expression -> ( List Error, context )
        , declarationVisitor : context -> Direction -> Node Declaration -> ( List Error, context )
        , finalEvaluationFn : context -> List Error
        }



-- RULE CONSTRUCTOR AND BUILDERS
-- RULES WITH ANALYSES


create : context -> Implementation context
create initialContext_ =
    Implementation
        { initialContext = initialContext_
        , moduleDefinitionVisitor = \context node -> ( [], context )
        , importVisitor = \context node -> ( [], context )
        , expressionVisitor = \context direction node -> ( [], context )
        , declarationVisitor = \context direction node -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


withModuleDefinitionVisitor : (context -> Node Module -> ( List Error, context )) -> Implementation context -> Implementation context
withModuleDefinitionVisitor visitor (Implementation impl) =
    Implementation { impl | moduleDefinitionVisitor = visitor }


withImportVisitor : (context -> Node Import -> ( List Error, context )) -> Implementation context -> Implementation context
withImportVisitor visitor (Implementation impl) =
    Implementation { impl | importVisitor = visitor }


withExpressionVisitor : (context -> Direction -> Node Expression -> ( List Error, context )) -> Implementation context -> Implementation context
withExpressionVisitor visitor (Implementation impl) =
    Implementation { impl | expressionVisitor = visitor }


withDeclarationVisitor : (context -> Direction -> Node Declaration -> ( List Error, context )) -> Implementation context -> Implementation context
withDeclarationVisitor visitor (Implementation impl) =
    Implementation { impl | declarationVisitor = visitor }


withFinalEvaluation : (context -> List Error) -> Implementation context -> Implementation context
withFinalEvaluation visitor (Implementation impl) =
    Implementation { impl | finalEvaluationFn = visitor }



-- RULES WITHOUT ANALYSIS


createSimple : Implementation ()
createSimple =
    create ()


withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> Implementation context -> Implementation context
withSimpleModuleDefinitionVisitor visitor (Implementation impl) =
    Implementation { impl | moduleDefinitionVisitor = \context node -> ( visitor node, context ) }


withSimpleImportVisitor : (Node Import -> List Error) -> Implementation context -> Implementation context
withSimpleImportVisitor visitor (Implementation impl) =
    Implementation { impl | importVisitor = \context node -> ( visitor node, context ) }


withSimpleExpressionVisitor : (Node Expression -> List Error) -> Implementation context -> Implementation context
withSimpleExpressionVisitor visitor (Implementation impl) =
    Implementation
        { impl
            | expressionVisitor =
                \context direction node ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }


withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> Implementation context -> Implementation context
withSimpleDeclarationVisitor visitor (Implementation impl) =
    Implementation
        { impl
            | declarationVisitor =
                \context direction node ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }



-- ACCESS


initialContext : Implementation context -> context
initialContext (Implementation impl) =
    impl.initialContext


evaluateModuleDefinition : Implementation context -> context -> Node Module -> ( List Error, context )
evaluateModuleDefinition (Implementation impl) =
    impl.moduleDefinitionVisitor


evaluateImport : Implementation context -> context -> Node Import -> ( List Error, context )
evaluateImport (Implementation impl) =
    impl.importVisitor


evaluateExpression : Implementation context -> context -> Direction -> Node Expression -> ( List Error, context )
evaluateExpression (Implementation impl) =
    impl.expressionVisitor


evaluateDeclaration : Implementation context -> context -> Direction -> Node Declaration -> ( List Error, context )
evaluateDeclaration (Implementation impl) =
    impl.declarationVisitor


finalEvaluation : Implementation context -> context -> List Error
finalEvaluation (Implementation impl) =
    impl.finalEvaluationFn


{-| Shortcut to the result of a lint rule
-}
type alias LintResult =
    Result (List String) (List Error)


{-| Shorthand for a function that takes a rule's implementation, a context and returns ( List Lint.Error.Error, context ).
A Visitor represents a node and calls the appropriate function for the given node type.
-}
type alias Visitor context =
    Implementation context -> context -> ( List Error, context )
