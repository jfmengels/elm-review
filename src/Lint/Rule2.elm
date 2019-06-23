module Lint.Rule2 exposing
    ( Rule2
    , newRuleSchema
    , withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor
    , withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation
    , evaluateDeclaration, evaluateExpression, evaluateImport, evaluateModuleDefinition, finalEvaluation, initialContext
    , Visitor2
    )

{-| This module contains functions that are used for writing rules.


# Definition

@docs Rule2


# Writing rules

@docs newRuleSchema
@docs withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor
@docs withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation


# ACCESS

@docs evaluateDeclaration, evaluateExpression, evaluateImport, evaluateModuleDefinition, finalEvaluation, initialContext


# Internal types

@docs Visitor

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


{-| Represents a `rule` that will be enforced.

        import Lint.Rule as Rule

        rule : Rule
        rule =
            Rule.newRuleSchema "NoDebug"
                |> Rule.withExpressionVisitor expressionVisitor
                |> Rule.fromSchema

-}
type Rule2 context
    = Rule2
        { name : String
        , initialContext : context
        , moduleDefinitionVisitor : context -> Node Module -> ( List Error, context )
        , importVisitor : context -> Node Import -> ( List Error, context )
        , expressionVisitor : context -> Direction -> Node Expression -> ( List Error, context )
        , declarationVisitor : context -> Direction -> Node Declaration -> ( List Error, context )
        , finalEvaluationFn : context -> List Error
        }


{-| Creates a new schema for a rule. Will require calling `fromSchema` to create a usable `Rule`.
-}
newRuleSchema : String -> Rule2 ()
newRuleSchema name =
    Rule2
        { name = name
        , initialContext = ()
        , moduleDefinitionVisitor = \context node -> ( [], context )
        , importVisitor = \context node -> ( [], context )
        , expressionVisitor = \context direction node -> ( [], context )
        , declarationVisitor = \context direction node -> ( [], context )
        , finalEvaluationFn = \context -> []
        }



-- TODO
-- fromSchema : Rule2 () -> ...


withInitialContext : context -> Rule2 () -> Rule2 context
withInitialContext initialContext_ (Rule2 rule) =
    Rule2
        { name = rule.name
        , initialContext = initialContext_
        , moduleDefinitionVisitor = \context node -> ( [], context )
        , importVisitor = \context node -> ( [], context )
        , expressionVisitor = \context direction node -> ( [], context )
        , declarationVisitor = \context direction node -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


withModuleDefinitionVisitor : (context -> Node Module -> ( List Error, context )) -> Rule2 context -> Rule2 context
withModuleDefinitionVisitor visitor (Rule2 rule) =
    Rule2 { rule | moduleDefinitionVisitor = visitor }


withImportVisitor : (context -> Node Import -> ( List Error, context )) -> Rule2 context -> Rule2 context
withImportVisitor visitor (Rule2 rule) =
    Rule2 { rule | importVisitor = visitor }


withExpressionVisitor : (context -> Direction -> Node Expression -> ( List Error, context )) -> Rule2 context -> Rule2 context
withExpressionVisitor visitor (Rule2 rule) =
    Rule2 { rule | expressionVisitor = visitor }


withDeclarationVisitor : (context -> Direction -> Node Declaration -> ( List Error, context )) -> Rule2 context -> Rule2 context
withDeclarationVisitor visitor (Rule2 rule) =
    Rule2 { rule | declarationVisitor = visitor }


withFinalEvaluation : (context -> List Error) -> Rule2 context -> Rule2 context
withFinalEvaluation visitor (Rule2 rule) =
    Rule2 { rule | finalEvaluationFn = visitor }



-- RULES WITHOUT ANALYSIS


withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> Rule2 context -> Rule2 context
withSimpleModuleDefinitionVisitor visitor (Rule2 rule) =
    Rule2 { rule | moduleDefinitionVisitor = \context node -> ( visitor node, context ) }


withSimpleImportVisitor : (Node Import -> List Error) -> Rule2 context -> Rule2 context
withSimpleImportVisitor visitor (Rule2 rule) =
    Rule2 { rule | importVisitor = \context node -> ( visitor node, context ) }


withSimpleExpressionVisitor : (Node Expression -> List Error) -> Rule2 context -> Rule2 context
withSimpleExpressionVisitor visitor (Rule2 rule) =
    Rule2
        { rule
            | expressionVisitor =
                \context direction node ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }


withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> Rule2 context -> Rule2 context
withSimpleDeclarationVisitor visitor (Rule2 rule) =
    Rule2
        { rule
            | declarationVisitor =
                \context direction node ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }



-- RULE CONSTRUCTOR AND BUILDERS
-- RULES WITH ANALYSES
-- ACCESS


initialContext : Rule2 context -> context
initialContext (Rule2 rule) =
    rule.initialContext


evaluateModuleDefinition : Rule2 context -> context -> Node Module -> ( List Error, context )
evaluateModuleDefinition (Rule2 rule) =
    rule.moduleDefinitionVisitor


evaluateImport : Rule2 context -> context -> Node Import -> ( List Error, context )
evaluateImport (Rule2 rule) =
    rule.importVisitor


evaluateExpression : Rule2 context -> context -> Direction -> Node Expression -> ( List Error, context )
evaluateExpression (Rule2 rule) =
    rule.expressionVisitor


evaluateDeclaration : Rule2 context -> context -> Direction -> Node Declaration -> ( List Error, context )
evaluateDeclaration (Rule2 rule) =
    rule.declarationVisitor


finalEvaluation : Rule2 context -> context -> List Error
finalEvaluation (Rule2 rule) =
    rule.finalEvaluationFn


{-| Shorthand for a function that takes a rule's implementation, a context and returns ( List Lint.Error.Error, context ).
A Visitor represents a node and calls the appropriate function for the given node type.
-}
type alias Visitor2 context =
    Rule2 context -> context -> ( List Error, context )
