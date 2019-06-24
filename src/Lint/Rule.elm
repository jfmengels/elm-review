module Lint.Rule exposing
    ( Rule, Schema
    , newSchema, fromSchema
    , withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor
    , withInitialContext, withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation
    , name, analyzer
    )

{-| This module contains functions that are used for writing rules.


# Definition

@docs Rule, Schema


# Writing rules

@docs newSchema, fromSchema
@docs withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleExpressionVisitor, withSimpleDeclarationVisitor
@docs withInitialContext, withModuleDefinitionVisitor, withImportVisitor, withExpressionVisitor, withDeclarationVisitor, withFinalEvaluation


# ACCESS

@docs name, analyzer

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node exposing (Node)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)
import Lint.Internal.Accumulate exposing (accumulateList)
import Lint.Internal.DeclarationVisitor as DeclarationVisitor


{-| Shortcut to a lint rule
-}
type Rule
    = Rule
        { name : String
        , analyzer : File -> List Error
        }


{-| Represents a `rule` that will be enforced.

        import Lint.Rule as Rule

        rule : Rule
        rule =
            Rule.newSchema "NoDebug"
                |> Rule.withExpressionVisitor expressionVisitor
                |> Rule.fromSchema

-}
type Schema context
    = Schema
        { name : String
        , initialContext : context
        , moduleDefinitionVisitor : Node Module -> context -> ( List Error, context )
        , importVisitor : Node Import -> context -> ( List Error, context )
        , expressionVisitor : Node Expression -> Direction -> context -> ( List Error, context )
        , declarationVisitor : Node Declaration -> Direction -> context -> ( List Error, context )
        , finalEvaluationFn : context -> List Error
        }


{-| Creates a new schema for a rule. Will require calling `fromSchema` to create a usable `Rule`.

        import Lint.Rule as Rule

        rule : Rule
        rule =
                |> Rule.withExpressionVisitor expressionVisitor
            Rule.newSchema "NoDebug"
                |> Rule.fromSchema

-}
newSchema : String -> Schema ()
newSchema name_ =
    Schema
        { name = name_
        , initialContext = ()
        , moduleDefinitionVisitor = \node context -> ( [], context )
        , importVisitor = \node context -> ( [], context )
        , expressionVisitor = \direction node context -> ( [], context )
        , declarationVisitor = \direction node context -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


fromSchema : Schema context -> Rule
fromSchema (Schema schema) =
    Rule
        { name = schema.name
        , analyzer =
            \file ->
                schema.initialContext
                    |> schema.moduleDefinitionVisitor file.moduleDefinition
                    |> accumulateList schema.importVisitor file.imports
                    |> accumulateList (DeclarationVisitor.visit schema.declarationVisitor schema.expressionVisitor) file.declarations
                    |> makeFinalEvaluation schema.finalEvaluationFn
                    |> List.reverse
        }


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluation : (context -> List Error) -> ( List Error, context ) -> List Error
makeFinalEvaluation finalEvaluationFn ( previousErrors, previousContext ) =
    finalEvaluationFn previousContext
        ++ previousErrors



-- RULES WITH ANALYSIS


withInitialContext : context -> Schema () -> Schema context
withInitialContext initialContext_ (Schema schema) =
    Schema
        { name = schema.name
        , initialContext = initialContext_
        , moduleDefinitionVisitor = \node context -> ( [], context )
        , importVisitor = \node context -> ( [], context )
        , expressionVisitor = \node direction context -> ( [], context )
        , declarationVisitor = \node direction context -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> Schema context -> Schema context
withSimpleModuleDefinitionVisitor visitor (Schema schema) =
    Schema { schema | moduleDefinitionVisitor = \node context -> ( visitor node, context ) }


withSimpleImportVisitor : (Node Import -> List Error) -> Schema context -> Schema context
withSimpleImportVisitor visitor (Schema schema) =
    Schema { schema | importVisitor = \node context -> ( visitor node, context ) }


withSimpleExpressionVisitor : (Node Expression -> List Error) -> Schema context -> Schema context
withSimpleExpressionVisitor visitor (Schema schema) =
    Schema
        { schema
            | expressionVisitor =
                \node direction context ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }


withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> Schema context -> Schema context
withSimpleDeclarationVisitor visitor (Schema schema) =
    Schema
        { schema
            | declarationVisitor =
                \node direction context ->
                    case direction of
                        Direction.Enter ->
                            ( visitor node, context )

                        Direction.Exit ->
                            ( [], context )
        }


withModuleDefinitionVisitor : (Node Module -> context -> ( List Error, context )) -> Schema context -> Schema context
withModuleDefinitionVisitor visitor (Schema schema) =
    Schema { schema | moduleDefinitionVisitor = visitor }


withImportVisitor : (Node Import -> context -> ( List Error, context )) -> Schema context -> Schema context
withImportVisitor visitor (Schema schema) =
    Schema { schema | importVisitor = visitor }


withExpressionVisitor : (Node Expression -> Direction -> context -> ( List Error, context )) -> Schema context -> Schema context
withExpressionVisitor visitor (Schema schema) =
    Schema { schema | expressionVisitor = visitor }


withDeclarationVisitor : (Node Declaration -> Direction -> context -> ( List Error, context )) -> Schema context -> Schema context
withDeclarationVisitor visitor (Schema schema) =
    Schema { schema | declarationVisitor = visitor }


withFinalEvaluation : (context -> List Error) -> Schema context -> Schema context
withFinalEvaluation visitor (Schema schema) =
    Schema { schema | finalEvaluationFn = visitor }



-- ACCESS


name : Rule -> String
name (Rule rule) =
    rule.name


analyzer : Rule -> (File -> List Error)
analyzer (Rule rule) =
    rule.analyzer
