module ApiDesign exposing
    ( Rule, Schema
    , newSchema, fromSchema
    , withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor
    , withInitialContext, withModuleDefinitionVisitor, withImportVisitor, Direction(..), withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalEvaluation
    , withElmJsonVisitor
    , withFixes
    , newMultiSchema, withProjectWideElmJsonVisitor
    , Error, error, errorMessage, errorDetails, errorRange, errorFixes
    , name, Analyzer(..)
    )

{-| Api Design

Problems to fix:

  - [ ] Be able to have a list containing single file and multi-file rules.
    Users should not have to care if the rule is for a single or multiple files
  - [ ] Be able to create a multi-file rule
  - [ ] Be able to specify a multi-file rule's file visitor in a nice way
      - [ ] Using the same functions as for a single rule
      - [ ] Forbid specifying a name
      - [ ] Forbid specifying an elmJsonVisitor
      - [ ] Figure if it makes sense to have a finalEvaluationFn, and potentially forbid it?
  - [ ] Be able to run both types of rules and get a list of errors
  - [ ] Find great type and functions names
  - [ ] Folding context
      - [ ] Make a nice API for when the multi-file context is different as the file visitor's
      - [ ] Make a nice API for when the multi-file context is the same as the file visitor's
  - [ ] Add a way to test multi-file rules
      - [ ] Make sure that the order does not matter by running a rule several
        times with a different order for the files every time.

Errors

  - [ ] Define a way to report errors in other files?
      - A FileKey/FileId similar to a Navigation.Key? It has no useful meaning, but
        makes it so you can't give an error to a non-existing file or file you haven't visited.
      - Needed
  - [ ] Define a way to report errors in elm.json?
  - [ ] Get rid of Review.Error
      - [ ] Need to be able to create an error without a file in Review.Rule

Renaming

  - [ ] Have the current package be more multi-purpose?
    It paves the way for other potential tools, like codemods, code crawlers
    (to gather data and do something with it, like code generation).


## Definition

@docs Rule, Schema


## Creating a Rule

@docs newSchema, fromSchema


## Builder functions without context

@docs withSimpleModuleDefinitionVisitor, withSimpleImportVisitor, withSimpleDeclarationVisitor, withSimpleExpressionVisitor


## Builder functions with context

@docs withInitialContext, withModuleDefinitionVisitor, withImportVisitor, Direction, withDeclarationVisitor, withDeclarationListVisitor, withExpressionVisitor, withFinalEvaluation


## Builder functions to analyze the project's data

@docs withElmJsonVisitor


## Automatic fixing

For more information on automatic fixing, read the documentation for [`Review.Fix`](./Review-Fix).

@docs withFixes


## Project-wide rules

@docs newMultiSchema, withProjectWideElmJsonVisitor


## Errors

@docs Error, error, errorMessage, errorDetails, errorRange, errorFixes


# ACCESS

@docs name, Analyzer

-}

import Elm.Project
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix exposing (Fix)
import Review.Project exposing (Project)


type Rule
    = Rule
        { name : String
        , analyzer : Analyzer
        }


type Analyzer
    = Single (Project -> File -> List Error)
    | Multi (Project -> List File -> List Error) -- List ( File, List Error )?


type Direction
    = OnEnter
    | OnExit


type Error
    = Error
        { message : String
        , details : List String
        , range : Range
        , fixes : Maybe (List Fix)
        }


type Schema configurationState context
    = Schema
        { name : String
        , initialContext : context
        , elmJsonVisitor : Maybe Elm.Project.Project -> context -> context
        , moduleDefinitionVisitor : Node Module -> context -> ( List Error, context )
        , importVisitor : Node Import -> context -> ( List Error, context )
        , declarationListVisitor : List (Node Declaration) -> context -> ( List Error, context )
        , declarationVisitor : Node Declaration -> Direction -> context -> ( List Error, context )
        , expressionVisitor : Node Expression -> Direction -> context -> ( List Error, context )
        , finalEvaluationFn : context -> List Error
        }


type MultiSchema context
    = MultiSchema
        { name : String
        , initialContext : context
        , elmJsonVisitor : Maybe (Maybe Elm.Project.Project -> context -> context)
        , fileVisitor : SimpleSchema context
        , finalEvaluationFn : context -> List Error
        }


newMultiSchema :
    String
    ->
        { initialContext : context
        , fileVisitor : SimpleSchema context
        , mergeContexts : context -> context -> context
        , finalEvaluation : context -> List Error
        }
    -> MultiSchema context
newMultiSchema name_ { initialContext, fileVisitor, mergeContexts, finalEvaluation } =
    MultiSchema
        { name = name_
        , initialContext = initialContext
        , elmJsonVisitor = Nothing
        , fileVisitor = fileVisitor
        , finalEvaluationFn = finalEvaluation
        }


withProjectWideElmJsonVisitor : (Maybe Elm.Project.Project -> context -> context) -> MultiSchema context -> MultiSchema context
withProjectWideElmJsonVisitor visitor (MultiSchema schema) =
    MultiSchema { schema | elmJsonVisitor = Just visitor }


type SimpleSchema context
    = SimpleSchema
        { moduleDefinitionVisitor : Node Module -> context -> ( List Error, context )
        , importVisitor : Node Import -> context -> ( List Error, context )
        , declarationListVisitor : List (Node Declaration) -> context -> ( List Error, context )
        , declarationVisitor : Node Declaration -> Direction -> context -> ( List Error, context )
        , expressionVisitor : Node Expression -> Direction -> context -> ( List Error, context )
        , finalEvaluationFn : context -> List Error
        }


newSchema : String -> Schema { hasNoVisitor : () } ()
newSchema name_ =
    Schema
        { name = name_
        , initialContext = ()
        , elmJsonVisitor = \elmJson context -> context
        , moduleDefinitionVisitor = \node context -> ( [], context )
        , importVisitor = \node context -> ( [], context )
        , declarationListVisitor = \declarationNodes context -> ( [], context )
        , declarationVisitor = \node direction context -> ( [], context )
        , expressionVisitor = \node direction context -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


fromSchema : Schema { hasAtLeastOneVisitor : () } context -> Rule
fromSchema (Schema schema) =
    Rule
        { name = schema.name
        , analyzer =
            Single
                (\project file ->
                    Debug.todo ""
                )
        }


withSimpleModuleDefinitionVisitor : (Node Module -> List Error) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withSimpleModuleDefinitionVisitor visitor (Schema schema) =
    Schema { schema | moduleDefinitionVisitor = \node context -> ( visitor node, context ) }


withSimpleImportVisitor : (Node Import -> List Error) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withSimpleImportVisitor visitor (Schema schema) =
    Schema { schema | importVisitor = \node context -> ( visitor node, context ) }


withSimpleDeclarationVisitor : (Node Declaration -> List Error) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withSimpleDeclarationVisitor visitor (Schema schema) =
    Schema
        { schema
            | declarationVisitor =
                \node direction context ->
                    case direction of
                        OnEnter ->
                            ( visitor node, context )

                        OnExit ->
                            ( [], context )
        }


withSimpleExpressionVisitor : (Node Expression -> List Error) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withSimpleExpressionVisitor visitor (Schema schema) =
    Schema
        { schema
            | expressionVisitor =
                \node direction context ->
                    case direction of
                        OnEnter ->
                            ( visitor node, context )

                        OnExit ->
                            ( [], context )
        }


withInitialContext : context -> Schema { hasNoVisitor : () } () -> Schema { hasNoVisitor : () } context
withInitialContext initialContext_ (Schema schema) =
    Schema
        { name = schema.name
        , initialContext = initialContext_
        , elmJsonVisitor = \elmJson context -> context
        , moduleDefinitionVisitor = \node context -> ( [], context )
        , importVisitor = \node context -> ( [], context )
        , declarationListVisitor = \declarationNodes context -> ( [], context )
        , declarationVisitor = \node direction context -> ( [], context )
        , expressionVisitor = \node direction context -> ( [], context )
        , finalEvaluationFn = \context -> []
        }


withElmJsonVisitor : (Maybe Elm.Project.Project -> context -> context) -> Schema anything context -> Schema anything context
withElmJsonVisitor visitor (Schema schema) =
    Schema { schema | elmJsonVisitor = visitor }


withModuleDefinitionVisitor : (Node Module -> context -> ( List Error, context )) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withModuleDefinitionVisitor visitor (Schema schema) =
    Schema { schema | moduleDefinitionVisitor = visitor }


withImportVisitor : (Node Import -> context -> ( List Error, context )) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withImportVisitor visitor (Schema schema) =
    Schema { schema | importVisitor = visitor }


withDeclarationVisitor : (Node Declaration -> Direction -> context -> ( List Error, context )) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withDeclarationVisitor visitor (Schema schema) =
    Schema { schema | declarationVisitor = visitor }


withDeclarationListVisitor : (List (Node Declaration) -> context -> ( List Error, context )) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withDeclarationListVisitor visitor (Schema schema) =
    Schema { schema | declarationListVisitor = visitor }


withExpressionVisitor : (Node Expression -> Direction -> context -> ( List Error, context )) -> Schema anything context -> Schema { hasAtLeastOneVisitor : () } context
withExpressionVisitor visitor (Schema schema) =
    Schema { schema | expressionVisitor = visitor }


withFinalEvaluation : (context -> List Error) -> Schema { hasAtLeastOneVisitor : () } context -> Schema { hasAtLeastOneVisitor : () } context
withFinalEvaluation visitor (Schema schema) =
    Schema { schema | finalEvaluationFn = visitor }


withFixes : List Fix -> Error -> Error
withFixes fixes (Error err) =
    if List.isEmpty fixes then
        Error { err | fixes = Nothing }

    else
        Error { err | fixes = Just fixes }



-- ERROR


error : { message : String, details : List String } -> Range -> Error
error { message, details } range =
    Error
        { message = message
        , details = details
        , range = range
        , fixes = Nothing
        }


errorMessage : Error -> String
errorMessage (Error err) =
    err.message


errorDetails : Error -> List String
errorDetails (Error err) =
    err.details


errorRange : Error -> Range
errorRange (Error err) =
    err.range


errorFixes : Error -> Maybe (List Fix)
errorFixes (Error err) =
    err.fixes



-- ACCESS


name : Rule -> String
name (Rule rule) =
    rule.name
