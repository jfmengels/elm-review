module Lint.Internal.Visit exposing (applyRule)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node exposing (Node, value)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)
import Lint.Internal.Accumulate exposing (accumulateList)
import Lint.Internal.DeclarationVisitor as DeclarationVisitor
import Lint.Rule2 as Rule exposing (Rule2)


{-| Applies a `Rule` on a file, and returns the list of errors that it found.
-}
applyRule : Rule2 context -> File -> List Error
applyRule rule file =
    Rule.initialContext rule
        |> Rule.evaluateModuleDefinition rule file.moduleDefinition
        |> accumulateList (Rule.evaluateImport rule) file.imports
        |> accumulateList (DeclarationVisitor.visit rule) file.declarations
        |> makeFinalEvaluation rule
        |> List.reverse


{-| Concatenate the errors of the previous step and of the last step.
-}
makeFinalEvaluation : Rule2 context -> ( List Error, context ) -> List Error
makeFinalEvaluation rule ( previousErrors, previousContext ) =
    Rule.finalEvaluation rule previousContext
        ++ previousErrors
