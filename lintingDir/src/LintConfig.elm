module LintConfig exposing (config)

import Lint.Types exposing (LintRule)
import Lint.Rules.DefaultPatternPosition
import Lint.Rules.NoConstantCondition
import Lint.Rules.NoDebug
import Lint.Rules.NoDuplicateImports
import Lint.Rules.NoExposingEverything
import Lint.Rules.NoImportingEverything
import Lint.Rules.NoNestedLet
import Lint.Rules.NoUnannotatedFunction
import Lint.Rules.NoUnusedVariables
import Lint.Rules.NoUselessIf
import Lint.Rules.NoUselessPatternMatching
import Lint.Rules.NoWarningComments
import Lint.Rules.SimplifyPiping
import Lint.Rules.SimplifyPropertyAccess


config : List LintRule
config =
    [ Lint.Rules.DefaultPatternPosition.rule { position = Lint.Rules.DefaultPatternPosition.Last }
    , Lint.Rules.NoConstantCondition.rule
    , Lint.Rules.NoDebug.rule
    , Lint.Rules.NoDuplicateImports.rule
    , Lint.Rules.NoExposingEverything.rule
    , Lint.Rules.NoImportingEverything.rule
    , Lint.Rules.NoNestedLet.rule
    , Lint.Rules.NoUnannotatedFunction.rule
    , Lint.Rules.NoUnusedVariables.rule
    , Lint.Rules.NoUselessIf.rule
    , Lint.Rules.NoUselessPatternMatching.rule
    , Lint.Rules.NoWarningComments.rule
    , Lint.Rules.SimplifyPiping.rule
    , Lint.Rules.SimplifyPropertyAccess.rule
    ]
