module LintConfig exposing (config)

import Lint.Rule exposing (Rule, Severity(..))
import Lint.Rule.DefaultPatternPosition
import Lint.Rule.ElmTest.NoDuplicateTestBodies
import Lint.Rule.NoConstantCondition
import Lint.Rule.NoDebug
import Lint.Rule.NoDuplicateImports
import Lint.Rule.NoExposingEverything
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoNestedLet
import Lint.Rule.NoUnannotatedFunction
import Lint.Rule.NoUnusedVariables
import Lint.Rule.NoUselessIf
import Lint.Rule.NoUselessPatternMatching
import Lint.Rule.NoWarningComments
import Lint.Rule.SimplifyPiping
import Lint.Rule.SimplifyPropertyAccess


config : List ( Severity, Rule )
config =
    [ ( Critical, Lint.Rule.DefaultPatternPosition.rule { position = Lint.Rule.DefaultPatternPosition.Last } )
    , ( Critical, Lint.Rule.NoExtraBooleanComparison.rule )
    , ( Critical, Lint.Rule.NoConstantCondition.rule )
    , ( Critical, Lint.Rule.NoDebug.rule )
    , ( Critical, Lint.Rule.NoDuplicateImports.rule )
    , ( Critical, Lint.Rule.NoExposingEverything.rule )
    , ( Critical, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html", "Ast.Expression", "Ast.Statement" ] } )
    , ( Critical, Lint.Rule.NoNestedLet.rule )
    , ( Critical, Lint.Rule.NoUnannotatedFunction.rule )
    , ( Critical, Lint.Rule.NoUnusedVariables.rule )
    , ( Critical, Lint.Rule.NoUselessIf.rule )
    , ( Critical, Lint.Rule.NoUselessPatternMatching.rule )
    , ( Warning, Lint.Rule.NoWarningComments.rule )
    , ( Critical, Lint.Rule.SimplifyPiping.rule )
    , ( Critical, Lint.Rule.SimplifyPropertyAccess.rule )
    , ( Critical, Lint.Rule.ElmTest.NoDuplicateTestBodies.rule )
    ]
