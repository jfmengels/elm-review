port module LintConfig exposing (..)

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


-- import Lint.Runner.Node exposing (runLint)

import Test exposing (describe, Test)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


-- Needs to be put into main somehow


files : List String
files =
    [ "src"
    ]


main : Test.Runner.Node.TestProgram
main =
    run emit all


all : Test
all =
    describe "Visitors" []


port emit : ( String, Value ) -> Cmd msg


rules : List (String -> List LintRule)
rules =
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
