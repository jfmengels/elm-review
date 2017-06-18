port module Main exposing (..)

import Test exposing (describe, Test)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import DefaultPatternPositionTest
import NoConstantConditionTest
import NoDebugTest
import NoDuplicateImportsTest
import NoImportingEverythingTest
import NoNestedLetTest
import NoUnannotatedFunctionTest
import NoUnusedVariablesTest
import NoUselessIfTest
import NoUselessPatternMatchingTest
import NoWarningCommentsTest
import SimplifyPipingTest
import SimplifyPropertyAccessTest


main : Test.Runner.Node.TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg


all : Test
all =
    describe "Rules"
        [ DefaultPatternPositionTest.all
        , NoConstantConditionTest.all
        , NoDebugTest.all
        , NoDuplicateImportsTest.all
        , NoImportingEverythingTest.all
        , NoNestedLetTest.all
        , NoUnannotatedFunctionTest.all
        , NoUnusedVariablesTest.all
        , NoUselessIfTest.all
        , NoUselessPatternMatchingTest.all
        , NoWarningCommentsTest.all
        , SimplifyPipingTest.all
        , SimplifyPropertyAccessTest.all
        ]
