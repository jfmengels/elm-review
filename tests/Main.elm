port module Tests exposing (..)

import Test exposing (describe, Test)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import NoDebugTest
import NoUnannotatedFunctionTest
import NoUnusedVariablesTest
import NoDuplicateImportsTest


main : Test.Runner.Node.TestProgram
main =
    run emit all


port emit : ( String, Value ) -> Cmd msg


all : Test
all =
    describe "Visitors"
        [ NoDebugTest.all
        , NoUnannotatedFunctionTest.all
        , NoUnusedVariablesTest.all
        , NoDuplicateImportsTest.all
        ]
