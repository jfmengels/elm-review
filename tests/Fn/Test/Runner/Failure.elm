module Fn.Test.Runner.Failure exposing (..)

import Elm.Syntax.ModuleName


emptyListVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
emptyListVariant =
    ( [ "Test", "Runner", "Failure" ], "EmptyList" )


nonpositiveFuzzCountVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
nonpositiveFuzzCountVariant =
    ( [ "Test", "Runner", "Failure" ], "NonpositiveFuzzCount" )


invalidFuzzerVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
invalidFuzzerVariant =
    ( [ "Test", "Runner", "Failure" ], "InvalidFuzzer" )


badDescriptionVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
badDescriptionVariant =
    ( [ "Test", "Runner", "Failure" ], "BadDescription" )


duplicatedNameVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
duplicatedNameVariant =
    ( [ "Test", "Runner", "Failure" ], "DuplicatedName" )


distributionInsufficientVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionInsufficientVariant =
    ( [ "Test", "Runner", "Failure" ], "DistributionInsufficient" )


distributionBugVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionBugVariant =
    ( [ "Test", "Runner", "Failure" ], "DistributionBug" )


customVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
customVariant =
    ( [ "Test", "Runner", "Failure" ], "Custom" )


equalityVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
equalityVariant =
    ( [ "Test", "Runner", "Failure" ], "Equality" )


comparisonVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
comparisonVariant =
    ( [ "Test", "Runner", "Failure" ], "Comparison" )


listDiffVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
listDiffVariant =
    ( [ "Test", "Runner", "Failure" ], "ListDiff" )


collectionDiffVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
collectionDiffVariant =
    ( [ "Test", "Runner", "Failure" ], "CollectionDiff" )


tODOVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
tODOVariant =
    ( [ "Test", "Runner", "Failure" ], "TODO" )


invalidVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
invalidVariant =
    ( [ "Test", "Runner", "Failure" ], "Invalid" )
