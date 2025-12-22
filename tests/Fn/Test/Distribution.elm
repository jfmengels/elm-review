module Fn.Test.Distribution exposing (..)

import Elm.Syntax.ModuleName


atLeast : ( Elm.Syntax.ModuleName.ModuleName, String )
atLeast =
    ( [ "Test", "Distribution" ], "atLeast" )


distributionReportTable : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionReportTable =
    ( [ "Test", "Distribution" ], "distributionReportTable" )


moreThanZero : ( Elm.Syntax.ModuleName.ModuleName, String )
moreThanZero =
    ( [ "Test", "Distribution" ], "moreThanZero" )


zero : ( Elm.Syntax.ModuleName.ModuleName, String )
zero =
    ( [ "Test", "Distribution" ], "zero" )


noDistributionVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
noDistributionVariant =
    ( [ "Test", "Distribution" ], "NoDistribution" )


distributionToReportVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionToReportVariant =
    ( [ "Test", "Distribution" ], "DistributionToReport" )


distributionCheckSucceededVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionCheckSucceededVariant =
    ( [ "Test", "Distribution" ], "DistributionCheckSucceeded" )


distributionCheckFailedVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
distributionCheckFailedVariant =
    ( [ "Test", "Distribution" ], "DistributionCheckFailed" )
