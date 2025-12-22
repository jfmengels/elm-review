module Fn.Test.Runner exposing (..)

import Elm.Syntax.ModuleName


formatLabels : ( Elm.Syntax.ModuleName.ModuleName, String )
formatLabels =
    ( [ "Test", "Runner" ], "formatLabels" )


fromTest : ( Elm.Syntax.ModuleName.ModuleName, String )
fromTest =
    ( [ "Test", "Runner" ], "fromTest" )


fuzz : ( Elm.Syntax.ModuleName.ModuleName, String )
fuzz =
    ( [ "Test", "Runner" ], "fuzz" )


getDistributionReport : ( Elm.Syntax.ModuleName.ModuleName, String )
getDistributionReport =
    ( [ "Test", "Runner" ], "getDistributionReport" )


getFailureReason : ( Elm.Syntax.ModuleName.ModuleName, String )
getFailureReason =
    ( [ "Test", "Runner" ], "getFailureReason" )


isTodo : ( Elm.Syntax.ModuleName.ModuleName, String )
isTodo =
    ( [ "Test", "Runner" ], "isTodo" )


simplify : ( Elm.Syntax.ModuleName.ModuleName, String )
simplify =
    ( [ "Test", "Runner" ], "simplify" )


plainVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
plainVariant =
    ( [ "Test", "Runner" ], "Plain" )


onlyVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
onlyVariant =
    ( [ "Test", "Runner" ], "Only" )


skippingVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
skippingVariant =
    ( [ "Test", "Runner" ], "Skipping" )


invalidVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
invalidVariant =
    ( [ "Test", "Runner" ], "Invalid" )
