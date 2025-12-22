module Fn.Platform exposing (..)

import Elm.Syntax.ModuleName


sendToApp : ( Elm.Syntax.ModuleName.ModuleName, String )
sendToApp =
    ( [ "Platform" ], "sendToApp" )


sendToSelf : ( Elm.Syntax.ModuleName.ModuleName, String )
sendToSelf =
    ( [ "Platform" ], "sendToSelf" )


worker : ( Elm.Syntax.ModuleName.ModuleName, String )
worker =
    ( [ "Platform" ], "worker" )
