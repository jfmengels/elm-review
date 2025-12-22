module Fn.Debug exposing (..)

import Elm.Syntax.ModuleName


log : ( Elm.Syntax.ModuleName.ModuleName, String )
log =
    ( [ "Debug" ], "log" )


toString : ( Elm.Syntax.ModuleName.ModuleName, String )
toString =
    ( [ "Debug" ], "toString" )


todo : ( Elm.Syntax.ModuleName.ModuleName, String )
todo =
    ( [ "Debug" ], "todo" )
