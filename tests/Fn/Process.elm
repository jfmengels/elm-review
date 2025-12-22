module Fn.Process exposing (..)

import Elm.Syntax.ModuleName


kill : ( Elm.Syntax.ModuleName.ModuleName, String )
kill =
    ( [ "Process" ], "kill" )


sleep : ( Elm.Syntax.ModuleName.ModuleName, String )
sleep =
    ( [ "Process" ], "sleep" )


spawn : ( Elm.Syntax.ModuleName.ModuleName, String )
spawn =
    ( [ "Process" ], "spawn" )
