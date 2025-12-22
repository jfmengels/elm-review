module Fn.Platform.Cmd exposing (..)

import Elm.Syntax.ModuleName


batch : ( Elm.Syntax.ModuleName.ModuleName, String )
batch =
    ( [ "Platform", "Cmd" ], "batch" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Platform", "Cmd" ], "map" )


none : ( Elm.Syntax.ModuleName.ModuleName, String )
none =
    ( [ "Platform", "Cmd" ], "none" )
