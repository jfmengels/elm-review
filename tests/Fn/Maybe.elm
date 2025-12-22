module Fn.Maybe exposing (..)

import Elm.Syntax.ModuleName


andThen : ( Elm.Syntax.ModuleName.ModuleName, String )
andThen =
    ( [ "Maybe" ], "andThen" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Maybe" ], "map" )


map2 : ( Elm.Syntax.ModuleName.ModuleName, String )
map2 =
    ( [ "Maybe" ], "map2" )


map3 : ( Elm.Syntax.ModuleName.ModuleName, String )
map3 =
    ( [ "Maybe" ], "map3" )


map4 : ( Elm.Syntax.ModuleName.ModuleName, String )
map4 =
    ( [ "Maybe" ], "map4" )


map5 : ( Elm.Syntax.ModuleName.ModuleName, String )
map5 =
    ( [ "Maybe" ], "map5" )


withDefault : ( Elm.Syntax.ModuleName.ModuleName, String )
withDefault =
    ( [ "Maybe" ], "withDefault" )


justVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
justVariant =
    ( [ "Maybe" ], "Just" )


nothingVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
nothingVariant =
    ( [ "Maybe" ], "Nothing" )
