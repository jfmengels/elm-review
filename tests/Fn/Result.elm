module Fn.Result exposing (..)

import Elm.Syntax.ModuleName


andThen : ( Elm.Syntax.ModuleName.ModuleName, String )
andThen =
    ( [ "Result" ], "andThen" )


fromMaybe : ( Elm.Syntax.ModuleName.ModuleName, String )
fromMaybe =
    ( [ "Result" ], "fromMaybe" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Result" ], "map" )


map2 : ( Elm.Syntax.ModuleName.ModuleName, String )
map2 =
    ( [ "Result" ], "map2" )


map3 : ( Elm.Syntax.ModuleName.ModuleName, String )
map3 =
    ( [ "Result" ], "map3" )


map4 : ( Elm.Syntax.ModuleName.ModuleName, String )
map4 =
    ( [ "Result" ], "map4" )


map5 : ( Elm.Syntax.ModuleName.ModuleName, String )
map5 =
    ( [ "Result" ], "map5" )


mapError : ( Elm.Syntax.ModuleName.ModuleName, String )
mapError =
    ( [ "Result" ], "mapError" )


toMaybe : ( Elm.Syntax.ModuleName.ModuleName, String )
toMaybe =
    ( [ "Result" ], "toMaybe" )


withDefault : ( Elm.Syntax.ModuleName.ModuleName, String )
withDefault =
    ( [ "Result" ], "withDefault" )


okVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
okVariant =
    ( [ "Result" ], "Ok" )


errVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
errVariant =
    ( [ "Result" ], "Err" )
