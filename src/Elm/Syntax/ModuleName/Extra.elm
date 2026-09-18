module Elm.Syntax.ModuleName.Extra exposing (toString)

import Elm.Syntax.ModuleName exposing (ModuleName)


{-|

    ["Foo", "Bar"] -> "Foo.Bar"
    ["Foo"] -> "Foo"
    [] -> ""

-}
toString : ModuleName -> String
toString moduleName =
    String.join "." moduleName
