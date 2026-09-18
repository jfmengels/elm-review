module Elm.Syntax.FullModuleName exposing
    ( FullModuleName
    , fromDotted
    , fromModuleName
    , fromModuleName_
    , toModuleName
    , toString
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import NonemptyList exposing (NonemptyList)


type alias FullModuleName =
    NonemptyList String


{-|

    ["Platform","Cmd"] -> Just ("Platform", ["Cmd"])
    [] -> Nothing

-}
fromModuleName : ModuleName -> Maybe FullModuleName
fromModuleName moduleName =
    NonemptyList.fromList moduleName


{-|

    "Foo" -> ("Foo", [])

-}
fromString : String -> FullModuleName
fromString string =
    NonemptyList.singleton string


{-|

    ["Platform","Cmd"] -> ("Platform", ["Cmd"])
    [] -> ("<BUG> The file didn't have a proper module name", [])

-}
fromModuleName_ : ModuleName -> FullModuleName
fromModuleName_ moduleName =
    moduleName
        |> fromModuleName
        |> Maybe.withDefault (fromString "<BUG> The file didn't have a proper module name")


{-|

    "Platform.Cmd" -> ("Platform", ["Cmd"])

-}
fromDotted : String -> FullModuleName
fromDotted dotted =
    dotted
        |> String.split "."
        |> fromModuleName_


{-|

    ("Platform", ["Cmd"]) -> ["Platform","Cmd"]

-}
toModuleName : FullModuleName -> ModuleName
toModuleName fullModuleName =
    NonemptyList.toList fullModuleName


{-|

    ("Platform", ["Cmd"]) -> "Platform.Cmd"

-}
toString : FullModuleName -> String
toString moduleName =
    moduleName
        |> NonemptyList.toList
        |> String.join "."
