module Elm.Syntax.ModuleName.Extra exposing
    ( dottedToFilePath
    , fromDotted
    , isNotEmpty
    , isSegment
    , splitLastDot
    , toString
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import String.ExtraExtra


{-|

    ["Foo", "Bar"] -> "Foo.Bar"
    ["Foo"] -> "Foo"
    [] -> ""

-}
toString : ModuleName -> String
toString moduleName =
    String.join "." moduleName


{-|

    "Foo.Bar" -> ["Foo", "Bar"]
    "Foo" -> ["Foo"]

-}
fromDotted : String -> ModuleName
fromDotted dotted =
    String.split "." dotted


{-|

    [ "Css", "Internal" ] --> "src/Css/Internal.elm"

-}
toFilePath : ModuleName -> String
toFilePath moduleName =
    "src/" ++ String.join "/" moduleName ++ ".elm"


{-|

    "Css.Internal" --> "src/Css/Internal.elm"

-}
dottedToFilePath : String -> String
dottedToFilePath dotted =
    toFilePath (fromDotted dotted)


isNotEmpty : ModuleName -> Bool
isNotEmpty moduleName =
    case moduleName of
        [] ->
            False

        _ :: _ ->
            True


{-|

    "Foo" -> True
    "foo" -> False
    "" -> False

-}
isSegment : String -> Bool
isSegment =
    String.ExtraExtra.firstCharIsUpper


{-|

    "Platform.Cmd.Cmd" -> ("Platform.Cmd", "Cmd")
    "Int" -> ("", "Int")

-}
splitLastDot : String -> ( String, String )
splitLastDot qualifiedName =
    case List.reverse (String.indexes "." qualifiedName) of
        [] ->
            ( "", qualifiedName )

        lastDot :: _ ->
            ( String.left lastDot qualifiedName
            , String.dropLeft (lastDot + 1) qualifiedName
            )
