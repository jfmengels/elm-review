module Elm.Syntax.ModuleName.Extra exposing
    ( dottedToFilePath
    , fromDotted
    , isSegment
    , splitLastDot
    , toString
    )

import Elm.Syntax.ModuleName exposing (ModuleName)


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


{-|

    "Foo" -> True
    "foo" -> False
    "" -> False

-}
isSegment : String -> Bool
isSegment segment =
    case String.toList segment of
        first :: _ ->
            Char.isUpper first

        [] ->
            False


{-|

    "Platform.Cmd.Cmd" -> ("Platform.Cmd", "Cmd")
    "Int" -> ("", "Int")

-}
splitLastDot : String -> ( String, String )
splitLastDot qualifiedName =
    case List.reverse (fromDotted qualifiedName) of
        [] ->
            ( "", qualifiedName )

        [ single ] ->
            ( "", single )

        last :: rest ->
            ( toString (List.reverse rest), last )
