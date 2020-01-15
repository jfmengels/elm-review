module Review.File exposing (ParsedFile, RawFile)

{-| TODO Documentation
-}

import Elm.Syntax.File


type alias RawFile =
    { path : String
    , source : String
    , ast : Maybe Elm.Syntax.File.File
    }


type alias ParsedFile =
    { path : String
    , source : String
    , ast : Elm.Syntax.File.File
    }
