module Review.FileParser exposing (parse)

import Elm.Parser as Parser
import Elm.Syntax.File exposing (File)


{-| Parse source code into a AST.
-}
parse : String -> Result () File
parse source =
    case Parser.parseToFile source of
        Ok file ->
            Ok file

        Err _ ->
            Err ()
