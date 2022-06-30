module Review.FileParser exposing (parse)

import Elm.Parser as Parser
import Elm.Processing
import Elm.Syntax.File exposing (File)
import Review.Dependencies


{-| Parse source code into a AST.
-}
parse : String -> Result () File
parse source =
    case Parser.parse source of
        Ok file ->
            Ok (Elm.Processing.process elmProcessContext file)

        Err _ ->
            Err ()


elmProcessContext : Elm.Processing.ProcessContext
elmProcessContext =
    Elm.Processing.init
        |> Elm.Processing.addDependency Review.Dependencies.elmCore
        |> Elm.Processing.addDependency Review.Dependencies.elmUrl
        |> Elm.Processing.addDependency Review.Dependencies.elmParser
