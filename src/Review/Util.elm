module Review.Util exposing (moduleName)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)


moduleName : Node ModuleName -> String
moduleName moduleName_ =
    moduleName_
        |> Node.value
        |> String.join "."
