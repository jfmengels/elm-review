module Elm.Syntax.File.Extra exposing (moduleName)

import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module as Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node


moduleName : File -> ModuleName
moduleName file =
    file.moduleDefinition
        |> Node.value
        |> Module.moduleName
