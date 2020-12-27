module Review.ModuleInformation exposing (ModuleInformation, empty)

import Elm.Docs


type alias ModuleInformation =
    Elm.Docs.Module


empty : List String -> ModuleInformation
empty moduleName =
    { name = String.join "." moduleName
    , comment = ""
    , unions = []
    , values = []
    , aliases = []
    , binops = []
    }
