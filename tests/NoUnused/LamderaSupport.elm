module NoUnused.LamderaSupport exposing (isLamderaApplication)

import Elm.Package


isLamderaApplication : List ( Elm.Package.Name, b ) -> Bool
isLamderaApplication depsDirect =
    List.any (\( name, _ ) -> Elm.Package.toString name == "lamdera/core") depsDirect
