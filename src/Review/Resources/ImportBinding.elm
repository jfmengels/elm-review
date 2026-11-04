module Review.Resources.ImportBinding exposing
    ( ImportBinding
    , id
    , moduleName
    , name
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Resources.BindingId as BindingId
import Review.Resources.Internal.Bindings as Internal


type alias ImportBinding =
    Internal.ImportBinding


id : ImportBinding -> BindingId.Id
id (Internal.ImportBinding binding) =
    binding.id


name : ImportBinding -> String
name (Internal.ImportBinding binding) =
    binding.name


moduleName : ImportBinding -> ModuleName
moduleName (Internal.ImportBinding binding) =
    binding.moduleName
