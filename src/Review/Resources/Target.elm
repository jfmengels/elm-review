module Review.Resources.Target exposing (Target(..))

import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Resources.Internal.Bindings exposing (Binding, ImportBinding)


type Target
    = LocalBinding Binding
    | ImportBinding_ ImportBinding
    | QualifiedReference { moduleName : ModuleName, name : String }
