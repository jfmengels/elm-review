module Review.Resources.Target exposing (Target(..))

import Elm.Syntax.ModuleName exposing (ModuleName)
import Review.Resources.Binding exposing (Binding)
import Review.Resources.ImportBinding exposing (ImportBinding)


type Target
    = LocalBinding Binding
    | ImportBinding_ ImportBinding
    | QualifiedReference { moduleName : ModuleName, name : String }
