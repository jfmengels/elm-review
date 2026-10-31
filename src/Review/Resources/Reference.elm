module Review.Resources.Reference exposing
    ( Reference
    , qualifier
    , range
    , target
    )

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import Review.Resources.ScopeInternal as Internal


type alias Reference =
    Internal.Reference


range : Reference -> Range
range (Internal.Reference reference) =
    reference.range


qualifier : Reference -> ModuleName
qualifier (Internal.Reference reference) =
    reference.qualifier


target : Internal.Reference -> Internal.Target
target (Internal.Reference reference) =
    reference.target
