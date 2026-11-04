module Review.Resources.Binding exposing
    ( Binding
    , id
    , name
    , range
    )

import Elm.Syntax.Range exposing (Range)
import Review.Resources.BindingId as BindingId
import Review.Resources.Internal.Bindings as Internal


type alias Binding =
    Internal.Binding


id : Binding -> BindingId.Id
id (Internal.Binding binding) =
    binding.id


name : Internal.Binding -> String
name (Internal.Binding binding) =
    binding.name


range : Internal.Binding -> Range
range (Internal.Binding binding) =
    binding.range
