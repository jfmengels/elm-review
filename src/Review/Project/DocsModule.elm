module Review.Project.DocsModule exposing (DocsModule, create, mod)

import Elm.Docs


type DocsModule
    = DocsModule Elm.Docs.Module


create : Elm.Docs.Module -> DocsModule
create =
    DocsModule


mod : DocsModule -> Elm.Docs.Module
mod (DocsModule mod_) =
    mod_
