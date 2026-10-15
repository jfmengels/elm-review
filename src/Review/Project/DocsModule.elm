module Review.Project.DocsModule exposing (DocsModule, create, mod)

import Elm.Docs
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type DocsModule
    = DocsModule (ContextHash Elm.Docs.Module) Elm.Docs.Module


create : Elm.Docs.Module -> DocsModule
create mod_ =
    DocsModule (ContextHash.create mod_) mod_


mod : DocsModule -> Elm.Docs.Module
mod (DocsModule _ mod_) =
    mod_
