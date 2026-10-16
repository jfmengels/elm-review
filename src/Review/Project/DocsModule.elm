module Review.Project.DocsModule exposing
    ( DocsModule
    , create
    , hash
    , mod
    )

import Elm.Docs
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type DocsModule
    = DocsModule (List (ContextHash Elm.Docs.Module)) (ContextHash Elm.Docs.Module) Elm.Docs.Module


create : List (ContextHash Elm.Docs.Module) -> Elm.Docs.Module -> DocsModule
create inputHashes mod_ =
    DocsModule inputHashes (ContextHash.create mod_) mod_


mod : DocsModule -> Elm.Docs.Module
mod (DocsModule _ _ mod_) =
    mod_


hash : DocsModule -> ContextHash Elm.Docs.Module
hash (DocsModule _ contextHash _) =
    contextHash
