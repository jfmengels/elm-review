module Review.Project.DocsModule exposing
    ( DocsModule
    , create
    , hash
    , mod
    )

import Elm.Docs
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type DocsModule
    = DocsModule
        { docsModule : Elm.Docs.Module
        , docsModuleHash : ContextHash Elm.Docs.Module
        , importDocsModuleHashes : List (ContextHash Elm.Docs.Module)
        }


create : List (ContextHash Elm.Docs.Module) -> Elm.Docs.Module -> DocsModule
create importDocsModuleHashes docsModule =
    DocsModule
        { docsModule = docsModule
        , docsModuleHash = ContextHash.create docsModule
        , importDocsModuleHashes = importDocsModuleHashes
        }


mod : DocsModule -> Elm.Docs.Module
mod (DocsModule d) =
    d.docsModule


hash : DocsModule -> ContextHash Elm.Docs.Module
hash (DocsModule d) =
    d.docsModuleHash
