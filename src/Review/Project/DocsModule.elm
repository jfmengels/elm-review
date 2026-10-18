module Review.Project.DocsModule exposing
    ( DocsModule
    , create
    , extract
    , hash
    , mod
    )

import Elm.Docs
import Review.Cache.ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type DocsModule
    = DocsModule
        { docsModule : Elm.Docs.Module
        , sourceHash : ContentHash
        , docsModuleHash : ContextHash Elm.Docs.Module
        , importDocsModuleHashes : List (ContextHash Elm.Docs.Module)
        }


create : List (ContextHash Elm.Docs.Module) -> ContentHash -> Elm.Docs.Module -> DocsModule
create importDocsModuleHashes sourceHash docsModule =
    DocsModule
        { docsModule = docsModule
        , sourceHash = sourceHash
        , docsModuleHash = ContextHash.create docsModule
        , importDocsModuleHashes = importDocsModuleHashes
        }


mod : DocsModule -> Elm.Docs.Module
mod (DocsModule d) =
    d.docsModule


hash : DocsModule -> ContextHash Elm.Docs.Module
hash (DocsModule d) =
    d.docsModuleHash


extract : List (ContextHash Elm.Docs.Module) -> ContentHash -> DocsModule -> Maybe Elm.Docs.Module
extract importDocsModuleHashes sourceHash (DocsModule d) =
    if sourceHash == d.sourceHash && importDocsModuleHashes == d.importDocsModuleHashes then
        Just d.docsModule

    else
        Nothing
