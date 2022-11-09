module Review.Cache exposing (Entry, Key, createEntry, errors, match, outputContext)

import Review.Project.CacheHash as CacheHash exposing (CacheHash)


type Entry error projectContext
    = Entry
        { cacheHash : CacheHash
        , inputContext : projectContext
        , errors : List error
        , outputContext : projectContext
        }


createEntry :
    { cacheHash : CacheHash
    , inputContext : projectContext
    , errors : List error
    , outputContext : projectContext
    }
    -> Entry error projectContext
createEntry entry =
    Entry entry


outputContext : Entry error projectContext -> projectContext
outputContext (Entry entry) =
    entry.outputContext


errors : Entry error projectContext -> List error
errors (Entry entry) =
    entry.errors


type alias Key projectContext =
    { cacheHash : CacheHash
    , context : projectContext
    }


match : CacheHash -> projectContext -> Entry error projectContext -> Bool
match cacheHash context (Entry entry) =
    CacheHash.areEqual cacheHash entry.cacheHash
        && (context == entry.inputContext)
