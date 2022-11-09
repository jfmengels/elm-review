module Review.Cache exposing (Entry, createEntry, outputContext)

import Review.Project.CacheHash exposing (CacheHash)


type alias Entry error projectContext =
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
    entry


outputContext : Entry error projectContext -> projectContext
outputContext entry =
    entry.outputContext
