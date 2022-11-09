module Review.Cache exposing (Entry)

import Review.Project.CacheHash exposing (CacheHash)


type alias Entry error projectContext =
    { cacheHash : CacheHash
    , inputContext : projectContext
    , errors : List error
    , outputContext : projectContext
    }
