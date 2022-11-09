module Review.Cache exposing (Entry, Key, createEntry, errors, match, outputContext)

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type Entry error projectContext
    = Entry
        { contentHash : ContentHash
        , inputContext : ContextHash projectContext
        , errors : List error
        , outputContext : projectContext
        }


createEntry :
    { contentHash : ContentHash
    , inputContext : projectContext
    , errors : List error
    , outputContext : projectContext
    }
    -> Entry error projectContext
createEntry entry =
    Entry
        { contentHash = entry.contentHash
        , inputContext = ContextHash.create entry.inputContext
        , errors = entry.errors
        , outputContext = entry.outputContext
        }


outputContext : Entry error projectContext -> projectContext
outputContext (Entry entry) =
    entry.outputContext


errors : Entry error projectContext -> List error
errors (Entry entry) =
    entry.errors


type alias Key projectContext =
    { contentHash : ContentHash
    , contextHash : projectContext
    }


match : ContentHash -> ContextHash projectContext -> Entry error projectContext -> Bool
match contentHash context (Entry entry) =
    ContentHash.areEqual contentHash entry.contentHash
        && (context == entry.inputContext)
