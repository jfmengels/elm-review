module Review.Cache exposing (Entry, Key, createEntry, errors, match, outputContext)

import Review.Project.ContentHash as ContentHash exposing (ContentHash)


type Entry error projectContext
    = Entry
        { contentHash : ContentHash
        , inputContext : projectContext
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
    Entry entry


outputContext : Entry error projectContext -> projectContext
outputContext (Entry entry) =
    entry.outputContext


errors : Entry error projectContext -> List error
errors (Entry entry) =
    entry.errors


type alias Key projectContext =
    { contentHash : ContentHash
    , context : projectContext
    }


match : ContentHash -> projectContext -> Entry error projectContext -> Bool
match contentHash context (Entry entry) =
    ContentHash.areEqual contentHash entry.contentHash
        && (context == entry.inputContext)
