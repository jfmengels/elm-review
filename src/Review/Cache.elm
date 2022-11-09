module Review.Cache exposing (Entry, EntryMaybe, createEntry, createEntryMaybe, errors, errorsMaybe, match, matchMaybe, outputContext, outputContextMaybe)

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)


type Entry error context
    = Entry
        { contentHash : ContentHash
        , inputContext : ContextHash context
        , errors : List error
        , outputContext : context
        }


createEntry :
    { contentHash : ContentHash
    , inputContext : context
    , errors : List error
    , outputContext : context
    }
    -> Entry error context
createEntry entry =
    Entry
        { contentHash = entry.contentHash
        , inputContext = ContextHash.create entry.inputContext
        , errors = entry.errors
        , outputContext = entry.outputContext
        }


outputContext : Entry error context -> context
outputContext (Entry entry) =
    entry.outputContext


errors : Entry error context -> List error
errors (Entry entry) =
    entry.errors


match : ContentHash -> ContextHash context -> Entry error context -> Bool
match contentHash context (Entry entry) =
    ContentHash.areEqual contentHash entry.contentHash
        && (context == entry.inputContext)



--- Variant where the content may be absent


type EntryMaybe error context
    = EntryMaybe
        { contentHash : Maybe ContentHash
        , inputContext : ContextHash context
        , errors : List error
        , outputContext : context
        }


createEntryMaybe :
    { contentHash : Maybe ContentHash
    , inputContext : context
    , errors : List error
    , outputContext : context
    }
    -> EntryMaybe error context
createEntryMaybe entry =
    EntryMaybe
        { contentHash = entry.contentHash
        , inputContext = ContextHash.create entry.inputContext
        , errors = entry.errors
        , outputContext = entry.outputContext
        }


outputContextMaybe : EntryMaybe error context -> context
outputContextMaybe (EntryMaybe entry) =
    entry.outputContext


errorsMaybe : Maybe (EntryMaybe error context) -> List error
errorsMaybe maybeEntry =
    case maybeEntry of
        Just (EntryMaybe entry) ->
            entry.errors

        Nothing ->
            []


matchMaybe : Maybe ContentHash -> ContextHash context -> EntryMaybe error context -> Bool
matchMaybe contentHash context (EntryMaybe entry) =
    ContentHash.areEqualForMaybe contentHash entry.contentHash
        && (context == entry.inputContext)
