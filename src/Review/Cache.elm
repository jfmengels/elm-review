module Review.Cache exposing (EntryMaybe, EntryNoOutputContext, ModuleEntry, createEntryMaybe, createModuleEntry, createNoOutput, errors, errorsFromEntryMaybe, errorsMaybe, match, matchMaybe, matchNoOutput, outputContext, outputContextMaybe, outputForNoOutput)

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)
import Review.RequestedData exposing (RequestedData(..))


type ModuleEntry error context
    = ModuleEntry
        { contentHash : ContentHash
        , inputContextHash : ContextHash context
        , isFileIgnored : Bool
        , errors : List error
        , outputContext : context
        }


createModuleEntry :
    { contentHash : ContentHash
    , inputContextHash : ContextHash context
    , isFileIgnored : Bool
    , errors : List error
    , outputContext : context
    }
    -> ModuleEntry error context
createModuleEntry =
    ModuleEntry


outputContext : ModuleEntry error context -> context
outputContext (ModuleEntry entry) =
    entry.outputContext


errors : ModuleEntry error context -> List error
errors (ModuleEntry entry) =
    entry.errors


match : ContentHash -> ContextHash context -> ModuleEntry error context -> { isFileIgnored : Bool, requestedData : RequestedData } -> Bool
match contentHash context (ModuleEntry entry) { isFileIgnored, requestedData } =
    ContentHash.areEqual contentHash entry.contentHash
        && ContextHash.areEqual context entry.inputContextHash
        && (not (ruleCaresAboutIgnoredFiles requestedData) || isFileIgnored == entry.isFileIgnored)


ruleCaresAboutIgnoredFiles : RequestedData -> Bool
ruleCaresAboutIgnoredFiles (RequestedData { ignoredFiles }) =
    ignoredFiles


{-| Variant where the content may be absent
-}
type EntryMaybe error context
    = EntryMaybe
        { contentHash : Maybe ContentHash
        , inputContextHash : ContextHash context
        , errors : List error
        , outputContext : context
        }


createEntryMaybe :
    { contentHash : Maybe ContentHash
    , inputContextHash : ContextHash context
    , errors : List error
    , outputContext : context
    }
    -> EntryMaybe error context
createEntryMaybe entry =
    EntryMaybe
        { contentHash = entry.contentHash
        , inputContextHash = entry.inputContextHash
        , errors = entry.errors
        , outputContext = entry.outputContext
        }


outputContextMaybe : EntryMaybe error context -> context
outputContextMaybe (EntryMaybe entry) =
    entry.outputContext


errorsFromEntryMaybe : EntryMaybe error context -> List error
errorsFromEntryMaybe (EntryMaybe entry) =
    entry.errors


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
        && ContextHash.areEqual context entry.inputContextHash


{-| Variant for final operations like the final evaluation or the extract
-}
type EntryNoOutputContext output context
    = EntryNoOutputContext
        { context : ContextHash context
        , output : output
        }


createNoOutput : context -> output -> EntryNoOutputContext output context
createNoOutput inputContext output =
    EntryNoOutputContext
        { context = ContextHash.create inputContext
        , output = output
        }


matchNoOutput : ContextHash context -> EntryNoOutputContext error context -> Bool
matchNoOutput context (EntryNoOutputContext entry) =
    ContextHash.areEqual context entry.context


outputForNoOutput : EntryNoOutputContext output context -> output
outputForNoOutput (EntryNoOutputContext entry) =
    entry.output
