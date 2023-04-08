module Review.Cache exposing (EntryNoOutputContext, ModuleEntry, ProjectFileCache, createEntryForProjectFileCache, createModuleEntry, createNoOutput, errors, errorsForMaybeProjectFileCache, errorsFromProjectFileCache, match, matchNoOutput, matchProjectFileCache, outputContext, outputContextForProjectFileCache, outputContextHash, outputContextHashForProjectFileCache, outputForNoOutput)

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ContextHash)
import Review.RequestedData exposing (RequestedData(..))


type ModuleEntry error context
    = ModuleEntry
        { contentHash : ContentHash
        , inputContextHashes : List (ContextHash context)
        , isFileIgnored : Bool
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


createModuleEntry :
    { contentHash : ContentHash
    , inputContextHashes : List (ContextHash context)
    , isFileIgnored : Bool
    , errors : List error
    , outputContext : context
    }
    -> ModuleEntry error context
createModuleEntry entry =
    ModuleEntry
        { contentHash = entry.contentHash
        , inputContextHashes = entry.inputContextHashes
        , isFileIgnored = entry.isFileIgnored
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


outputContext : ModuleEntry error context -> context
outputContext (ModuleEntry entry) =
    entry.outputContext


outputContextHash : ModuleEntry error context -> ContextHash context
outputContextHash (ModuleEntry entry) =
    entry.outputContextHash


errors : ModuleEntry error context -> List error
errors (ModuleEntry entry) =
    entry.errors


match : ContentHash -> List (ContextHash context) -> ModuleEntry error context -> { isFileIgnored : Bool, requestedData : RequestedData } -> Bool
match contentHash inputContexts (ModuleEntry entry) { isFileIgnored, requestedData } =
    ContentHash.areEqual contentHash entry.contentHash
        && (inputContexts == entry.inputContextHashes)
        && (not (ruleCaresAboutIgnoredFiles requestedData) || isFileIgnored == entry.isFileIgnored)


ruleCaresAboutIgnoredFiles : RequestedData -> Bool
ruleCaresAboutIgnoredFiles (RequestedData { ignoredFiles }) =
    ignoredFiles


{-| Variant where the content may be absent
-}
type ProjectFileCache error context
    = ProjectFileCache
        { contentHash : Maybe ContentHash
        , inputContextHash : List (ContextHash context)
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


createEntryForProjectFileCache :
    { contentHash : Maybe ContentHash
    , inputContextHash : List (ContextHash context)
    , errors : List error
    , outputContext : context
    }
    -> ProjectFileCache error context
createEntryForProjectFileCache entry =
    ProjectFileCache
        { contentHash = entry.contentHash
        , inputContextHash = entry.inputContextHash
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


outputContextForProjectFileCache : ProjectFileCache error context -> context
outputContextForProjectFileCache (ProjectFileCache entry) =
    entry.outputContext


outputContextHashForProjectFileCache : ProjectFileCache error context -> ContextHash context
outputContextHashForProjectFileCache (ProjectFileCache entry) =
    entry.outputContextHash


errorsFromProjectFileCache : ProjectFileCache error context -> List error
errorsFromProjectFileCache (ProjectFileCache entry) =
    entry.errors


errorsForMaybeProjectFileCache : Maybe (ProjectFileCache error context) -> List error
errorsForMaybeProjectFileCache maybeEntry =
    case maybeEntry of
        Just (ProjectFileCache entry) ->
            entry.errors

        Nothing ->
            []


matchProjectFileCache : Maybe ContentHash -> List (ContextHash context) -> ProjectFileCache error context -> Bool
matchProjectFileCache contentHash contexts (ProjectFileCache entry) =
    ContentHash.areEqualForMaybe contentHash entry.contentHash
        && (contexts == entry.inputContextHash)


{-| Variant for final operations like the final evaluation or the extract
-}
type EntryNoOutputContext output context
    = EntryNoOutputContext
        { inputContextHashes : ContextHash context
        , output : output
        }


createNoOutput : ContextHash context -> output -> EntryNoOutputContext output context
createNoOutput inputContextHash output =
    EntryNoOutputContext
        { inputContextHashes = inputContextHash
        , output = output
        }


matchNoOutput : ContextHash context -> EntryNoOutputContext error context -> Bool
matchNoOutput context (EntryNoOutputContext entry) =
    ContextHash.areEqual context entry.inputContextHashes


outputForNoOutput : EntryNoOutputContext output context -> output
outputForNoOutput (EntryNoOutputContext entry) =
    entry.output
