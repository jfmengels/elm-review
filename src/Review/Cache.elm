module Review.Cache exposing (FinalProjectEvaluationCache, ModuleEntry, ProjectFileCache, createEntryForProjectFileCache, createFinalProjectEvaluationCache, createModuleEntry, errors, errorsForFinalProjectEvaluationCache, errorsForMaybeProjectFileCache, errorsFromProjectFileCache, match, matchFinalProjectEvaluationCache, matchProjectFileCache, outputContext, outputContextForProjectFileCache, outputContextHash, outputContextHashForProjectFileCache, setErrorsForFinalProjectEvaluationCache, setErrorsForMaybeProjectFileCache, setErrorsForModule)

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.RequestedData exposing (RequestedData(..))


type ModuleEntry error context
    = ModuleEntry
        { contentHash : ContentHash
        , inputContextHashes : ComparableContextHash context
        , isFileIgnored : Bool
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


createModuleEntry :
    { contentHash : ContentHash
    , inputContextHashes : ComparableContextHash context
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


setErrorsForModule : List error -> ModuleEntry error context -> ModuleEntry error context
setErrorsForModule newErrors (ModuleEntry entry) =
    ModuleEntry { entry | errors = newErrors }


match : ContentHash -> ComparableContextHash context -> ModuleEntry error context -> { isFileIgnored : Bool, requestedData : RequestedData } -> Bool
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
        , inputContextHash : ComparableContextHash context
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


createEntryForProjectFileCache :
    { contentHash : Maybe ContentHash
    , inputContextHash : ComparableContextHash context
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


setErrorsForMaybeProjectFileCache : List error -> Maybe (ProjectFileCache error context) -> Maybe (ProjectFileCache error context)
setErrorsForMaybeProjectFileCache newErrors maybeEntry =
    case maybeEntry of
        Just (ProjectFileCache entry) ->
            Just (ProjectFileCache { entry | errors = newErrors })

        Nothing ->
            Nothing


errorsForMaybeProjectFileCache : Maybe (ProjectFileCache error context) -> List error
errorsForMaybeProjectFileCache maybeEntry =
    case maybeEntry of
        Just (ProjectFileCache entry) ->
            entry.errors

        Nothing ->
            []


matchProjectFileCache : Maybe ContentHash -> ComparableContextHash context -> ProjectFileCache error context -> Bool
matchProjectFileCache contentHash contexts (ProjectFileCache entry) =
    ContentHash.areEqualForMaybe contentHash entry.contentHash
        && (contexts == entry.inputContextHash)


{-| Variant for final operations like the final evaluation or the extract
-}
type FinalProjectEvaluationCache output context
    = EntryNoOutputContext
        { inputContextHashes : ComparableContextHash context
        , output : output
        }


createFinalProjectEvaluationCache : ComparableContextHash context -> output -> FinalProjectEvaluationCache output context
createFinalProjectEvaluationCache inputContextHashes output =
    EntryNoOutputContext
        { inputContextHashes = inputContextHashes
        , output = output
        }


matchFinalProjectEvaluationCache : ComparableContextHash context -> FinalProjectEvaluationCache error context -> Bool
matchFinalProjectEvaluationCache context (EntryNoOutputContext entry) =
    context == entry.inputContextHashes


errorsForFinalProjectEvaluationCache : FinalProjectEvaluationCache output context -> output
errorsForFinalProjectEvaluationCache (EntryNoOutputContext entry) =
    entry.output


setErrorsForFinalProjectEvaluationCache : output -> Maybe (FinalProjectEvaluationCache output context) -> Maybe (FinalProjectEvaluationCache output context)
setErrorsForFinalProjectEvaluationCache newOutput maybeEntry =
    case maybeEntry of
        Just (EntryNoOutputContext entry) ->
            Just (EntryNoOutputContext { inputContextHashes = entry.inputContextHashes, output = newOutput })

        Nothing ->
            Nothing
