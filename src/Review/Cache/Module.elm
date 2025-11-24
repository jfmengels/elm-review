module Review.Cache.Module exposing
    ( Entry, create
    , match
    , errors, setErrors
    , outputContext, outputContextHash
    )

{-| Cache for the result of the analysis of a single module.

@docs Entry, create
@docs match
@docs errors, setErrors
@docs outputContext, outputContextHash

-}

import Review.Cache.ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.RequestedData exposing (RequestedData(..))


type Entry error context
    = Entry
        { contentHash : ContentHash
        , inputContextHashes : ComparableContextHash context
        , isFileIgnored : Bool
        , isFileFixable : Bool
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


create :
    { contentHash : ContentHash
    , inputContextHashes : ComparableContextHash context
    , isFileIgnored : Bool
    , isFileFixable : Bool
    , errors : List error
    , outputContext : context
    }
    -> Entry error context
create entry =
    Entry
        { contentHash = entry.contentHash
        , inputContextHashes = entry.inputContextHashes
        , isFileIgnored = entry.isFileIgnored
        , isFileFixable = entry.isFileFixable
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


match : ContentHash -> ComparableContextHash context -> Entry error context -> { isFileIgnored : Bool, isFileFixable : Bool, requestedData : RequestedData } -> Bool
match contentHash inputContexts (Entry entry) { isFileIgnored, isFileFixable, requestedData } =
    (contentHash == entry.contentHash)
        && (inputContexts == entry.inputContextHashes)
        && (isFileIgnored == entry.isFileIgnored || not (ruleCaresAboutIgnoredFiles requestedData))
        && (isFileFixable == entry.isFileFixable || not (ruleCaresAboutFixableFiles requestedData))


ruleCaresAboutIgnoredFiles : RequestedData -> Bool
ruleCaresAboutIgnoredFiles (RequestedData { ignoredFiles }) =
    ignoredFiles


ruleCaresAboutFixableFiles : RequestedData -> Bool
ruleCaresAboutFixableFiles (RequestedData { ignoredFixes }) =
    ignoredFixes


errors : Entry error context -> List error
errors (Entry entry) =
    entry.errors


setErrors : List error -> Entry error context -> Entry error context
setErrors newErrors (Entry entry) =
    Entry { entry | errors = newErrors }


outputContext : Entry error context -> context
outputContext (Entry entry) =
    entry.outputContext


outputContextHash : Entry error context -> ContextHash context
outputContextHash (Entry entry) =
    entry.outputContextHash
