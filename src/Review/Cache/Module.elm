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

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.RequestedData exposing (RequestedData(..))


type Entry error context
    = Entry
        { contentHash : ContentHash
        , inputContextHashes : ComparableContextHash context
        , isFileIgnored : Bool
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


create :
    { contentHash : ContentHash
    , inputContextHashes : ComparableContextHash context
    , isFileIgnored : Bool
    , errors : List error
    , outputContext : context
    }
    -> Entry error context
create entry =
    Entry
        { contentHash = entry.contentHash
        , inputContextHashes = entry.inputContextHashes
        , isFileIgnored = entry.isFileIgnored
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


match : ContentHash -> ComparableContextHash context -> Entry error context -> { isFileIgnored : Bool, requestedData : RequestedData } -> Bool
match contentHash inputContexts (Entry entry) { isFileIgnored, requestedData } =
    ContentHash.areEqual contentHash entry.contentHash
        && (inputContexts == entry.inputContextHashes)
        && (not (ruleCaresAboutIgnoredFiles requestedData) || isFileIgnored == entry.isFileIgnored)


ruleCaresAboutIgnoredFiles : RequestedData -> Bool
ruleCaresAboutIgnoredFiles (RequestedData { ignoredFiles }) =
    ignoredFiles


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
