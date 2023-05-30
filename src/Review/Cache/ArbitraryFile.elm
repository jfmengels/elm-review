module Review.Cache.ArbitraryFile exposing
    ( Entry, create
    , match
    , errors, errorsForMaybe, setErrors
    , outputContext, outputContextHash
    )

{-| Cache for the result of the analysis of arbitrary files.

@docs Entry, create
@docs match
@docs errors, errorsForMaybe, setErrors
@docs outputContext, outputContextHash

-}

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)


type Entry error context
    = Entry
        { contentHashes : List ContentHash
        , inputContextHash : ComparableContextHash context
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


create :
    { contentHashes : List ContentHash
    , inputContextHash : ComparableContextHash context
    , errors : List error
    , outputContext : context
    }
    -> Entry error context
create entry =
    Entry
        { contentHashes = entry.contentHashes
        , inputContextHash = entry.inputContextHash
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


match : List ContentHash -> ComparableContextHash context -> Entry error context -> Bool
match contentHashes contexts (Entry entry) =
    ContentHash.areEqualForList contentHashes entry.contentHashes
        && (contexts == entry.inputContextHash)


errors : Entry error context -> List error
errors (Entry entry) =
    entry.errors


errorsForMaybe : Maybe (Entry error context) -> List error
errorsForMaybe maybeEntry =
    case maybeEntry of
        Just (Entry entry) ->
            entry.errors

        Nothing ->
            []


setErrors : List error -> Maybe (Entry error context) -> Maybe (Entry error context)
setErrors newErrors maybeEntry =
    case maybeEntry of
        Just (Entry entry) ->
            Just (Entry { entry | errors = newErrors })

        Nothing ->
            Nothing


outputContext : Entry error context -> context
outputContext (Entry entry) =
    entry.outputContext


outputContextHash : Entry error context -> ContextHash context
outputContextHash (Entry entry) =
    entry.outputContextHash
