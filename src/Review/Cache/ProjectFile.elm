module Review.Cache.ProjectFile exposing
    ( Entry, create
    , match
    , errors, errorsForMaybe, setErrors
    , outputContext, outputContextHash
    )

{-| Cache for the result of the analysis of a project file (elm.json, README.md, docs.json for dependencies).

@docs Entry, create
@docs match
@docs errors, errorsForMaybe, setErrors
@docs outputContext, outputContextHash

-}

import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)


type Entry error context
    = Entry
        { contentHash : Maybe ContentHash
        , inputContextHash : ComparableContextHash context
        , errors : List error
        , outputContext : context
        , outputContextHash : ContextHash context
        }


create :
    { contentHash : Maybe ContentHash
    , inputContextHash : ComparableContextHash context
    , errors : List error
    , outputContext : context
    }
    -> Entry error context
create entry =
    Entry
        { contentHash = entry.contentHash
        , inputContextHash = entry.inputContextHash
        , errors = entry.errors
        , outputContext = entry.outputContext
        , outputContextHash = ContextHash.create entry.outputContext
        }


match : Maybe ContentHash -> ComparableContextHash context -> Entry error context -> Bool
match contentHash contexts (Entry entry) =
    ContentHash.areEqualForMaybe contentHash entry.contentHash
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
