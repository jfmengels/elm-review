module Review.Cache.FinalProjectEvaluationCache exposing
    ( Entry, create
    , match
    , errors, setErrors
    )

{-| Cache for the result of the final project evaluation analysis.

@docs Entry, create
@docs match
@docs errors, setErrors

-}

import Review.Cache.ContextHash exposing (ComparableContextHash, ContextHash)


type Entry output context
    = Entry
        { inputContextHashes : ComparableContextHash context
        , output : output
        }


create : ComparableContextHash context -> output -> Entry output context
create inputContextHashes output =
    Entry
        { inputContextHashes = inputContextHashes
        , output = output
        }


match : ComparableContextHash context -> Entry error context -> Bool
match context (Entry entry) =
    context == entry.inputContextHashes


errors : Entry output context -> output
errors (Entry entry) =
    entry.output


setErrors : output -> Maybe (Entry output context) -> Maybe (Entry output context)
setErrors newOutput maybeEntry =
    case maybeEntry of
        Just (Entry entry) ->
            Just (Entry { inputContextHashes = entry.inputContextHashes, output = newOutput })

        Nothing ->
            Nothing
