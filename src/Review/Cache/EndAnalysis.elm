module Review.Cache.EndAnalysis exposing
    ( Entry, create
    , match
    , output, setOutput
    )

{-| Cache for the result of the final project evaluation analysis and data extract.

@docs Entry, create
@docs match
@docs output, setOutput

-}

import Review.Cache.ContextHash exposing (ComparableContextHash)


type Entry output context
    = Entry
        { inputContextHashes : ComparableContextHash context
        , output : output
        }


create : ComparableContextHash context -> output -> Entry output context
create inputContextHashes output_ =
    Entry
        { inputContextHashes = inputContextHashes
        , output = output_
        }


match : ComparableContextHash context -> Entry error context -> Bool
match context (Entry entry) =
    context == entry.inputContextHashes


output : Entry output context -> output
output (Entry entry) =
    entry.output


setOutput : output -> Maybe (Entry output context) -> Maybe (Entry output context)
setOutput newOutput maybeEntry =
    case maybeEntry of
        Just (Entry entry) ->
            Just (Entry { inputContextHashes = entry.inputContextHashes, output = newOutput })

        Nothing ->
            Nothing
