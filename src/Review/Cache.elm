module Review.Cache exposing
    ( FinalProjectEvaluationCache
    , createFinalProjectEvaluationCache
    , errorsForFinalProjectEvaluationCache
    , matchFinalProjectEvaluationCache
    , setErrorsForFinalProjectEvaluationCache
    )

import Review.Cache.ContextHash exposing (ComparableContextHash, ContextHash)


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
