module Review.Error.Fixes exposing
    ( ErrorFixes(..)
    , FileFix
    , add
    , from
    , isEmpty
    , none
    , qualify
    , toList
    )

import Review.Error.FileTarget as FileTarget exposing (FileTarget)
import Review.Fix exposing (Fix)
import SimpleAssocList exposing (SimpleAssocList)


type ErrorFixes
    = ErrorFixes (SimpleAssocList FileTarget FileFix)


type alias FileFix =
    ( FileTarget, List Fix )


none : ErrorFixes
none =
    ErrorFixes SimpleAssocList.empty


from : FileTarget -> List Fix -> ErrorFixes
from target edits =
    SimpleAssocList.singleton target ( target, edits )
        |> ErrorFixes


add : List { path : String, target : FileTarget, fixes : List Fix } -> ErrorFixes -> ErrorFixes
add providedFixes (ErrorFixes initialFixes) =
    List.foldl
        (\{ path, target, fixes } acc ->
            if List.isEmpty fixes then
                acc

            else
                SimpleAssocList.upsert target
                    (\maybePreviousFixes ->
                        case maybePreviousFixes of
                            Just ( _, previousFixes_ ) ->
                                ( target, fixes ++ previousFixes_ )

                            Nothing ->
                                ( target, fixes )
                    )
                    acc
        )
        initialFixes
        providedFixes
        |> ErrorFixes


qualify : String -> ErrorFixes -> ErrorFixes
qualify filePath (ErrorFixes dict) =
    SimpleAssocList.mapKeyAndValue (FileTarget.Module "")
        (\( target, fixes ) ->
            ( FileTarget.setCurrentFilePathOnTargetIfNeeded filePath target, ( FileTarget.setCurrentFilePathOnTargetIfNeeded filePath target, fixes ) )
        )
        dict
        |> ErrorFixes


toList : ErrorFixes -> List ( FileTarget, FileFix )
toList (ErrorFixes dict) =
    SimpleAssocList.toList dict


isEmpty : ErrorFixes -> Bool
isEmpty (ErrorFixes dict) =
    SimpleAssocList.isEmpty dict
