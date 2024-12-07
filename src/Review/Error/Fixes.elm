module Review.Error.Fixes exposing
    ( ErrorFixes(..)
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
    = ErrorFixes (SimpleAssocList FileTarget (List Fix))


none : ErrorFixes
none =
    ErrorFixes SimpleAssocList.empty


from : FileTarget -> List Fix -> ErrorFixes
from target edits =
    SimpleAssocList.singleton target edits
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
                            Just previousFixes_ ->
                                fixes ++ previousFixes_

                            Nothing ->
                                fixes
                    )
                    acc
        )
        initialFixes
        providedFixes
        |> ErrorFixes


qualify : String -> ErrorFixes -> ErrorFixes
qualify filePath (ErrorFixes dict) =
    SimpleAssocList.mapKeyAndValue (FileTarget.Module "")
        (\fixes ->
            ( FileTarget.Module filePath, fixes )
        )
        dict
        |> ErrorFixes


toList : ErrorFixes -> List ( FileTarget, List Fix )
toList (ErrorFixes dict) =
    SimpleAssocList.toList dict


isEmpty : ErrorFixes -> Bool
isEmpty (ErrorFixes dict) =
    SimpleAssocList.isEmpty dict
