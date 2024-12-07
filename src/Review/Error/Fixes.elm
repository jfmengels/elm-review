module Review.Error.Fixes exposing
    ( ErrorFixes(..)
    , FixKind(..)
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
    = ErrorFixes (SimpleAssocList FileTarget FixKind)


type FixKind
    = Edit (List Fix)


none : ErrorFixes
none =
    ErrorFixes SimpleAssocList.empty


from : FileTarget -> List Fix -> ErrorFixes
from target edits =
    SimpleAssocList.singleton target (Edit edits)
        |> ErrorFixes


add : List { target : FileTarget, fixes : FixKind } -> ErrorFixes -> ErrorFixes
add providedFixes (ErrorFixes initialFixes) =
    List.foldl
        (\{ target, fixes } acc ->
            SimpleAssocList.update target
                (\maybePreviousFixes ->
                    case fixes of
                        Edit [] ->
                            maybePreviousFixes

                        Edit newFixes ->
                            case maybePreviousFixes of
                                Just (Edit previousFixes_) ->
                                    Just (Edit (newFixes ++ previousFixes_))

                                Nothing ->
                                    Just fixes
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


toList : ErrorFixes -> List ( FileTarget, FixKind )
toList (ErrorFixes dict) =
    SimpleAssocList.toList dict


isEmpty : ErrorFixes -> Bool
isEmpty (ErrorFixes dict) =
    SimpleAssocList.isEmpty dict
