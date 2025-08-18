module Review.Error.Fixes exposing
    ( ErrorFixes
    , FixKind(..)
    , FixV2(..)
    , add
    , edit
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
    | Remove


none : ErrorFixes
none =
    ErrorFixes SimpleAssocList.empty


type FixV2
    = FixV2 FileTarget FixKind


edit : FileTarget -> List Fix -> ErrorFixes
edit target edits =
    SimpleAssocList.singleton target (Edit edits)
        |> ErrorFixes


add : FileTarget -> FixKind -> ErrorFixes -> ErrorFixes
add target fixes (ErrorFixes initialFixes) =
    SimpleAssocList.update target
        (\maybePreviousFixes ->
            case fixes of
                Remove ->
                    Just fixes

                Edit [] ->
                    maybePreviousFixes

                Edit newFixes ->
                    case maybePreviousFixes of
                        Just Remove ->
                            maybePreviousFixes

                        Just (Edit previousFixes_) ->
                            Just (Edit (newFixes ++ previousFixes_))

                        Nothing ->
                            Just fixes
        )
        initialFixes
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
