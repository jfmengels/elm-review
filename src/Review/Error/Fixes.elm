module Review.Error.Fixes exposing
    ( ErrorFixes(..)
    , FileFix
    , add
    , from
    , none
    , qualify
    , toDict
    )

import Dict exposing (Dict)
import Review.Error.Target as Target exposing (Target(..))
import Review.Fix exposing (Fix)


type ErrorFixes
    = ErrorFixes (Dict String FileFix)


type alias FileFix =
    ( Target, List Fix )


none : ErrorFixes
none =
    ErrorFixes Dict.empty


from : Target -> List Fix -> ErrorFixes
from target edits =
    case Target.filePath target of
        Just filePath ->
            Dict.singleton filePath ( target, edits )
                |> ErrorFixes

        Nothing ->
            ErrorFixes Dict.empty


add : List { path : String, target : Target, fixes : List Fix } -> ErrorFixes -> ErrorFixes
add providedFixes (ErrorFixes initialFixes) =
    List.foldl
        (\{ path, target, fixes } acc ->
            if List.isEmpty fixes then
                acc

            else
                Dict.update path
                    (\maybePreviousFixes ->
                        case maybePreviousFixes of
                            Just ( _, previousFixes_ ) ->
                                Just ( target, fixes ++ previousFixes_ )

                            Nothing ->
                                Just ( target, fixes )
                    )
                    acc
        )
        initialFixes
        providedFixes
        |> ErrorFixes


qualify : String -> ErrorFixes -> ErrorFixes
qualify filePath ((ErrorFixes dict) as untouched) =
    case Dict.get "" dict of
        Just ( target, fixes ) ->
            dict
                |> Dict.remove ""
                |> Dict.insert filePath ( Target.setCurrentFilePathOnTargetIfNeeded filePath target, fixes )
                |> ErrorFixes

        Nothing ->
            untouched


toDict : ErrorFixes -> Dict String FileFix
toDict (ErrorFixes dict) =
    dict
