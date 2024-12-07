module Review.Error.Fixes exposing (ErrorFixes(..), FileFix, add, from, qualify)

import Dict exposing (Dict)
import Review.Error.Target as Target exposing (Target(..))
import Review.Fix exposing (Fix)


type ErrorFixes
    = NoFixes
    | ErrorFixes (Dict String FileFix)


type alias FileFix =
    ( Target, List Fix )


from : Target -> List Fix -> ErrorFixes
from target edits =
    case Target.filePath target of
        Just filePath ->
            Dict.singleton filePath ( target, edits )
                |> ErrorFixes

        Nothing ->
            NoFixes


add : List { path : String, target : Target, fixes : List Fix } -> ErrorFixes -> ErrorFixes
add providedFixes previousFixes =
    let
        initialFixes : Dict String ( Target, List Fix )
        initialFixes =
            case previousFixes of
                ErrorFixes previousFixes_ ->
                    previousFixes_

                NoFixes ->
                    Dict.empty

        dict : Dict String FileFix
        dict =
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
    in
    if Dict.isEmpty dict then
        NoFixes

    else
        ErrorFixes dict


qualify : String -> ErrorFixes -> ErrorFixes
qualify filePath baseFixes =
    case baseFixes of
        ErrorFixes dict ->
            if Dict.isEmpty dict then
                NoFixes

            else
                case Dict.get "" dict of
                    Just ( target, fixes ) ->
                        dict
                            |> Dict.remove ""
                            |> Dict.insert filePath ( Target.setCurrentFilePathOnTargetIfNeeded filePath target, fixes )
                            |> ErrorFixes

                    Nothing ->
                        baseFixes

        NoFixes ->
            baseFixes
