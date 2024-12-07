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

import Dict exposing (Dict)
import Review.Error.FileTarget exposing (FileTarget)
import Review.Error.Target as Target exposing (Target(..))
import Review.Fix exposing (Fix)
import SimpleAssocList exposing (SimpleAssocList)


type ErrorFixes
    = ErrorFixes (SimpleAssocList String FileFix)


type alias FileFix =
    ( Target, List Fix )


none : ErrorFixes
none =
    ErrorFixes SimpleAssocList.empty


from : Target -> List Fix -> ErrorFixes
from target edits =
    case Target.filePath target of
        Just filePath ->
            SimpleAssocList.singleton filePath ( target, edits )
                |> ErrorFixes

        Nothing ->
            ErrorFixes SimpleAssocList.empty


add : List { path : String, target : FileTarget, fixes : List Fix } -> ErrorFixes -> ErrorFixes
add providedFixes (ErrorFixes initialFixes) =
    List.foldl
        (\{ path, target, fixes } acc ->
            if List.isEmpty fixes then
                acc

            else
                SimpleAssocList.upsert path
                    (\maybePreviousFixes ->
                        case maybePreviousFixes of
                            Just ( _, previousFixes_ ) ->
                                ( FileTarget target, fixes ++ previousFixes_ )

                            Nothing ->
                                ( FileTarget target, fixes )
                    )
                    acc
        )
        initialFixes
        providedFixes
        |> ErrorFixes


qualify : String -> ErrorFixes -> ErrorFixes
qualify filePath (ErrorFixes dict) =
    SimpleAssocList.mapKeyAndValue ""
        (\( target, fixes ) ->
            ( filePath, ( Target.setCurrentFilePathOnTargetIfNeeded filePath target, fixes ) )
        )
        dict
        |> ErrorFixes


toList : ErrorFixes -> List ( String, FileFix )
toList (ErrorFixes dict) =
    SimpleAssocList.toList dict


isEmpty : ErrorFixes -> Bool
isEmpty (ErrorFixes dict) =
    SimpleAssocList.isEmpty dict
