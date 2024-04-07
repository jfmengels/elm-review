module Review.RequestedData exposing (RequestedData(..), combine, combineJust, none, withFiles)


type RequestedData
    = RequestedData
        { moduleNameLookupTable : Bool
        , sourceCodeExtractor : Bool
        , ignoredFiles : Bool
        , files : List { files : List { string : String, included : Bool }, excludedFolders : List String }
        }


none : RequestedData
none =
    RequestedData
        { moduleNameLookupTable = False
        , sourceCodeExtractor = False
        , ignoredFiles = False
        , files = []
        }


combine : Maybe RequestedData -> Maybe RequestedData -> RequestedData
combine maybeA maybeB =
    case maybeA of
        Nothing ->
            Maybe.withDefault none maybeB

        Just a ->
            case maybeB of
                Just b ->
                    combineJust a b

                Nothing ->
                    a


withFiles : List { files : List { string : String, included : Bool }, excludedFolders : List String } -> RequestedData -> RequestedData
withFiles files ((RequestedData requested) as untouched) =
    if List.isEmpty files then
        untouched

    else
        RequestedData { requested | files = files }


combineJust : RequestedData -> RequestedData -> RequestedData
combineJust (RequestedData a) (RequestedData b) =
    RequestedData
        { moduleNameLookupTable = a.moduleNameLookupTable || b.moduleNameLookupTable
        , sourceCodeExtractor = a.sourceCodeExtractor || b.sourceCodeExtractor
        , ignoredFiles = a.ignoredFiles || b.ignoredFiles
        , files = a.files ++ b.files
        }
