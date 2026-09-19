module Review.RequestedData exposing
    ( RequestedData(..)
    , none, withFiles
    , types
    , combine, combineJust
    )

{-|

@docs RequestedData


## Create

@docs none, withFiles


## Inspect

@docs types
@docs combine, combineJust

-}


type RequestedData
    = RequestedData
        { moduleNameLookupTable : Bool
        , types : Bool
        , sourceCodeExtractor : Bool
        , ignoredFiles : Bool
        , ignoredFixes : Bool
        , files : List { files : List { pattern : String, included : Bool }, excludedDirectories : List String }
        }


none : RequestedData
none =
    RequestedData
        { moduleNameLookupTable = False
        , types = False
        , sourceCodeExtractor = False
        , ignoredFiles = False
        , ignoredFixes = False
        , files = []
        }


withFiles : List { files : List { pattern : String, included : Bool }, excludedDirectories : List String } -> RequestedData -> RequestedData
withFiles files ((RequestedData requested) as untouched) =
    if List.isEmpty files then
        untouched

    else
        RequestedData { requested | files = files }


types : RequestedData -> Bool
types (RequestedData requestedData) =
    requestedData.types


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


combineJust : RequestedData -> RequestedData -> RequestedData
combineJust (RequestedData a) (RequestedData b) =
    RequestedData
        { moduleNameLookupTable = a.moduleNameLookupTable || b.moduleNameLookupTable
        , types = a.types || b.types
        , sourceCodeExtractor = a.sourceCodeExtractor || b.sourceCodeExtractor
        , ignoredFiles = a.ignoredFiles || b.ignoredFiles
        , ignoredFixes = a.ignoredFixes || b.ignoredFixes
        , files = a.files ++ b.files
        }
