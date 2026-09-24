module Review.RequestedData exposing
    ( RequestedData(..)
    , none, withFiles
    , needsTypes
    , moduleNameLookupTable
    , combine, combineJust
    , requestsTypes
    )

{-|

@docs RequestedData


## Create

@docs none, withFiles


## Inspect

@docs types, needsTypes
@docs moduleNameLookupTable
@docs combine, combineJust

-}


type RequestedData
    = RequestedData
        { moduleNameLookupTable : Bool
        , types : Bool
        , skipRuleOnTypeError : Bool
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
        , skipRuleOnTypeError = False
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


requestsTypes : RequestedData -> Bool
requestsTypes (RequestedData requestedData) =
    requestedData.types


needsTypes : RequestedData -> Bool
needsTypes (RequestedData requestedData) =
    requestedData.types && not requestedData.skipRuleOnTypeError


moduleNameLookupTable : RequestedData -> Bool
moduleNameLookupTable (RequestedData requestedData) =
    requestedData.moduleNameLookupTable


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
        , skipRuleOnTypeError = a.skipRuleOnTypeError || b.skipRuleOnTypeError
        , sourceCodeExtractor = a.sourceCodeExtractor || b.sourceCodeExtractor
        , ignoredFiles = a.ignoredFiles || b.ignoredFiles
        , ignoredFixes = a.ignoredFixes || b.ignoredFixes
        , files = a.files ++ b.files
        }
