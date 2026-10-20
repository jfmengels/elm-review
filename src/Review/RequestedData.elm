module Review.RequestedData exposing
    ( RequestedData(..), Data
    , none, withFiles
    , types, moduleDocsDerivatives
    , combine, combineJust
    )

{-|

@docs RequestedData, Data


## Create

@docs none, withFiles


## Inspect

@docs types, moduleDocsDerivatives
@docs combine, combineJust

-}


type RequestedData
    = RequestedData Data


type alias Data =
    { moduleNameLookupTable : Bool
    , types : Bool
    , sourceCodeExtractor : Bool
    , exposed : Bool
    , moduleDocs : Bool
    , constructorToType : Bool
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
        , exposed = False
        , moduleDocs = False
        , constructorToType = False
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


moduleDocsDerivatives : RequestedData -> Bool
moduleDocsDerivatives (RequestedData requestedData) =
    requestedData.moduleNameLookupTable
        || requestedData.exposed
        || requestedData.moduleDocs
        || requestedData.constructorToType


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
        , exposed = a.exposed || b.exposed
        , moduleDocs = a.moduleDocs || b.moduleDocs
        , constructorToType = a.constructorToType || b.constructorToType
        , ignoredFiles = a.ignoredFiles || b.ignoredFiles
        , ignoredFixes = a.ignoredFixes || b.ignoredFixes
        , files = a.files ++ b.files
        }
