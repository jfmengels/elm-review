module Review.Fix.FixedErrors exposing (FixedErrors, count, empty, insert, shouldAbort, toDict)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type FixedErrors
    = FixedErrors
        { count : Int
        , errors : Dict String (List ReviewError)
        , shouldAbort : Bool
        }


empty : FixedErrors
empty =
    FixedErrors
        { count = 0
        , errors = Dict.empty
        , shouldAbort = False
        }


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath, target }) as error) (FixedErrors fixedErrors) =
    FixedErrors
        { count = fixedErrors.count + 1
        , errors =
            Dict.update
                filePath
                (\errors -> Just (error :: Maybe.withDefault [] errors))
                fixedErrors.errors
        , shouldAbort = fixedErrors.shouldAbort || target == Review.Error.ElmJson
        }


toDict : FixedErrors -> Dict String (List ReviewError)
toDict (FixedErrors fixedErrors) =
    fixedErrors.errors


count : FixedErrors -> Int
count (FixedErrors fixedErrors) =
    fixedErrors.count


shouldAbort : FixedErrors -> Bool
shouldAbort (FixedErrors fixedErrors) =
    fixedErrors.shouldAbort
