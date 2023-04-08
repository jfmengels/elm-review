module Review.Fix.FixedErrors exposing (FixedErrors, count, empty, insert, toDict)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type FixedErrors
    = FixedErrors { count : Int, errors : Dict String (List ReviewError) }


empty : FixedErrors
empty =
    FixedErrors
        { count = 0
        , errors = Dict.empty
        }


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath }) as error) (FixedErrors fixedErrors) =
    FixedErrors
        { count = fixedErrors.count + 1
        , errors =
            Dict.update
                filePath
                (\errors -> Just (error :: Maybe.withDefault [] errors))
                fixedErrors.errors
        }


toDict : FixedErrors -> Dict String (List ReviewError)
toDict (FixedErrors fixedErrors) =
    fixedErrors.errors


count : FixedErrors -> Int
count (FixedErrors fixedErrors) =
    fixedErrors.count
