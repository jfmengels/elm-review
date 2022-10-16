module Review.Fix.FixedErrors exposing (FixedErrors, insert)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type alias FixedErrors =
    Dict String (List ReviewError)


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath }) as error) fixedErrors =
    Dict.update
        filePath
        (\errors -> Just (error :: Maybe.withDefault [] errors))
        fixedErrors
