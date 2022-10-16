module Review.Fix.FixedErrors exposing (FixedErrors, empty, hasChanged, insert)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type alias FixedErrors =
    Dict String (List ReviewError)


empty : FixedErrors
empty =
    Dict.empty


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath }) as error) fixedErrors =
    Dict.update
        filePath
        (\errors -> Just (error :: Maybe.withDefault [] errors))
        fixedErrors


hasChanged : FixedErrors -> FixedErrors -> Bool
hasChanged before after =
    before /= after
