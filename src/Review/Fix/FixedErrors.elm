module Review.Fix.FixedErrors exposing (FixedErrors, empty, hasChanged, insert)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type FixedErrors
    = FixedErrors (Dict String (List ReviewError))


empty : FixedErrors
empty =
    FixedErrors Dict.empty


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath }) as error) (FixedErrors fixedErrors) =
    FixedErrors
        (Dict.update
            filePath
            (\errors -> Just (error :: Maybe.withDefault [] errors))
            fixedErrors
        )


hasChanged : FixedErrors -> FixedErrors -> Bool
hasChanged (FixedErrors before) (FixedErrors after) =
    before /= after
