module Review.Fix.FixedErrors exposing (FixedErrors, empty, hasChanged, insert, toDict)

import Dict exposing (Dict)
import Review.Error exposing (ReviewError)


type FixedErrors
    = FixedErrors Int (Dict String (List ReviewError))


empty : FixedErrors
empty =
    FixedErrors 0 Dict.empty


insert : ReviewError -> FixedErrors -> FixedErrors
insert ((Review.Error.ReviewError { filePath }) as error) (FixedErrors count fixedErrors) =
    FixedErrors
        (count + 1)
        (Dict.update
            filePath
            (\errors -> Just (error :: Maybe.withDefault [] errors))
            fixedErrors
        )


hasChanged : FixedErrors -> FixedErrors -> Bool
hasChanged (FixedErrors beforeCount _) (FixedErrors afterCount _) =
    beforeCount /= afterCount


toDict : FixedErrors -> Dict String (List ReviewError)
toDict (FixedErrors _ dict) =
    dict
