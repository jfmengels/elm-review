module Review.Options exposing
    ( ReviewOptions
    , defaults
    , withDataExtraction, withLogger, withFixAll, withFixLimit, withSuppressedErrors
    )

{-| Configure how `elm-review` runs.

You should not have to use this when writing a rule. This is only necessary if you wish to run `elm-review` in a new
process like the CLI.

@docs ReviewOptions
@docs defaults
@docs withDataExtraction, withLogger, withFixAll, withFixLimit, withSuppressedErrors

-}

import Dict exposing (Dict)
import Review.Logger as Logger
import Review.Options.Internal exposing (ReviewOptionsInternal(..))


{-| Represents the different options you can use to run the review process.
-}
type alias ReviewOptions =
    ReviewOptionsInternal


{-| Somewhat arbitrary default options.

  - Does not enable data extraction

-}
defaults : ReviewOptions
defaults =
    ReviewOptionsInternal
        { extract = False
        , logger = Logger.none
        , fixAll = False
        , fixLimit = Nothing
        , suppressions = Dict.empty
        }


{-| Enable or disable data extraction.
-}
withDataExtraction : Bool -> ReviewOptions -> ReviewOptions
withDataExtraction extract (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | extract = extract }


{-| Add a logger.
-}
withLogger : Maybe (String -> String) -> ReviewOptions -> ReviewOptions
withLogger maybeLogger (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal
        { reviewOptions
            | logger =
                case maybeLogger of
                    Just fn ->
                        Logger.fromFn fn

                    Nothing ->
                        Logger.none
        }


{-| Enable fix all mode.
-}
withFixAll : Bool -> ReviewOptions -> ReviewOptions
withFixAll enableFixAll (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | fixAll = enableFixAll }


{-| Specify how many fixes should be applied before we abort the review process.

This only makes sense when [`withFixAll`](#withFixAll) has been set to `True`.

-}
withFixLimit : Int -> ReviewOptions -> ReviewOptions
withFixLimit fixLimit (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | fixLimit = Just fixLimit }


{-| Add suppressions from the suppressed folder.
-}
withSuppressedErrors : Dict ( String, String ) Int -> ReviewOptions -> ReviewOptions
withSuppressedErrors suppressions (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | suppressions = suppressions }
