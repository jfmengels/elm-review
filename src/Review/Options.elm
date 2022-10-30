module Review.Options exposing
    ( ReviewOptions
    , defaults
    , withDataExtraction, withLogger, withFixes, withSuppressedErrors
    )

{-| Configure how `elm-review` runs.

You should not have to use this when writing a rule. This is only necessary if you wish to run `elm-review` in a new
process like the CLI.

@docs ReviewOptions
@docs defaults
@docs withDataExtraction, withLogger, withFixes, withSuppressedErrors

-}

import Dict exposing (Dict)
import Review.Logger as Logger
import Review.Options.Internal as Internal exposing (ReviewOptionsInternal(..))


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
        , fixes = Internal.Disabled
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


{-| Indicate whether to apply fixes, and if so, how many fixes should be applied before we abort the review process.

If the limit is `Nothing`, then all available fixes will be applied.

-}
withFixes : Bool -> Maybe Int -> ReviewOptions -> ReviewOptions
withFixes enableFixes limit (ReviewOptionsInternal reviewOptions) =
    let
        fixes : Internal.Fixes
        fixes =
            if enableFixes then
                Internal.Enabled limit

            else
                Internal.Disabled
    in
    ReviewOptionsInternal { reviewOptions | fixes = fixes }


{-| Add suppressions from the suppressed folder.
-}
withSuppressedErrors : Dict ( String, String ) Int -> ReviewOptions -> ReviewOptions
withSuppressedErrors suppressions (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | suppressions = suppressions }
