module Review.Options exposing
    ( ReviewOptions
    , defaults
    , withDataExtraction, withLogger, withFixes, withSuppressedErrors
    , FixMode, fixedDisabled, fixesEnabledWithLimit, fixesEnabledWithoutLimits
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
        , fixMode = Internal.Disabled
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


{-| Set the fix mode.
-}
withFixes : FixMode -> ReviewOptions -> ReviewOptions
withFixes fixMode (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | fixMode = fixMode }


type alias FixMode =
    Internal.FixMode


fixesEnabledWithoutLimits : Internal.FixMode
fixesEnabledWithoutLimits =
    Internal.Enabled Nothing


fixesEnabledWithLimit : Int -> Internal.FixMode
fixesEnabledWithLimit limit =
    Internal.Enabled (Just limit)


fixedDisabled : Internal.FixMode
fixedDisabled =
    Internal.Disabled


{-| Add suppressions from the suppressed folder.
-}
withSuppressedErrors : Dict ( String, String ) Int -> ReviewOptions -> ReviewOptions
withSuppressedErrors suppressions (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | suppressions = suppressions }
