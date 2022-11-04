module Review.Options exposing
    ( ReviewOptions
    , defaults
    , withDataExtraction, withLogger, withSuppressedErrors
    , withFixes
    , FixMode, fixedDisabled, fixesEnabledWithLimit, fixesEnabledWithoutLimits
    , withIgnoredFixes
    )

{-| Configure how `elm-review` runs.

You should not have to use this when writing a rule. This is only necessary if you wish to run `elm-review` in a new
process like the CLI.

@docs ReviewOptions
@docs defaults
@docs withDataExtraction, withLogger, withSuppressedErrors

@docs withFixes
@docs FixMode, fixedDisabled, fixesEnabledWithLimit, fixesEnabledWithoutLimits
@docs withIgnoredFixes

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import Json.Encode as Encode
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
        , ignoreFix = always False
        }


{-| Enable or disable data extraction.
-}
withDataExtraction : Bool -> ReviewOptions -> ReviewOptions
withDataExtraction extract (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | extract = extract }


{-| Add a logger.
-}
withLogger : Maybe (List ( String, Encode.Value ) -> List ( String, Encode.Value )) -> ReviewOptions -> ReviewOptions
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


{-| Provide a predicate for ignoring fixes to apply. This is useful to ignore previously refused fixes in `elm-review --fix`.
-}
withIgnoredFixes : ({ ruleName : String, filePath : String, message : String, details : List String, range : Range } -> Bool) -> ReviewOptions -> ReviewOptions
withIgnoredFixes ignoreFix (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | ignoreFix = ignoreFix }


{-| Represents whether `elm-review` should apply fixes found in the reported errors.
-}
type alias FixMode =
    Internal.FixMode


{-| Apply the fixes for every error whenever possible.
-}
fixesEnabledWithoutLimits : FixMode
fixesEnabledWithoutLimits =
    Internal.Enabled Nothing


{-| Apply the fixes for every error whenever possible, but abort the whole review process once a number of errors have been fixed.
-}
fixesEnabledWithLimit : Int -> FixMode
fixesEnabledWithLimit limit =
    Internal.Enabled (Just limit)


{-| Don't apply fixes.
-}
fixedDisabled : FixMode
fixedDisabled =
    Internal.Disabled


{-| Add suppressions from the suppressed folder.
-}
withSuppressedErrors : Dict ( String, String ) Int -> ReviewOptions -> ReviewOptions
withSuppressedErrors suppressions (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | suppressions = suppressions }
