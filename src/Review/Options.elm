module Review.Options exposing
    ( ReviewOptions
    , defaults
    , withDataExtraction
    )

{-| Configure how `elm-review` runs.

You should not have to use this when writing a rule. This is only necessary if you wish to run `elm-review` in a new
process like the CLI.

@docs ReviewOptions
@docs defaults
@docs withDataExtraction

-}

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
        }


{-| Enable or disable data extraction.
-}
withDataExtraction : Bool -> ReviewOptions -> ReviewOptions
withDataExtraction extract (ReviewOptionsInternal reviewOptions) =
    ReviewOptionsInternal { reviewOptions | extract = extract }
