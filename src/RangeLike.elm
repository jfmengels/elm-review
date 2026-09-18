module RangeLike exposing (RangeLike, fromRange)

{-| -}

import Bitwise
import Elm.Syntax.Range exposing (Range)


{-| Like Range but comparable.

Taken from elm-review: <https://github.com/jfmengels/elm-review/blob/298f85355541af7ceff4e76cb1fa47b2a2bf2b0d/src/Review/ModuleNameLookupTable/Internal.elm#L31-L39>

Smushes two ints into one, only works for columns < 65536.

-}
type alias RangeLike =
    ( Int, Int )


fromRange : Range -> RangeLike
fromRange { start, end } =
    ( Bitwise.shiftLeftBy 16 start.row + start.column
    , Bitwise.shiftLeftBy 16 end.row + end.column
    )
