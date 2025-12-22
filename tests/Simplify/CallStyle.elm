module Simplify.CallStyle exposing (FunctionCallStyle(..), LeftOrRightDirection(..), pipeLeftToRight, pipeRightToLeft)

{-|

@docs FunctionCallStyle, LeftOrRightDirection, pipeLeftToRight, pipeRightToLeft

-}


{-| How an argument is given as input to a function:

  - `Pipe RightToLeft`: `function <| argument`
  - `Pipe LeftToRight`: `argument |> function`
  - `Application`: `function argument`

-}
type FunctionCallStyle
    = Application
    | Pipe LeftOrRightDirection


type LeftOrRightDirection
    = RightToLeft
    | LeftToRight


{-| `Pipe LeftToRight`
-}
pipeLeftToRight : FunctionCallStyle
pipeLeftToRight =
    Pipe LeftToRight


{-| `Pipe RightToLeft`
-}
pipeRightToLeft : FunctionCallStyle
pipeRightToLeft =
    Pipe RightToLeft
