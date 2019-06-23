module Lint.Internal.Accumulate exposing (accumulate, accumulateList)

import Elm.Syntax.Node exposing (Node)
import Lint.Direction as Direction exposing (Direction)
import Lint.Error exposing (Error)


accumulateList : (Node a -> context -> ( List Error, context )) -> List (Node a) -> ( List Error, context ) -> ( List Error, context )
accumulateList visitor nodes ( previousErrors, previousContext ) =
    List.foldl
        (\node -> accumulate (visitor node))
        ( previousErrors, previousContext )
        nodes


{-| Concatenate the errors of the previous step and of the last step, and take the last step's context.
-}
accumulate : (context -> ( List Error, context )) -> ( List Error, context ) -> ( List Error, context )
accumulate visitor ( previousErrors, previousContext ) =
    let
        ( newErrors, newContext ) =
            visitor previousContext
    in
    ( newErrors ++ previousErrors, newContext )
