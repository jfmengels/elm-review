module Result.ExtraExtra exposing (firstJustLazy)

{-| -}


{-| Try thunks until we find something other than Ok Nothing.
-}
firstJustLazy : List (() -> Result e (Maybe a)) -> Result e (Maybe a)
firstJustLazy lookups =
    case lookups of
        [] ->
            Ok Nothing

        lookup :: rest ->
            case lookup () of
                Ok Nothing ->
                    firstJustLazy rest

                found ->
                    found
