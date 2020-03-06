module Vendor.IntDict.Safe exposing
    ( InvalidKey(..), SafeKeyResult
    , safeInsert, safeUpdate, safeRemove
    , safeMember, safeGet
    )

{-| Safe API wrappers for `IntDict`s build and query operators
to prevent integer overflows with JavaScripts number type.

Prior to delegating to their 'unsafe' equivalent, these functions verify
the validity of the key via `IntDict.isValidKey`.


# Return type

@docs InvalidKey, SafeKeyResult


# Build

@docs safeInsert, safeUpdate, safeRemove


# Query

@docs safeMember, safeGet

-}

import Vendor.IntDict exposing (..)


{-| An error type signaling an invalid key.
-}
type InvalidKey
    = InvalidKey


{-| A value of this type is returned by all functions in this module.
See the core libraries' `Result` documentation for how to cope with it.
-}
type alias SafeKeyResult a =
    Result InvalidKey a


safeWrapper : Int -> (() -> a) -> SafeKeyResult a
safeWrapper k f =
    if not (isValidKey k) then
        Err InvalidKey

    else
        Ok (f ())


{-| Version of `IntDict.insert` that validates the key.
-}
safeInsert : Int -> v -> IntDict v -> SafeKeyResult (IntDict v)
safeInsert k v dict =
    safeWrapper k <| \() -> insert k v dict


{-| Version of `IntDict.remove` that validates the key.
-}
safeRemove : Int -> IntDict v -> SafeKeyResult (IntDict v)
safeRemove k dict =
    safeWrapper k <| \() -> remove k dict


{-| Version of `IntDict.update` that validates the key.
-}
safeUpdate : Int -> (Maybe v -> Maybe v) -> IntDict v -> SafeKeyResult (IntDict v)
safeUpdate k alter dict =
    safeWrapper k <| \() -> update k alter dict


{-| Version of `IntDict.get` that validates the key.
-}
safeGet : Int -> IntDict v -> SafeKeyResult (Maybe v)
safeGet k dict =
    safeWrapper k <| \() -> get k dict


{-| Version of `IntDict.member` that validates the key.
-}
safeMember : Int -> IntDict v -> SafeKeyResult Bool
safeMember k dict =
    safeWrapper k <| \() -> member k dict
