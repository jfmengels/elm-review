module Review.Api.Module exposing
    ( ModuleApi
    , values, ports, aliases, unions, binops
    )

{-| Represents the exposed API of an Elm module.

@docs ModuleApi
@docs values, ports, aliases, unions, binops

-}

import Dict exposing (Dict)
import Review.Api.Alias exposing (Alias)
import Review.Api.Binop exposing (Binop)
import Review.Api.Port exposing (Port)
import Review.Api.Union exposing (Union)
import Review.Api.Value exposing (Value)
import Review.Internal.Module


{-| Contains the full API of an Elm module.
-}
type alias ModuleApi =
    Review.Internal.Module.Module



-- MODULE DATA ACCESS


{-| Get all the values (functions and constants) exposed from a module, including ports and values created by type definitions.
-}
values : ModuleApi -> Dict String Value
values (Review.Internal.Module.Module m) =
    m.values


{-| Get all the ports exposed from a module.
-}
ports : ModuleApi -> Dict String Port
ports (Review.Internal.Module.Module m) =
    m.ports


{-| Get all the type aliases exposed from a module.
-}
aliases : ModuleApi -> Dict String Alias
aliases (Review.Internal.Module.Module m) =
    m.aliases


{-| Get all the unions (custom types) exposed from a module.
-}
unions : ModuleApi -> Dict String Union
unions (Review.Internal.Module.Module m) =
    m.unions


{-| Get all the binary operators exposed from a module.

You will likely only find these in packages under the `elm/` organization.

-}
binops : ModuleApi -> List Binop
binops (Review.Internal.Module.Module m) =
    m.binops



-- TODO Make it easy to only get function/constant declarations, without having to check for string casing
{- TODO Find a nice API to query
   - Only top-level declared functions and constants (including ports?)
   - All values serving as functions/constants (including from types)
   - Get all types?
   - ...?
-}
-- TODO get real type and declared type
