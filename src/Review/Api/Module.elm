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


type alias ModuleApi =
    Review.Internal.Module.Module



-- MODULE DATA ACCESS


{-| Get all the values from a module. These include
-}
values : ModuleApi -> Dict String Value
values (Review.Internal.Module.Module m) =
    m.values


ports : ModuleApi -> Dict String Port
ports (Review.Internal.Module.Module m) =
    m.ports


aliases : ModuleApi -> Dict String Alias
aliases (Review.Internal.Module.Module m) =
    m.aliases


unions : ModuleApi -> Dict String Union
unions (Review.Internal.Module.Module m) =
    m.unions


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
