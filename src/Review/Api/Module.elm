module Review.Api.Module exposing
    ( ModuleApi
    , aliases
    , binops
    , ports
    , unions
    , values
    )

import Dict exposing (Dict)
import Review.Api.Alias exposing (Alias)
import Review.Api.Binop exposing (Binop)
import Review.Api.Port exposing (Port)
import Review.Api.Union exposing (Union)
import Review.Internal.Module
import Review.Internal.Value exposing (Value)


type alias ModuleApi =
    Review.Internal.Module.Module



-- MODULE DATA ACCESS


unions : ModuleApi -> Dict String Union
unions (Review.Internal.Module.Module m) =
    m.unions


aliases : ModuleApi -> Dict String Alias
aliases (Review.Internal.Module.Module m) =
    m.aliases


values : ModuleApi -> Dict String Value
values (Review.Internal.Module.Module m) =
    m.values


ports : ModuleApi -> Dict String Port
ports (Review.Internal.Module.Module m) =
    m.ports


binops : ModuleApi -> List Binop
binops (Review.Internal.Module.Module m) =
    m.binops



-- TODO Check if ports are present in values
-- TODO Make it easy to only get function/constant declarations, without having to check for string casing
-- TODO get real type and declared type
