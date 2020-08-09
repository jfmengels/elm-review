module NoInconsistentAliases.Visitor.Options exposing (AliasLookup, Options, fromConfig)

import Elm.Syntax.ModuleName exposing (ModuleName)
import NoInconsistentAliases.Config as Config exposing (Config)


type alias Options =
    { lookupAlias : AliasLookup
    , canMissAliases : Bool
    }


type alias AliasLookup =
    ModuleName -> Maybe String


fromConfig : Config -> Options
fromConfig config =
    { lookupAlias = Config.lookupAlias config
    , canMissAliases = Config.canMissAliases config
    }
