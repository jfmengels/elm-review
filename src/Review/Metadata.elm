module Review.Metadata exposing (Metadata, create, moduleNameNode)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


type Metadata
    = Metadata
        { moduleNameNode : Node ModuleName
        }


create : { moduleNameNode : Node ModuleName } -> Metadata
create data =
    Metadata data


moduleNameNode : Metadata -> Node ModuleName
moduleNameNode (Metadata metadata) =
    metadata.moduleNameNode
