module Review.Metadata exposing (Metadata, createMetadata, moduleNameNode)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


type Metadata
    = Metadata
        { moduleNameNode : Node ModuleName
        }


createMetadata : { moduleNameNode : Node ModuleName } -> Metadata
createMetadata data =
    Metadata data


moduleNameNode : Metadata -> Node ModuleName
moduleNameNode (Metadata metadata) =
    metadata.moduleNameNode
