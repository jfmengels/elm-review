module Path exposing (makeOSAgnostic)

import Review.FilePath exposing (FilePath)


makeOSAgnostic : FilePath -> FilePath
makeOSAgnostic path =
    String.replace "\\" "/" path
