module Path exposing (makeOSAgnostic)


makeOSAgnostic : String -> String
makeOSAgnostic path =
    String.replace "\\" "/" path
