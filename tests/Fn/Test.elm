module Fn.Test exposing (..)

import Elm.Syntax.ModuleName


concat : ( Elm.Syntax.ModuleName.ModuleName, String )
concat =
    ( [ "Test" ], "concat" )


describe : ( Elm.Syntax.ModuleName.ModuleName, String )
describe =
    ( [ "Test" ], "describe" )


expectDistribution : ( Elm.Syntax.ModuleName.ModuleName, String )
expectDistribution =
    ( [ "Test" ], "expectDistribution" )


fuzz : ( Elm.Syntax.ModuleName.ModuleName, String )
fuzz =
    ( [ "Test" ], "fuzz" )


fuzz2 : ( Elm.Syntax.ModuleName.ModuleName, String )
fuzz2 =
    ( [ "Test" ], "fuzz2" )


fuzz3 : ( Elm.Syntax.ModuleName.ModuleName, String )
fuzz3 =
    ( [ "Test" ], "fuzz3" )


fuzzWith : ( Elm.Syntax.ModuleName.ModuleName, String )
fuzzWith =
    ( [ "Test" ], "fuzzWith" )


noDistribution : ( Elm.Syntax.ModuleName.ModuleName, String )
noDistribution =
    ( [ "Test" ], "noDistribution" )


only : ( Elm.Syntax.ModuleName.ModuleName, String )
only =
    ( [ "Test" ], "only" )


reportDistribution : ( Elm.Syntax.ModuleName.ModuleName, String )
reportDistribution =
    ( [ "Test" ], "reportDistribution" )


skip : ( Elm.Syntax.ModuleName.ModuleName, String )
skip =
    ( [ "Test" ], "skip" )


test : ( Elm.Syntax.ModuleName.ModuleName, String )
test =
    ( [ "Test" ], "test" )


todo : ( Elm.Syntax.ModuleName.ModuleName, String )
todo =
    ( [ "Test" ], "todo" )
