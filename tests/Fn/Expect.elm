module Fn.Expect exposing (..)

import Elm.Syntax.ModuleName


all : ( Elm.Syntax.ModuleName.ModuleName, String )
all =
    ( [ "Expect" ], "all" )


atLeast : ( Elm.Syntax.ModuleName.ModuleName, String )
atLeast =
    ( [ "Expect" ], "atLeast" )


atMost : ( Elm.Syntax.ModuleName.ModuleName, String )
atMost =
    ( [ "Expect" ], "atMost" )


equal : ( Elm.Syntax.ModuleName.ModuleName, String )
equal =
    ( [ "Expect" ], "equal" )


equalDicts : ( Elm.Syntax.ModuleName.ModuleName, String )
equalDicts =
    ( [ "Expect" ], "equalDicts" )


equalLists : ( Elm.Syntax.ModuleName.ModuleName, String )
equalLists =
    ( [ "Expect" ], "equalLists" )


equalSets : ( Elm.Syntax.ModuleName.ModuleName, String )
equalSets =
    ( [ "Expect" ], "equalSets" )


err : ( Elm.Syntax.ModuleName.ModuleName, String )
err =
    ( [ "Expect" ], "err" )


fail : ( Elm.Syntax.ModuleName.ModuleName, String )
fail =
    ( [ "Expect" ], "fail" )


greaterThan : ( Elm.Syntax.ModuleName.ModuleName, String )
greaterThan =
    ( [ "Expect" ], "greaterThan" )


lessThan : ( Elm.Syntax.ModuleName.ModuleName, String )
lessThan =
    ( [ "Expect" ], "lessThan" )


notEqual : ( Elm.Syntax.ModuleName.ModuleName, String )
notEqual =
    ( [ "Expect" ], "notEqual" )


notWithin : ( Elm.Syntax.ModuleName.ModuleName, String )
notWithin =
    ( [ "Expect" ], "notWithin" )


ok : ( Elm.Syntax.ModuleName.ModuleName, String )
ok =
    ( [ "Expect" ], "ok" )


onFail : ( Elm.Syntax.ModuleName.ModuleName, String )
onFail =
    ( [ "Expect" ], "onFail" )


pass : ( Elm.Syntax.ModuleName.ModuleName, String )
pass =
    ( [ "Expect" ], "pass" )


within : ( Elm.Syntax.ModuleName.ModuleName, String )
within =
    ( [ "Expect" ], "within" )


absoluteVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
absoluteVariant =
    ( [ "Expect" ], "Absolute" )


relativeVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
relativeVariant =
    ( [ "Expect" ], "Relative" )


absoluteOrRelativeVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
absoluteOrRelativeVariant =
    ( [ "Expect" ], "AbsoluteOrRelative" )
