module Fn.Parser.Advanced exposing (..)

import Elm.Syntax.ModuleName


andThen : ( Elm.Syntax.ModuleName.ModuleName, String )
andThen =
    ( [ "Parser", "Advanced" ], "andThen" )


backtrackable : ( Elm.Syntax.ModuleName.ModuleName, String )
backtrackable =
    ( [ "Parser", "Advanced" ], "backtrackable" )


chompIf : ( Elm.Syntax.ModuleName.ModuleName, String )
chompIf =
    ( [ "Parser", "Advanced" ], "chompIf" )


chompUntil : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntil =
    ( [ "Parser", "Advanced" ], "chompUntil" )


chompUntilEndOr : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntilEndOr =
    ( [ "Parser", "Advanced" ], "chompUntilEndOr" )


chompWhile : ( Elm.Syntax.ModuleName.ModuleName, String )
chompWhile =
    ( [ "Parser", "Advanced" ], "chompWhile" )


commit : ( Elm.Syntax.ModuleName.ModuleName, String )
commit =
    ( [ "Parser", "Advanced" ], "commit" )


end : ( Elm.Syntax.ModuleName.ModuleName, String )
end =
    ( [ "Parser", "Advanced" ], "end" )


float : ( Elm.Syntax.ModuleName.ModuleName, String )
float =
    ( [ "Parser", "Advanced" ], "float" )


getChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
getChompedString =
    ( [ "Parser", "Advanced" ], "getChompedString" )


getCol : ( Elm.Syntax.ModuleName.ModuleName, String )
getCol =
    ( [ "Parser", "Advanced" ], "getCol" )


getIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
getIndent =
    ( [ "Parser", "Advanced" ], "getIndent" )


getOffset : ( Elm.Syntax.ModuleName.ModuleName, String )
getOffset =
    ( [ "Parser", "Advanced" ], "getOffset" )


getPosition : ( Elm.Syntax.ModuleName.ModuleName, String )
getPosition =
    ( [ "Parser", "Advanced" ], "getPosition" )


getRow : ( Elm.Syntax.ModuleName.ModuleName, String )
getRow =
    ( [ "Parser", "Advanced" ], "getRow" )


getSource : ( Elm.Syntax.ModuleName.ModuleName, String )
getSource =
    ( [ "Parser", "Advanced" ], "getSource" )


inContext : ( Elm.Syntax.ModuleName.ModuleName, String )
inContext =
    ( [ "Parser", "Advanced" ], "inContext" )


int : ( Elm.Syntax.ModuleName.ModuleName, String )
int =
    ( [ "Parser", "Advanced" ], "int" )


keyword : ( Elm.Syntax.ModuleName.ModuleName, String )
keyword =
    ( [ "Parser", "Advanced" ], "keyword" )


lazy : ( Elm.Syntax.ModuleName.ModuleName, String )
lazy =
    ( [ "Parser", "Advanced" ], "lazy" )


lineComment : ( Elm.Syntax.ModuleName.ModuleName, String )
lineComment =
    ( [ "Parser", "Advanced" ], "lineComment" )


loop : ( Elm.Syntax.ModuleName.ModuleName, String )
loop =
    ( [ "Parser", "Advanced" ], "loop" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Parser", "Advanced" ], "map" )


mapChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
mapChompedString =
    ( [ "Parser", "Advanced" ], "mapChompedString" )


multiComment : ( Elm.Syntax.ModuleName.ModuleName, String )
multiComment =
    ( [ "Parser", "Advanced" ], "multiComment" )


number : ( Elm.Syntax.ModuleName.ModuleName, String )
number =
    ( [ "Parser", "Advanced" ], "number" )


oneOf : ( Elm.Syntax.ModuleName.ModuleName, String )
oneOf =
    ( [ "Parser", "Advanced" ], "oneOf" )


problem : ( Elm.Syntax.ModuleName.ModuleName, String )
problem =
    ( [ "Parser", "Advanced" ], "problem" )


run : ( Elm.Syntax.ModuleName.ModuleName, String )
run =
    ( [ "Parser", "Advanced" ], "run" )


sequence : ( Elm.Syntax.ModuleName.ModuleName, String )
sequence =
    ( [ "Parser", "Advanced" ], "sequence" )


spaces : ( Elm.Syntax.ModuleName.ModuleName, String )
spaces =
    ( [ "Parser", "Advanced" ], "spaces" )


succeed : ( Elm.Syntax.ModuleName.ModuleName, String )
succeed =
    ( [ "Parser", "Advanced" ], "succeed" )


symbol : ( Elm.Syntax.ModuleName.ModuleName, String )
symbol =
    ( [ "Parser", "Advanced" ], "symbol" )


token : ( Elm.Syntax.ModuleName.ModuleName, String )
token =
    ( [ "Parser", "Advanced" ], "token" )


variable : ( Elm.Syntax.ModuleName.ModuleName, String )
variable =
    ( [ "Parser", "Advanced" ], "variable" )


withIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
withIndent =
    ( [ "Parser", "Advanced" ], "withIndent" )


notNestableVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
notNestableVariant =
    ( [ "Parser", "Advanced" ], "NotNestable" )


nestableVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
nestableVariant =
    ( [ "Parser", "Advanced" ], "Nestable" )


loopVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
loopVariant =
    ( [ "Parser", "Advanced" ], "Loop" )


doneVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
doneVariant =
    ( [ "Parser", "Advanced" ], "Done" )


tokenVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
tokenVariant =
    ( [ "Parser", "Advanced" ], "Token" )


forbiddenVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
forbiddenVariant =
    ( [ "Parser", "Advanced" ], "Forbidden" )


optionalVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
optionalVariant =
    ( [ "Parser", "Advanced" ], "Optional" )


mandatoryVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
mandatoryVariant =
    ( [ "Parser", "Advanced" ], "Mandatory" )
