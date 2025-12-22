module Fn.Parser exposing (..)

import Elm.Syntax.ModuleName


andThen : ( Elm.Syntax.ModuleName.ModuleName, String )
andThen =
    ( [ "Parser" ], "andThen" )


backtrackable : ( Elm.Syntax.ModuleName.ModuleName, String )
backtrackable =
    ( [ "Parser" ], "backtrackable" )


chompIf : ( Elm.Syntax.ModuleName.ModuleName, String )
chompIf =
    ( [ "Parser" ], "chompIf" )


chompUntil : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntil =
    ( [ "Parser" ], "chompUntil" )


chompUntilEndOr : ( Elm.Syntax.ModuleName.ModuleName, String )
chompUntilEndOr =
    ( [ "Parser" ], "chompUntilEndOr" )


chompWhile : ( Elm.Syntax.ModuleName.ModuleName, String )
chompWhile =
    ( [ "Parser" ], "chompWhile" )


commit : ( Elm.Syntax.ModuleName.ModuleName, String )
commit =
    ( [ "Parser" ], "commit" )


deadEndsToString : ( Elm.Syntax.ModuleName.ModuleName, String )
deadEndsToString =
    ( [ "Parser" ], "deadEndsToString" )


end : ( Elm.Syntax.ModuleName.ModuleName, String )
end =
    ( [ "Parser" ], "end" )


float : ( Elm.Syntax.ModuleName.ModuleName, String )
float =
    ( [ "Parser" ], "float" )


getChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
getChompedString =
    ( [ "Parser" ], "getChompedString" )


getCol : ( Elm.Syntax.ModuleName.ModuleName, String )
getCol =
    ( [ "Parser" ], "getCol" )


getIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
getIndent =
    ( [ "Parser" ], "getIndent" )


getOffset : ( Elm.Syntax.ModuleName.ModuleName, String )
getOffset =
    ( [ "Parser" ], "getOffset" )


getPosition : ( Elm.Syntax.ModuleName.ModuleName, String )
getPosition =
    ( [ "Parser" ], "getPosition" )


getRow : ( Elm.Syntax.ModuleName.ModuleName, String )
getRow =
    ( [ "Parser" ], "getRow" )


getSource : ( Elm.Syntax.ModuleName.ModuleName, String )
getSource =
    ( [ "Parser" ], "getSource" )


int : ( Elm.Syntax.ModuleName.ModuleName, String )
int =
    ( [ "Parser" ], "int" )


keyword : ( Elm.Syntax.ModuleName.ModuleName, String )
keyword =
    ( [ "Parser" ], "keyword" )


lazy : ( Elm.Syntax.ModuleName.ModuleName, String )
lazy =
    ( [ "Parser" ], "lazy" )


lineComment : ( Elm.Syntax.ModuleName.ModuleName, String )
lineComment =
    ( [ "Parser" ], "lineComment" )


loop : ( Elm.Syntax.ModuleName.ModuleName, String )
loop =
    ( [ "Parser" ], "loop" )


map : ( Elm.Syntax.ModuleName.ModuleName, String )
map =
    ( [ "Parser" ], "map" )


mapChompedString : ( Elm.Syntax.ModuleName.ModuleName, String )
mapChompedString =
    ( [ "Parser" ], "mapChompedString" )


multiComment : ( Elm.Syntax.ModuleName.ModuleName, String )
multiComment =
    ( [ "Parser" ], "multiComment" )


number : ( Elm.Syntax.ModuleName.ModuleName, String )
number =
    ( [ "Parser" ], "number" )


oneOf : ( Elm.Syntax.ModuleName.ModuleName, String )
oneOf =
    ( [ "Parser" ], "oneOf" )


problem : ( Elm.Syntax.ModuleName.ModuleName, String )
problem =
    ( [ "Parser" ], "problem" )


run : ( Elm.Syntax.ModuleName.ModuleName, String )
run =
    ( [ "Parser" ], "run" )


sequence : ( Elm.Syntax.ModuleName.ModuleName, String )
sequence =
    ( [ "Parser" ], "sequence" )


spaces : ( Elm.Syntax.ModuleName.ModuleName, String )
spaces =
    ( [ "Parser" ], "spaces" )


succeed : ( Elm.Syntax.ModuleName.ModuleName, String )
succeed =
    ( [ "Parser" ], "succeed" )


symbol : ( Elm.Syntax.ModuleName.ModuleName, String )
symbol =
    ( [ "Parser" ], "symbol" )


token : ( Elm.Syntax.ModuleName.ModuleName, String )
token =
    ( [ "Parser" ], "token" )


variable : ( Elm.Syntax.ModuleName.ModuleName, String )
variable =
    ( [ "Parser" ], "variable" )


withIndent : ( Elm.Syntax.ModuleName.ModuleName, String )
withIndent =
    ( [ "Parser" ], "withIndent" )


notNestableVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
notNestableVariant =
    ( [ "Parser" ], "NotNestable" )


nestableVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
nestableVariant =
    ( [ "Parser" ], "Nestable" )


expectingVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingVariant =
    ( [ "Parser" ], "Expecting" )


expectingIntVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingIntVariant =
    ( [ "Parser" ], "ExpectingInt" )


expectingHexVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingHexVariant =
    ( [ "Parser" ], "ExpectingHex" )


expectingOctalVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingOctalVariant =
    ( [ "Parser" ], "ExpectingOctal" )


expectingBinaryVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingBinaryVariant =
    ( [ "Parser" ], "ExpectingBinary" )


expectingFloatVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingFloatVariant =
    ( [ "Parser" ], "ExpectingFloat" )


expectingNumberVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingNumberVariant =
    ( [ "Parser" ], "ExpectingNumber" )


expectingVariableVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingVariableVariant =
    ( [ "Parser" ], "ExpectingVariable" )


expectingSymbolVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingSymbolVariant =
    ( [ "Parser" ], "ExpectingSymbol" )


expectingKeywordVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingKeywordVariant =
    ( [ "Parser" ], "ExpectingKeyword" )


expectingEndVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
expectingEndVariant =
    ( [ "Parser" ], "ExpectingEnd" )


unexpectedCharVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
unexpectedCharVariant =
    ( [ "Parser" ], "UnexpectedChar" )


problemVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
problemVariant =
    ( [ "Parser" ], "Problem" )


badRepeatVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
badRepeatVariant =
    ( [ "Parser" ], "BadRepeat" )


loopVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
loopVariant =
    ( [ "Parser" ], "Loop" )


doneVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
doneVariant =
    ( [ "Parser" ], "Done" )


forbiddenVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
forbiddenVariant =
    ( [ "Parser" ], "Forbidden" )


optionalVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
optionalVariant =
    ( [ "Parser" ], "Optional" )


mandatoryVariant : ( Elm.Syntax.ModuleName.ModuleName, String )
mandatoryVariant =
    ( [ "Parser" ], "Mandatory" )
