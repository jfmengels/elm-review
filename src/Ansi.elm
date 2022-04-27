module Ansi exposing (backgroundRed, bold, cyan, green, red, yellow)

-- FONTS


bold : String -> String
bold text =
    String.concat [ "\u{001B}[1m", text, "\u{001B}[22m" ]



-- COLORS


applyColor : String -> String -> String
applyColor color string =
    String.concat [ "\u{001B}[" ++ color ++ "m", string, noColor ]


red : String -> String
red =
    applyColor "31"


green : String -> String
green =
    applyColor "32"


yellow : String -> String
yellow =
    applyColor "33"


cyan : String -> String
cyan =
    applyColor "36"


noColor : String
noColor =
    "\u{001B}[39m"



-- BACKGROUND COLORS


applyBackgroundColor : String -> String -> String
applyBackgroundColor color string =
    String.concat [ "\u{001B}[" ++ color ++ "m", string, noBackgroundColor ]


backgroundRed : String -> String
backgroundRed =
    applyBackgroundColor "41"


noBackgroundColor : String
noBackgroundColor =
    "\u{001B}[0m"
