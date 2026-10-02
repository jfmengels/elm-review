module NoUnused.CustomTypeConstructorArgsTest exposing (all)

import Elm.Project
import Json.Decode as Decode
import NoUnused.CustomTypeConstructorArgs exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


details : List String
details =
    [ "This field is never extracted and therefore never used. You should either use it somewhere, or remove it at the location I pointed at."
    ]


all : Test
all =
    describe "NoUnused.CustomTypeConstructorArgs"
        [ baseTests
        , directEqualityTests
        , indirectEqualityTests
        ]


baseTests : Test
baseTests =
    Test.concat
        [ test "should report an error when custom type constructor argument is never used" <|
            \() ->
                """module A exposing (..)
type CustomType
  = B B_Data

b = B ()

something =
  case foo of
    B _ -> ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of B is never used"
                            , details = details
                            , under = "B_Data"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type CustomType
  = B

b = B

something =
  case foo of
    B -> ()
"""
                        ]
        , test "should report an error when custom type constructor argument is never used, even in parens" <|
            \() ->
                """module A exposing (..)
type CustomType
  = B B_Data

b = B ()

something =
  case foo of
    B (_) -> ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of B is never used"
                            , details = details
                            , under = "B_Data"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type CustomType
  = B

b = B

something =
  case foo of
    B -> ()
"""
                        ]
        , test "should not report an error if custom type constructor argument is used" <|
            \() ->
                """module A exposing (..)
type CustomType
  = B B_Data

b = B ()

something =
  case foo of
    B value -> value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error only for the unused arguments (multiple arguments)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor Int ()

b = Constructor 0 ()

something =
  case foo of
    Constructor _ value -> value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Constructor is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
type CustomType
  = Constructor ()

b = Constructor ()

something =
  case foo of
    Constructor value -> value
"""
                        ]
        , test "should not report an error for used arguments in nested patterns (tuple)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor SomeData
type SomeData = SomeData

b = Constructor SomeData

something =
  case foo of
    (_, Constructor value) -> value
    _ -> SomeData
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in nested patterns (list)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor SomeData
type SomeData = SomeData

b = Constructor SomeData

something =
  case foo of
    [Constructor value] -> value
    _ -> SomeData
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in nested patterns (uncons)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A B
type A = A
type B = B

b = Constructor A B

something =
  case foo of
    Constructor a _ :: [Constructor _ b] -> b
    _ -> B
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in nested patterns (parens)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A B
type A = A
type B = B

b = Constructor A B

something =
  case foo of
    ( Constructor a b ) -> a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in nested patterns (nested case)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A B
type A = A
type B = B

b = Constructor A B

something =
  case foo of
    Constructor _ (Constructor a _ ) -> a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in nested patterns (as pattern)" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A
type A = A

b = Constructor A

something =
  case foo of
    (Constructor a ) as thing -> a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in top-level function argument destructuring" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A
type A = A

b = Constructor A

something (Constructor a) =
  a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in let in function argument destructuring" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A
type A = A

b = Constructor A

something =
  let
    foo (Constructor a) = 1
  in
  foo (Constructor A)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in lambda argument destructuring" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A
type A = A

b = Constructor A

something =
  \\(Constructor a) -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments in let declaration destructuring" <|
            \() ->
                """module A exposing (..)
type CustomType
  = Constructor A
type A = A

b = Constructor A

something =
  let
    (Constructor a) = b
  in
  a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error for used arguments used in a different file" <|
            \() ->
                [ """module A exposing (..)
type CustomType
  = Constructor ()
""", """module B exposing (..)
import A

something =
  case foo of
    A.Constructor value -> value
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should report errors for non-exposed modules in a package (exposing everything)" <|
            \() ->
                """module NotExposed exposing (..)
type CustomType
  = Constructor SomeData
type SomeData = SomeData

b = Constructor SomeData

something =
  case foo of
    Constructor _ -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Constructor is never used"
                            , details = details
                            , under = "SomeData"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 17 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed """module NotExposed exposing (..)
type CustomType
  = Constructor
type SomeData = SomeData

b = Constructor

something =
  case foo of
    Constructor -> 1
"""
                        ]
        , test "should report errors for non-exposed modules in a package (exposing explicitly)" <|
            \() ->
                """module NotExposed exposing (CustomType(..))
type CustomType
  = Constructor SomeData
type SomeData = SomeData

b = Constructor SomeData

something =
  case foo of
    Constructor _ -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Constructor is never used"
                            , details = details
                            , under = "SomeData"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 17 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed """module NotExposed exposing (CustomType(..))
type CustomType
  = Constructor
type SomeData = SomeData

b = Constructor

something =
  case foo of
    Constructor -> 1
"""
                        ]
        , test "should not report errors for exposed modules that expose everything" <|
            \() ->
                """module Exposed exposing (..)
type CustomType
  = Constructor ()

b = Constructor ()

something =
  case foo of
    Constructor -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report errors for exposed modules in a package (exposing explicitly)" <|
            \() ->
                """module Exposed exposing (CustomType(..))
type CustomType
  = Constructor ()

b = Constructor ()

something =
  case foo of
    Constructor _ -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report errors if the type is not exposed outside the module" <|
            \() ->
                """module Exposed exposing (b)
type CustomType
  = Constructor ()

b = Constructor ()

something =
  case foo of
    Constructor _ -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Constructor is never used"
                            , details = details
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 17 }, end = { row = 3, column = 19 } }
                            |> Review.Test.whenFixed """module Exposed exposing (b)
type CustomType
  = Constructor

b = Constructor

something =
  case foo of
    Constructor -> 1
"""
                        ]
        , test "should report errors if the type is exposed but not its constructors" <|
            \() ->
                """module Exposed exposing (CustomType)
type CustomType
  = Constructor SomeData
type alias SomeData = ()

b = Constructor ()

something =
  case foo of
    Constructor _ -> 1
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Constructor is never used"
                            , details = details
                            , under = "SomeData"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 17 }, end = { row = 3, column = 25 } }
                            |> Review.Test.whenFixed """module Exposed exposing (CustomType)
type CustomType
  = Constructor
type alias SomeData = ()

b = Constructor

something =
  case foo of
    Constructor -> 1
"""
                        ]
        , test "should not report args if they are used in a different module" <|
            \() ->
                [ """
module Main exposing (Model, main)
import Messages exposing (Msg(..))

update : Msg -> Model -> Model
update msg model =
   case msg of
       Content s ->
           { model | content = "content " ++ s }

       Search string ->
           { model | content = "search " ++ string }
"""
                , """
module Messages exposing (Msg(..))
type Msg
   = Content String
   | Search String
"""
                ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectNoErrors
        , test "should not report Never arguments" <|
            \() ->
                """
module Main exposing (a)
a = 1
type CustomType
  = B Never
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report Never arguments even when aliased" <|
            \() ->
                """
module Main exposing (a)
import Basics as B
a = 1
type CustomType
  = B B.Never
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report arguments next to Never" <|
            -- Honestly I'm unsure about doing this, but I currently don't see
            -- the point of having other args next to a Never arg.
            \() ->
                """
module Main exposing (a)
a = 1
type CustomType
  = B SomeData Never
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of B is never used"
                            , details = details
                            , under = "SomeData"
                            }
                            |> Review.Test.whenFixed """
module Main exposing (a)
a = 1
type CustomType
  = B Never
"""
                        ]
        , test "should remove field in calls using (|>)" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int
a = 0 |> Unused
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a)
type Foo = Unused
a = Unused
"""
                        ]
        , test "should remove field in calls using (|>) (multiline)" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int
a = 0
        |> Unused
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a)
type Foo = Unused
a = Unused
"""
                        ]
        , test "should remove field in calls using (<|)" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int
a = Unused <| 0
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a)
type Foo = Unused
a = Unused
"""
                        ]
        , test "should remove field in calls using (<|) (multiline)" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int
a = Unused <| 0
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a)
type Foo = Unused
a = Unused
"""
                        ]
        , test "should report args for type constructors used in non-equality operator expressions" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = Unused <| b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a, b)
type Foo = Unused | B
a = Unused
b = B
"""
                        ]
        , test "should report args for type constructors starting with a non-ASCII letter used in non-equality operator expressions" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Ö_Unused Int | Ö_B
a = Ö_Unused <| b
b = Ö_B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Ö_Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a, b)
type Foo = Ö_Unused | Ö_B
a = Ö_Unused
b = Ö_B
"""
                        ]
        ]


directEqualityTests : Test
directEqualityTests =
    describe "Direct (in)equality checks"
        [ test "should not report args for type constructors used in an equality expression (==)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = Unused 0 == b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report args for type constructors that are siblings of ones referenced in an equality expression (==)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = B == b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """
module MyModule exposing (a, b)
type Foo = Unused | B
a = B == b
b = B
"""
                        ]
        , test "should not report args for type constructors starting with a non-ASCII letter used in an equality expression (==)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Ö_Unused Int | Ö_B
a = Ö_Unused 0 == b
b = Ö_B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used in an inequality expression (/=)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = Unused 0 /= b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used as arguments to a prefixed equality operator (==)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = (==) b (Unused 0)
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used as arguments to a prefixed inequality operator (/=)" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = (/=) Unused 0 b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used in an equality expression with parenthesized expressions" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = ( Unused 0 ) == b
b = ( B )
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used in an equality expression with tuples" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = ( Unused 0, Unused 1 ) == b
b = ( B, B )
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used in an equality expression with lists" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = [ Unused 0 ] == b
b = [ B ]
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for type constructors used in an equality expression (==) in a different module" <|
            \() ->
                [ """
module MyModule exposing (a, b)
import Foo as F
a = F.Unused 0 == b
b = F.B
""", """
module Foo exposing (Foo(..))
type Foo = Unused Int | B
""" ]
                    |> Review.Test.runOnModulesWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should report args for type constructors used in an equality expression when value is passed to a function" <|
            \() ->
                """
module MyModule exposing (a, b)
type Foo = Unused Int | B
a = foo (Unused 0) == b
b = B
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The 1st field of Unused is never used"
                            , details = details
                            , under = "Int"
                            }
                            |> Review.Test.whenFixed """

module MyModule exposing (a, b)
type Foo = Unused | B
a = foo (Unused) == b
b = B
"""
                        ]
        ]


indirectEqualityTests : Test
indirectEqualityTests =
    describe "Indirect (in)equality checks"
        [ Test.only <|
            test "should not report args for custom types passed to (==) as an operator" <|
                \() ->
                    """
module MyModule exposing (a)
type Foo = Unused Int | B

areEqual : Foo -> Foo -> Bool
areEqual a b =
    a == b
"""
                        |> Review.Test.runWithProjectData packageProject rule
                        |> Review.Test.expectNoErrors
        , test "should not report args for custom types passed to (/=) as an operator" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int | B

areNotEqual : Foo -> Foo -> Bool
areNotEqual a b =
    a /= b
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for custom types passed to (==) as a function" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int | B

areEqual : Foo -> Foo -> Bool
areEqual a b =
    (==) a b
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        , test "should not report args for custom types passed to (/=) as a function" <|
            \() ->
                """
module MyModule exposing (a)
type Foo = Unused Int | B

areNotEqual : Foo -> Foo -> Bool
areNotEqual a b =
    (/=) a b
"""
                    |> Review.Test.runWithProjectData packageProject rule
                    |> Review.Test.expectNoErrors
        ]


packageProject : Project
packageProject =
    Review.Test.Dependencies.projectWithElmCore
        |> Project.addElmJson (createElmJson packageElmJson)


packageElmJson : String
packageElmJson =
    """
{
    "type": "package",
    "name": "author/package",
    "summary": "Summary",
    "license": "BSD-3-Clause",
    "version": "1.0.0",
    "exposed-modules": [
        "Exposed"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}"""


createElmJson : String -> { path : String, raw : String, project : Elm.Project.Project }
createElmJson rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok elmJson ->
            { path = "elm.json"
            , raw = rawElmJson
            , project = elmJson
            }

        Err err ->
            Debug.todo ("Invalid elm.json supplied to test: " ++ Debug.toString err)
