module Review.Test.TestExpectationTests exposing (all)

import NoDebug.Log
import Review.Test
import Review.Test.FailureMessageHelper exposing (expectFailure)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Test expectations"
        [ test "should fail test when a file is edited instead of removed (using module name)" <|
            \_ ->
                """module A exposing (..)
a = Debug.log "message" 1"""
                    |> Review.Test.run NoDebug.Log.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Remove the use of `Debug.log` before shipping to production"
                            , details = [ "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = "Debug.log"
                            }
                            |> Review.Test.shouldFixFilesWithIO [ ( "A", Review.Test.removed ) ]
                        ]
                    |> expectFailure """INCORRECT FIX TYPE

I expected that the error for module `A` with the following message

  `Remove the use of `Debug.log` before shipping to production`

would provide fixes, but I found an unexpected fix for `A`.
I expected the file to be removed, but instead it gets edited to:

  ```
    module A exposing (..)
    a = Debug.log "message" 1
  ```"""
        , test "should fail test when a file is edited instead of removed (using file path)" <|
            \_ ->
                """module A exposing (..)
a = Debug.log "message" 1"""
                    |> Review.Test.run NoDebug.Log.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Remove the use of `Debug.log` before shipping to production"
                            , details = [ "`Debug.log` is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                            , under = "Debug.log"
                            }
                            |> Review.Test.shouldFixFilesWithIO [ ( "src/A.elm", Review.Test.removed ) ]
                        ]
                    |> expectFailure """INCORRECT FIX TYPE

I expected that the error for module `A` with the following message

  `Remove the use of `Debug.log` before shipping to production`

would provide fixes, but I found an unexpected fix for `src/A.elm`.
I expected the file to be removed, but instead it gets edited to:

  ```
    module A exposing (..)
    a = Debug.log "message" 1
  ```"""
        ]
