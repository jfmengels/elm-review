module Review.Test.TestExpectationTests exposing (all)

import NoDebug.Log
import NoUnused.Exports
import Review.Test
import Review.Test.FailureMessageHelper exposing (expectFailure)
import Test exposing (Test, describe, test)
import TestProject


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
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "A", Review.Test.removed ) ]
                        ]
                    |> expectFailure """INCORRECT FIX TYPE

I expected that the error for module `A` with the following message:

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
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "src/A.elm", Review.Test.removed ) ]
                        ]
                    |> expectFailure """INCORRECT FIX TYPE

I expected that the error for module `A` with the following message:

  `Remove the use of `Debug.log` before shipping to production`

would provide fixes, but I found an unexpected fix for `src/A.elm`.
I expected the file to be removed, but instead it gets edited to:

  ```
    module A exposing (..)
    a = Debug.log "message" 1
  ```"""
        , test "should fail test when a file is removed instead of edited" <|
            \() ->
                """
module Reported exposing (..)
import Something
a = 1
"""
                    |> Review.Test.runWithProjectData TestProject.application NoUnused.Exports.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Module `Reported` is never used."
                            , details = [ "This module is never used. You may want to remove it to keep your project clean, and maybe detect some unused code in your project." ]
                            , under = "Reported"
                            }
                            |> Review.Test.shouldFixFilesWithFileRemoval [ ( "Reported", Review.Test.edited "something" ) ]
                        ]
                    |> expectFailure """INCORRECT FIX TYPE

I expected that the error for module `Reported` with the following message:

  `Module `Reported` is never used.`

would provide fixes, but I found an unexpected fix for `Reported`.
I expected the file to be edited, but instead it gets removed."""
        ]
