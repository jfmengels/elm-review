module Review.Exceptions exposing
    ( Exceptions
    , init
    , addDirectories, addFiles
    , addFilter
    , isFileWeWantReportsFor
    )

{-| Configuration for elements that should be ignored from the review rules.

@docs Exceptions
@docs init
@docs addDirectories, addFiles
@docs addFilter
@docs isFileWeWantReportsFor

-}

import Path
import Set exposing (Set)


type Exceptions
    = Exceptions (List (String -> Bool))


init : Exceptions
init =
    Exceptions []


addFilter : (String -> Bool) -> Exceptions -> Exceptions
addFilter condition (Exceptions conditions) =
    Exceptions (condition :: conditions)


addDirectories : List String -> Exceptions -> Exceptions
addDirectories directories exceptions =
    let
        cleanedDirectories : List String
        cleanedDirectories =
            directories
                |> List.map
                    (Path.makeOSAgnostic
                        >> (\dir ->
                                if String.endsWith "/" dir then
                                    dir

                                else
                                    dir ++ "/"
                           )
                    )
    in
    addFilter (\path -> not (isInAnIgnoredDirectory cleanedDirectories path)) exceptions


addFiles : List String -> Exceptions -> Exceptions
addFiles files exceptions =
    let
        cleanedFiles : Set String
        cleanedFiles =
            files
                |> List.map Path.makeOSAgnostic
                |> Set.fromList
    in
    addFilter (\file -> Set.member file cleanedFiles |> not) exceptions


isFileWeWantReportsFor : Exceptions -> String -> Bool
isFileWeWantReportsFor (Exceptions conditions) filePath =
    let
        allConditions : String -> Bool
        allConditions path =
            List.all (\condition -> condition path) conditions
    in
    filePath
        |> Path.makeOSAgnostic
        |> allConditions


isInAnIgnoredDirectory : List String -> String -> Bool
isInAnIgnoredDirectory directories path =
    List.any (\dir -> String.startsWith dir path) directories
