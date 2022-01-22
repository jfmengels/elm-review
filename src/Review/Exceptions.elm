module Review.Exceptions exposing
    ( Exceptions
    , init
    , addDirectories, addFiles
    , addFilter
    , apply
    )

{-| Configuration for elements that should be ignored from the review rules.

@docs Exceptions
@docs init
@docs addDirectories, addFiles
@docs addFilter
@docs apply

-}

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
addDirectories directories =
    let
        cleanedDirectories : List String
        cleanedDirectories =
            directories
                |> List.map
                    (makePathOSAgnostic
                        >> (\dir ->
                                if String.endsWith "/" dir then
                                    dir

                                else
                                    dir ++ "/"
                           )
                    )
    in
    addFilter (isInAnIgnoredDirectory cleanedDirectories >> not)


addFiles : List String -> Exceptions -> Exceptions
addFiles files =
    let
        cleanedFiles : Set String
        cleanedFiles =
            files
                |> List.map makePathOSAgnostic
                |> Set.fromList
    in
    addFilter (\file -> Set.member file cleanedFiles |> not)


apply : Exceptions -> (a -> String) -> List a -> List a
apply (Exceptions conditions) getPath items =
    let
        allConditions path =
            List.all (\condition -> condition path) conditions
    in
    List.filter (getPath >> makePathOSAgnostic >> allConditions) items


isInAnIgnoredDirectory : List String -> String -> Bool
isInAnIgnoredDirectory directories path =
    List.any (\dir -> String.startsWith dir path) directories


makePathOSAgnostic : String -> String
makePathOSAgnostic path =
    String.replace "\\" "/" path
