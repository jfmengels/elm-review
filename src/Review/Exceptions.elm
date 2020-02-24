module Review.Exceptions exposing
    ( Exceptions
    , init
    , addDirectories, addFiles
    , apply
    )

{-| Configuration for elements that should be ignored from the review rules.

@docs Exceptions
@docs init
@docs addDirectories, addFiles
@docs apply

-}

import Set exposing (Set)


type Exceptions
    = Exceptions
        { directories : List String
        , files : Set String
        }


init : Exceptions
init =
    Exceptions
        { directories = []
        , files = Set.empty
        }


addDirectories : List String -> Exceptions -> Exceptions
addDirectories directories (Exceptions exceptions) =
    let
        cleanedDirectories : List String
        cleanedDirectories =
            directories
                |> List.map
                    (\dir ->
                        makePathOSAgnostic <|
                            if String.endsWith "/" dir then
                                dir

                            else
                                dir ++ "/"
                    )
    in
    Exceptions { exceptions | directories = cleanedDirectories ++ exceptions.directories }


addFiles : List String -> Exceptions -> Exceptions
addFiles files (Exceptions exceptions) =
    let
        cleanedFiles : Set String
        cleanedFiles =
            files
                |> List.map makePathOSAgnostic
                |> Set.fromList
    in
    Exceptions { exceptions | files = Set.union cleanedFiles exceptions.files }


apply : Exceptions -> (a -> String) -> List a -> List a
apply (Exceptions exceptions) getPath items =
    if Set.isEmpty exceptions.files && List.isEmpty exceptions.directories then
        items

    else
        List.filter (getPath >> shouldBeIgnored exceptions >> not) items


shouldBeIgnored : { directories : List String, files : Set String } -> String -> Bool
shouldBeIgnored exceptions path =
    let
        cleanedPath : String
        cleanedPath =
            makePathOSAgnostic path
    in
    Set.member cleanedPath exceptions.files
        || isInAnIgnoredDirectory exceptions.directories cleanedPath


isInAnIgnoredDirectory : List String -> String -> Bool
isInAnIgnoredDirectory directories path =
    List.any (\dir -> String.startsWith dir path) directories


makePathOSAgnostic : String -> String
makePathOSAgnostic path =
    String.replace "\"" "/" path
