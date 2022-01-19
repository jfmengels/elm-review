module Review.Exceptions exposing
    ( Exceptions
    , init
    , ignoreDirectories, ignoreFiles
    , allowDirectories, allowFiles
    , apply
    )

{-| Configuration for elements that should be ignored from the review rules.

@docs Exceptions
@docs init
@docs ignoreDirectories, ignoreFiles
@docs allowDirectories, allowFiles
@docs apply

-}

import Set exposing (Set)


type Exceptions
    = Exceptions
        { ignoredDirectories : List String
        , ignoredFiles : Set String
        , allowedDirectories : Maybe (List String)
        , allowedFiles : Maybe (Set String)
        }


init : Exceptions
init =
    Exceptions
        { ignoredDirectories = []
        , ignoredFiles = Set.empty
        , allowedDirectories = Nothing
        , allowedFiles = Nothing
        }


ignoreDirectories : List String -> Exceptions -> Exceptions
ignoreDirectories directories (Exceptions ({ ignoredDirectories } as exceptions)) =
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
    Exceptions { exceptions | ignoredDirectories = cleanedDirectories ++ ignoredDirectories }


ignoreFiles : List String -> Exceptions -> Exceptions
ignoreFiles files (Exceptions ({ ignoredFiles } as exceptions)) =
    let
        cleanedFiles : Set String
        cleanedFiles =
            files
                |> List.map makePathOSAgnostic
                |> Set.fromList
    in
    Exceptions { exceptions | ignoredFiles = Set.union cleanedFiles ignoredFiles }


allowDirectories : List String -> Exceptions -> Exceptions
allowDirectories directories (Exceptions ({ allowedDirectories } as exceptions)) =
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
    case allowedDirectories of
        Nothing ->
            Exceptions { exceptions | allowedDirectories = Just cleanedDirectories }

        Just existingDirectories ->
            Exceptions { exceptions | allowedDirectories = Just (cleanedDirectories ++ existingDirectories) }


allowFiles : List String -> Exceptions -> Exceptions
allowFiles files (Exceptions ({ allowedFiles } as exceptions)) =
    let
        cleanedFiles : Set String
        cleanedFiles =
            files
                |> List.map makePathOSAgnostic
                |> Set.fromList
    in
    case allowedFiles of
        Nothing ->
            Exceptions { exceptions | allowedFiles = Just cleanedFiles }

        Just existingFiles ->
            Exceptions { exceptions | allowedFiles = Just (Set.union cleanedFiles existingFiles) }


apply : Exceptions -> (a -> String) -> List a -> List a
apply (Exceptions exceptions) getPath items =
    items
        |> List.filter (getPath >> shouldBeConsidered exceptions)
        |> List.filter (getPath >> shouldBeIgnored exceptions >> not)


shouldBeIgnored : { a | ignoredDirectories : List String, ignoredFiles : Set String } -> String -> Bool
shouldBeIgnored exceptions path =
    let
        cleanedPath : String
        cleanedPath =
            makePathOSAgnostic path
    in
    Set.member cleanedPath exceptions.ignoredFiles
        || isInOneDirectory exceptions.ignoredDirectories cleanedPath


shouldBeConsidered : { a | allowedDirectories : Maybe (List String), allowedFiles : Maybe (Set String) } -> String -> Bool
shouldBeConsidered exceptions path =
    let
        cleanedPath : String
        cleanedPath =
            makePathOSAgnostic path
    in
    case ( exceptions.allowedDirectories, exceptions.allowedFiles ) of
        ( Nothing, Nothing ) ->
            True

        ( Just allowedDirectories, Nothing ) ->
            isInOneDirectory allowedDirectories cleanedPath

        ( Nothing, Just allowedFiles ) ->
            Set.member cleanedPath allowedFiles

        ( Just allowedDirectories, Just allowedFiles ) ->
            isInOneDirectory allowedDirectories cleanedPath
                || Set.member cleanedPath allowedFiles


isInOneDirectory : List String -> String -> Bool
isInOneDirectory directories path =
    List.any (\dir -> String.startsWith dir path) directories


makePathOSAgnostic : String -> String
makePathOSAgnostic path =
    String.replace "\\" "/" path
