module Review.Exceptions exposing
    ( Exceptions
    , init
    , addDirectories, addFiles
    , addFilter, isFileWeWantReportsFor
    , avoidFixing, isFileFixable
    )

{-| Configuration for elements that should be ignored from the review rules.

@docs Exceptions
@docs init
@docs addDirectories, addFiles
@docs addFilter, isFileWeWantReportsFor
@docs avoidFixing, isFileFixable

-}

import Path
import Review.FilePattern as FilePattern exposing (FilePattern)
import Set exposing (Set)


type Exceptions
    = Exceptions
        { ignored : List (String -> Bool)
        , filesNotToFix : List FilePattern.Summary
        }


init : Exceptions
init =
    Exceptions { ignored = [], filesNotToFix = [] }


addFilter : (String -> Bool) -> Exceptions -> Exceptions
addFilter condition (Exceptions exceptions) =
    Exceptions
        { ignored = condition :: exceptions.ignored
        , filesNotToFix = exceptions.filesNotToFix
        }


avoidFixing : List FilePattern -> Exceptions -> Result (List String) Exceptions
avoidFixing filePatterns (Exceptions exceptions) =
    FilePattern.compact filePatterns
        |> Result.map
            (\filePatternSummary ->
                Exceptions
                    { ignored = exceptions.ignored
                    , filesNotToFix = filePatternSummary :: exceptions.filesNotToFix
                    }
            )


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
isFileWeWantReportsFor (Exceptions exceptions) filePath =
    let
        allConditions : String -> Bool
        allConditions path =
            List.all (\condition -> condition path) exceptions.ignored
    in
    filePath
        |> Path.makeOSAgnostic
        |> allConditions


isFileFixable : Exceptions -> String -> Bool
isFileFixable ((Exceptions exceptions) as exc) filePath =
    isFileWeWantReportsFor exc filePath
        && List.all (doesntExcludeFix filePath) exceptions.filesNotToFix


doesntExcludeFix : String -> FilePattern.Summary -> Bool
doesntExcludeFix filePath filesNotToFix =
    FilePattern.match filesNotToFixMatchParams filesNotToFix filePath


filesNotToFixMatchParams : { includeByDefault : Bool }
filesNotToFixMatchParams =
    { includeByDefault = True }


isInAnIgnoredDirectory : List String -> String -> Bool
isInAnIgnoredDirectory directories path =
    List.any (\dir -> String.startsWith dir path) directories
