port module DependencyCreator exposing (main)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type
import Elm.Version
import Json.Decode as Decode
import String


type alias Flags =
    { elmJson : String
    , docsJson : String
    }


main : Program Flags () ()
main =
    Platform.worker
        { init = \flags -> ( (), sendToJs (parseThings flags) )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


parseThings : Flags -> ( String, String )
parseThings flags =
    let
        elmJson : Result String Elm.Project.PackageInfo
        elmJson =
            Decode.decodeString Elm.Project.decoder flags.elmJson
                |> Result.mapError (\err -> "Problem parsing elm.json: " ++ Debug.toString err)
                |> Result.andThen
                    (\elmJson_ ->
                        case elmJson_ of
                            Elm.Project.Application _ ->
                                Err "elm.json is for an application, not a project."

                            Elm.Project.Package package ->
                                Ok package
                    )

        docsJson : Result String (List Elm.Docs.Module)
        docsJson =
            Decode.decodeString (Decode.list Elm.Docs.decoder) flags.docsJson
                |> Result.mapError (\err -> "Problem parsing docs.json: " ++ Debug.toString err)
    in
    case Result.map2 formatFile elmJson docsJson of
        Ok str ->
            str

        Err error ->
            ( "", error )


formatFile : Elm.Project.PackageInfo -> List Elm.Docs.Module -> ( String, String )
formatFile elmJson docsJson =
    let
        listOfModuleNames : List Elm.Module.Name -> String
        listOfModuleNames list =
            listOfThings SingleLine (\name -> "unsafeModuleName " ++ stringify (Elm.Module.toString name)) list

        exposed : String
        exposed =
            case elmJson.exposed of
                Elm.Project.ExposedList list ->
                    "Elm.Project.ExposedList " ++ listOfModuleNames list

                Elm.Project.ExposedDict dict ->
                    "Elm.Project.ExposedDict " ++ listOfThings MultipleLines (\( section, list ) -> "( \"" ++ section ++ "\", " ++ listOfModuleNames list ++ " ) ") dict

        moduleName : String
        moduleName =
            "Review.Test.Dependencies."
                ++ (elmJson.name
                        |> Elm.Package.toString
                        |> String.replace "/" "-"
                        |> String.split "-"
                        |> List.map capitalize
                        |> String.join ""
                   )

        dependencyModules : String
        dependencyModules =
            listOfThings MultipleLines formatModule docsJson
    in
    ( "src/" ++ String.replace "." "/" moduleName ++ ".elm"
    , "module " ++ moduleName ++ """ exposing (dependency)

import Elm.Constraint
import Elm.Docs
import Elm.License
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type exposing (Type(..))
import Elm.Version
import Review.Project.Dependency as Dependency exposing (Dependency)

dependency : Dependency
dependency =
    Dependency.create """ ++ stringify (Elm.Package.toString elmJson.name) ++ """
        elmJson
        dependencyModules


elmJson : Elm.Project.Project
elmJson =
    Elm.Project.Package
        { elm = unsafeConstraint """ ++ stringify (Elm.Constraint.toString elmJson.elm) ++ """
        , exposed = """ ++ exposed ++ """
        , license = Elm.License.fromString """ ++ stringify (Elm.License.toString elmJson.license) ++ """ |> Maybe.withDefault Elm.License.bsd3
        , name = unsafePackageName """ ++ stringify (Elm.Package.toString elmJson.name) ++ """
        , summary = """ ++ stringify elmJson.summary ++ """
        , deps = """ ++ listOfThings MultipleLines formatDep elmJson.deps ++ """
        , testDeps = """ ++ listOfThings MultipleLines formatDep elmJson.testDeps ++ """
        , version = Elm.Version.fromString """ ++ stringify (Elm.Version.toString elmJson.version) ++ """ |> Maybe.withDefault Elm.Version.one
        }


dependencyModules : List Elm.Docs.Module
dependencyModules =
    """ ++ dependencyModules ++ """


unsafePackageName : String -> Elm.Package.Name
unsafePackageName packageName =
    case Elm.Package.fromString packageName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafePackageName packageName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeModuleName : String -> Elm.Module.Name
unsafeModuleName moduleName =
    case Elm.Module.fromString moduleName of
        Just name ->
            name

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeModuleName moduleName
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity


unsafeConstraint : String -> Elm.Constraint.Constraint
unsafeConstraint constraint =
    case Elm.Constraint.fromString constraint of
        Just constr ->
            constr

        Nothing ->
            -- unsafe, but if the generation went well, it should all be good.
            unsafeConstraint constraint
                -- Disables the tail-call optimization, so that the test crashes if we enter this case
                |> identity
"""
    )


formatModule : Elm.Docs.Module -> String
formatModule mod =
    "{ name = " ++ stringify mod.name ++ """
    , comment = """ ++ stringify mod.comment ++ """
    , aliases = """ ++ listOfThings MultipleLines formatAlias mod.aliases ++ """
    , unions = """ ++ listOfThings MultipleLines formatUnion mod.unions ++ """
    , binops = """ ++ listOfThings MultipleLines formatBinop mod.binops ++ """
    , values = """ ++ listOfThings MultipleLines formatValue mod.values ++ """
    }"""


formatValue : Elm.Docs.Value -> String
formatValue value =
    "{ name = " ++ stringify value.name ++ """
    , comment = """ ++ stringify value.comment ++ """
    , tipe = """ ++ formatType value.tipe ++ """
    }"""


formatBinop : Elm.Docs.Binop -> String
formatBinop binop =
    "{ name = " ++ stringify binop.name ++ """
    , comment = """ ++ stringify binop.comment ++ """
    , tipe = """ ++ formatType binop.tipe ++ """
    , associativity = Elm.Docs.""" ++ Debug.toString binop.associativity ++ """
    , precedence = """ ++ String.fromInt binop.precedence ++ """
    }"""


formatAlias : Elm.Docs.Alias -> String
formatAlias alias_ =
    "{ name = " ++ stringify alias_.name ++ """
    , args = """ ++ listOfThings SingleLine stringify alias_.args ++ """
    , comment = """ ++ stringify alias_.comment ++ """
    , tipe = """ ++ formatType alias_.tipe ++ """
    }"""


formatUnion : Elm.Docs.Union -> String
formatUnion union =
    "{ name = " ++ stringify union.name ++ """
    , args = """ ++ listOfThings SingleLine stringify union.args ++ """
    , comment = """ ++ stringify union.comment ++ """
    , tags = """ ++ listOfThings MultipleLines (\( name, types ) -> "( " ++ stringify name ++ ", " ++ listOfThings SingleLine formatType types ++ ")") union.tags ++ """
    }"""


formatType : Elm.Type.Type -> String
formatType type_ =
    case type_ of
        Elm.Type.Var name ->
            "Var " ++ stringify name

        Elm.Type.Tuple list ->
            "Tuple " ++ listOfThings SingleLine formatType list

        Elm.Type.Type name list ->
            "Type " ++ stringify name ++ " " ++ listOfThings SingleLine formatType list

        Elm.Type.Record fields maybeVar ->
            "Record " ++ listOfThings SingleLine (\( field, subType ) -> "( " ++ stringify field ++ ", " ++ formatType subType ++ " )") fields ++ " " ++ Debug.toString maybeVar

        Elm.Type.Lambda input output ->
            "Lambda (" ++ formatType input ++ ") (" ++ formatType output ++ ")"


formatDep : ( Elm.Package.Name, Elm.Constraint.Constraint ) -> String
formatDep ( name, constraint ) =
    "( unsafePackageName " ++ stringify (Elm.Package.toString name) ++ ", unsafeConstraint " ++ stringify (Elm.Constraint.toString constraint) ++ ")"


stringify : String -> String
stringify s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> wrapInQuotes


wrapInQuotes : String -> String
wrapInQuotes s =
    if String.contains "\n" s then
        "\"\"\"" ++ s ++ "\"\"\""

    else
        "\"" ++ s ++ "\""


type LineFormatting
    = SingleLine
    | MultipleLines


listOfThings : LineFormatting -> (a -> String) -> List a -> String
listOfThings lineFormatting mapper list =
    if List.isEmpty list then
        "[]"

    else
        case lineFormatting of
            SingleLine ->
                "[ " ++ String.join ", " (List.map mapper list) ++ " ]"

            MultipleLines ->
                "[ " ++ String.join "\n    , " (List.map mapper list) ++ " ]"


capitalize : String -> String
capitalize s =
    String.toUpper (String.left 1 s) ++ String.dropLeft 1 s


port sendToJs : ( String, String ) -> Cmd msg
