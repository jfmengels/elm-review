module Review.ElmProjectEncoder exposing (encode)

import Elm.Constraint as Constraint
import Elm.License as License
import Elm.Module as Module
import Elm.Package as Package
import Elm.Project exposing (Deps, Exposed(..), Project(..))
import Elm.Version as Version
import Json.Encode as E


{-| Turn a `Project` into the JSON that goes in `elm.json`
-}
encode : Project -> E.Value
encode project =
    case project of
        Application { elm, dirs, depsDirect, depsIndirect, testDepsDirect, testDepsIndirect } ->
            E.object
                [ ( "type", E.string "application" )
                , ( "source-directories", E.list E.string dirs )
                , ( "elm-version", Version.encode elm )
                , ( "dependencies"
                  , E.object
                        [ ( "direct", encodeDeps Version.encode depsDirect )
                        , ( "indirect", encodeDeps Version.encode depsIndirect )
                        ]
                  )
                , ( "test-dependencies"
                  , E.object
                        [ ( "direct", encodeDeps Version.encode testDepsDirect )
                        , ( "indirect", encodeDeps Version.encode testDepsIndirect )
                        ]
                  )
                ]

        Package { name, summary, license, version, exposed, deps, testDeps, elm } ->
            E.object
                [ ( "type", E.string "package" )
                , ( "name", Package.encode name )
                , ( "summary", E.string summary )
                , ( "license", License.encode license )
                , ( "version", Version.encode version )
                , ( "exposed-modules", encodeExposed exposed )
                , ( "elm-version", Constraint.encode elm )
                , ( "dependencies", encodeDeps Constraint.encode deps )
                , ( "test-dependencies", encodeDeps Constraint.encode testDeps )
                ]


encodeExposed : Exposed -> E.Value
encodeExposed exposed =
    case exposed of
        ExposedList modules ->
            E.list Module.encode modules

        ExposedDict chunks ->
            E.object (List.map encodeChunk chunks)


encodeChunk : ( String, List Module.Name ) -> ( String, E.Value )
encodeChunk ( header, list ) =
    ( header, E.list Module.encode list )


encodeDeps : (constraint -> E.Value) -> Deps constraint -> E.Value
encodeDeps encodeConstraint deps =
    E.object <|
        List.sortBy (Tuple.first >> String.split "/") <|
            List.map (encodeDep encodeConstraint) deps


encodeDep : (constraint -> E.Value) -> ( Package.Name, constraint ) -> ( String, E.Value )
encodeDep encodeConstraint ( name, constraint ) =
    ( Package.toString name, encodeConstraint constraint )
