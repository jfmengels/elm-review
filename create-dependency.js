const util = require('util');
const https = require('https');

function get(url) {
    return new Promise((resolve, reject) => {
        https.get(url, (resp) => {
            let data = '';
            resp.on('data', (chunk) => {
                data += chunk;
            });
            resp.on('end', () => {
                resolve(data);
            });
        }).on("error", resolve);
    })
}

const packageName = process.argv[2];

if (!packageName) {
    console.error(`Need to pass in a package name. For instance:
 
        node create-dependency.js elm/html
`)
}

async function downloadFiles() {
    const [elmJson, docsJson] = await Promise.all([
        get(`https://package.elm-lang.org/packages/${packageName}/latest/elm.json`).then(s => JSON.parse(s)),
        get(`https://package.elm-lang.org/packages/${packageName}/latest/docs.json`).then(s => JSON.parse(s))
    ]);
    return [elmJson, docsJson];
}

function createFile([elmJson, docsJson]) {
    const moduleName = formatModuleName(packageName);

    return `module Dependencies.${moduleName} exposing (dependency)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import Json.Encode as Encode
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create
        "${moduleName}"
        (createElmJsonProject elmJson)
        dependencyModules


createElmJsonProject : String -> Elm.Project.Project
createElmJsonProject rawElmJson =
    case Decode.decodeString Elm.Project.decoder rawElmJson of
        Ok project ->
            project

        Err error ->
            Debug.todo ("Failed to decode elm.json for ${packageName}: " ++ Debug.toString error)


elmJson : String
elmJson =
    """${JSON.stringify(elmJson, null, 4)}
"""


dependencyModules : List Elm.Docs.Module
dependencyModules =
    [ ${docsJson.map(formatModule).join("\n    , ")}
    ]
`
}

function formatModuleName(packageName) {
    return "ElmParser"
}

function formatModule(moduleDoc) {
    return `{ name = "${moduleDoc.name}"
      , comment = ${formatComment(moduleDoc.comment)}
      , unions = [ ${moduleDoc.unions.map(formatUnion).join("\n    , ")} ]
      , aliases = [ ${moduleDoc.aliases.map(formatAlias).filter(Boolean).join("\n    , ")} ]
      , values = [ ${moduleDoc.values.map(formatValue).filter(Boolean).join("\n    , ")} ]
      , binops = [ ${moduleDoc.binops.map(formatBinop).filter(Boolean).join("\n    , ")} ]
      }`
    // if (JSON.stringify(moduleDoc).includes("initialize 4")) {
    //     console.log(JSON.stringify(moduleDoc).slice(2470, 2530))
    // }

    return JSON.stringify(JSON.stringify(moduleDoc));
    // .split("\\n")
    // .join("\\\\n");
}

function formatUnion(union) {
    return `{ name = "${union.name}"
      , comment = ${formatComment(union.comment)}
      , args = ${JSON.stringify(union.args)}
      , tags = [ ${union.cases.map(
        (([name, type]) =>
            `( "${name}", ${JSON.stringify(type)} |> Decode.decodeString Elm.Type.decoder |> Result.toMaybe |> Maybe.withDefault [] )`)
    ).join("\n    , ")} ]
      }`
}

function formatComment(comment) {
    const withEscapedTripleQuotes = comment.split(`"""`).join(`\\"\\"\\"`);
    return `"""${withEscapedTripleQuotes}"""`
}

function formatAlias(alias) {
    return ""
    return `{
    name = "${alias.name}"
        , comment = ${formatComment(alias.comment)}
            , args = ${JSON.stringify(alias.unions)}
      , cases = [${alias.aliases.map(formatAlias).join("\n    , ")}]
        , values = [${alias.values.map(formatValue).join("\n    , ")}]
        , binops = [${alias.binops.map(formatBinop).join("\n    , ")}]
} `
}

function formatValue(value) {
    return ""

    return `{
    name = "${value.name}"
        , comment = ${formatComment(value.comment)}
            , unions = [${value.unions.map(formatUnion).join("\n    , ")}]
            , aliases = [${value.aliases.map(formatAlias).join("\n    , ")}]
            , values = [${value.values.map(formatValue).join("\n    , ")}]
            , binops = [${value.binops.map(formatBinop).join("\n    , ")}]
} `
}


function formatBinop(binop) {
    return ""
    return `{
    name = "${binop.name}"
        , comment = ${formatComment(binop.comment)}
            , unions = [${binop.unions.map(formatUnion).join("\n    , ")}]
            , aliases = [${binop.aliases.map(formatAlias).join("\n    , ")}]
            , values = [${binop.values.map(formatValue).join("\n    , ")}]
            , binops = [${binop.binops.map(formatBinop).join("\n    , ")}]
} `
}

downloadFiles()
    .then(createFile)
    .then(console.log)
    .catch(console.error);