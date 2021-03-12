/* To run this script:

- Clone `elm-review` locally
- npm install

Then run

    npm run generate-dep -- <dependency>

Example:

    npm run generate-dep -- elm/url
*/

const path = require('path');
const https = require('https');
const fs = require('fs').promises;
const util = require('util');
const exec = util.promisify(require('child_process').exec);

async function runElmMake() {
  process.chdir('create-dependency/');
  await exec('npx elm make src/DependencyCreator.elm --output elm-stuff/app.js');
  process.chdir('..');
}

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
        }).on("error", reject);
    })
}

const packageName = process.argv[2];

if (!packageName) {
    console.error(`Need to pass in a package name. For instance:

    npm run generate-dep -- <dependency>

Example:

    npm run generate-dep -- elm/url
`)
    process.exit(1);
}

async function downloadFiles() {
    const [elmJson, docsJson] = await Promise.all([
        get(`https://package.elm-lang.org/packages/${packageName}/latest/elm.json`),
        get(`https://package.elm-lang.org/packages/${packageName}/latest/docs.json`)
    ]);
    return [elmJson, docsJson];
}

async function createFile([elmJson, docsJson]) {
    await runElmMake();

    const oldWarn = console.warn;
    console.warn = () => { }

    const app = require('./elm-stuff/app.js').Elm.DependencyCreator.init({
        flags: { elmJson, docsJson }
    });
    console.warn = oldWarn;

    app.ports.sendToJs.subscribe(async ([filePath, source]) => {
        const relativeFilePath = path.resolve(process.cwd(), filePath);
        await fs.mkdir(path.dirname(relativeFilePath), { recursive: true });
        await fs.writeFile(relativeFilePath, source);
        await exec('npx elm-format --yes ' + relativeFilePath);
        console.log("File created! You can find it at:\n   ", relativeFilePath);
    });
    return;

    const moduleName = packageName
        .replace("/", "-")
        .split("-")
        .map(capitalize)
        .join("");

    return `module Review.Test.Dependencies.${moduleName} exposing (dependency)

import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode as Decode
import Review.Project.Dependency as Dependency exposing (Dependency)


dependency : Dependency
dependency =
    Dependency.create
        "${packageName}"
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


decodeType : String -> Elm.Type.Type
decodeType type_ =
    case Decode.decodeString Elm.Type.decoder type_ of
        Ok resultType ->
            resultType
        Err _ ->
            Elm.Type.Var "unknown"
`
}

function capitalize(str) {
    return str[0].toUpperCase() + str.slice(1);
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
        (([name, types]) =>
            `( "${name}", [ ${types.map(formatType).join(", ")} ] )`)
    ).join("\n    , ")} ]
      }`
}

function formatType(type) {
    return `decodeType "${type}"`;
}

function formatComment(comment) {
    const withEscapedTripleQuotes = comment
        .split(`"""`).join(`\\"\\"\\"`)
        .split("\\").join("\\\\");
    return `"""${withEscapedTripleQuotes}"""`
}

function formatAlias(alias) {
    return `{ name = "${alias.name}"
            , comment = ${formatComment(alias.comment)}
    , args = ${JSON.stringify(alias.args)}
    , tipe = ${formatType(alias.type)}
    }`
}

function formatValue(value) {
    return `{ name = "${value.name}"
    , comment = ${formatComment(value.comment)}
    , tipe = ${formatType(value.type)}
    }`
}


function formatBinop(binop) {
    return `{ name = "${binop.name}"
    , comment = ${formatComment(binop.comment)}
    , tipe = ${formatType(binop.type)}
    , associativity = ${formatAssociativity(binop.associativity)}
    , precedence = ${binop.precedence}
    }`
}

function formatAssociativity(associativity) {
    switch (associativity) {
        case "left": return "Elm.Docs.Left"
        case "right": return "Elm.Docs.Right"
        case "non": return "Elm.Docs.None"
        default: return "unknown"
    }
}

downloadFiles()
    .then(createFile)
    // .then(console.log)
    .catch(console.error);