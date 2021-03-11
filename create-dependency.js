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
        get(`https://package.elm-lang.org/packages/${packageName}/latest/docs.json`)
    ]);
    return [elmJson, docsJson];
}

function createFile([elmJson, docsJson]) {
    const moduleName = formatModuleName(packageName);

    return `module Dependencies.${moduleName} exposing (dependency)

import Elm.Docs
import Elm.Project
import Json.Decode as Decode
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


dependencyModules : List Elm.Docs.Module
dependencyModules =
    case Decode.decodeString (Decode.list Elm.Docs.decoder) docsJson of
        Ok modules ->
            modules

        Err error ->
            Debug.todo ("Failed to decode docs.json for ${packageName}: " ++ Debug.toString error)


elmJson : String
elmJson =
    """${JSON.stringify(elmJson, null, 4)}
"""


docsJson : String
docsJson =
    """${docsJson
            .split("\\\\n")
            .map(s => s.split("\\n").join("\\\\n"))
            .join("\\\\\\\\n")
            .split('\\"')
            .join('\\\\"')
        }
"""

    `
}

function formatModuleName(packageName) {
    return "ElmParser"
}

function formatModule(moduleDoc) {
    // if (JSON.stringify(moduleDoc).includes("initialize 4")) {
    //     console.log(JSON.stringify(moduleDoc).slice(2470, 2530))
    // }

    return JSON.stringify(JSON.stringify(moduleDoc));
    // .split("\\n")
    // .join("\\\\n");
}


downloadFiles()
    .then(createFile)
    .then(console.log)
    .catch(console.error);