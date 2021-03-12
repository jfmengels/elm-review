/* To run this script:

- Clone `elm-review` locally

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
}

downloadFiles()
    .then(createFile)
    .catch(console.error);