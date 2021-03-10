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
                resolve(JSON.parse(data));
            });
        }).on("error", resolve);
    })
}

if (!process.argv[2]) {
    console.error(`Need to pass in a package name. For instance:
 
        node create-dependency.js elm/html
`)
}

async function downloadFiles(packageName) {
    const [elmJson, docsJson] = await Promise.all([
        get(`https://package.elm-lang.org/packages/${packageName}/latest/elm.json`),
        get(`https://package.elm-lang.org/packages/${packageName}/latest/docs.json`)
    ]);
    console.log(elmJson);
}

downloadFiles(process.argv[2])
    .catch(console.error);
