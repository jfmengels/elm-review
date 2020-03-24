const fs = require('fs');
const path = require('path');
const { execSync } = require("child_process");

const files = fs.readdirSync(__dirname);
const elmFiles = files.filter(file => file.endsWith(".elm"));

elmFiles.forEach(elmFile => {
  const expectedOutput = fs.readFileSync(path.join(__dirname, `${elmFile.slice(0, -4)}.txt`), 'utf8');

  try {
    const output = execSync(`elm make ./${elmFile}`, {encoding:"utf8", stdio: 'pipe', cwd: __dirname}).toString();
    console.log(`ERROR: File ${elmFile} compiled, though it shouldn't have!`);
    process.exit(1);
  }
  catch (error) {
    if (error.stderr !== expectedOutput) {
        console.log(`ERROR: File ${elmFile} failed to compile, but with the wrong error message!`);
        console.log(`EXPECTED:\n\n${expectedOutput}\n\n`);
        console.log(`GOT:\n\n${error.stderr}`);
        process.exit(1);
    }
  }
});

console.log('No problems with phantom types!');
