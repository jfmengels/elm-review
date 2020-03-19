const fs = require('fs');
const { execSync } = require("child_process");

const files = fs.readdirSync(__dirname);
const elmFiles = files.filter(file => file.endsWith(".elm"));

elmFiles.forEach(elmFile => {
  try {
    execSync(`elm make ./${elmFile} 2> ./${elmFile.slice(0, -4)}.txt`, {encoding:"utf8", stdio: 'pipe', cwd: __dirname}).toString();
  } catch (e) {}
});
