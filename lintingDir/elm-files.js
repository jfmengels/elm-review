const path = require('path');
const _ = require('lodash');
const fs = require('fs-extra');
const glob = require('glob');

const defaultGlob = '**/*.elm';
const ignore = ['**/elm-stuff/**', '**/node_modules/**', 'lintingDir/**'];

function getFiles(filename) {
  if (!fs.existsSync(filename)) {
    return [];
  }
  if (fs.lstatSync(filename).isDirectory()) {
    console.log(candidate);
    return _.flatMap(
      glob.sync('/' + defaultGlob, {
        root: filename,
        nocase: true,
        ignore: ['/**/elm-stuff/**', '/**/node_modules/**'],
        nodir: true
      }),
      resolveFilePath
    );
  }
  return [filename];
}

// Recursively search directories for *.elm files, excluding elm-stuff/
function resolveFilePath(filename) {
  // Exclude everything having anything to do with elm-stuff
  return getFiles(filename).filter(
    candidate => !candidate.split(path.sep).includes('elm-stuff')
  );
}

function globify(filename) {
  return glob.sync(filename, {
    nocase: true,
    ignore: ignore,
    nodir: false
  });
}

function globifyWithRoot(root, filename) {
  return glob.sync(filename, {
    root: root,
    nocase: true,
    ignore: ignore,
    nodir: false
  });
}

function getElmFilePaths(filePathArgs) {
  if (filePathArgs.length > 0) {
    return _.flatMap(filePathArgs, globify);
  }

  const root = path.join(path.resolve(process.cwd()), '..');
  return globifyWithRoot(root, '**/*.elm');
}


function getElmFiles(filePathArgs) {
  const relativeElmFiles = getElmFilePaths(filePathArgs);
  return _.flatMap(relativeElmFiles, resolveFilePath).map(file => {
    return {
      filename: file,
      source: fs.readFileSync(file, 'utf8')
    };
  });
}

module.exports = getElmFiles;
