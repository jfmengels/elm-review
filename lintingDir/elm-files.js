const path = require('path');
const fs = require('fs-extra');
const glob = require('glob');

const defaultGlob = '**/*.elm';
const ignore = ['**/elm-stuff/**', '**/node_modules/**', 'lintingDir/**'];

function flatMap(array, fn) {
  return array.reduce((res, item) => res.concat(fn(item)), []);
}

function getFiles(filename) {
  if (!fs.existsSync(filename)) {
    return [];
  }
  if (fs.lstatSync(filename).isDirectory()) {
    return flatMap(
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
  return getFiles(filename).filter(candidate => !candidate.split(path.sep).includes('elm-stuff'));
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
    return flatMap(filePathArgs, globify);
  }

  const root = path.join(path.resolve(process.cwd()), '..');
  return globifyWithRoot(root, '**/*.elm');
}

function getElmFiles(filePathArgs) {
  const relativeElmFiles = getElmFilePaths(filePathArgs);
  return flatMap(relativeElmFiles, resolveFilePath).map(file => {
    return {
      filename: file,
      source: fs.readFileSync(file, 'utf8')
    };
  });
}

module.exports = getElmFiles;
