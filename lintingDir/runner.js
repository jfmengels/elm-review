#!/usr/bin/env node

const getElmFiles = require('./elm-files')
const Elm = require('./compiledLintApp');

const elmFiles = getElmFiles([]);
if (elmFiles.length === 0) {
  console.error('Could not find any files to lint.');
  process.exit(1);
}

const app = Elm.LintApp.worker();

app.ports.linting.send(elmFiles);

app.ports.resultPort.subscribe(function(report) {
  console.log(report);
});
