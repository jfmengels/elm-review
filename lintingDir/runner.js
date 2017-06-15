#!/usr/bin/env node

const Elm = require('./ok');
const app = Elm.LintApp.worker();
app.ports.linting.send({
  filename: 'SomeFile.elm',
  source: "add a b = a + b"
});

app.ports.resultPort.subscribe(function(result) {
  console.log('result', result)
  // app.ports.suggestions.send(suggestions);
});
