# elm-lint

An [Elm](http://elm-lang.org/) linter written in Elm. Get your code from correct to better.
You can learn about the API and the rules it provides on the [package documentation](http://package.elm-lang.org/packages/jfmengels/elm-ast).

## What does `elm-lint` do?

`elm-lint` analyzes your Elm source code, and tries to recognize patterns that may be considered harmful or can be improved upon.
If you are familiar with [ESLint](http://eslint.org/) from JavaScript, this is pretty much the same idea.

## Try it

The preferred method, if you have `Node.js` and `npm` installed, you can check out [`node-elm-lint`](https://github.com/jfmengels/node-elm-lint), which has instructions on. This will allow you to lint your whole project.

Also, you can try the unpolished online version [here](https://elm-lint.now.sh), where you can copy-paste your source code and see the linting errors.

Please note that the tool that analyzes your code has parsing issues and valid code may not be considered as such.

## Rules

These are the rules that are built-in and available for you to choose from.

- **DefaultPatternPosition** - Enforce the default pattern to always appear first or last.
- **NoConstantCondition** - Forbid the use of expressions in an If condition whose value are always the same.
- **NoDebug** - Forbid the use of `Debug` before it goes into production.
- **NoDuplicateImports** - Forbid importing the same module several times in a file.
- **NoExposingEverything** - Forbid exporting everything in your modules `module Main exposing (..)`, to make your module explicit in what it exposes.
- **NoImportingEverything** - Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed functions and types are unknown to them.
- **NoNestedLet** - Forbid nesting let expressions directly.
- **NoUnannotatedFunction** - Ensure every top-level function declaration has a type annotation.
- **NoUnusedVariables** - Reports variables that are declared but never used.
- **NoUselessIf** - Reports when both paths of an If expression result will lead to the same value.
- **NoUselessPatternMatching** - Reports case expressions that can be simplified. Either when all patterns will lead to the same value, or when a pattern will lead to the same value as the default pattern.
- **NoWarningComments** - Detect comments containing words like `TODO`, `FIXME` and `XXX`.
- **SimplifyPiping** - Simplify piped functions like `List.map f >> List.map g` to `List.map (f >> g)`
- **SimplifyPropertyAccess** - Replace functions that only return the property of its parameter by an access function, like `(\x -> x.foo)` to `.foo`
- **ElmTest.NoDuplicateTestBodies** - Forbid having multiple tests with the same bodies. Often a consequence of copy-pasting tests.

Note that some rules were implemented but may not be good ideas. Think for yourself and ask the community whether you should enable them.

### FAQ

- I get the error `(Critical) Parsing error: expected end of input` for some of my files, what is happening?

This means that your file could not be parsed.
You should try and copy-paste that file's source code into the [`elm-ast` online demo](http://bogdanp.github.io/elm-ast/example/) to see if you can reproduce the error. If you are able to, then try to make a minimal reproducible example and open an issue on [`elm-ast`](https://github.com/Bogdanp/elm-ast).

**In most cases**, this is due to comments made using `--` (e.g. `-- a comment`) that are not well handled by the parser. [elm-lint] tries to remove them before parsing, but does a bad job at it at the moment (help wanted).

- Thanks for pointing out the error, but I would like to know **where** in my code the error is.

At the moment, `elm-ast` is missing positional information on the generated AST, that `elm-lint` uses. You can follow [this issue](https://github.com/Bogdanp/elm-ast/issues/13) if you want to know more or to contribute.

- I have an idea for a rule, how can I get it integrated into elm-lint?

Please open an issue here so we can talk about it. Try to make your proposal look like [this](https://github.com/eslint/eslint/blob/master/templates/rule-proposal.md).

[elm-lint] would like to be able to provide support for a plugin system so that you can work on it without my approval. Maybe that already works, but if it doesn't, please open an issue about that.

- The code looks bad and can be improved upon, also the documentation is lacking.

You're absolutely right. Please open an issue if you have suggestions or open a pull request!

- I want to know more!

I [presented](http://slides.com/jeroenengels/elm-lint) `elm-lint` a long time ago, before it was even remotely usable at the Paris Elm Meetup. Maybe it will teach you about it. Otherwise, just open an issue to ask your question or hit me on the [Elm Slack channel](https://elmlang.slack.com).
