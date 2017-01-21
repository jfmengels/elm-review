# elm-lint

An [Elm](http://elm-lang.org/) linter written in Elm.

## Try it

This is a prototype, so there is no CLI yet, but you can test `elm-lint` online [here](https://elm-lint.now.sh).
Enter your source code in the top-left box, and see the reported errors in the bottom-left box.

Please note that the tool that analyzes your code has parsing issues and valid code may not be considered as such.

## What does this tool do?

`elm-lint` analyzes your Elm source code, and tries to recognize patterns that may be considered harmful.
If you are familiar with [ESLint](http://eslint.org/) from JavaScript, this is pretty much the same idea.

You can read the slides for my [presentation](http://slides.com/jeroenengels/elm-lint) of this tool to learn more about it.

## Rules

- [NoConstantCondition](rules/NoConstantCondition.md) - Forbid the use of expressions in an If condition whose value are always the same.
- [NoDebug](rules/NoDebug.md) - Forbid the use of `Debug` before it goes into production.
- [NoDuplicateImports](rules/NoDuplicateImports.md) - Forbid importing the same module several times in a file.
- [NoExposingEverything](rules/NoExposingEverything.md) - Forbid exporting everything in your modules `module Main exposing (..)`, to make your module explicit in what it exposes.
- [NoUnannotatedFunction](rules/NoUnannotatedFunction.md) - Ensure every top-level function declaration has a type annotation.
- [NoUnusedVariables](rules/NoUnusedVariables.md) - Reports variables that are declared but never used.
- [SimplifyPiping](rules/SimplifyPiping.md) - Simplify piped functions like `List.map f >> List.map g` to `List.map (f >> g)`

More rule ideas in this [slide](http://slides.com/jeroenengels/elm-lint#/5/3) and the ones below it.
Note that some rules were implemented but may not be good ideas. Think for yourself and ask the community whether you should enable them.

MIT © [Jeroen Engels](https://github.com/jfmengels)
