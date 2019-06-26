# elm-lint

An [Elm](http://elm-lang.org/) linter written in Elm. Get your code from correct to better.
You can learn about the API and the rules it provides on the [package documentation](http://package.elm-lang.org/packages/jfmengels/elm-lint).

## What does `elm-lint` do?

`elm-lint` analyzes your Elm source code, and tries to recognize patterns that may be considered harmful or can be improved upon.
If you are familiar with [ESLint](http://eslint.org/) from JavaScript, this is pretty much the same idea.

The idea is to improve your Elm source code base, after it passes compilation and [elm-format](https://github.com/avh4/elm-format) has been run on it.

This packages offers a few rules that you can pick and configure to improve your code base, but you can also create your own rules, to enforce rules specific to your project or team. A few cases:
- You noticed a bad pattern in your codebase, wrote a nice module to handle the pattern better, and want to prevent your team from writing that pattern from now on. You can then write a rule to detect that pattern and have it suggest using your module instead. If you don't, you need to communicate this well to all your
  - When using the [core HTML package](https://package.elm-lang.org/packages/elm/html/latest/), you may style your tags using the [style function](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#style). When using [elm-css](https://package.elm-lang.org/packages/rtfeldman/elm-css), you probably want to avoid using that function (or the core HTML package altogether) and can write a rule for it.
- You published a library in the Elm package registry, and notice some pitfalls that users can fall in, that all your research for a better API do not prevent. You can then publish a separate package (or even in the same package) with rules preventing those pitfalls, should the user use `elm-lint` in their project.
- You often notice that strings in your codebase contain very common typos, or bad use of punctuation (like a missing space after `;`).

When solving a problem, a good API is a usually a better solution than writing a linter rule. But in some cases, even if you've written a good API, nothing prevents teammates or yourself from falling in the same unwanted patterns as before, especially when dealing with primitive values or constructs.

When introducing `elm-lint` or new rules to your project and team, you should probably discuss it with them first. It is easy to think that some patterns are always better and want to enforce them, where in reality some edge cases exist where they aren't wanted. Also, people don't usually like it when seemingly arbitrary rules are imposed on them, especially if it relates to code style, so be sure to talk with them and explain the rationale.

## Project aim and status

At the moment, this is a toy project where I experiment to try the best way to write linter rules for Elm. It's potential strength and aim, compared to [elm-analyse](https://github.com/stil4m/elm-analyse), which is the solution you'd want to use if you were looking for an Elm linter, is to be able to write your own rules.

I'd love to be able to use it myself on Elm projects and maybe to see it adopted by the community, but I don't have the energy to start this endeavor by myself. For this to be usable outside of the example demo (linked below), it would require at least a CLI that loads all files from a project and returns a list of errors (which [elm-analyse](https://github.com/stil4m/elm-analyse) does).

## Try it

The preferred method, if you have `Node.js` and `npm` installed, you can check out [`node-elm-lint`](https://github.com/jfmengels/node-elm-lint), which has instructions on. This will allow you to lint your whole project.

Also, you can try the unpolished online version [here](https://elm-lint.now.sh), where you can copy-paste your source code and see the linting errors.

Do remember that `elm-lint` is supposed to be run after the Elm compiler has validated the code, and is thus very unhelpful if you have parsing errors.


## Rules

These are the rules that are built-in and available for you to choose from.

- **DefaultPatternPosition** - Enforce the default pattern to always appear first or last.
- **NoDebug** - Forbid the use of `Debug` before it goes into production.
- **NoExtraBooleanComparison** - Forbid extraneous comparisons with booleans, like `(isAdmin user) == True`.
- **NoImportingEverything** - Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed functions and types are unknown to them.
- **NoUnusedVariables** - Reports variables that are declared but never used.

The following is a list of rules that were temporarily removed when changing the AST implementation, and which can potentially be re-added later.
- **NoExposingEverything** - Forbid exporting everything in your modules `module Main exposing (..)`, to make your module explicit in what it exposes.
- **NoConstantCondition** - Forbid the use of expressions in an If condition whose value are always the same.
- **NoNestedLet** - Forbid nesting let expressions directly.
- **NoUnannotatedFunction** - Ensure every top-level function declaration has a type annotation.
- **NoUselessIf** - Reports when both paths of an If expression result will lead to the same value.
- **NoUselessPatternMatching** - Reports case expressions that can be simplified. Either when all patterns will lead to the same value, or when a pattern will lead to the same value as the default pattern.
- **NoWarningComments** - Detect comments containing words like `TODO`, `FIXME` and `XXX`.
- **SimplifyPiping** - Simplify piped functions like `List.map f >> List.map g` to `List.map (f >> g)`
- **SimplifyPropertyAccess** - Replace functions that only return the property of its parameter by an access function, like `(\x -> x.foo)` to `.foo`
- **ElmTest.NoDuplicateTestBodies** - Forbid having multiple tests with the same bodies. Often a consequence of copy-pasting tests.


## Configuration

Configuration is done via an Elm file. Note that this is an experiment, as loading a configuration written in JSON, YAML or similar format is probably much faster to load than compiling the configuration using the Elm compiler. The benefit of having the configuration written in Elm, is having nicer error messages when there is a misconfiguration, potential auto-completion, and more explicit rule locations (no need for some magic to find the rules defined by a plugin for instance).

```elm
module LintConfig exposing (config)

import Lint exposing (Severity(..))
import Lint.Rule exposing (Rule)
import Lint.Rule.DefaultPatternPosition as DefaultPatternPosition
import Lint.Rule.NoUnusedVariables
import Lint.Rule.NoDebug
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything


config : List ( Severity, Rule )
config =
    [ ( Critical, DefaultPatternPosition.rule DefaultPatternPosition.ShouldBeLast )
    , ( Critical, Lint.Rule.NoExtraBooleanComparison.rule )
    , ( Critical, Lint.Rule.NoUnusedVariables.rule )
    , ( Warning, Lint.Rule.NoDebug.rule )
    , ( Critical, Lint.Rule.NoDuplicateImports.rule )
    , ( Critical, Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] } )
    ]
```

For each rule, you need to associate a `Severity` level.
- `Critical` - The rule is enforced and any patterns that the rule finds will be reported. The (yet non-existent) `elm-lint` CLI will fail with an error code if anything gets reported.
- `Warning` - The rule is enforced and any patterns that the rule finds will be reported. But the (yet non-existent) `elm-lint` CLI will not fail with an error code even if something gets reported.
- `Disabled` - The associated rule is not enforced and nothing will get reported for this rule.


## Write your own rule

You can write your own rule using this package's API and [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest).

Here's an example of a rule that forbids using the `Html` module, and suggests using `elm-css` or `elm-ui` instead.

```elm
module NoCoreHtml exposing (rule)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newSchema "NoCoreHtml"
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromSchema


importVisitor : Node Import -> List Error
importVisitor node =
    let
        moduleName : List String
        moduleName =
            node
                |> Node.value
                |> .moduleName
                |> Node.value
    in
    case moduleName of
        [ "Html" ] ->
            [ Rule.error "Use `elm-css` or `elm-ui` instead of the core HTML package." (Node.range node) ]

        _ ->
            []
```

Then add the rule in your configuration:

```elm
module LintConfig exposing (config)

import Lint exposing (Severity(..))
import Lint.Rule exposing (Rule)
import NoCoreHtml


config : List ( Severity, Rule )
config =
    [ ( Critical, NoCoreHtml.rule )
    -- other rules...
    ]
```


## Questions and comments you may have

- Is there a way to disable a reported error only in some locations?

There is none at the moment, but one may be added at a later time, probably in the form of a comment, as is done in [`ESLint`](https://eslint.org/docs/user-guide/configuring#disabling-rules-with-inline-comments). Please let me know if and when you need it.

- Is there a way to automatically fix the reported errors?

There is none at the moment. Maybe this will happen at a later time.

- I have an idea for a rule, how can I get it integrated into `elm-lint`?

Please open an issue here so we can talk about it. Try to make your proposal look like [this](https://github.com/eslint/eslint/blob/master/templates/rule-proposal.md).

You can also write your rule using the API provided in this package in a new package or in your project directly. There is no need to have it in `elm-lint` to use it.

- I am writing my own rule, but some nodes are not visited / something is not working as expected.

I know about some missing pieces, and there are others that I don't know about as I haven't written any rules using those nodes. If something is not working, please open an issue.

- I want to know more!

I [presented](http://slides.com/jeroenengels/elm-lint) `elm-lint` a long time ago, before it was even remotely usable at the Paris Elm Meetup. Maybe it will teach you about it. Otherwise, just open an issue to ask your question or hit me on the [Elm Slack channel](https://elmlang.slack.com), my username is `jfmengels`.
