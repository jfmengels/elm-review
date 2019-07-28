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
- You have one module in your codebase which centralizes some data used accross the application (the paths to all the images, a list of all the available colors, ...), but you keep finding new definitions of that data accross the codebase.

When solving a problem, a good API is a usually a better solution than writing a linter rule. But in some cases, even if you've written a good API, nothing prevents teammates or yourself from falling in the same unwanted patterns as before, especially when dealing with primitive values or constructs.

When introducing `elm-lint` or new rules to your project and team, you should discuss it with them first. It is easy to think that some patterns are always better and want to enforce them, where in reality some edge cases exist where they aren't wanted. Also, people don't usually like it when seemingly arbitrary rules are imposed on them, especially if it relates to code style, so be sure to talk with them and explain the rationale.

## Try it

The preferred method, if you have `Node.js` and `npm` installed, is to use [`node-elm-lint`](https://github.com/jfmengels/node-elm-lint), which has instructions on how to install it. This will allow you to lint your whole project.

Also, you can try the online version [here](https://elm-lint.now.sh), where you can copy-paste your source code and see the linting errors.

Do remember that `elm-lint` is supposed to be run after the Elm compiler has validated the code, and is thus very unhelpful if you have parsing errors.

## Rules

These are the rules that are built-in and available for you to choose from.

- **NoDebug** - Forbid the use of `Debug` before it goes into production.
- **NoExtraBooleanComparison** - Forbid extraneous comparisons with booleans, like `(isAdmin user) == True`.
- **NoImportingEverything** - Forbid importing everything from your module. This can especially be confusing to newcomers when the exposed functions and types are unknown to them.
- **NoUnusedVariables** - Reports variables that are declared but never used.
- **NoUnusedTypeConstructors** - Reports type constructors that are declared but never used.

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

Since the rule is written in Elm, the rules are publishable on the Elm package registry, and more Elm users should be able to write their own rule than if it was written in a different language like Haskell.

```elm
module LintConfig exposing (config)

import Lint.Rule exposing (Rule)
import Lint.Rule.NoDebug
import Lint.Rule.NoExtraBooleanComparison
import Lint.Rule.NoImportingEverything
import Lint.Rule.NoUnusedTypeConstructors
import Lint.Rule.NoUnusedVariables


config : List Rule
config =
    [ Lint.Rule.NoExtraBooleanComparison.rule
    , Lint.Rule.NoUnusedVariables.rule
    , Lint.Rule.NoUnusedTypeConstructors.rule
    , Lint.Rule.NoDebug.rule
    , Lint.Rule.NoImportingEverything.rule { exceptions = [ "Html" ] }
    ]
```

## Write your own rule

You can write your own rule using this package's API and [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest). Check out the [`Lint.Rule`](./Lint-Rule) module for more instructions.

Here's an example of a rule that prevents a typo in a string that was made too often at your company.

```elm
module NoStringWithMisspelledCompanyName exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Lint.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newSchema "NoStringWithMisspelledCompanyName"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        Literal str ->
            if String.contains "frits.com" str then
                [ Rule.error
                    { message = "Replace `frits.com` by `fruits.com`"
                    , details = [ "This typo has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                    }
                    (Node.range node)
                ]

            else
                []

        _ ->
            []
```

Then add the rule in your configuration:

```elm
module LintConfig exposing (config)

import Lint.Rule exposing (Rule)
import NoStringWithMisspelledCompanyName


config : List Rule
config =
    [ NoStringWithMisspelledCompanyName.rule
    -- other rules...
    ]
```


## When to write or enable a rule

The bar to write or enable a rule in your configuration should be pretty high. I
recommend having the default answer to the question "Should I write/enable this
rule?" be "no". I think that the ratio that compares the rule's value to the
nuisances it will cause should be very high, and it is often hard to foresee
all the nuisances.

Linting rules are useful when something must never appear in the code. It gets
much less useful when something should not appear only 99% of the time, as there
is no good solution for handling exceptions (`elm-lint` doesn't offer an option
for disabling a rule locally, see why [here](#is-there-a-way-to-ignore-an-error-or-disable-a-rule-only-in-some-locations-)).

First of all, if you have never encountered a problem with some pattern before,
then you probably don't need to forbid it. There are chances the problem will
never occur, and writing the rule is a waste of time. Or maybe when using it,
you will find that the pattern is actually not so bad at all and there are
situations where using it is actually the best option.

For rules that enforce a certain coding style, or even suggest simplifications
to your code. I would ask you to raise the bar even higher, as I think it is
rarely applicable or better 100% of the time. A few examples:
  - I much prefer using `|>` over `<|`, and I think using the latter to pipe
  functions over several lines is harder to read. Even if my teammates agree,
  this prevents me from writing tests [the suggested way](https://github.com/elm-explorations/test#quick-start).
  - If a record contains only one field, then I could suggest not using a record
  and use the field directly, which would make things much simpler. But using a
  record can have the advantage of being more explicit: `findFiles [] folder` is
  harder to understand than `findFiles { exceptions = [] } folder`.

It is very important to communicate with your teammates about the rule and that
they agree. Fighting to avoid a rule is very frustrating if the user who gets
the error actually thinks the code would be better with the forbidden pattern.
It is possible they will try to circumvent the rule in a way that was not the
suggested way, probably making the code worse than before.

Some rules might suggest using more advanced techniques to avoid some pitfalls,
and this might make it harder for newcomers or beginners to get something done.
When enabling this kind of rule, make sure that it is helpful enough to unblock
users, otherwise this can frustrate and/or block them.

When wondering whether to write or enable a rule, I suggest using this checklist:
  - I have had problems with the pattern I want to forbid
  - If the rule exists, I have read its documentation and the section about when
  not to enable the rule, and it doesn't apply to my situation
  - I have thought very hard about what the corner cases could be and what kind
  of patterns this would forbid that are actually okay, and there are none
  - I think the rule will not make it more difficult for newcomers or beginners,
  or it will but it gives some very helpful suggestions
  - I have communicated with my teammates and they all agree to enforce the rule
  - I am ready to disable the rule if after some time, the team is finding it too
  often disturbing or simply unhelpful


## Is there a way to ignore an error or disable a rule only in some locations?

There is none at the moment, for several reasons:
  - The most practical way to locally disable a rule would probably be through comments, like [how `ESLint` does it](https://eslint.org/docs/user-guide/configuring#disabling-rules-with-inline-comments). But since [elm-format](https://github.com/avh4/elm-format) would move the comments around, this would require you to try and figure out how/where to place the comment, or the rule would need to be disabled for a bigger section of the code than wanted. Neither option provides a good experience.
  - If there are some rules that you really want to enforce, and it's possible to ignore it, then you will want a second system to ensure those rules are never ignored.
  - When people encounter a linting error, quite often they will try to disable it by default, because they don't agree with the rule, or because they want to do later or not at all.
  The more I think about it, the more I think that if you need to make an exception to your rule somewhere, then maybe the rule is not worth enforcing in the first place, and that you should probably remove it from your configuration.
  - The more I think about it, the more I think that if you need to make an exception to your rule somewhere, then maybe the rule is not worth enforcing in the first place, and that you should probably remove it from your configuration.

It's a very all-or-nothing approach, but I prefer to start without the ability to disable rules, be convinced by good arguments and add it, rather than have it from the start with a not-so-great solution and have people write rules and add exceptions everywhere. Please let me know if and when you need it. That said, I suggest not waiting for this option to arrive and instead reading [when to write or enable a rule in a configuration](#when-to-write-or-enable-a-rule)

## Is there a way to automatically fix the reported errors?

There is none at the moment, but this might be added a later time. That said, it
is unlikely that all errors will be fixable, so there will always be a need for
manual fixing for some rules.
