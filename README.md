# elm-review

![](https://travis-ci.com/jfmengels/elm-review.svg?branch=master)

`elm-review` analyzes [Elm](https://elm-lang.org/) source code, to add additional guarantees to your project.

![elm-review reporter output](https://github.com/jfmengels/elm-review/blob/master/documentation/images/elm-review-report.png?raw=true)

## What does `elm-review` do?

`elm-review` analyzes your [Elm](https://elm-lang.org/) source code, and tries to recognize patterns that may be considered harmful or can be improved upon.
If you are familiar with [ESLint](http://eslint.org/) from JavaScript, this is pretty much the same idea.

The idea is to improve your Elm source code base, after it passes compilation and [elm-format](https://github.com/avh4/elm-format) has been run on it.

You can configure your project to be analyzed by different "rules". You can find [some in the Elm packages](https://klaftertief.github.io/elm-search/?q=Review.Rule.Rule), or you can write your own rules to enforce rules specific to your project or team. A few use-cases:
- You noticed a bad pattern in your codebase, wrote a nice module to handle the pattern better, and want to prevent your team from writing that pattern from now on. You can then write a rule to detect that pattern and have it suggest using your module instead. If you don't, you need to communicate this well to all your teammates, but there is no way to prevent the bad pattern from occurring again.
- You often notice that strings in your codebase contain very common typos, or bad use of punctuation (like a missing space after `;`).
- You have one module in your codebase which centralizes some data used across the application (the paths to all the images, a list of all the available colors, ...), but you keep finding new definitions of that data across the codebase.
- You published a library in the Elm package registry, and notice some pitfalls that users can fall in, that all your research for a better API does not prevent. You can then publish a separate package with rules preventing those pitfalls, should the user use `elm-review` in their project.

When solving a problem, a good API is a usually a better solution than writing a review rule. But in some cases, even if you've written a good API, nothing prevents teammates or yourself from falling in the same unwanted patterns as before, especially when dealing with primitive values or constructs.

When introducing `elm-review` or new rules to your project and team, you should discuss it with them first. It is easy to think that some patterns are always better and want to enforce them, where in reality some edge cases exist where they aren't wanted. Also, people don't usually like it when seemingly arbitrary rules are imposed on them, especially if it relates to code style, so be sure to talk with them and explain the rationale.

## Try it

The preferred method, if you have `Node.js` and `npm` installed, is to use [`elm-review`](https://github.com/jfmengels/node-elm-review).

```bash
# Save it to your package.json, if you use npm in your project.
# This is the recommended way.
npm install elm-review --save-dev

# Install globally. This is not recommended.
npm install -g elm-review
```

Also, you can try the online version [here](https://elm-review.now.sh), where you can copy-paste your source code and see the review errors.

## Configuration

Configuration is done via an Elm file. The benefit of having the configuration written in Elm, is having nicer error messages when there is a misconfiguration, potential auto-completion, and more explicit rule locations (no need for some magic to find the rules defined by a package for instance). Since the rules are written in Elm, they are publishable on the Elm package registry, and writing them should be more accessible than if it was written in a different language.


```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import Third.Party.Rule
import My.Own.Custom.rule
import Another.Rule


config : List Rule
config =
    [ Third.Party.Rule.rule
    , My.Own.Custom.rule
    , Another.Rule.rule { ruleOptions = [] }
    ]
```

You can get started with an empty configuration with the command line tool by running
`elm-review init`, which you can then add rules to. Before you do, I suggest
reading the rest of this document, but especially the section on
[when to enable a rule in your configuration](#when-to-write-or-enable-a-rule).

`elm-review` does not come with any built-in rules. You can read why [here](https://github.com/jfmengels/elm-review/blob/master/documentation/design/no-built-in-rules.md). You can find rules in the Elm package registry by [using `elm-search` and searching for `Review.Rule.Rule`](https://klaftertief.github.io/elm-search/?q=Review.Rule.Rule), and use by going to your review directory and running `elm install` in your terminal.

```bash
cd review/ # Go inside your review configuration directory
elm install authorName/packageName
```

You can also [write your own rules](#write-your-own-rule), as shown in the next
section.

## Write your own rule

You can write your own rule using this package's API and
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
Check out the [`Review.Rule`](./Review-Rule) module for more instructions.

Here's an example of a rule that prevents a typo in a string that was made too
often at your company.

```elm
module NoStringWithMisspelledCompanyName exposing (rule)

import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)

-- Create a new rule
rule : Rule
rule =
    -- Define the rule with the same name as the module it is defined in
    Rule.newModuleRuleSchema "NoStringWithMisspelledCompanyName" ()
        -- Make it look at expressions
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema

-- This function will visit all the expressions (like `1`, `"string"`, `foo bar`, `a + b`, ...)
-- and report problems that it finds
expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        -- It will look at string literals (like "a", """a""")
        Literal str ->
            if String.contains "frits.com" str then
                -- Return a single error, describing the problem
                [ Rule.error
                    { message = "Replace `frits.com` by `fruits.com`"
                    , details = [ "This typo has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                    }
                    -- This is the location of the problem in the source code
                    (Node.range node)
                ]

            else
                []

        _ ->
            []
```

Then add the rule in your configuration:

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
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

Review rules are useful when something must never appear in the code. It gets
much less useful when something should not appear only 99% of the time, as there
is no good solution for handling exceptions (`elm-review` doesn't offer an option
for disabling a rule locally, see why [here](#is-there-a-way-to-ignore-an-error-or-disable-a-rule-only-in-some-locations-)).
If you really need to make exceptions, they should be written in the rule itself
or defined in the rule's parameters and/or configuration.

First of all, if you have never encountered a problem with a pattern before,
then you probably don't need to forbid it. There are chances the problem will
never occur, and writing the rule is a waste of time. Or maybe when using it,
you will find that the pattern is actually not so bad at all and that there are
situations where using it is actually the best option.

For rules that enforce a certain **coding style**, or even suggest simplifications
to your code, I would ask you to raise the bar even higher, as I think it is
rarely applicable or better 100% of the time. A few examples:
  - I much prefer using `|>` over `<|`, and I think using the latter to pipe
  functions over several lines is harder to read. Even if using `|>` was indeed
  better for most situations and even if my teammates agree, this would prevent
  me from writing tests [the suggested way](https://github.com/elm-explorations/test#quick-start)
  for instance.
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
When enabling this kind of rule, make sure that the message it gives is helpful
enough to unblock users, otherwise this can frustrate and/or block them.

When wondering whether to write or enable a rule, I suggest using this checklist:
  - I have had problems with the pattern I want to forbid
  - I could not find a way to solve the problem by changing the API of the
  problematic code or introducing a new API
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

You can prevent errors from being reported by either changing the implementation
of your rules or by [configuring exceptions](./Rule-Review#configuring-exceptions)
for directories or for files.

It is however not possible to ignore errors on a case-by-case basis, for several reasons:

  - The most practical way to locally disable a rule would probably be through
  comments, like [how `ESLint` does it](https://eslint.org/docs/user-guide/configuring#disabling-rules-with-inline-comments).
  But since [elm-format](https://github.com/avh4/elm-format) would move the
  comments around, this would require you to try and figure out how/where to
  place the comment, or the rule would need to be disabled for a bigger section
  of the code than wanted. Neither option provides a good experience.
  - If there are some rules that you really want to enforce because you want to
  create additional guarantees in your codebase, and it is possible to ignore it,
  then you will want a second system to ensure those rules are never ignored.
  - When people encounter a review error, quite often they will try to disable
  it by default, because they disagree with the rule, are annoyed by it or
  because they think they will fix it later or not at all. Just like we learned
  with the compiler errors, some problems require us to do some additional
  work for good reasons, and I think this should apply to errors reported by
  `elm-review` too. Obviously, not being able to ignore an error means that the
  bar to write or enable a rule should be even higher.
  - The more I think about it, the more I think that if you need to make an
  exception to your rule somewhere, then maybe the rule is not worth enforcing
  in the first place, and that you should probably remove it from your
  configuration. Except for rules that try to enforce having a pattern allowed
  only in certain locations (like a single file that contains all the color
  definitions).

I suggest (re-)reading [when to write or enable a rule in a configuration](#when-to-write-or-enable-a-rule)
if you really need to ignore an error.
