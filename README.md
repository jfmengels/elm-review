# elm-review

![](https://travis-ci.com/jfmengels/elm-review.svg?branch=master)

`elm-review` analyzes [Elm](https://elm-lang.org/) code, to help find mistakes before your users find them.

![elm-review reporter output](https://github.com/jfmengels/elm-review/blob/master/documentation/images/elm-review-report.png?raw=true)

## What does `elm-review` do?

`elm-review` analyzes your source code, trying to recognize code that is known to cause problems.
It is a linting program, similar to e.g. [ESLint](http://eslint.org/) for JavaScript, or [clippy](https://github.com/rust-lang/rust-clippy) for Rust.

To make `elm-review` check for problematic code, `elm-review` must be configured with rules, that describes what to look for.
Rules can be published as Elm packages, so you can find common rules by [searching the package registry](https://klaftertief.github.io/elm-search/?q=Review.Rule.Rule).
`elm-review` allows custom rules, so you can make rules that only make sense for your project.
Like rules that:

  - enforce that e.g. image paths only live in an `Images` module, which other modules can reference.
  - make everyone use a common `Button` component, instead of creating their own.
  - help users of a library you made, to avoid making mistakes that your API could not prevent them from doing.

Beware how and why you introduce rules though.
Often a good API, that guides users to correct solutions, is the best way to go, so instead of writing a rule, maybe there is an API that can be improved?
But if a rule seems like the best solution, remember to discuss it with your team.
It's easy to mix up patterns that are objectively bad, with patterns that you personally find problematic, and forbidding patterns that other people use can be very disruptive for them.

## Try it

The preferred method, if you have `Node.js` and `npm` installed, is to use the [`elm-review` CLI tool](https://github.com/jfmengels/node-elm-review).

```bash
# Save it to your package.json, if you use npm in your project.
# This is the recommended way.
npm install elm-review --save-dev

# Install globally. This is not recommended.
npm install -g elm-review
```

You can also try [the online version here](https://elm-review.now.sh), where you can copy-paste your source code and see the review errors.

## Configuration

Rules are configured in the `ReviewConfig.elm` file:

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

You can get started with an empty configuration with the command line tool by running `elm-review init`.
This will add a `review` folder to your project, which is a self-contained Elm project where you can write, import, and configure review rules.
`elm-review` does not [come with any built-in rules](https://github.com/jfmengels/elm-review/blob/master/documentation/design/no-built-in-rules.md).
Instead you can find rules in the Elm package registry by [using `elm-search` and searching for `Review.Rule.Rule`](https://klaftertief.github.io/elm-search/?q=Review.Rule.Rule), and installing them with the `elm install` command.

```bash
cd review/ # Go inside your review configuration directory
elm install authorName/packageName
```

Before you start adding rules though, I suggest reading the rest of this document, especially the section on [when to enable a rule](#when-to-write-or-enable-a-rule).

## Write your own rule

You can write your own rule using this package's API and [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
Check out the [`Review.Rule`](./Review-Rule) module for more instructions.

Here's an example of a rule that prevents a typo in a string that was made too often at your company.

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

The bar to write or enable a rule should be pretty high.
If you're asking yourself if you should enable a certain rule, I recommend your default answer be "no".
A new rule will often turn out to be a nuisance to someone, sometimes in ways you can't predict, so making sure your team is on board with the rule is important.
If a developer disagrees with a rule, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the rule should be much greater than the trouble it causes, to keep everyone happy.

Review rules are most useful when some pattern must never appear in the code.
It gets less useful when a pattern is allowed to appear in certain cases, as there is [no good solution for handling exceptions to rules](#is-there-a-way-to-ignore-an-error-or-disable-a-rule-only-in-some-locations-).
If you really need to make exceptions, they must be written in the rule itself, or the rule should be configurable.

For rules that enforce a certain **coding style**, or suggest simplifications to your code, I would ask you to raise the bar for inclusion even higher.
A few examples:

  - I much prefer using `|>` over `<|`, and I think using the latter to pipe
  functions over several lines is harder to read. Even if using `|>` was indeed
  better for most situations and even if my teammates agree, this would prevent
  me from writing tests [the suggested way](https://github.com/elm-explorations/test#quick-start)
  for instance.
  - If a record contains only one field, then I could suggest not using a record
  and use the field directly, which would make things simpler. But using a
  record can have the advantage of being more explicit: `findFiles [] folder` is
  harder to understand than `findFiles { exceptions = [] } folder`.

Some rules might suggest using advanced techniques to avoid pitfalls, which can make it harder for newcomers to get something done.
When enabling this kind of rule, make sure that the message it gives is helpful enough to unblock users.

When wondering whether to enable a rule, I suggest using this checklist:
  - [x] I have had problems with the pattern I want to forbid.
  - [x] I could not find a way to solve the problem by changing the API of the problematic code or introducing a new API.
  - [x] If the rule exists, I have read its documentation and the section about when not to enable the rule, and it doesn't apply to my situation.
  - [x] I have thought very hard about what the corner cases could be and what kind of patterns this would forbid that are actually okay, and they are acceptable.
  - [x] I think the rule explains well enough how to solve the issue, to make sure beginners are not blocked by it.
  - [x] I have communicated with my teammates and they all agree to enforce the rule.
  - [x] I am ready to disable the rule if turns out the team is finding it disturbing or unhelpful.
  
If you have never encountered a problem with a pattern before, then you probably don't need to forbid it.
There are chances the problem will never occur, and writing the rule is a waste of time.

## Is there a way to ignore an error or disable a rule only in some locations?

You can prevent errors from being reported by either changing the implementation of your rules or by [configuring exceptions](./Rule-Review#configuring-exceptions) for directories or for files.

It is however not possible to ignore errors on a case-by-case basis, for several reasons:

  - The most practical way to locally disable a rule would probably be through
  comments, like [how `ESLint` does it](https://eslint.org/docs/user-guide/configuring#disabling-rules-with-inline-comments).
  But since [elm-format](https://github.com/avh4/elm-format) moves comments around, this is not practical.
  - If there are some rules that you really want to enforce because you want to
  create additional guarantees in your codebase, and it is possible to ignore it,
  then you will need a second system to ensure those rules are never ignored.
  - When people encounter a review error, some will try to disable
  it by default, because they disagree with the rule, are annoyed by it, or
  because they think they will fix the issue later or not at all. Just like we learned
  with the compiler errors, some problems require us to do some additional
  work for good reasons, and I think this should apply to errors reported by
  `elm-review` too. Obviously, not being able to ignore an error means that the
  bar to write or enable a rule should be even higher.
  - If you are looking to make an exception to a rule, really consider if the rule as a whole shouldn't just be disabled.
