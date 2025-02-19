# elm-review

`elm-review` analyzes [Elm](https://elm-lang.org/) projects, to help find mistakes before your users find them.

[![elm-review reporter output](https://github.com/jfmengels/elm-review/blob/2.2.0/documentation/images/elm-review-report.png?raw=true)](https://github.com/jfmengels/elm-review/blob/2.0.1/documentation/images/elm-review-report-hq.png?raw=true)

## What does `elm-review` do?

`elm-review` analyzes your source code, trying to recognize code that is known to cause problems.
All the rules describing problematic code are written in Elm, and `elm-review` does not come with built-in rules;
instead users are encouraged to write rules themselves and publish them as Elm packages, for everyone to benefit.
[Search the package registry](https://klaftertief.github.io/elm-search/?q=Review.Rule.Rule) to find what's out there!

Encouraging users to write rules also makes it easy to add custom rules that only apply to your project.
Such as rules that:

  - enforce that e.g. image paths only live in an `Images` module, which other modules can reference.
  - make everyone use a common `Button` component, instead of creating their own.
  - help users of a library you made, to avoid making mistakes that your API could not prevent them from doing.

Beware how and why you introduce rules in your project though.
Often a good API, that guides users to correct solutions, is the best way to go, so instead of writing a rule, maybe there is an API that can be improved?
But if a rule seems like the best solution, remember to discuss it with your team.
It's easy to mix up patterns that are objectively bad, with patterns that you personally find problematic, and forbidding patterns that other people find useful can be very disruptive.


## Try it out

The easiest way to run `elm-review`, if you have `Node.js` and `npm` installed, is to use the [`elm-review` CLI tool](https://github.com/jfmengels/node-elm-review).

```bash
# Save it to your package.json, if you use npm in your project.
# This is the recommended way.
npm install elm-review --save-dev

# Install globally. This is not recommended.
npm install -g elm-review
```

You can also **try it out without installing it or configuring it** if you have Node.js installed.
All you need is to find a configuration on GitHub, and note that `elm-review` packages are encouraged to provide an example one. Once you found the configuration, run `elm-review` by specifying the name of the GitHub repository and the path to the configuration:

```bash
npx elm-review --template jfmengels/elm-review-unused/example
```

Do you want to find and remove all the dead code in your project? Then run the following command (this might take a while if your project has a lot of dead code though!), and think about whether you want this goodness in your project!

```bash
npx elm-review --template jfmengels/elm-review-unused/example --fix-all
```

More information on that in [the CLI documentation](https://github.com/jfmengels/node-elm-review#try-it-out).


## Configuration

`elm-review` is configured through a `review/` folder in your project. It is a self-contained Elm project where you can
specify your dependencies, and write, import, and configure review rules.

Rules are configured in the `review/src/ReviewConfig.elm` file:

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


## Get started

You can get started with a fresh configuration by running the `elm-review init` command with the command line tool installed,
which will add a `review` folder to your project.

You can also use an existing configuration using `elm-review init --template <some configuration>`.
I created [some configurations](https://github.com/jfmengels/elm-review-config) that I believe can be good **starting** points.

```bash
# Start with an empty configuration
elm-review init

# Starter configuration for an Elm application
elm-review init --template jfmengels/elm-review-config/application

# Starter configuration for an Elm package
elm-review init --template jfmengels/elm-review-config/package
```

Once you have set up an initial configuration, you can add new rules. As `elm-review` does not
[come with built-in rules](https://github.com/jfmengels/elm-review/blob/master/documentation/design/no-built-in-rules.md),
you can look for packages with rules on the [Elm package registry](https://package.elm-lang.org/) by searching for packages named `elm-review-`.

Once you've found a package that you like, you can install it with the `elm install` command, just like any other Elm project dependency.

```bash
cd review/ # Go inside your review configuration directory
elm install authorName/packageName
# then update your `review/src/ReviewConfig.elm` to add the rule
# as explained in the package's documentation
```

Before you start adding rules or an unfamiliar existing configuration, I suggest reading the rest of this document, especially the section on [when to enable a rule](#when-to-write-or-enable-a-rule).


## Write your own rule

You can write your own rule using this package's API and [`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/).
Check out the [`Review.Rule`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.15.1/Review-Rule/) documentation for how to get started.

**NOTE**: If you want to **create a package** containing `elm-review` rules, I highly recommend using the
[CLI's](https://github.com/jfmengels/node-elm-review/) `elm-review new-package` subcommand. This will create a new package that will help you use the best practices and give you helpful tools like easy auto-publishing. More information is available in the maintenance file generated along with it.

If you want to **add/create a rule** for the package or for your local configuration, then I recommend using `elm-review new-rule`, which will create a source and test file which you can use as a starting point. For packages, it will add the rule everywhere it should be present (`exposed-modules`, README, ...).


Here's an example of a rule that prevents a typo in a string that was made too often at your company.

```elm
module NoStringWithMisspelledCompanyName exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
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
expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        -- It will look at string literals (like "a", """a""")
        Expression.Literal str ->
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

import NoStringWithMisspelledCompanyName
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoStringWithMisspelledCompanyName.rule
    -- other rules...
    ]
```

If you want to write a rule but might not have an idea of where to start,
have a look on the [elm-review-rule-ideas](https://github.com/jfmengels/elm-review-rule-ideas)
repository. You may also want to look for rules in the [Elm packages registry](https://package.elm-lang.org/) and in the [GitHub `elm-review` topic](https://github.com/topics/elm-review).


## When to write or enable a rule

The bar to write or enable a rule should be pretty high.
A new rule can often turn out to be a nuisance to someone, sometimes in ways you didn't predict, so making sure the rule solves a real problem, and that your team is on board with it, is important.
If a developer disagrees with a rule, they may try to circumvent it, resulting in code that is even more error prone than the pattern that was originally forbidden.
So the value provided by the rule should be much greater than the trouble it causes, and if you find that a rule doesn't live up to this, consider disabling it.

Review rules are most useful when some pattern must never appear in the code.
It gets less useful when a pattern is allowed to appear in certain cases, as there is [no good solution for handling exceptions to rules](#is-there-a-way-to-ignore-errors-).
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
  - [ ] I have had problems with the pattern I want to forbid.
  - [ ] I could not find a way to solve the problem by changing the API of the problematic code or introducing a new API.
  - [ ] If the rule exists, I have read its documentation and the section about when not to enable the rule, and it doesn't apply to my situation.
  - [ ] I have thought very hard about what the corner cases could be and what kind of patterns this would forbid that are actually okay, and they are acceptable.
  - [ ] I think the rule explains well enough how to solve the issue, to make sure beginners are not blocked by it.
  - [ ] I have communicated with my teammates and they all agree to enforce the rule.
  - [ ] I am ready to disable the rule if it turns out to be more disturbing than helpful.

## Is there a way to ignore errors?

`elm-review` does not provide a way to disable errors on a case-by-case basis — by line or sections of code —
like a lot of static analysis tools do. I've written about this in
_[How disable comments make static analysis tools worse](https://jfmengels.net/disable-comments/)_.

Because you can't ignore errors easily, `elm-review` puts more burden on the rules, requiring them to be of higher quality
— less false positives — and better designed — avoiding rules that will inherently have lots of exceptions or false positives.

It does provide 2 systems that I think are better alternatives for the health of your project.

### Configuring exceptions

You can [configure exceptions](https://package.elm-lang.org/packages/jfmengels/elm-review/2.15.1/Review-Rule/#configuring-exceptions),
which consists of marking specific directories or files as not relevant to a rule or set of rules, preventing errors to be reported for those.

It is a good fit if you wish for `elm-review` to not report errors in vendored or generated code,
or in files and directories that by the nature of the rule should be exempted.

### Temporarily suppressing errors

`elm-review` has a system to temporarily suppressed errors which aims to help you gradually adopt rules that
report many errors in your project without having you fix all the issues beforehand.

Running `elm-review suppress` will generate one JSON file in `review/suppressed/` (in your review configuration) for
every rule that currently reports errors, and records the number of suppressed errors per file in your project.
These files should be included in your versioning system.

As long as suppression files exist for your project, running `elm-review` will behave as usual but with these additional behaviors:
- Suppressed errors won't be reported.
- If there are outstanding errors for the ignored rules and files, the related
  suppressed errors will be reported until you reduce the number of errors
  back to the number in the JSON file. This is a good opportunity to fix more!
- If no errors are being reported and there are less suppressed errors than
  before, suppression files will be updated automatically, in order to make
  sure no new errors get re-introduced unknowingly.

While you can run the `suppress` command to ignore newly reported errors, please do so with moderation. The aim is to
allow enabling rules while there are errors remaining and to have these fixed incrementally, not to make it easier to
ignore errors.

Use `elm-review suppress --help` to start using this, and read more about the
[design choices](https://github.com/jfmengels/elm-review/blob/master/documentation/design/suppress.md) that went into
the feature.

Note that to avoid uncommitted suppression files in your project's main branch, it is recommended to use
`elm-review suppress --check-after-tests` at the end of your test suite.


## Extract information

`elm-review` has quite a nice way of traversing the files of a project and collecting data, especially when things
aren't as simple as grepping or applying a regex on the code.

While the tool is mainly designed around reporting issues, you can also use it to extract information from
the codebase. You can use this to gain insight into your codebase, or provide information to other tools to enable
powerful integrations.

To make use of this feature, run `elm-review --extract --report=json` with a configuration containing a rule that uses
[`Rule.withDataExtractor`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.15.1/Review-Rule/#withDataExtractor).

The result for a rule will be stored under `<json>.extracts.<YourRuleName>`. To access it, you can then pipe the result
into either a `Node.js` script, a tool that expects JSON, or [`jq`](https://stedolan.github.io/jq/) as in the example below.

```bash
elm-review --extract --report=json |
    jq -r '.extracts["YourRuleName"]'

# or to be slightly faster
elm-review --extract --report=json --rules YourRuleName |
    jq -r '.extracts["YourRuleName"]'
```

Combine the above out with `--watch` for fast feedback when editing your code!

```bash
elm-review --report=json --extract
```

and by reading the value at `<output>.extracts["YourRuleName"]` in the output.