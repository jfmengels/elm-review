# elm-review suppress

This feature comes from the frustration that it can be hard to enable a new rule. There are two choices when attempting to do so:
1. Fix all reported errors before enabling the rule. This means potentially a lot of work and/or delaying enabling the rule, which can cause new errors to be introduced in the meantime.
2. Enable the rule, but ignore the errors for the files that are being reported.

The suppression system aims to fix this problem.

There are several ways to implement this, and different tools have chosen different strategies. This document outlines
what I believed to be important, and therefore why `suppress` works the way it does. There are a lot of
trade-offs involved, so it does not get full marks everywhere, but overall I believe works pretty nicely.

### Make it easy to discern between purposefully ignored errors and temporarily suppressed errors.

This is one of the main goals of the feature. I don't think there's too much to say, except that we know have a
different system for each:
- `ignoreRule*` for the purposefully ignored errors
- the suppression system for temporarily suppressed errors


### Make it hard to introduce new errors

`elm-review` has always been very strict about requiring fixing errors that get reported (compared to tools with
[disable comments](https://jfmengels.net/disable-comments/)), and I'd like it to stay that way.

Before the suppression system, we were using the `ignoreRule*` functions for temporarily ignoring some errors,
but one of its problems was that it was possible for errors to be introduced.

The way that this feature is designed, it is at most possible to exchange an error for a rule and file with
a different error for the same rule and the same file.

I don't like that it's now easier than ever to add new suppressions, but I don't think there's a way around that.


### Make it painless to fix errors

Fixing errors is ultimately what needs to be done. I want this to be as painless as possible.

This translates to the following in practice:
- Running `elm-review` when you have fixed some errors (and no outstanding errors) will automatically update the
suppression files.

Without doing that, I foresee that the workflow would be something like the following:
- Run `npm test` (which contains `elm-review`)
- Wait for `elm-review` to fail the test suite because you fixed stuff
- Run `elm-review` with a special flag that updates the suppression files
- Run `npm test` again because you still don't know if the rest of the suite passes
- Commit and push

With the workflow above, fixing errors may feel like a punishment (it sounds like that to me), and I imagine that people
will avoid fixing issues to avoid having to go through all of this.

Instead, with the current workflow, it will more look like this:
- Run `npm test`, and suppression files get updated automatically
- At the end of your test suite, if you're running `elm-review suppress --check-after-tests`,
  you are kindly asked to commit, but also notified that you don't need to run the entire suite again, since all tests passed
- Commit and push

The downsides of this approach that I can _imagine_:
- People forget to run `elm-review` and/or to commit their suppression files, which cause future unrelated PRs from including this change because they did run `elm-review`.
That's what `--check-after-tests` is for, and I hope that fixes the issue.
- People get surprised by files being updated. That's fair, but it's not too hard to let people know I'd say? 🤷
- People want to undo their changes, and therefore also undo the changes to the suppression files. Also fair, but nothing
that `git restore HEAD review/suppressed` can't solve (and it's not too hard to teach? 🤷).

When balancing these up and downsides, I consider this to be the superior approach.


### Make it easy to have an overview of suppressed errors

Having suppressed errors are nice and easy, but at some point they need to be fixed, and that will likely need some
manual work. Once someone decides to tackle the issues, they need to be able to decide how and where to get
started, meaning they need to have some kind of overview.

I decided to make the suppression files be formatted in a way that give an easy overview.
- Want to know what rules were suppressed? Look at the names of the files in `review/suppressed/`.
- Want to know in what files rule X was suppressed? Look at the related file.
- Want to tackle one of the files? Choose the first or the last one, depending on how you prefer tackling these,
  as they are sorted from the most to the least suppressed errors.
- Want to tackle file Y? Delete the line manually (easy!), and now those errors will be reported.

I could have chosen to have the overview be done through CLI subcommands, and I'm not opposed to having that as well if there
is useful information we can compile for the user, but having it in a file makes it accessible (you can look at it on
GitHub, even see the history), and even actionable (removing suppressions for a file).


### Make it easy to go fix errors

A bit similar to the previous goal, but more about giving you the tools to do the job.

This translates to the following in practice:
- Being able to change the suppression files (as seen before)
- Several CLI flags to report the suppressed errors: `--unsuppress` and `--unsuppress-rules`
- The rest of the CLI flags working nicely with this system
  - Running with `--fix` and `--fix-all` give you automatic fix proposals if the errors are unsuppressed (manually or not)
  - Running with `--watch` automatically update the suppression files continuously

I think that editors will make this especially nice, which is why `elm-review`
[provides other tooling](https://github.com/jfmengels/node-elm-review/blob/master/documentation/tooling-integration.md)
with all the necessary information to lead users towards doing the right thing.


### Make it easy to see when more errors get suppressed

One drawback of this system is that it makes it _too_ easy to ignore new errors, and I don't want that to become easy.
When this happens, I want to push the team to at least have a discussion about whether this suppression makes sense or
if it was the developer being unnecessarily lazy.

This translates to the following in practice:
- A new line shows up in the Git diff, or the count in a line shows up as higher than before in the diff.

I believe that the most important information here is what rule was suppressed, which would be visible in the diff.
Seeing a teammate ignore `NoUnused.Variables` can be brushed off easily, but noticing them ignore `NoLeakingSecretKeys`
will likely start a discussion.

This could be improved by having the suppressed error (message, details, position, ...) in the suppressed files, as the
diff would make it clear what new error was suppressed, but that would make the suppression more fragile.
I think that the rule name is in general enough. Also, the files would become humongous with all that information.


### Celebrate fixing errors

I am always happy when I see that I or someone fixed `elm-review` issues, and I want to celebrate that effort or happy accident,
which I think/hope will make people more inclined to fix issues.

This translates to the following in practice:
- In the CLI, there is a message saying when you fixed errors.
- In the diff of a PR, you see a difference. It's best visible when all the errors for a rule and file have been fixed.


### Have consistent results

Just like pure functions are simple and predictable, I want `elm-review` to feel the same way.
So if you have the same input (project), you should get the same output (reported errors).

(Ok, with the automatically updating of suppression files, maybe we can consider that `elm-review` has a side effect...)


This mostly comes down to the handling of time. [`ember-template-lint`](https://github.com/ember-template-lint/ember-template-lint) has an interesting feature of
suppressing errors for a certain duration, before being reported [as warnings](https://github.com/jfmengels/elm-review/blob/master/documentation/design/severity-levels.md),
and before being reported as errors after some more time.

While I agree that it may push people towards fixing reported problems, I don't think it
works out (I have asked some users, but not tried it out myself though), and I imagine the following problems:
- The CI for the main branch suddenly breaks because warnings have been ignored for too long (no ownership was given) and blocks everyone
- Reported problems get snoozed like alarm clocks do, because they don't appear at a good time
- People get used to snoozing/suppressing errors, and they do it more and more when it feels easier to do so
- People wait until warnings show up to fix them, instead of actively tackling them (may still be better than in `elm-review`'s approach though, who knows)
- The project has been untouched for a while, and now running the test suite reports a bunch of errors that I need to fix, or suppress again
If I'm new to the project, I may not know that it's possible (or culturally acceptable) to re-suppress errors

Side-note on `ember-template-lint`'s approach: because the location (~line number) is part of the hash (see next section),
when you move the problematic code (by adding an unrelated import at the top of the file), you need to re-generate the
suppressions (or fix them), resetting the time it will take before the problem will be reported as a warning. Meaning that for files
that get changed a lot, it may take a very long time before the errors show up through the postponing mechanism.


### Avoid Git conflicts

I want to avoid having the changes in suppression files from causing Git conflicts.

This translates to the following in practice:
- Each file is on its own line. 

This is probably one of the least successful goals. Suggestions to improve this are welcome.

[`ember-template-lint`](https://github.com/ember-template-lint/ember-template-lint) pushed this goal very far, making
suppressed error (they named them "todo") have their own file, hashed by the contents of the file (which contains error location
and suppression time, meaning it's unique in practice).

The downside I see in this approach is that it makes it really hard to have an overview of the suppressed errors left to
fix. The hash that changes at every generation of the error suppressions also defeats the consistent results goal.


---


PS: I hope you know, but it is not my intention to bash on the `ember-template-lint` project and choices.
I compare the approach against theirs because they have made courageous and interesting choices,
and because they're one of the few projects out there with a similar feature ❤,
but I want to make it clear why I didn't end up making the same choices that they did.