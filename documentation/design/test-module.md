# Design goals for the `Review.Test` module

`elm-review` comes with a testing library under `Review.Test`, which allows you to test the rules you write using [`elm-test`](https://github.com/elm-community/elm-test). It comes with a guide explaining how to write tests efficiently.

`Review.Test` has been created around several goals:
  - Help developers with great test failure messages
  - Test rules completely

## Great failure messages

In order to achieve the goal of "testing rules completely", the test module makes a lot of under the hood assertions. Showing the default `elm-test`'s failure messages would not help the developer because they would not know what is tested and therefore not know which part was a problem.

For this reason, custom error messages were mandatory. And since we are used to great error messages, I figured we might as well have great failure messages while we're at it.


## Test rules completely

When making a rule, there are plenty of things to test, and when left to developers with a general testing framework, a lot of them end up being ignored. Testing only that a rule reports an error for a given source code without checking the actual error information is a test that is likely to keep succeeding for the wrong reasons.

Because I think that all the information in an error is important, the test module forces you to test them all. Following are the element that `elm-review` forces you to test.

### Error message and details

As mentioned in several others sections, communication is key, and the information that is communicated to the user mostly comes from these.

Not testing this means that the user is more likely to get unhelpful information or see typos.

### Error location

The error location is where you would see the squiggly lines in the report or in your editor.

Not testing this means that the user is more likely to see the error at the wrong location, or on a too large section of code, making it a pain to have the error in the editor.

This is usually a pain to test because it requires giving it a range (of the form `{ start : { row : Int, column : Int}, end : { row : Int, column : Int} }`) which you always get wrong the first time, because it is tedious to compute in your head, and because it starts with row and column 1, which is just very confusing for developers. Since you always get it wrong, you either just follow what the test suggests you instead, or don't test it at all (from my experience with ESLint, the location is very rarely tested).
`Review.Test` tries to make this more helpful with the field `under`, where you specify the text at the location, instead of the position itself. This is mostly guessable by the developers, but is also much more readable when reading the test.

### Result of fixing

If (and only if) the error provides fixes, the test module requires you to provide what the code should look like after fixes have been applied. If the fix is invalid because it is malformed, doesn't make any changes or makes the code not syntactically valid, then the test will fail.
 (In those situations, the fix is not presented to the user in the CLI)

When the user encounters an error and it provides fixes, `elm-review` will mention them a fix is available, without checking that the fix is valid, because I estimate that trying out each change is probably too expensive in terms of performance. When running `elm-review` with `--fix`, fixes will be evaluated and will be ignored (not presented to the user) if they are found invalid, which is not a great experience for them. `elm-review` will warn about fixes that could not be applied, but having correct fixes in the first place will make for a much greater experience for the user.

Not testing the fixed code means the user is more likely to see false fix positives, and to get valid but incorrect fixes.
