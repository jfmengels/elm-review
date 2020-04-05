# Migration from v1 to v2

## Rule writing

- [`newSchema`] is renamed to [`newModuleRuleSchema`]. It now takes the initial context as a new argument. If you were using [`withInitialContext`], then you should use that function's value, otherwise you can use `()`.
- [`fromSchema`] is renamed to [`fromModuleRuleSchema`]
- [`withInitialContext`] has been removed, since the initial context is now supplied by [`newModuleRuleSchema`]
- [`withFixes`] has been removed. If you now want to add fixes to an error, use `errorWithFix` instead of error.
- The [`Error`] type now has a type variable. You can replace all `Error` by `Error {}`
- [`Schema`] has been renamed to [`ModuleRuleSchema`]

There are a lot of other API changes not related to creating rules, which are related to making everything work for the CLI. I am not going to describe these here because it's unlikely that someonetiuched that. If I am wrong about this and you did depend on these APIs, come talk to me or open an issue.

## Review configuration

### Folder structure change

The default folder structure has been updated, therefore I recommend removing your `review/` folder (at least the `ReviewConfig.elm` and `elm.json`) then running `elm-review init`. Then you can copy paste what you had in your previous ReviewConfig into the new one.

In practice, you should be able to have a very similar configuration, but the following sections might make you change a few things.

### Tests now included by default

If you were running `elm-review` with arguments, I recommend removing them and to see if that works out.

The `tests/` directory is now included by default, so you might get errors from that. To ignore these errors for the moment, you can change the arguments to `elm-review`, or use [`Rule.ignoreErrorsForDirectories`]. I recommend having your tests reviewed after the migration phase.

### Rule changes

Here are the changes for the review packages from `jfmengels`.

#### [jfmengels/review-debug]

The [`NoDebug`] rule is split into NoDebug.Log and NoDebug.TodoOrToString. Just remove the former and add the latter two rules to have the same.

#### [jfmengels/review-unused]

[`NoUnused.CustomTypeConstructors`] now takes an argument, which you can replace by `[]`.

Notice that both [`NoUnused.Variables`] and [`NoUnused.CustomTypeConstructors`] can report new errors though, so you might need to fix the errors or to disable the rule temporarily.

There are now a lot of new rules that you can pick up from, so go check those out!

[`Schema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/Review-Rule#Schema
[`newSchema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/Review-Rule#newSchema
[`fromSchema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/Review-Rule#fromSchema
[`withInitialContext`]: https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/Review-Rule#withInitialContext
[`withFixes`]: https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/Review-Rule#withFixes

[`ModuleRuleSchema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#ModuleRuleSchema
[`newModuleRuleSchema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#newModuleRuleSchema
[`fromModuleRuleSchema`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#fromModuleRuleSchema
[`Error`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#Error
[`error`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#error
[`errorWithFix`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#errorWithFix
[`Rule.ignoreErrorsForDirectories`]: https://package.elm-lang.org/packages/jfmengels/elm-review/2.0.0/Review-Rule#ignoreErrorsForDirectories


[jfmengels/review-debug]: https://package.elm-lang.org/packages/jfmengels/review-debug/2.0.0/
[`NoDebug`]: https://package.elm-lang.org/packages/jfmengels/review-debug/1.0.0/NoDebug

[jfmengels/review-unused]: https://package.elm-lang.org/packages/jfmengels/review-unused/2.0.0/
[`NoUnused.Variables`]: https://package.elm-lang.org/packages/jfmengels/review-unused/2.0.0/NoUnused-Variables
[`NoUnused.CustomTypeConstructors`]: https://package.elm-lang.org/packages/jfmengels/review-unused/2.0.0/NoUnused-CustomTypeConstructors
