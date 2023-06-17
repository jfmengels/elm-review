# Changelog

## [Unreleased]

## [2.13.1] - 2023-06-17

Fixed an issue where the module name lookup table would yield an incorrect result. [#159](https://github.com/jfmengels/elm-review/pull/159)

## [2.13.0] - 2023-04-16

1) Changed the order in which rules are applied on modules. [#153](https://github.com/jfmengels/elm-review/pull/153)

Instead of visiting the entire project for each rule sequentially, we now visit the entire project once but apply each 
rule on each module. This should hopefully result in a small speed improvement, and make it more interesting to precompute
interesting information to provide the rules (such as the module name lookup table)

2) Made it less costly to compute whether cached analysis can be reused [#154](https://github.com/jfmengels/elm-review/pull/154) 

The caching mechanism introduced in [2.11.0] felt inefficient. It improved the performance a bit but not as significantly as expected.
The reason for that was that the method to check whether a cached analysis could be re-used or not was extremely inefficient.
Changing the representation of that key vastly improved the performance of the whole cache system, which now feels worth it.

3) Applying fixes for all targets [#155](https://github.com/jfmengels/elm-review/pull/155)

In [2.10.0] the package introduced the ability to apply fixes on its own, without the need of the CLI. It did however not
support applying fixes for the `elm.json` file, as that can have important repercussions on the analysis (if
`source-directories` or dependencies are changed). These fixes are now applied as well.

Applying all fixes in the package means that there is no need to try and apply fixes in the CLI, which will be removed in its v2.10.0.
The CLI was responsible for annotating fixes as failing, which is why this release introduces [`Review.Rule.errorFixFailure`] to allow
the CLI to show when a fix failed to apply.


## [2.12.2] - 2023-02-02

Fixed a bug where errors were skipped/ignored when running `elm-review` after having run `elm-review --fix-all` [#150](https://github.com/jfmengels/elm-review/pull/150)

## [2.12.1] - 2023-01-25

Fixed a bug where rules would report false positives in the presence of the file-system cache.

## [2.12.0] - 2023-01-22

Fixed a bug that ignored fixes from rules that were not marked as providing fixes (using `Rule.providesFixesFor*Rule`).
This was meant to be handled by the CLI, but didn't work as expected.

When the rule uses [`Review.Rule.withIsFileIgnored`], the test runner will now attempt to re-run the rule while ignoring
some files (it will do so for every possible combination) and assert that the results are the same. This is to
check that the knowledge is only used for performance improvements and change the result. This way, complex rules can
avoid writing extremely complicated test setups to test that complex scenarios work behave as expected even when some
files are ignored.

Some rules might not wish for this behavior, in which case [`Review.Test.ignoredFilesImpactResults`] can be used to
opt out of this re-running mechanism.


## [2.11.1] - 2022-12-20

- Fix [`Review.Rule.withIsFileIgnored`] returning the wrong value.

## [2.11.0] - 2022-12-17

- Adds [`Review.Rule.withIsFileIgnored`] ([#145](https://github.com/jfmengels/elm-review/pull/145))
- Behind the scenes work to allow the CLI to save the internal result cache to the file system. Adds as an internal function [`Review.Rule.withRuleId`] for that purpose.
- Fixed the test failure message reported when a test was missing an expected extract (it reported the failure message for a different problem).

## [2.10.0] - 2022-11-08

### Faster fixes

Includes a large rework of the internals to include fixes (instead of in the CLI) which results in much faster fixes.
See the [announcement blog post](https://jfmengels.net/much-faster-fixes/) on the topic.

Breaking change (in the sense that tests will fail): Rules that provide fixes now have to indicate that they will do so,
by using `Rule.providesFixesForModuleRule` or `Rule.providesFixesForProjectRule`.


### Extract feature

Rules can now include a "data extractor" using `Rule.withDataExtractor`, which allows you to extract information from a
project if you run `elm-review --extract --report=json`. See the "Extract information" section in the README for more
information.


### New testing functions

The testing API provided in `Review.Test` worked well, but in cases where you had to report multiple different things,
for instance errors for modules + global errors, then you would have to switch to using `expectGlobalAndModuleErrors`.
If you reported local errors + global errors, you would have to use `expectGlobalAndLocalErrors`, and so on.

With the addition of data extracts, this approach would require a few new functions, because we'd need a combination of
all the possible things a rule would report for any given test.

Since this looked like a combinatorial explosion, we are now switching to a different approach. The two functions I just
mentioned are now deprecated, and some new functions are introduced to replace them using a more flexible approach,
center around the new `Review.Test.expect` function.


## [2.9.2] - 2022-10-12

Bumps the dependency to [`elm-explorations/test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/) to v2.
We recommend upgrading by using `elm-json`, like this:

```bash
cd review/
elm-json upgrade
```

## [2.9.1] - 2022-09-19

This release contains HUGE performance updates. `elm-review` should now run quite a bit faster (rough estimate says 50%).

## [2.9.0] - 2022-08-23

- Adds [`Review.Rule.withModuleDocumentationVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleDocumentationVisitor) ([#132](https://github.com/jfmengels/elm-review/pull/132))
- Adds [`Review.Rule.withModuleDocumentation`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleDocumentation) ([#133](https://github.com/jfmengels/elm-review/pull/133))
- Adds [`Review.Rule.withFullAst`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withFullAst) ([#133](https://github.com/jfmengels/elm-review/pull/133))
- Adds [`Review.ModuleNameLookupTable.createForTests`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-ModuleNameLookupTable#createForTests) ([d1c4102ec9113cd8e7fef1824554925e89d0b0f1])
- Adds [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) and [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) ([#136](https://github.com/jfmengels/elm-review/pull/136))
- Adds [`Review.Project.directDependencies`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Project#directDependencies) ([#136](https://github.com/jfmengels/elm-review/pull/136))
- Fixes a bug in the `ModuleNameLookupTable` where the module name would not be resolved due to the presence of indirect dependencies ([#135](https://github.com/jfmengels/elm-review/pull/135))


## [2.8.1] - 2022-07-17

- Fixes confusing test failure messages from the test module ([9bdc37b98c5e29f00e9485cf78bce0a3ff715761])


## [2.8.0] - 2022-07-05

- Adds [`Review.ModuleNameLookupTable.fullModuleNameAt`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-ModuleNameLookupTable#fullModuleNameAt) and [`Review.ModuleNameLookupTable.fullModuleNameFor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-ModuleNameLookupTable#fullModuleNameFor) ([3ff9098d30bbd48a710bcc978c531b686b33c784])

- Adds [`Review.Rule.withFilePath`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withFilePath) ([319ecb29a69ee8c35914b14827d7f068ed48669c])
- Adds [`Review.Rule.withModuleName`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleName) ([d17d995f0eae4e0455143027da50e17280286414])
- Adds [`Review.Rule.withModuleNameNode`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleNameNode) ([0fe88c90cfee887c046a3765400c3883797a0bd3])
- Adds [`Review.Rule.withIsInSourceDirectories`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withIsInSourceDirectories) ([e1e522eafbb389484adaca8c95642c9e14d15011])
- Deprecates [`Review.Rule.Metadata`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.8.0/Review-Rule#Metadata), [`Review.Rule.moduleNameFromMetadata`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.8.0/Review-Rule#moduleNameFromMetadata), [`Review.Rule.moduleNameNodeFromMetadata`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.8.0/Review-Rule#moduleNameNodeFromMetadata) and [`Review.Rule.isInSourceDirectories`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.8.0/Review-Rule#isInSourceDirectories) ([2c0254a4694bc4994127be8565744a4af9412605])
- Updated the way that deprecations functions and types were annotated so that [`NoDeprecated`] could report them.

- The path of a file when passed to a test module is now by default `src/<ModuleName>.elm` instead of `src/File_<index>.elm` ([f79e9700192c73dafea215410c8f67f4fdd8ffd4])
- Updated some test failure messages from the test module so that they are always in the order "expected X but got Y" ([fae198f186a7659fa98f6f3400bb57960be57b57])


## [2.7.2] - 2022-04-26

- Updated documentation to mention the [starter configurations](https://jfmengels.net/starter-configurations/

## [2.7.1] - 2022-03-18

- Fixed a bug where the module graph wasn't properly computed and would lead to outdated/unexpected results in watch or fix mode ([648d386d9d812f95c200ce6b6d94b6f5c2dd168d])


## [2.7.0] - 2022-02-04

- Adds [`Review.Rule.filterErrorsForFiles`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#filterErrorsForFiles) ([#115](https://github.com/jfmengels/elm-review/pull/115)) (thanks [@jiegillet](https://github.com/jiegillet)!)


## Missing changelog

Help would be appreciated to fill the blanks!

[`NoDeprecated`]: https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoDeprecated
[`Review.Rule.withIsFileIgnored`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withIsFileIgnored
[`Review.Rule.withRuleId`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withRuleId
[`Review.Rule.errorFixFailure`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#errorFixFailure
[`Review.Test.ignoredFilesImpactResults`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule-Test#ignoredFilesImpactResults

[Unreleased]: https://github.com/jfmengels/elm-review-unused/compare/v2.13.1...HEAD
[2.13.1]: https://github.com/jfmengels/elm-review/releases/tag/2.13.1
[2.13.0]: https://github.com/jfmengels/elm-review/releases/tag/2.13.0
[2.12.2]: https://github.com/jfmengels/elm-review/releases/tag/2.12.2
[2.12.1]: https://github.com/jfmengels/elm-review/releases/tag/2.12.1
[2.12.0]: https://github.com/jfmengels/elm-review/releases/tag/2.12.0
[2.11.1]: https://github.com/jfmengels/elm-review/releases/tag/2.11.1
[2.11.0]: https://github.com/jfmengels/elm-review/releases/tag/2.11.0
[2.10.0]: https://github.com/jfmengels/elm-review/releases/tag/2.10.0
[2.9.2]: https://github.com/jfmengels/elm-review/releases/tag/2.9.2
[2.9.1]: https://github.com/jfmengels/elm-review/releases/tag/2.9.1
[2.9.0]: https://github.com/jfmengels/elm-review/releases/tag/2.9.0
[2.8.1]: https://github.com/jfmengels/elm-review/releases/tag/2.8.1
[2.8.0]: https://github.com/jfmengels/elm-review/releases/tag/2.8.0
[2.7.2]: https://github.com/jfmengels/elm-review/releases/tag/2.7.2
[2.7.1]: https://github.com/jfmengels/elm-review/releases/tag/2.7.1
[2.7.0]: https://github.com/jfmengels/elm-review/releases/tag/2.7.0

[9bdc37b98c5e29f00e9485cf78bce0a3ff715761]: https://github.com/jfmengels/elm-review/commit/9bdc37b98c5e29f00e9485cf78bce0a3ff715761
[319ecb29a69ee8c35914b14827d7f068ed48669c]: https://github.com/jfmengels/elm-review/commit/319ecb29a69ee8c35914b14827d7f068ed48669c
[d17d995f0eae4e0455143027da50e17280286414]: https://github.com/jfmengels/elm-review/commit/d17d995f0eae4e0455143027da50e17280286414
[0fe88c90cfee887c046a3765400c3883797a0bd3]: https://github.com/jfmengels/elm-review/commit/0fe88c90cfee887c046a3765400c3883797a0bd3
[e1e522eafbb389484adaca8c95642c9e14d15011]: https://github.com/jfmengels/elm-review/commit/e1e522eafbb389484adaca8c95642c9e14d15011
[3ff9098d30bbd48a710bcc978c531b686b33c784]: https://github.com/jfmengels/elm-review/commit/3ff9098d30bbd48a710bcc978c531b686b33c784
[2c0254a4694bc4994127be8565744a4af9412605]: https://github.com/jfmengels/elm-review/commit/2c0254a4694bc4994127be8565744a4af9412605
[f79e9700192c73dafea215410c8f67f4fdd8ffd4]: https://github.com/jfmengels/elm-review/commit/f79e9700192c73dafea215410c8f67f4fdd8ffd4
[fae198f186a7659fa98f6f3400bb57960be57b57]: https://github.com/jfmengels/elm-review/commit/fae198f186a7659fa98f6f3400bb57960be57b57
[648d386d9d812f95c200ce6b6d94b6f5c2dd168d]: https://github.com/jfmengels/elm-review/commit/648d386d9d812f95c200ce6b6d94b6f5c2dd168d
[d1c4102ec9113cd8e7fef1824554925e89d0b0f1]: https://github.com/jfmengels/elm-review/commit/d1c4102ec9113cd8e7fef1824554925e89d0b0f1
