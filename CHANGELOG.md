# Changelog

## Unreleased

- Adds [`Review.Rule.withModuleDocumentationVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleDocumentationVisitor) ([#132](https://github.com/jfmengels/elm-review/pull/132))
- Adds [`Review.Rule.withModuleDocumentation`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withModuleDocumentation) ([#133](https://github.com/jfmengels/elm-review/pull/133))
- Adds [`Review.Rule.withFullAst`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withFullAst) ([#133](https://github.com/jfmengels/elm-review/pull/133))
- Adds [`Review.ModuleNameLookupTable.createForTests`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-ModuleNameLookupTable#createForTests) ([d1c4102ec9113cd8e7fef1824554925e89d0b0f1])
- Adds [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) and [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) ([#136](https://github.com/jfmengels/elm-review/pull/136))
- Adds [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) and [`Review.Rule.withDirectDependenciesModuleVisitor`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Rule#withDirectDependenciesModuleVisitor) ([#136](https://github.com/jfmengels/elm-review/pull/136))
- Adds [`Review.Project.directDependencies`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/Review-Project#directDependencies) ([#136](https://github.com/jfmengels/elm-review/pull/136))


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

[`NoDeprecated`]: (https://package.elm-lang.org/packages/jfmengels/elm-review-common/latest/NoDeprecated)

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
