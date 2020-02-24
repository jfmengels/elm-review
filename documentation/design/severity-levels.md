# Why are there no severity levels in `elm-review`?

In almost all similar tools (linters, code checkers, ...) I have seen for other languages ([here is a nice list of linters](https://github.com/caramelomartins/awesome-linters)), there is a concept of severity levels.

In the example of [`ESLint`](https://eslint.org), there are three:
- "error": If an error with this severity level is found, it is reported and the CLI exits with a non-zero status.
- "warn": If an error with this severity level is found, it is reported but the CLI exits with status 0 (unless an "error" has been found).
- "off": The rule is turned off. This is useful due to how configuration is done in ESLint.

To me, warnings are equivalent to rules that are not enforced. They are used for patterns that are not great, but not forbidden. The problem with non-enforced rules is that, because they are not necessarily enforced, some will very likely be ignored. When they do, they will start to pile up, and after some time, they will form a mass of warnings that no one looks at but that everyone find dangerous or concerning, and in which no one sees the new warnings appear anymore.

To me, non-enforced rules should not be used for potential errors that can be ignored. They may be used for improvement suggestions, but they will pile up and then be drowned in the mass, making them useful for only a relatively short amount of time. As far as I can tell, suggestions are a better fit in an editor though, as some editors already do (for other languages), where you'll see them for a brief short of time when editing a section, and if you think the suggestion is irrelevant, you can ignore it and never see it again. This is a tool that I'd be interested in having, and that I think could be great for beginners if done well.
