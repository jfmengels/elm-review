# Why was the project renamed from elm-lint to elm-review?

"Linting"/"delinting" is etymologically something about cleaning dirty clothes. In programming terms, it's mostly about removing useless clutter and enforcing a coding style, and sometimes about simplifying the code. Since in the Elm community, we have `elm-format` to format the code, most stylistic issues that "linters" enforce are already solved. Therefore, the name "lint" doesn't match what this tool does.

Instead, this tool should be used more in order to catch problems, add additional guarantees to your code or project structure, and not so much about enforcing a code style. In practice, this tool will report things that your colleagues would tell you during code reviews, so I thought it fitting to rename it to `elm-review`.
