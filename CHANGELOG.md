# Changelog

## 3.0.0 -> 4.0.0

v4.0.0 is a full rewrite of the project, with plenty of API, design, and opinion
changes. It is best to consider this version as a new project rather than a
continuation of the previous version. If you were using previous versions of
this project, it would be best to re-read the documentation from scratch.

In case you are wondering, here are the API changes:

```
---- ADDED MODULES - MINOR ----

    Lint.Fix
    Lint.Project
    Lint.Rule
    Lint.Test


---- REMOVED MODULES - MAJOR ----

    Lint.Rules.NoDebug
    Lint.Types


---- Lint - MAJOR ----

    Added:
        type Error
        errorDetails : Lint.Error -> List.List String.String
        errorFixes : Lint.Error -> Maybe.Maybe (List.List Lint.Fix.Fix)
        errorMessage : Lint.Error -> String.String
        errorModuleName : Lint.Error -> Maybe.Maybe String.String
        errorRange : Lint.Error -> Elm.Syntax.Range.Range
        errorRuleName : Lint.Error -> String.String

    Removed:
        doNothing : context -> Direction (Node a) -> ( List LintError, context )
        lintSource :
            List ( Severity, LintRule )
            -> String
            -> Result (List String) (List ( Severity, LintError ))
        parseSource : String -> Result (List String) File
        visitExpression :
            LintRuleImplementation context
            -> Node Expression
            -> ( List LintError, context )

    Changed:
      - lint : File -> LintRuleImplementation context -> List LintError
      + lint :
            List.List Lint.Rule.Rule
            -> Lint.Project.Project
            -> { path : String.String, source : String.String }
            -> List.List Lint.Error
```
