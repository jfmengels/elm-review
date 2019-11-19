# (Ongoing) API design for multi-file support

## Main idea

Currently, `elm-review` only looks at a single file at a time, and when it looks at another file, it has forgotten about the previous file. This is fine for a number of rules, but in a lot of cases, if you want to report problems completely, then you need to be able to know what is happening in multiple files.

Here are a few examples:
- You want to know when a module is never used. For that, you need to go through your whole project, register the used modules, and report the ones that were never imported.
- You want to know when a type constructor of an custom type is never used. If the type is opaque or not exposed, this can be done currently without needing to look at other files. But if the type's constructors are exposed, then you need to know if other files use them.

## Problems to fix

- [X] Be able to have a list containing single file and multi-file rules.
  Users should not have to care if the rule is for a single or multiple files
- [X] Be able to create a multi-file rule
- [X] Be able to specify a multi-file rule's file visitor
    - [X] Using the same functions as for a single rule
    - [X] Forbid specifying a name
    - [X] Forbid specifying an elmJsonVisitor
- [X] Be able to run both types of rules and get a list of errors
- [X] Be able to re-run a rule when a file has changed
    - [X] For single rules, re-run the rule on the file and replace the errors on the file
    - [X] For multi rules:
        - For every file, keep and associate to the file the resulting context and the errors thrown while visiting
        - When a file changes, recompute the associated context and errors (and keep them)
        - Re-merge the contexts, call finalEvaluation and concatenate the errors of the other files
- [ ] Polish type and functions names
- [ ] Replace the phantom types by custom types, instead of records
- [ ] Folding context
    - [ ] Make a nice API for when the multi-file context is different as the file visitor's
    - [ ] Make a nice API for when the multi-file context is the same as the file visitor's
- [ ] Add a way to test multi-file rules
    - [ ] Make sure that the order does not matter by running a rule several
      times with a different order for the files every time.

- Solve cases where you may want to re-review a file
    - Need to know what the interface of a module is (what do `import Html exposing (..)` and `import A exposing (B(..))` bring into the scope?).
        - Providing the signatures of modules should suffice.
        - For internal files, https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Interface
        - For dependencies, load them one way or another (TBD)
    - Need to know the contents of a function body (does this function use `Html.lazy`?) or type (does this type alias to or wrap a function?)
        - The imported files should be analyzed before the importing file, and the resulting context should be available using `withInitialContext` or a similar function.
        - This may warrant a different function or an additional parameter to create the rule, so that `elm-review` knows in which order files should be analyzed. (Maybe `withInitialContext` and its alternative can do that job)
        - When loading the files, it's probably worth creating an acyclic graph of files by the way they import themselves imports can probably be created so as to know the order in which. The code using `elm-review` should only have to collect the files, but should not have to create the graph itself. This might be a bit similar to what is being done with https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Processing#ProcessContext.
        - When a file gets updated, the files importing it get re-evaluated, and so on (until the resulting context is the same as before).
        - It should be possible to add or remove files from the graph/list of files, to prepare for a potential watch mode.

- [ ] Dependency signature loading
    - The signatures are available by
        - Reading local file $elm_home/packages/<author>/<packagename>/docs.json
        - Downloading https://package.elm-lang.org/packages/elm/core/latest/docs.json
    - Cache the `docs.json` above so as not to have to download the packages at every run
    - Add these to `elm-syntax`'s `ProcessContext` in order to better parse the files (wrt operator precedence)
    - Add them to the Project
    - Load dependencies before loading the files (wrt the `ProcessContext`)
    - Make `Project` handle the list of files, the parsing, the graph creation, etc.

## Caching

- For multi-rules
  - Add a way to decode/encode the context for each file
  - If the rule supports encoding/decoding, in `Review.Test` check that `context == decode <| encode context` for every file
  - Store the contexts for each file/rule to the file system
  - Load the contexts for each file/rule from the file system
  - Figure out what to do if the initial context changes (discard the cache?)
- Propose a way in the CLI to
  - remove the cache?
  - run without cache?
  - specify the cache location?

## Work on errors

- [X] Define a way to report errors in other files?
    - A FileKey/FileId similar to a Navigation.Key? It has no useful meaning, but
      makes it so you can't give an error to a non-existing file or file you haven't visited.
- [ ] Allow multi-file to generate errors without a file key (and make sure that they point to the right file)
- [ ] Define a way to report errors in elm.json?
- [ ] Get rid of Review.Error
    - [ ] Need to be able to create an error without a file in Review.Rule

## Extract the underlying workings into a different package?

`elm-review` does a lot of things that pave the way for other potential tools, like codemods, code crawlers
(to gather data and do something with it, like code generation, project graph visualization).

Maybe we could extract the underlying implementation for `elm-review` into another package that does most of the work, and have `elm-review` be a specific implementation of this crawler that gathers errors as you go?

It is probably not worth doing this at the moment, as we'd first need to explore how the other tools would work and need.
