# Why there are no built-in rules in `elm-review`

`elm-review` comes with no rules out of the box. It used to have some, but then I
decided it was better to have them all extracted into different packages.

The main reason, is that if `elm-review` had "core" rules, users would think they
are all best practices. I am of the opinion that in practice, any rule that
could be written can potentially be unwanted under certain circumstances, and
can therefore hardly be enforced all the time without more consideration.

`elm-review` is plenty opinionated, but what counts as best practices in terms of
which configuration rules are enabled is not its concern, it is up to each user
to decide carefully whether or not to enable it. `elm-review` will just provide
the tools to do so.

If there are no core rules, then there are also no technical distinctions
between them and package rules. They all have access to the same tools, and
there is no rule that can do more than an other could.

A benefit of this, is that `elm-review` will not need to have a major version
bump when one of its rules gets updated or removed, because it doesn't have any.

The inconvenience, is that if `elm-review` happens to have a major version, which
I try to prevent as much as possible, all packages that depend on it (meaning at
least all packages with review rules) will need to be updated and re-released,
similar to what would happen when Elm gets a new (major or minor) version. Since
Elm doesn't (in practice) allow for duplicate dependencies with different major
versions, users would not be able to use the next version of `elm-review` with the
updated version of each package, until all packages are updated. Or they will
need to disable the rules from non-updated packages for a while.
This would happen even if there are built-in rules too, but more work would need
to be done by the community to bump their packages.
