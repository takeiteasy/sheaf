# The Coalton fork

Sheaf depends on a fork of Coalton, pinned as the submodule `vendor/coalton`.
The fork is [takeiteasy/coalton](https://github.com/takeiteasy/coalton),
branch `sheaf`: upstream Coalton plus Sheaf's patches.

## Patches

| Patch | Gives Sheaf |
|---|---|
| Codegen nodes carry a source location | Error messages that point at source |
| `:CODEGEN-AST` codegen hook category | Each translation unit's IR before optimization |
| `traverse` keeps a rewritten node's location | Locations that survive Sheaf's passes |

## Remotes

| Remote | Points at |
|---|---|
| `origin` | `takeiteasy/coalton` on GitHub |
| `mirror` | `takeiteasy/coalton` on a private mirror |
| `upstream` | `coalton-lang/coalton` |

## Updating from upstream

Merge, never rebase: the submodule pins commits on `sheaf`, and a rebase
would orphan them.

```sh
cd ~/git/coalton
git fetch upstream
git checkout main && git merge --ff-only upstream/main
git checkout sheaf && git merge main
# run Coalton's tests, then push both branches to origin and mirror
cd ~/git/sheaf/vendor/coalton && git pull
cd ../.. && git add vendor/coalton    # then run make test and commit
```

Coalton's tests need the fork's own `examples/` systems, so register the
whole tree[^registry].

[^registry]: `(asdf:initialize-source-registry '(:source-registry (:tree
    "/path/to/coalton/") :inherit-configuration))`, then
    `(asdf:test-system "coalton")`. Pushing only the root directory onto
    `asdf:*central-registry*` lets Quicklisp's older `quil-coalton` load
    instead, which fails to compile.
