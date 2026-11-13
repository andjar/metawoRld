# Save (commit) the project's changes

Used after actions in the metawoRld app, so the Git history reads like a
log of the review. Does nothing if the project is not a Git repository
or nothing changed.

## Usage

``` r
commit_project(path = ".", message)
```

## Arguments

- path:

  Project root.

- message:

  Commit message.

## Value

`TRUE` if a commit was made, invisibly.
