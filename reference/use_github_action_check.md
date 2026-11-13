# Add a GitHub Actions workflow that runs check_project()

The workflow runs
[`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
on every push and pull request, so invalid data or files edited outside
metawoRld fail the check. The command is a plain `Rscript` call and
works in any CI system.

## Usage

``` r
use_github_action_check(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  Project root.

- overwrite:

  Replace an existing workflow? Defaults to `FALSE`.

## Value

The workflow file, invisibly.
