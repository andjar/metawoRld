# Share and fetch work with the rest of the team

One step for people who do not use Git: saves (commits) your changes,
fetches the team's changes from the shared repository (e.g. GitHub) and
merges them, then sends (pushes) your changes. New submissions from
others then appear in
[`list_submissions()`](https://andjar.github.io/metawoRld/reference/list_submissions.md).
Each extraction lives in its own folder and only arbiters write `data/`,
so conflicts are rare; `screening.csv`, which several people append to,
is merged line by line.

## Usage

``` r
sync_project(path = ".", message = NULL, push = TRUE)
```

## Arguments

- path:

  Project root.

- message:

  Commit message for your changes. Defaults to a summary.

- push:

  Send your changes? Defaults to `TRUE`. Set `FALSE` to only fetch.

## Value

A list with `committed` (logical), `new_submissions` (tibble of
submissions that arrived), `pushed` (logical) and `message` (a sentence
for the user), invisibly.
