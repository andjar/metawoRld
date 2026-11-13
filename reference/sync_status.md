# Sharing status of a project

Sharing status of a project

## Usage

``` r
sync_status(path = ".")
```

## Arguments

- path:

  Project root.

## Value

A list: `repo` (is the project a Git repository?), `branch`, `remote`
(URL of `origin`, or `NA`), `changes` (number of changed or new files
not yet shared), `ahead` and `behind` (commits not yet pushed / pulled,
as of the last fetch).
