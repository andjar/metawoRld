# Validate all studies in a project

Validate all studies in a project

## Usage

``` r
validate_world(path = ".", check_links = TRUE)
```

## Arguments

- path:

  Project root.

- check_links:

  Check `_ref` links? Defaults to `TRUE`.

## Value

A tibble with one row per study: `study_id`, `valid`, `n_errors` and
`errors` (list of messages). A summary is printed.
