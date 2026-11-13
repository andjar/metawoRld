# List submissions

List submissions

## Usage

``` r
list_submissions(path = ".", study_id = NULL, status = NULL)
```

## Arguments

- path:

  Project root.

- study_id:

  Only this study. Defaults to all.

- status:

  Only these statuses (`pending`, `accepted`, `rejected`, `superseded`).
  Defaults to all.

## Value

A tibble with one row per submission.
