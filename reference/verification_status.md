# Verification status of data points

Verification status of data points

## Usage

``` r
verification_status(path = ".", study_ids = NULL)
```

## Arguments

- path:

  Project root.

- study_ids:

  Studies to include. Defaults to all.

## Value

A tibble with `study_id`, `measurement_id`, `status` (`"unverified"`,
`"verified"`, `"rejected"` or `"changed"`, the latter meaning the values
changed after review), `reviewer`, `timestamp` and `note`.
