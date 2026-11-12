# Correct a data point

Changes one data point, re-validates the study against the schema and,
by default, marks the data point as verified with a note listing the
changes (e.g. `"value1: 5.1 -> 5.2"`). Use this (or
[`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md))
rather than editing `data.csv` by hand, so corrections are documented.

## Usage

``` r
update_data_point(
  path = ".",
  study_id,
  measurement_id,
  ...,
  verify = TRUE,
  reviewer = NULL,
  note = NULL
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study identifier.

- measurement_id:

  The data point to change.

- ...:

  Named new values, e.g. `value1 = 5.2, unit = "pg/mL"`.

- verify:

  Mark the corrected data point as verified? Defaults to `TRUE`.

- reviewer, note:

  See
  [`set_verification()`](https://andjar.github.io/metawoRld/reference/set_verification.md).
  `note` is appended to the list of changes.

## Value

A character vector describing the changes, invisibly.
