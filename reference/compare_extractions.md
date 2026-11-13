# Compare extractions of the same study

Lines up the data points of two or more extractions by their identifying
fields (by default the form's `_form$entry` rows, columns and context,
e.g. variable, group, method, time point and statistic type) and
compares every value; metadata are compared field by field. Extractors
should use the same group and method ids for data points to line up.

## Usage

``` r
compare_extractions(path = ".", study_id, extraction_ids = NULL)
```

## Arguments

- path:

  Project root.

- study_id:

  Study id.

- extraction_ids:

  Extractions to compare. Defaults to all pending submissions of the
  study.

## Value

A tibble with `part` (`metadata` or `data`), `key` (the data point's
identifying values, or the top-level metadata field), `field` (`(row)`
for presence of a data point), one column per extraction and `agree`.
