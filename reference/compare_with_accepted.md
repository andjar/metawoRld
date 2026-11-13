# What a submission would change in an accepted study

Before accepting a submission for a study that already has accepted
data, see exactly what would change: values that differ, data points
added or removed, and changed metadata.

## Usage

``` r
compare_with_accepted(path = ".", study_id, extraction_id)
```

## Arguments

- path:

  Project root.

- study_id, extraction_id:

  The submission.

## Value

A tibble with `part` (`data` or `metadata`), `id` (measurement id or
metadata field), `field`, `accepted` and `submitted` (`NA` where
absent). Zero rows if nothing would change; `NULL` if the study has no
accepted data yet.
