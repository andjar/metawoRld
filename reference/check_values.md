# Check extracted values for plausibility and consistency

Runs the built-in checks configured in `_rules.yml` on every data point:
internal consistency of each statistic type (e.g. Q1 \<= median \<= Q3,
SD \>= 0, n/N matching the reported percentage), missing dispersion,
sample sizes larger than the group, duplicates, unknown variable names,
unit mismatches and implausible values. Nothing is changed; the flags
guide human review in
[`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md).

## Usage

``` r
check_values(path = ".", study_ids = NULL, rules = read_rules(path))
```

## Arguments

- path:

  Project root.

- study_ids:

  Studies to check. Defaults to all.

- rules:

  Rules to use. Defaults to
  [`read_rules()`](https://andjar.github.io/metawoRld/reference/read_rules.md).

## Value

A tibble with `study_id`, `measurement_id`, `check`, `severity` and
`message` (one row per flag).
