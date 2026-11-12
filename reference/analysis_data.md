# Data used in a meta-analysis

Selects the data points for an analysis, assigns groups to categories,
converts statistics and units, combines subgroups within a study (e.g.
mild and severe disease, using the formulas in the Cochrane Handbook
section 6.5.2.10) and returns one row per study.

## Usage

``` r
analysis_data(path = ".", analysis, world = NULL)
```

## Arguments

- path:

  Project root.

- analysis:

  An analysis from
  [`read_analyses()`](https://andjar.github.io/metawoRld/reference/read_analyses.md),
  or its name.

- world:

  Optional result of
  [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
  (with `drop_rejected = TRUE`) to avoid re-reading the project.

## Value

A list with `data` (one row per included study) and `excluded` (studies
reporting the variable but not usable, with a reason).
