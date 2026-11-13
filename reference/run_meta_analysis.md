# Run a meta-analysis

Random-effects meta-analysis with
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
of the data from
[`analysis_data()`](https://andjar.github.io/metawoRld/reference/analysis_data.md),
plus a cumulative meta-analysis by publication year showing how the
pooled estimate has evolved.

## Usage

``` r
run_meta_analysis(path = ".", analysis, world = NULL)
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

An object of class `metawoRld_meta`: a list with `analysis`, `data`
(with effect sizes `yi`, `vi`), `excluded`, `selection` (how each study
contributes: which groups, at which level, and whether subgroups were
pooled), `model` (the `rma` fit, or `NULL` with fewer than two studies),
`summary` (one-row tibble), `subgroups` and `subgroup_test` (for
analyses with `subgroups`), and `cumulative` (pooled estimate after each
publication year).
