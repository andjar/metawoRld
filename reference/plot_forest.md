# Forest plot of a meta-analysis

Studies whose values are not yet verified are drawn in grey; studies
whose mean/SD were estimated (from median/IQR, range, SE or CI) are
marked `(c)`.

## Usage

``` r
plot_forest(ma, ...)
```

## Arguments

- ma:

  A `metawoRld_meta` object from
  [`run_meta_analysis()`](https://andjar.github.io/metawoRld/reference/run_meta_analysis.md).

- ...:

  Passed to
  [`metafor::forest()`](https://wviechtb.github.io/metafor/reference/forest.html).

## Value

Draws the plot; returns `ma` invisibly.
