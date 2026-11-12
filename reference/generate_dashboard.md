# Generate the dashboard pages of the website

Runs every meta-analysis in `_analyses.yml` and writes two Quarto
dashboard pages with their figures (in `figures/`):

- `analyses.qmd`: one page per meta-analysis with the pooled estimate,
  heterogeneity, prediction interval, a forest plot, a cumulative
  meta-analysis by publication year (how the evidence has evolved) and
  the studies that could not be included, with reasons.

- `results.qmd`: every value reported for each variable, across studies
  and groups (descriptive, no pooling).

## Usage

``` r
generate_dashboard(path = ".", max_variables = 40)
```

## Arguments

- path:

  Project root.

- max_variables:

  Maximum number of variables on the results page (those reported by
  most studies first).

## Value

Paths of the generated pages, invisibly.

## Details

Called by
[`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md);
the overview page (`index.qmd`) is a template that summarises the same
results.
