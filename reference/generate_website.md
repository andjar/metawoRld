# Generate (and render) the project website

Builds a living meta-analysis dashboard: an overview page (studies,
verification progress, summary of findings), one dashboard page per
meta-analysis in `_analyses.yml` (see
[`generate_dashboard()`](https://andjar.github.io/metawoRld/reference/generate_dashboard.md)),
a results page per variable, plus the study list, the full data and one
page per study (pages of removed studies are deleted). Renders into
`_site/`.

## Usage

``` r
generate_website(path = ".", render = TRUE, update_templates = FALSE)
```

## Arguments

- path:

  Project root.

- render:

  Render with Quarto? Requires the Quarto CLI (<https://quarto.org>);
  the `quarto` R package is used when installed. Defaults to `TRUE`.

- update_templates:

  Overwrite `_quarto.yml`, `index.qmd`, `studies.qmd`, `data.qmd`,
  `styles.css` and `study.qmd.tmpl` with the current package versions?
  Defaults to `FALSE` so your edits are kept; missing files are always
  added.

## Value

The project path, invisibly.

## Details

Existing projects: use `update_templates = TRUE` once to get the new
`_quarto.yml` and overview page.
