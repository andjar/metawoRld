# Generate (and render) the project website

Writes one Quarto page per study into `study/` (removing pages of
studies that no longer exist) and renders the site into `_site/`.

## Usage

``` r
generate_website(path = ".", render = TRUE, update_templates = FALSE)
```

## Arguments

- path:

  Project root.

- render:

  Render with Quarto? Requires the `quarto` R package and the
  Quarto CLI. Defaults to `TRUE`.

- update_templates:

  Overwrite `_quarto.yml`, `index.qmd`, `studies.qmd`, `data.qmd`,
  `styles.css` and `study.qmd.tmpl` with the current package versions?
  Defaults to `FALSE` so your edits are kept; missing files are always
  added.

## Value

The project path, invisibly.
