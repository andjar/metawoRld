# Generate one web page per study

Fills `study.qmd.tmpl` for each study and writes `study/<study>.qmd`.
Pages for studies that were removed are deleted.

## Usage

``` r
generate_study_webpage(path = ".")
```

## Arguments

- path:

  Project root.

## Value

Paths of the generated pages, invisibly.
