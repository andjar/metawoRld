# Read the project's meta-analysis definitions

Reads `_analyses.yml` (see the comments in the template added by
[`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
or
[`use_templates()`](https://andjar.github.io/metawoRld/reference/use_templates.md))
and checks each entry.

## Usage

``` r
read_analyses(path = ".")
```

## Arguments

- path:

  Project root.

## Value

A list of analyses (each a list with `name`, `variable`, `measure`,
`compare` or `group`, `timepoint`, `verified_only`, `method`).
