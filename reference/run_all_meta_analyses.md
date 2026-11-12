# Run all meta-analyses of a project

Run all meta-analyses of a project

## Usage

``` r
run_all_meta_analyses(path = ".", world = NULL)
```

## Arguments

- path:

  Project root.

- world:

  Optional result of
  [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md).

## Value

A named list of `metawoRld_meta` objects (or error conditions for
analyses that failed).
