# Write the project's meta-analysis definitions

Replaces the analyses in `_analyses.yml`, keeping the explanatory
comments at the top of the file.

## Usage

``` r
write_analyses(path = ".", analyses)
```

## Arguments

- path:

  Project root.

- analyses:

  A list of analyses (as returned by
  [`read_analyses()`](https://andjar.github.io/metawoRld/reference/read_analyses.md),
  or lists with at least `name`, `variable`, `measure` and `compare` or
  `group`). Each is checked.

## Value

The analyses, invisibly.
