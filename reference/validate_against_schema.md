# Validate data against a metawoRld schema

Checks types, required fields and enum values recursively. Missing
values (`NULL` or `NA`) in required fields are reported; empty arrays
are allowed. Data frames are accepted wherever the schema expects an
array of objects (each row is validated as an object), which is what
`ellmer` and `readr` return.

## Usage

``` r
validate_against_schema(data, schema, path = "root", strict = FALSE)
```

## Arguments

- data:

  The data to validate (list, data frame or scalar).

- schema:

  A schema node, e.g. `get_schema(path)` or a sub-node such as
  `get_schema(path)$data_points`.

- path:

  Label used as the root of the error locations.

- strict:

  If `TRUE`, fields that are not defined in the schema are reported as
  errors.

## Value

A character vector of error messages; empty if the data are valid.

## Examples

``` r
schema <- list(`_type` = "object",
               n = list(`_type` = "integer"),
               label = list(`_type` = "string", `_required` = FALSE))
validate_against_schema(list(n = 3), schema)
#> character(0)
validate_against_schema(list(n = 2.5, label = 1), schema)
#> [1] "root.n: expected an integer but got numeric '2.5'."
#> [2] "root.label: expected a string but got numeric '1'."
```
