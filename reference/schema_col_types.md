# Column types for a study's data.csv derived from the schema

Used when reading `data.csv` files so that, for example, a string field
containing `"0.03"` is not silently turned into a number.

## Usage

``` r
schema_col_types(schema, columns = NULL)
```

## Arguments

- schema:

  The full extraction schema.

- columns:

  Optional column names present in the file; only these get a
  specification.

## Value

A [`readr::cols()`](https://readr.tidyverse.org/reference/cols.html)
specification.
