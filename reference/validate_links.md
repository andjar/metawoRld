# Validate links between data points and metadata

Uses the `_ref` keys in the `data_points` part of the schema (e.g.
`_ref: groups.group_id`) to check that every id used in the data is
defined in the corresponding metadata array.

## Usage

``` r
validate_links(metadata, data, schema)
```

## Arguments

- metadata:

  Study metadata (list).

- data:

  Data points (data frame or list of objects).

- schema:

  The full extraction schema.

## Value

A character vector of error messages; empty if all links are valid.
