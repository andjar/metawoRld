# Read one study from a metawoRld project

Read one study from a metawoRld project

## Usage

``` r
read_study(path = ".", study_id)
```

## Arguments

- path:

  Project root.

- study_id:

  Study identifier (as given to
  [`add_study_data()`](https://andjar.github.io/metawoRld/reference/add_study_data.md)).

## Value

A list with `metadata` (list) and `data` (tibble). `data.csv` is read
with column types taken from the schema.
