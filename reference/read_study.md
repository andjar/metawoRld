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

  Study id.

## Value

A list with `study` (contents of `study.yml`), `metadata` (list), `data`
(tibble; column types from the form) and `provenance` (tibble).
