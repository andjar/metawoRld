# Counts for a PRISMA 2020 flow diagram

Computed only from `records.csv`, `screening.csv` and the accepted
studies.

## Usage

``` r
prisma_counts(path = ".")
```

## Arguments

- path:

  Project root.

## Value

A tibble with `stage` and `n`, including one row per full-text exclusion
reason.
