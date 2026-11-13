# Record ids

The DOI when known, else `pmid:<PMID>`, else `rec-` and 12 hex digits of
a hash of the normalized title and year.

## Usage

``` r
record_id(doi = NA, pmid = NA, title = NA, year = NA)
```

## Arguments

- doi, pmid, title, year:

  Vectors of equal length (use `NA` or `""` for missing values).

## Value

Character vector.
