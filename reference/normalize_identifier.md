# Normalize study identifiers

Brings DOIs and PMIDs into one canonical form so the same paper always
maps to the same cache entry and study folder: surrounding whitespace is
removed, `https://doi.org/`, `doi:` and `PMID:` prefixes are stripped,
and DOIs are lower-cased (DOIs are case-insensitive).

## Usage

``` r
normalize_identifier(x)
```

## Arguments

- x:

  Character vector of identifiers.

## Value

Character vector of the same length.

## Examples

``` r
normalize_identifier(c(" https://doi.org/10.1000/ABC.1 ", "PMID: 12345", "doi:10.1/X"))
#> [1] "10.1000/abc.1" "12345"         "10.1/X"       
```
