# Detect the type of a study identifier

Detect the type of a study identifier

## Usage

``` r
identifier_type(x)
```

## Arguments

- x:

  Character vector of (preferably normalized) identifiers.

## Value

Character vector: `"doi"`, `"pmid"` or `"other"`.

## Examples

``` r
identifier_type(c("10.1000/abc", "12345678", "Smith2020"))
#> [1] "doi"   "pmid"  "other"
```
