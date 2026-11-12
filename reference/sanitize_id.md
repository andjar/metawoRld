# Encode an identifier so it can be used as a file or folder name

Replaces characters that are not allowed in file names (such as `/` in
DOIs) with reversible codes. Use `desanitize_id()` to get the original
back.

## Usage

``` r
sanitize_id(id)

desanitize_id(sanitized_id)
```

## Arguments

- id:

  Character vector of identifiers.

- sanitized_id:

  Character vector produced by `sanitize_id()`.

## Value

Character vector of file-system-safe strings (`""` for missing input).

## Examples

``` r
sanitize_id("10.1000/abc:1")
#> [1] "10.1000_fslash_abc_colon_1"
desanitize_id(sanitize_id("10.1000/abc:1"))
#> [1] "10.1000/abc:1"
```
