# Validate one study

Reads the study's files and checks them against the schema, including
the `_ref` links between data points and metadata. Useful after editing
files by hand.

## Usage

``` r
validate_study(study_id, path = ".", check_links = TRUE)
```

## Arguments

- study_id:

  Study identifier.

- path:

  Project root.

- check_links:

  Check `_ref` links? Defaults to `TRUE`.

## Value

`TRUE` invisibly if valid; otherwise an error listing all problems.
