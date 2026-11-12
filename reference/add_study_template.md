# Create template files for manual data entry

Writes `metadata.yml` with a placeholder for every schema field (one
example element for arrays of objects) and a header-only `data.csv`.

## Usage

``` r
add_study_template(path = ".", study_id, overwrite = FALSE)
```

## Arguments

- path:

  Project root.

- study_id:

  Study identifier.

- overwrite:

  Replace an existing study folder? Defaults to `FALSE`.

## Value

The study folder, invisibly.
