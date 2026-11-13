# Check a whole metawoRld project

The single check to run locally and in continuous integration. It checks
the project files, every accepted study (schema and link errors, file
hashes in `datapackage.json`, provenance, form version, people) and the
submissions, records and screening log.

## Usage

``` r
check_project(path = ".", ci = FALSE)
```

## Arguments

- path:

  Project root.

- ci:

  Running in CI? If `TRUE`, the R session exits with status 1 when
  errors are found, so the CI job fails. Use
  `Rscript -e 'metawoRld::check_project(ci = TRUE)'`.

## Value

A tibble of issues (`level`, `where`, `message`), invisibly.

## Details

Errors mean the data are invalid or were changed outside metawoRld (for
example a value edited in `data.csv` without updating provenance and
hashes). Warnings point at work left to do.
