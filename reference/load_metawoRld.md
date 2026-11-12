# Load all studies of a metawoRld project

Load all studies of a metawoRld project

## Usage

``` r
load_metawoRld(
  path = ".",
  verbose = TRUE,
  drop_rejected = TRUE,
  verified_only = FALSE
)
```

## Arguments

- path:

  Project root.

- verbose:

  Report skipped studies? Defaults to `TRUE`.

- drop_rejected:

  Leave out data points a reviewer rejected? Defaults to `TRUE`.

- verified_only:

  Keep only data points whose current values were verified? Defaults to
  `FALSE`.

## Value

A list with

- `studies`:

  One row per study with the simple (scalar or string-vector) metadata
  fields; vectors are collapsed with `"; "`.

- `studies_data`:

  All data points with a `study_id` column. For each `_ref` field in the
  schema the scalar fields of the referenced object are joined, prefixed
  with the field name without `_id` (e.g. `method_unit`, `group_name`).
  `verification_status` (see
  [`verification_status()`](https://andjar.github.io/metawoRld/reference/verification_status.md))
  and, when `_rules.yml` defines variables, `variable_canonical` are
  added.

- `studies_metadata`:

  Named list of each study's metadata.
