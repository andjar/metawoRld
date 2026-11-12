# Load all studies of a metawoRld project

Load all studies of a metawoRld project

## Usage

``` r
load_metawoRld(path = ".", verbose = TRUE)
```

## Arguments

- path:

  Project root.

- verbose:

  Report skipped studies? Defaults to `TRUE`.

## Value

A list with

- `studies`:

  One row per study with the simple (scalar or string-vector) metadata
  fields; vectors are collapsed with `"; "`.

- `studies_data`:

  All data points with a `study_id` column. For each `_ref` field in the
  schema the scalar fields of the referenced object are joined, prefixed
  with the field name without `_id` (e.g. `method_unit`, `group_name`).

- `studies_metadata`:

  Named list of each study's metadata.
