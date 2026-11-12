# Add a study to a metawoRld project

Validates the metadata and data points against the project's extraction
schema (types, required fields, enum values and `_ref` links) and writes
`data/<study>/metadata.yml` and `data/<study>/data.csv`. All validation
problems are reported at once.

## Usage

``` r
add_study_data(
  path = ".",
  study_id,
  metadata,
  data,
  overwrite = FALSE,
  validate = TRUE
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study identifier, preferably the DOI or PMID. It is normalized with
  [`normalize_identifier()`](https://andjar.github.io/metawoRld/reference/normalize_identifier.md),
  stored as `metadata$study_id` and used (encoded with
  [`sanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md))
  as the folder name.

- metadata:

  Named list matching the `metadata` part of the schema. Arrays of
  objects may be given as data frames.

- data:

  Data frame (or list of objects) matching `data_points`. May have zero
  rows.

- overwrite:

  Replace an existing study? Defaults to `FALSE`.

- validate:

  Validate before writing? Defaults to `TRUE`.

## Value

The study folder, invisibly.
