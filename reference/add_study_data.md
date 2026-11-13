# Write accepted study data (low level)

Validates the metadata and data points against the extraction form and
writes `data/<study_id>/` (`study.yml`, `metadata.yml`, `data.csv`,
`provenance.csv` and `datapackage.json`; see `SPEC.md` section 8). All
validation problems are reported at once.

## Usage

``` r
add_study_data(
  path = ".",
  study_id,
  metadata,
  data,
  reports = NULL,
  provenance = list(channel = "manual_edit"),
  overwrite = FALSE,
  validate = TRUE
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study id (see
  [`check_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md)),
  e.g. `"hansen2019"`.

- metadata:

  Named list matching the `metadata` part of the form. Arrays of objects
  may be given as data frames.

- data:

  Data frame (or list of objects) matching `data_points`. May have zero
  rows.

- reports:

  Reports (publications) of the study: a list of lists with `doi`,
  `pmid` and `role` (`primary` or `secondary`). Defaults to the existing
  reports, else `metadata$doi`/`metadata$pmid`.

- provenance:

  Where the values came from: either a list of defaults
  (`extraction_id`, `person`, `channel`, `tool`, `model`,
  `source_sha256`, `timestamp`) applied to every data point, or a data
  frame with a `measurement_id` column (and `_metadata` for the
  metadata). When overwriting, the provenance of data points whose
  values did not change is kept.

- overwrite:

  Replace an existing study? Defaults to `FALSE`.

- validate:

  Validate before writing? Defaults to `TRUE`.

## Value

The study folder, invisibly.

## Details

This bypasses the submission workflow. Input channels should use
[`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md)
and
[`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md),
which call this function and record where each value came from.
