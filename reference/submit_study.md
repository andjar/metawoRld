# Submit an extraction of a study

The one door for data from any channel (the entry form, CSV files,
DataFindR, scripts). The extraction is checked against the extraction
form (types, required fields, allowed values, links) and, if valid,
written to `submissions/<study_id>/<extraction_id>/` with a gate report.
Value checks from `_rules.yml` are recorded as warnings. Nothing is
written to `data/` until the submission is accepted with
[`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md)
or
[`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md).

## Usage

``` r
submit_study(
  path = ".",
  study_id,
  metadata,
  data = data.frame(),
  person,
  channel = "script",
  tool = NULL,
  model = NULL,
  source_sha256 = NULL,
  reports = NULL,
  provenance = NULL,
  blind = FALSE
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study id, e.g. `"hansen2019"` (see
  [`suggest_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md)).

- metadata:

  Named list matching the form's `metadata`, or the path to a
  `metadata.yml` file.

- data:

  Data frame of data points, or the path to a `data.csv` file.

- person:

  Id of the person or tool (from `people.yml`) who made the extraction.

- channel:

  How the data were produced, e.g. `"entry_form"`, `"csv_import"`,
  `"datafindr"`, `"script"`.

- tool:

  Producing software as `name@version`. Defaults to metawoRld's version.

- model:

  LLM model, if one was used.

- source_sha256:

  Hash of the full text the values were read from (see
  [`hash_file()`](https://andjar.github.io/metawoRld/reference/hash_file.md)).

- reports:

  Reports of the study (list of lists with `doi`, `pmid`, `role`).
  Defaults to `metadata$doi`/`metadata$pmid`.

- provenance:

  Optional data frame with per-data-point provenance (`measurement_id`
  and any of the provenance columns), e.g. a model per value.

- blind:

  Hide this extraction from other extractors until reconciled (used by
  the entry form for dual extraction)?

## Value

The extraction id, invisibly. Validation errors abort with class
`metawoRld_validation_error`; nothing is written then.
