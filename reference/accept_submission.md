# Accept a submission

Writes the submission to `data/<study_id>/` with provenance pointing to
the extraction, marks it `accepted` and marks earlier accepted
submissions of the study `superseded`. If the study already has data,
they are replaced; verification of unchanged data points is kept.
Records whose DOI or PMID match the study's reports are linked to the
study.

## Usage

``` r
accept_submission(
  path = ".",
  study_id,
  extraction_id,
  person = getOption("metawoRld.person")
)
```

## Arguments

- path:

  Project root.

- study_id, extraction_id:

  The submission.

- person:

  Id of the person accepting (recorded in `submission.yml`). Defaults to
  the `metawoRld.person` option.

## Value

The study folder, invisibly.

## Details

For dual extraction, use
[`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md)
and
[`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md)
instead.
