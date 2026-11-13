# Reject a submission

Reject a submission

## Usage

``` r
reject_submission(
  path = ".",
  study_id,
  extraction_id,
  reason = "",
  person = getOption("metawoRld.person")
)
```

## Arguments

- path:

  Project root.

- study_id, extraction_id:

  The submission.

- reason:

  Why it was rejected.

- person:

  Id of the person accepting (recorded in `submission.yml`). Defaults to
  the `metawoRld.person` option.

## Value

`NULL`, invisibly.
