# Check a submission

Re-runs the intake gate on a submission folder, e.g. one written by a
program in another language, and records the result in its
`submission.yml`. A pending submission with errors is marked `rejected`.

## Usage

``` r
check_submission(path = ".", study_id, extraction_id)
```

## Arguments

- path:

  Project root.

- study_id, extraction_id:

  The submission.

## Value

The gate report (`errors`, `warnings`), invisibly.
