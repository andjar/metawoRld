# Record the verification of data points

Record the verification of data points

## Usage

``` r
set_verification(
  path = ".",
  study_id,
  measurement_id,
  status = c("verified", "rejected", "unverified"),
  reviewer = NULL,
  note = NULL
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study identifier.

- measurement_id:

  One or more `measurement_id`s in the study.

- status:

  `"verified"`, `"rejected"` (the value is wrong or should not be used)
  or `"unverified"` (removes an earlier decision).

- reviewer:

  Reviewer name or initials. Defaults to the `metawoRld.reviewer`
  option, then the system user name.

- note:

  Optional note, e.g. why a value was rejected.

## Value

The updated verification table of the study, invisibly.
