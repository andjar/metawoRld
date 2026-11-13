# Screening decisions

`screening.csv` is append-only. Each person (or tool) records their own
decision; a `consensus` decision settles disagreements.

## Usage

``` r
add_screening(
  path = ".",
  record_id,
  stage = c("title_abstract", "full_text"),
  decision,
  reason = "",
  person,
  kind = c("individual", "consensus")
)

screening_status(path = ".")
```

## Arguments

- path:

  Project root.

- record_id:

  Record id(s) (must be in `records.csv`).

- stage:

  `"title_abstract"` or `"full_text"`.

- decision:

  `"include"`, `"exclude"` or `"unsure"` (recycled).

- reason:

  Exclusion reason (recycled).

- person:

  Person or tool id.

- kind:

  `"individual"` (default) or `"consensus"`.

## Value

The appended rows, invisibly.

`screening_status()`: one row per record and stage with `outcome`
(`include`, `exclude`, `unsure`, `conflict`), `reason` and the number of
individual decisions.
