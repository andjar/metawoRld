# Reconcile two or more extractions

Combines extractions of the same study into the accepted data. Values on
which all extractions agree are taken as they are; disagreements must be
resolved with `choices` (or taken from `base` with
`unresolved = "base"`). `provenance.csv` records, for each data point,
which extraction it came from and which other extractions agreed with
it; values set by the arbiter are attributed to them with channel
`reconcile`.

## Usage

``` r
reconcile(
  path = ".",
  study_id,
  extraction_ids = NULL,
  base = NULL,
  choices = NULL,
  person,
  unresolved = c("error", "base")
)
```

## Arguments

- path:

  Project root.

- study_id:

  Study id.

- extraction_ids:

  Extractions to compare. Defaults to all pending submissions of the
  study.

- base:

  Extraction whose values are used where nothing else is chosen.
  Defaults to the first one.

- choices:

  Data frame resolving disagreements, with columns `part`, `key`,
  `field` (as in
  [`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md))
  and either `from` (an extraction id to take the value from) or `value`
  (the value to use). For `field = "(row)"`, `from` adds or keeps the
  data point from that extraction and `value = "drop"` removes it. For
  metadata, `from` takes the whole top-level field (`key`) from that
  extraction.

- person:

  Id of the arbiter.

- unresolved:

  What to do with disagreements not covered by `choices`: `"error"`
  (default) or `"base"`.

## Value

The study folder, invisibly.
