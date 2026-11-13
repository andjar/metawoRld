# Amend an accepted study

Refinements during a review go through the same door as extractions: a
small submission that changes an accepted study, accepted with
[`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md).
Use it to add a fact to groups (e.g. `onset` after it was added to the
form), link subgroups to their parent group, add a group that was not
extracted the first time, or add or correct data points. When accepted,
the provenance and verification of everything that did not change are
kept.

## Usage

``` r
amend_study(
  path = ".",
  study_id,
  person,
  groups = NULL,
  data = NULL,
  remove_data = NULL,
  metadata = NULL,
  note = "",
  channel = "amendment"
)
```

## Arguments

- path:

  Project root.

- study_id:

  Accepted study to amend.

- person:

  Person making the amendment.

- groups:

  List of groups to change or add, each a list with `group_id` and the
  fields to set, e.g.
  `list(list(group_id = "pe_early", parent_group_id = "pe", onset = "early"))`.
  Fields of existing groups not mentioned are kept.

- data:

  Data frame (or list) of data points to add, or to replace (matched by
  `measurement_id`).

- remove_data:

  `measurement_id`s of data points to remove.

- metadata:

  Named list of other metadata fields to replace.

- note:

  Why the study was amended (stored in `submission.yml`).

- channel:

  Channel name. Defaults to `"amendment"`.

## Value

The extraction id of the amendment, invisibly.
