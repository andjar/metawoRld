# Groups waiting for a fact

When a fact is added to the groups of the extraction form during the
review (for example `onset` with values early/late), studies extracted
before have no value for it. This lists the groups where the fact is not
assessed (neither set on the group nor inherited from its parent), so
they can be revisited, e.g. with
[`amend_study()`](https://andjar.github.io/metawoRld/reference/amend_study.md).

## Usage

``` r
refinement_queue(path = ".", field, category = NULL)
```

## Arguments

- path:

  Project root.

- field:

  The group field (fact) in `forms/extraction.yml`.

- category:

  Only groups belonging to this category (see
  [`group_memberships()`](https://andjar.github.io/metawoRld/reference/group_memberships.md)),
  e.g. `"PE"`.

## Value

A tibble with `study_id`, `group_id`, `name`, `parent_group_id`.
