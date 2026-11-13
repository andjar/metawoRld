# Add a fact to the groups of the extraction form

For refining groups during a review, e.g. when early- and late-onset
preeclampsia turn out to matter. Adds an optional field to `groups` in
`forms/extraction.yml` and bumps the form's minor version. Studies
accepted before stay valid;
[`refinement_queue()`](https://andjar.github.io/metawoRld/reference/refinement_queue.md)
lists the groups where the fact is not yet assessed, and
[`amend_study()`](https://andjar.github.io/metawoRld/reference/amend_study.md)
fills it in. Define categories that use the fact with `where` in
`_rules.yml` (see
[`group_memberships()`](https://andjar.github.io/metawoRld/reference/group_memberships.md)).

## Usage

``` r
add_group_fact(path = ".", name, values = NULL, description)
```

## Arguments

- path:

  Project root.

- name:

  Field name, e.g. `"onset"`.

- values:

  Allowed values (makes the field an enum), e.g.
  `c("early", "late", "not reported")`. `NULL` for free text.

- description:

  Description shown in the entry form and to LLMs.

## Value

The new form version, invisibly.
