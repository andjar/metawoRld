# Which categories each study group belongs to

Categories are defined in `_rules.yml`. A category with `where` matches
groups whose facts (fields of `groups` in the extraction form, inherited
from the parent group unless set on the subgroup) have the given values:

## Usage

``` r
group_memberships(groups, rules)
```

## Arguments

- groups:

  The `groups` array of a study's metadata.

- rules:

  Rules from
  [`read_rules()`](https://andjar.github.io/metawoRld/reference/read_rules.md).

## Value

A tibble with one row per group and category it belongs to: `group_id`,
`category`.

## Details

    group_categories:
      PE:       {where: {category: PE}, synonyms: [Preeclampsia]}
      PE_early: {where: {category: PE, onset: early}}

A category without `where` matches groups whose (inherited) `category`
field, or own `name`, is the category name or one of its synonyms. A
group can belong to several categories.
