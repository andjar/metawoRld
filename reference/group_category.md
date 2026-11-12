# Assign study groups to group categories

Uses a group's `category` field if it names a known category (or one of
its synonyms); otherwise matches the group's `name` against the category
names and synonyms in `_rules.yml` (case-insensitive).

## Usage

``` r
group_category(groups, rules)
```

## Arguments

- groups:

  The `groups` array of a study's metadata.

- rules:

  Rules from
  [`read_rules()`](https://andjar.github.io/metawoRld/reference/read_rules.md).

## Value

A character vector of categories named by `group_id` (`NA` where no
category applies).
