# Assign study groups to group categories

The most specific category of each group (see
[`group_memberships()`](https://andjar.github.io/metawoRld/reference/group_memberships.md)):
a category defined with more conditions wins.

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
