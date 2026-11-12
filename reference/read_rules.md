# Read the project's value rules

Reads `_rules.yml` (see the comments in the file created by
[`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
or
[`use_templates()`](https://andjar.github.io/metawoRld/reference/use_templates.md)).
Missing files or sections fall back to the defaults: no variable
vocabulary and all checks enabled.

## Usage

``` r
read_rules(path = ".")
```

## Arguments

- path:

  Project root.

## Value

A list with `variables` (named list), `group_categories` (named list)
and `checks` (named list of severities: `"error"`, `"warning"` or
`"off"`).
