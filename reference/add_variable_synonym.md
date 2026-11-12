# Add a variable or synonym to the project's rules

Used by
[`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md)
when a reviewer maps an unknown variable name. Creates the variable if
it does not exist yet. Comments at the top of `_rules.yml` are kept;
other comments are lost.

## Usage

``` r
add_variable_synonym(path = ".", synonym, variable, unit = NULL)
```

## Arguments

- path:

  Project root.

- synonym:

  The name as reported in a paper.

- variable:

  The canonical variable name.

- unit:

  Optional expected unit (only set when the variable is new).

## Value

The updated rules, invisibly.
