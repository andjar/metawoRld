# The project vocabulary formatted for an LLM prompt

Used by DataFindR to fill `{{known_variables}}` and
`{{group_categories}}` in `_extraction_prompt.txt`.

## Usage

``` r
rules_for_prompt(path = ".")
```

## Arguments

- path:

  Project root.

## Value

A list with the strings `known_variables` and `group_categories`.
