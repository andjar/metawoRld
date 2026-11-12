# Read a schema from a metawoRld project

Reads `_extraction_schema.yml` or `_assessment_schema.yml` from the
project root. See the comments at the top of the template files (created
by
[`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md))
for the schema format.

## Usage

``` r
get_schema(path = ".", schema = c("extraction", "assessment"))
```

## Arguments

- path:

  Path to the project root. Defaults to the working directory.

- schema:

  `"extraction"` (default) or `"assessment"`.

## Value

The schema as a nested list.
