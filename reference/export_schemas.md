# Write JSON Schemas and Table Schemas for the project's forms

Writes `forms/<name>.schema.json` (JSON Schema 2020-12) for every form,
and `forms/extraction.data.tableschema.json` (Frictionless Table Schema
for `data.csv`). Tools in any language can validate submissions with
these files. They are generated: do not edit them by hand.

## Usage

``` r
export_schemas(path = ".")
```

## Arguments

- path:

  Project root.

## Value

Paths of the written files, invisibly.
