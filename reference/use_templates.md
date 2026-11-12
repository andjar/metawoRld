# Add or update the schema and prompt templates in a project

Copies `_extraction_schema.yml`, `_assessment_schema.yml`,
`_extraction_prompt.txt` and `_assessment_prompt.txt` into the project.

## Usage

``` r
use_templates(path = ".", overwrite = FALSE)
```

## Arguments

- path:

  Project root.

- overwrite:

  Replace files that already exist? Defaults to `FALSE`, so your edits
  are kept.

## Value

Paths of the files that were written, invisibly.
