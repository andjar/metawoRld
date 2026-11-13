# Add or update the forms, prompts and rules templates in a project

Copies the default `forms/`, `prompts/`, `_rules.yml` and
`_analyses.yml` into the project.

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
