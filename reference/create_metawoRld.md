# Create a new metawoRld project

Creates the project folder laid out as described in `SPEC.md`: settings
(`_metawoRld.yml`), forms, prompts, rules, the people list, the search
and screening log, `submissions/` and `data/`, a README, website
templates and (optionally) a Git repository.

## Usage

``` r
create_metawoRld(
  path,
  project_name,
  project_description,
  inclusion_criteria = character(0),
  exclusion_criteria = character(0),
  git_init = TRUE,
  website = TRUE,
  license = "CC-BY-4.0",
  ...
)
```

## Arguments

- path:

  Folder to create. Must not exist yet.

- project_name:

  Short name of the review.

- project_description:

  One or two sentences describing the scope.

- inclusion_criteria, exclusion_criteria:

  Character vectors of criteria used for screening.

- git_init:

  Initialise a Git repository? Defaults to `TRUE`.

- website:

  Add Quarto website templates? Defaults to `TRUE`.

- license:

  Data license, stored in `_metawoRld.yml` and in every study's
  `datapackage.json`. Defaults to `"CC-BY-4.0"`.

- ...:

  Further named values stored in `_metawoRld.yml` (e.g.
  `search_query = "..."`).

## Value

The project path, invisibly.

## Details

The project contains:

- `_metawoRld.yml`: project name, description, criteria and other
  settings.

- `_metawoRld.yml`: settings, criteria and `spec_version`.

- `forms/extraction.yml` (what is extracted) and `forms/screening.yml`.
  Edit these to fit your review.

- `prompts/`: LLM prompts used by DataFindR.

- `_rules.yml`, `_analyses.yml`: vocabulary, value checks and planned
  analyses.

- `people.yml`, `searches.yml`, `records.csv`, `screening.csv`: who
  works on the review, and the search and screening log.

- `submissions/`: staged extractions; `data/<study_id>/`: accepted data.

## Examples

``` r
if (FALSE) { # \dontrun{
create_metawoRld(
  file.path(tempdir(), "my_review"),
  project_name = "Biomarker X in pregnancy",
  project_description = "Living review of maternal serum levels of X.",
  inclusion_criteria = c("Human pregnancy", "Serum or plasma levels of X reported"),
  exclusion_criteria = c("Animal study", "Review or case report")
)
} # }
```
