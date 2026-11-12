# Create a new metawoRld project

Creates the project folder with a configuration file (`_metawoRld.yml`),
the default schema and prompt templates, a `data/` folder, a README,
website templates and (optionally) a Git repository.

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

- ...:

  Further named values stored in `_metawoRld.yml` (e.g.
  `search_query = "..."`).

## Value

The project path, invisibly.

## Details

The project contains:

- `_metawoRld.yml`: project name, description, criteria and other
  settings.

- `_extraction_schema.yml`, `_assessment_schema.yml`: what is extracted
  and how screening results look. Edit these to fit your review.

- `_extraction_prompt.txt`, `_assessment_prompt.txt`: LLM prompts used
  by DataFindR. `{{placeholders}}` are filled in automatically.

- `data/<study>/metadata.yml` and `data/<study>/data.csv` for each
  study.

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
