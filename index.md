# metawoRld

Structured, validated, version-controlled data for living systematic
reviews.

A metawoRld project is a folder (usually a Git repository) holding:

- the review’s settings and screening criteria (`_metawoRld.yml`),
- a schema describing what is extracted from each study
  (`_extraction_schema.yml`),
- one folder per study with `metadata.yml` and `data.csv`,
- a Quarto website that can be published automatically.

It works on its own with manual data entry, and with
[DataFindR](https://github.com/andjar/DataFindR), which uses LLMs to
screen studies and extract data into the project.

See
[`vignette("getting_started")`](https://andjar.github.io/metawoRld/articles/getting_started.md)
and
[`vignette("conceptual_overview")`](https://andjar.github.io/metawoRld/articles/conceptual_overview.md).

## Installation

``` r

# install.packages("remotes")
remotes::install_github("andjar/metawoRld")
remotes::install_github("andjar/DataFindR")   # optional, for LLM screening/extraction
```

## Example

``` r

library(metawoRld)

create_metawoRld(
  "my_review",
  project_name = "Biomarker X in pregnancy",
  project_description = "Living review of maternal serum levels of X.",
  inclusion_criteria = c("Human pregnancy", "Serum or plasma X reported"),
  exclusion_criteria = c("Animal study", "Review or case report")
)

add_study_data("my_review", "10.1000/abc", metadata = list(...), data = my_data_frame)
validate_world("my_review")
world <- load_metawoRld("my_review")
generate_website("my_review")
```

## Main functions

| Task | Function |
|----|----|
| Create a project / refresh templates | [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md), [`use_templates()`](https://andjar.github.io/metawoRld/reference/use_templates.md) |
| Add data | [`add_study_data()`](https://andjar.github.io/metawoRld/reference/add_study_data.md), [`add_study_template()`](https://andjar.github.io/metawoRld/reference/add_study_template.md) |
| Validate | [`validate_study()`](https://andjar.github.io/metawoRld/reference/validate_study.md), [`validate_world()`](https://andjar.github.io/metawoRld/reference/validate_world.md), [`validate_against_schema()`](https://andjar.github.io/metawoRld/reference/validate_against_schema.md), [`validate_links()`](https://andjar.github.io/metawoRld/reference/validate_links.md) |
| Load | [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md), [`read_study()`](https://andjar.github.io/metawoRld/reference/read_study.md), [`list_studies()`](https://andjar.github.io/metawoRld/reference/list_studies.md) |
| Website | [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md), [`use_github_action_quarto()`](https://andjar.github.io/metawoRld/reference/use_github_action_quarto.md) |
| Identifiers | [`normalize_identifier()`](https://andjar.github.io/metawoRld/reference/normalize_identifier.md), [`sanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md), [`desanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md) |
