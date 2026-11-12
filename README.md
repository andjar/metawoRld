# metawoRld

Structured, validated, version-controlled data for living systematic reviews.

A metawoRld project is a folder (usually a Git repository) holding:

* the review's settings and screening criteria (`_metawoRld.yml`),
* a schema describing what is extracted from each study (`_extraction_schema.yml`),
* one folder per study with `metadata.yml` and `data.csv`,
* meta-analyses defined in `_analyses.yml`, re-run whenever data change,
* a Quarto dashboard website (summary of findings, forest plots, cumulative
  meta-analyses, results per variable) that can be published automatically.

It works on its own with manual data entry, and with
[DataFindR](https://github.com/andjar/DataFindR), which uses LLMs to screen
studies and extract data into the project.

See `vignette("getting_started")` and `vignette("conceptual_overview")`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("andjar/metawoRld")
remotes::install_github("andjar/DataFindR")   # optional, for LLM screening/extraction
```

## Example

```r
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
|---|---|
| Create a project / refresh templates | `create_metawoRld()`, `use_templates()` |
| Add data | `add_study_data()`, `add_study_template()` |
| Validate | `validate_study()`, `validate_world()`, `validate_against_schema()`, `validate_links()` |
| Load | `load_metawoRld()`, `read_study()`, `list_studies()` |
| Review values against the papers | `review_values()`, `check_values()`, `verification_status()`, `set_verification()`, `update_data_point()` |
| Value rules (`_rules.yml`) | `read_rules()`, `add_variable_synonym()`, `canonical_variable()` |
| Living meta-analysis (`_analyses.yml`) | `read_analyses()`, `run_meta_analysis()`, `run_all_meta_analyses()`, `summary_of_findings()`, `plot_forest()`, `plot_cumulative()`, `plot_variable()`, `to_mean_sd()` |
| Website dashboard | `generate_website()`, `generate_dashboard()`, `use_github_action_quarto()` |
| Identifiers | `normalize_identifier()`, `sanitize_id()`, `desanitize_id()` |
