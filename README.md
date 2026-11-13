# metawoRld

Structured, validated, traceable data for living systematic reviews.

A metawoRld project is a folder (usually a Git repository) of plain YAML,
CSV and JSON files, laid out as defined in [SPEC.md](SPEC.md):

* settings, criteria and **forms** describing what is extracted
  (`_metawoRld.yml`, `forms/extraction.yml`),
* the **search and screening log** (`searches.yml`, `records.csv`,
  `screening.csv`), from which PRISMA counts are computed,
* **one door for data**: every input channel (an entry form, CSV files, the
  LLM pipeline [DataFindR](https://github.com/andjar/DataFindR), scripts)
  calls `submit_study()`, which validates the extraction and stages it in
  `submissions/`,
* **accepted studies** in `data/<study_id>/`, with `provenance.csv` (who or
  what produced each value, from which source), `verification.csv` (human
  checks, tied to a hash of the values) and `datapackage.json` (a
  Frictionless Data Package with file hashes),
* meta-analyses defined in `_analyses.yml` and a Quarto **dashboard**
  website, rebuilt whenever data change.

`check_project()` checks all of it, locally or in CI, and catches data edited
outside metawoRld.

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
  "my-review",
  project_name = "Biomarker X in pregnancy",
  project_description = "Living review of maternal serum levels of X.",
  inclusion_criteria = c("Human pregnancy", "Serum or plasma X reported"),
  exclusion_criteria = c("Animal study", "Review or case report")
)
add_person("my-review", "ahj", "Anders Hagen Jarmund", roles = c("admin", "extractor", "verifier"))

id <- submit_study("my-review", "hansen2019", metadata = list(...), data = my_data_frame,
                   person = "ahj", channel = "script")
accept_submission("my-review", "hansen2019", id, person = "ahj")
review_values("my-review")          # verify values against the papers
check_project("my-review")          # schemas, hashes, provenance, people
generate_website("my-review")       # living meta-analysis dashboard
```

## Main functions

| Task | Function |
|---|---|
| Create a project | `create_metawoRld()`, `use_templates()`, `add_person()`, `read_people()` |
| Forms | `get_schema()`, `list_forms()`, `export_schemas()`, `table_schema()` |
| Search and screening log | `add_search()`, `add_records()`, `read_records()`, `link_record()`, `add_screening()`, `screening_status()`, `prisma_counts()` |
| Submit (the one door) | `submit_study()`, `submission_template()`, `check_submission()`, `list_submissions()`, `read_submission()` |
| Accept | `accept_submission()`, `reject_submission()`, `compare_extractions()`, `reconcile()` |
| Study ids | `suggest_study_id()`, `check_study_id()`, `is_study_id()` |
| Load | `load_metawoRld()`, `read_study()`, `list_studies()` |
| Verify values against the papers | `review_values()`, `check_values()`, `verification_status()`, `set_verification()`, `update_data_point()` |
| Integrity | `check_project()`, `use_github_action_check()`, `hash_file()`, `validate_study()`, `validate_world()` |
| Value rules (`_rules.yml`) | `read_rules()`, `add_variable_synonym()`, `canonical_variable()` |
| Living meta-analysis (`_analyses.yml`) | `read_analyses()`, `run_meta_analysis()`, `run_all_meta_analyses()`, `summary_of_findings()`, `plot_forest()`, `plot_cumulative()`, `plot_variable()`, `to_mean_sd()` |
| Website dashboard | `generate_website()`, `generate_dashboard()`, `use_github_action_quarto()` |
