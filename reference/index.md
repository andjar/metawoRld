# Package index

## Create a project

- [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
  : Create a new metawoRld project
- [`use_templates()`](https://andjar.github.io/metawoRld/reference/use_templates.md)
  : Add or update the forms, prompts and rules templates in a project
- [`read_config()`](https://andjar.github.io/metawoRld/reference/read_config.md)
  : Read the project configuration
- [`read_people()`](https://andjar.github.io/metawoRld/reference/read_people.md)
  [`add_person()`](https://andjar.github.io/metawoRld/reference/read_people.md)
  : People and tools of a project
- [`example_project()`](https://andjar.github.io/metawoRld/reference/example_project.md)
  : Build the worked example project

## Forms

What is extracted, defined in `forms/*.yml`.

- [`get_schema()`](https://andjar.github.io/metawoRld/reference/get_schema.md)
  [`read_form()`](https://andjar.github.io/metawoRld/reference/get_schema.md)
  : Read a form from a metawoRld project
- [`list_forms()`](https://andjar.github.io/metawoRld/reference/list_forms.md)
  : List the forms of a project
- [`export_schemas()`](https://andjar.github.io/metawoRld/reference/export_schemas.md)
  : Write JSON Schemas and Table Schemas for the project's forms
- [`table_schema()`](https://andjar.github.io/metawoRld/reference/table_schema.md)
  : Frictionless Table Schema for the data points of a form
- [`validate_against_schema()`](https://andjar.github.io/metawoRld/reference/validate_against_schema.md)
  : Validate data against a metawoRld schema
- [`validate_links()`](https://andjar.github.io/metawoRld/reference/validate_links.md)
  : Validate links between data points and metadata
- [`schema_refs()`](https://andjar.github.io/metawoRld/reference/schema_refs.md)
  : List the cross-references defined in a schema
- [`schema_col_types()`](https://andjar.github.io/metawoRld/reference/schema_col_types.md)
  : Column types for a study's data.csv derived from the schema

## Search and screening log

Searches, records and screening decisions; PRISMA counts are computed
from these files.

- [`add_search()`](https://andjar.github.io/metawoRld/reference/add_search.md)
  : Record a literature search
- [`read_records()`](https://andjar.github.io/metawoRld/reference/read_records.md)
  [`add_records()`](https://andjar.github.io/metawoRld/reference/read_records.md)
  [`link_record()`](https://andjar.github.io/metawoRld/reference/read_records.md)
  : The record registry
- [`record_id()`](https://andjar.github.io/metawoRld/reference/record_id.md)
  : Record ids
- [`add_screening()`](https://andjar.github.io/metawoRld/reference/add_screening.md)
  [`screening_status()`](https://andjar.github.io/metawoRld/reference/add_screening.md)
  : Screening decisions
- [`prisma_counts()`](https://andjar.github.io/metawoRld/reference/prisma_counts.md)
  : Counts for a PRISMA 2020 flow diagram

## Submit (the one door)

Every input channel submits through the same validated intake.

- [`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md)
  : Submit an extraction of a study
- [`submission_template()`](https://andjar.github.io/metawoRld/reference/submission_template.md)
  : Write blank files for a submission
- [`check_submission()`](https://andjar.github.io/metawoRld/reference/check_submission.md)
  : Check a submission
- [`list_submissions()`](https://andjar.github.io/metawoRld/reference/list_submissions.md)
  : List submissions
- [`read_submission()`](https://andjar.github.io/metawoRld/reference/read_submission.md)
  : Read a submission

## Accept and reconcile

- [`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md)
  : Accept a submission
- [`reject_submission()`](https://andjar.github.io/metawoRld/reference/reject_submission.md)
  : Reject a submission
- [`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md)
  : Compare extractions of the same study
- [`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md)
  : Reconcile two or more extractions
- [`add_study_data()`](https://andjar.github.io/metawoRld/reference/add_study_data.md)
  : Write accepted study data (low level)

## Study ids and identifiers

- [`is_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md)
  [`check_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md)
  [`suggest_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md)
  : Check or suggest study ids
- [`normalize_identifier()`](https://andjar.github.io/metawoRld/reference/normalize_identifier.md)
  : Normalize study identifiers
- [`identifier_type()`](https://andjar.github.io/metawoRld/reference/identifier_type.md)
  : Detect the type of a study identifier
- [`sanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md)
  [`desanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md)
  : Encode an identifier so it can be used as a file or folder name

## Load data

- [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
  : Load all studies of a metawoRld project
- [`read_study()`](https://andjar.github.io/metawoRld/reference/read_study.md)
  : Read one study from a metawoRld project
- [`list_studies()`](https://andjar.github.io/metawoRld/reference/list_studies.md)
  : List the studies in a project

## Verify values against the papers

- [`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md)
  [`review_values_app()`](https://andjar.github.io/metawoRld/reference/review_values.md)
  : Review extracted values against the papers
- [`check_values()`](https://andjar.github.io/metawoRld/reference/check_values.md)
  : Check extracted values for plausibility and consistency
- [`verification_status()`](https://andjar.github.io/metawoRld/reference/verification_status.md)
  : Verification status of data points
- [`set_verification()`](https://andjar.github.io/metawoRld/reference/set_verification.md)
  : Record the verification of data points
- [`update_data_point()`](https://andjar.github.io/metawoRld/reference/update_data_point.md)
  : Correct a data point

## Integrity

- [`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
  : Check a whole metawoRld project
- [`use_github_action_check()`](https://andjar.github.io/metawoRld/reference/use_github_action_check.md)
  : Add a GitHub Actions workflow that runs check_project()
- [`hash_file()`](https://andjar.github.io/metawoRld/reference/hash_file.md)
  : SHA-256 hash of a file
- [`validate_study()`](https://andjar.github.io/metawoRld/reference/validate_study.md)
  : Validate one study
- [`validate_world()`](https://andjar.github.io/metawoRld/reference/validate_world.md)
  : Validate all studies in a project

## Vocabulary and value rules

- [`read_rules()`](https://andjar.github.io/metawoRld/reference/read_rules.md)
  : Read the project's value rules
- [`add_variable_synonym()`](https://andjar.github.io/metawoRld/reference/add_variable_synonym.md)
  : Add a variable or synonym to the project's rules
- [`canonical_variable()`](https://andjar.github.io/metawoRld/reference/canonical_variable.md)
  : Map variable names to their canonical names
- [`group_category()`](https://andjar.github.io/metawoRld/reference/group_category.md)
  : Assign study groups to group categories
- [`rules_for_prompt()`](https://andjar.github.io/metawoRld/reference/rules_for_prompt.md)
  : The project vocabulary formatted for an LLM prompt

## Living meta-analysis

- [`read_analyses()`](https://andjar.github.io/metawoRld/reference/read_analyses.md)
  : Read the project's meta-analysis definitions
- [`run_meta_analysis()`](https://andjar.github.io/metawoRld/reference/run_meta_analysis.md)
  : Run a meta-analysis
- [`run_all_meta_analyses()`](https://andjar.github.io/metawoRld/reference/run_all_meta_analyses.md)
  : Run all meta-analyses of a project
- [`summary_of_findings()`](https://andjar.github.io/metawoRld/reference/summary_of_findings.md)
  : Summary of findings table
- [`analysis_data()`](https://andjar.github.io/metawoRld/reference/analysis_data.md)
  : Data used in a meta-analysis
- [`to_mean_sd()`](https://andjar.github.io/metawoRld/reference/to_mean_sd.md)
  : Convert summary statistics to mean and SD
- [`plot_forest()`](https://andjar.github.io/metawoRld/reference/plot_forest.md)
  : Forest plot of a meta-analysis
- [`plot_cumulative()`](https://andjar.github.io/metawoRld/reference/plot_cumulative.md)
  : Cumulative meta-analysis plot
- [`plot_variable()`](https://andjar.github.io/metawoRld/reference/plot_variable.md)
  : Plot all reported values of a variable

## Website dashboard

- [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md)
  : Generate (and render) the project website
- [`generate_dashboard()`](https://andjar.github.io/metawoRld/reference/generate_dashboard.md)
  : Generate the dashboard pages of the website
- [`generate_study_webpage()`](https://andjar.github.io/metawoRld/reference/generate_study_webpage.md)
  : Generate one web page per study
- [`use_github_action_quarto()`](https://andjar.github.io/metawoRld/reference/use_github_action_quarto.md)
  : Add a GitHub Actions workflow that publishes the website
