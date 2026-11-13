# metawoRld

Structured, validated, traceable data for living systematic reviews.

A metawoRld project is a folder (usually a Git repository) of plain
YAML, CSV and JSON files, laid out as defined in
[SPEC.md](https://andjar.github.io/metawoRld/SPEC.md):

- settings, criteria and **forms** describing what is extracted
  (`_metawoRld.yml`, `forms/extraction.yml`),
- the **search and screening log** (`searches.yml`, `records.csv`,
  `screening.csv`), from which PRISMA counts are computed,
- **one door for data**: every input channel (an entry form, CSV files,
  the LLM pipeline [DataFindR](https://github.com/andjar/DataFindR),
  scripts) calls
  [`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md),
  which validates the extraction and stages it in `submissions/`,
- **accepted studies** in `data/<study_id>/`, with `provenance.csv` (who
  or what produced each value, from which source), `verification.csv`
  (human checks, tied to a hash of the values) and `datapackage.json` (a
  Frictionless Data Package with file hashes),
- meta-analyses defined in `_analyses.yml` and a Quarto **dashboard**
  website, rebuilt whenever data change.

[`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
checks all of it, locally or in CI, and catches data edited outside
metawoRld.

**No R code needed to get going:**
[`metawoRld::launch()`](https://andjar.github.io/metawoRld/reference/launch.md)
opens an app to start a review, accept and reconcile submissions, verify
values, check, publish and share the project with a team (Git, without
Git commands):

![The metawoRld app](reference/figures/app.gif)

The metawoRld app

**Documentation:** <https://andjar.github.io/metawoRld/>, including a
[worked
example](https://andjar.github.io/metawoRld/articles/worked-example.html)
with real data from four open-access studies of IL-6 in preeclampsia.

![The living meta-analysis dashboard built from the worked
example](reference/figures/dashboard-analysis.png)

The living meta-analysis dashboard built from the worked example

Values are verified against the papers in a keyboard-driven review app;
every decision is tied to a hash of the values it approved:

![Verifying values with the keyboard in
review_values()](reference/figures/review-app.gif)

Verifying values with the keyboard in review_values()

## Installation

``` r

# install.packages("remotes")
remotes::install_github("andjar/metawoRld")
remotes::install_github("andjar/DataFindR")   # optional, for LLM screening/extraction
```

## Example

Open the app (start a review, or explore the worked example):

``` r

metawoRld::launch()
```

Or try the worked example from R:

``` r

library(metawoRld)
p <- example_project(file.path(tempdir(), "il6"))
review_values(p)       # verify values
generate_website(p)    # build the dashboard (needs Quarto)
```

Or start your own review:

``` r

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
|----|----|
| The app | [`launch()`](https://andjar.github.io/metawoRld/reference/launch.md) |
| Create a project | [`new_review()`](https://andjar.github.io/metawoRld/reference/new_review.md), [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md), [`use_templates()`](https://andjar.github.io/metawoRld/reference/use_templates.md), [`add_person()`](https://andjar.github.io/metawoRld/reference/read_people.md), [`read_people()`](https://andjar.github.io/metawoRld/reference/read_people.md) |
| Forms | [`get_schema()`](https://andjar.github.io/metawoRld/reference/get_schema.md), [`list_forms()`](https://andjar.github.io/metawoRld/reference/list_forms.md), [`export_schemas()`](https://andjar.github.io/metawoRld/reference/export_schemas.md), [`table_schema()`](https://andjar.github.io/metawoRld/reference/table_schema.md) |
| Search and screening log | [`add_search()`](https://andjar.github.io/metawoRld/reference/add_search.md), [`add_records()`](https://andjar.github.io/metawoRld/reference/read_records.md), [`read_records()`](https://andjar.github.io/metawoRld/reference/read_records.md), [`link_record()`](https://andjar.github.io/metawoRld/reference/read_records.md), [`add_screening()`](https://andjar.github.io/metawoRld/reference/add_screening.md), [`screening_status()`](https://andjar.github.io/metawoRld/reference/add_screening.md), [`prisma_counts()`](https://andjar.github.io/metawoRld/reference/prisma_counts.md) |
| Submit (the one door) | [`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md), [`submission_template()`](https://andjar.github.io/metawoRld/reference/submission_template.md), [`check_submission()`](https://andjar.github.io/metawoRld/reference/check_submission.md), [`list_submissions()`](https://andjar.github.io/metawoRld/reference/list_submissions.md), [`read_submission()`](https://andjar.github.io/metawoRld/reference/read_submission.md) |
| Accept | [`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md), [`reject_submission()`](https://andjar.github.io/metawoRld/reference/reject_submission.md), [`compare_with_accepted()`](https://andjar.github.io/metawoRld/reference/compare_with_accepted.md), [`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md), [`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md) |
| Study ids | [`suggest_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md), [`check_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md), [`is_study_id()`](https://andjar.github.io/metawoRld/reference/is_study_id.md) |
| Load | [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md), [`read_study()`](https://andjar.github.io/metawoRld/reference/read_study.md), [`list_studies()`](https://andjar.github.io/metawoRld/reference/list_studies.md) |
| Verify values against the papers | [`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md), [`check_values()`](https://andjar.github.io/metawoRld/reference/check_values.md), [`verification_status()`](https://andjar.github.io/metawoRld/reference/verification_status.md), [`set_verification()`](https://andjar.github.io/metawoRld/reference/set_verification.md), [`update_data_point()`](https://andjar.github.io/metawoRld/reference/update_data_point.md) |
| Integrity | [`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md), [`use_github_action_check()`](https://andjar.github.io/metawoRld/reference/use_github_action_check.md), [`hash_file()`](https://andjar.github.io/metawoRld/reference/hash_file.md), [`validate_study()`](https://andjar.github.io/metawoRld/reference/validate_study.md), [`validate_world()`](https://andjar.github.io/metawoRld/reference/validate_world.md) |
| Groups and subgroups | [`group_memberships()`](https://andjar.github.io/metawoRld/reference/group_memberships.md), [`add_group_fact()`](https://andjar.github.io/metawoRld/reference/add_group_fact.md), [`refinement_queue()`](https://andjar.github.io/metawoRld/reference/refinement_queue.md), [`amend_study()`](https://andjar.github.io/metawoRld/reference/amend_study.md) |
| Value rules (`_rules.yml`) | [`read_rules()`](https://andjar.github.io/metawoRld/reference/read_rules.md), [`add_variable_synonym()`](https://andjar.github.io/metawoRld/reference/add_variable_synonym.md), [`canonical_variable()`](https://andjar.github.io/metawoRld/reference/canonical_variable.md) |
| Living meta-analysis (`_analyses.yml`) | [`read_analyses()`](https://andjar.github.io/metawoRld/reference/read_analyses.md), [`run_meta_analysis()`](https://andjar.github.io/metawoRld/reference/run_meta_analysis.md), [`run_all_meta_analyses()`](https://andjar.github.io/metawoRld/reference/run_all_meta_analyses.md), [`summary_of_findings()`](https://andjar.github.io/metawoRld/reference/summary_of_findings.md), [`plot_forest()`](https://andjar.github.io/metawoRld/reference/plot_forest.md), [`plot_cumulative()`](https://andjar.github.io/metawoRld/reference/plot_cumulative.md), [`plot_variable()`](https://andjar.github.io/metawoRld/reference/plot_variable.md), [`to_mean_sd()`](https://andjar.github.io/metawoRld/reference/to_mean_sd.md) |
| Team work (Git) | [`sync_project()`](https://andjar.github.io/metawoRld/reference/sync_project.md), [`sync_status()`](https://andjar.github.io/metawoRld/reference/sync_status.md), [`commit_project()`](https://andjar.github.io/metawoRld/reference/commit_project.md) |
| Website dashboard | [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md), [`generate_dashboard()`](https://andjar.github.io/metawoRld/reference/generate_dashboard.md), [`use_github_action_quarto()`](https://andjar.github.io/metawoRld/reference/use_github_action_quarto.md) |
