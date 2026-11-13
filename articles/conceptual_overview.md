# Conceptual overview: the living review pipeline

Systematic reviews go out of date quickly. A *living* review is updated
as new evidence appears. metawoRld keeps the review’s data in plain,
validated, traceable files; tools put data in through one door.

- **metawoRld** owns the review: settings, forms describing what is
  extracted, the search and screening log, accepted study data with
  provenance and verification, analyses and the website. It has no AI
  dependencies.
- **Channels** put data in. Each channel produces *submissions* that
  pass the same intake gate
  ([`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md)):
  the entry form, CSV files filled in with any editor, scripts, or
  **DataFindR**, which uses large language models to screen records and
  extract data. DataFindR depends on metawoRld, never the other way
  around.

The full format is defined in `SPEC.md` in the package repository.

## The project

[`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
creates a folder (by default a Git repository):

    my-review/
    ├── _metawoRld.yml       # settings, criteria, spec_version, license
    ├── _rules.yml           # variable vocabulary and value checks
    ├── _analyses.yml        # planned meta-analyses
    ├── people.yml           # people (with roles) and tools
    ├── searches.yml         # every search that was run
    ├── records.csv          # one row per unique record (search hit)
    ├── screening.csv        # screening decisions, append-only
    ├── forms/
    │   ├── extraction.yml   # what is extracted from each study
    │   └── screening.yml    # what a screening decision looks like
    ├── prompts/             # LLM instructions (used by DataFindR)
    ├── submissions/         # extractions waiting to be accepted
    │   └── <study_id>/<extraction_id>/
    ├── data/                # accepted data
    │   └── <study_id>/
    │       ├── study.yml         # identity, reports (DOIs/PMIDs), form version
    │       ├── metadata.yml      # design, population, methods, groups
    │       ├── data.csv          # one row per reported statistic
    │       ├── provenance.csv    # who/what produced each value, from which source
    │       ├── verification.csv  # reviewers' decisions per value
    │       └── datapackage.json  # Frictionless descriptor with file hashes
    └── index.qmd, _quarto.yml, ...   # website

A **study** (one cohort or trial, id like `hansen2019`) can have several
**reports** (publications, identified by DOI or PMID). **Records** are
search hits; an included record is linked to its study.

## Forms

The default extraction form is generic: studies have `methods` and
`groups`, and every reported statistic is a data point with a
`method_id`, `group_id`, `variable_name`, `timepoint`, `sample_size`,
`statistic_type` and up to three values. Edit `forms/extraction.yml` to
fit your review: add fields, tighten descriptions, or turn free-text
fields into enums. Bump the form’s `version` when you change it.

``` yaml
_form:
  name: extraction
  kind: extraction
  version: 1.0.0
...
group_id:
  _type: string                 # object, array, string, integer, number, boolean, enum
  _description: "Shown to the LLM and in the entry form."
  _required: true               # default
  _ref: groups.group_id         # must match an id in metadata$groups
```

[`export_schemas()`](https://andjar.github.io/metawoRld/reference/export_schemas.md)
writes JSON Schemas and Frictionless Table Schemas, so channels written
in other languages can validate their output.

## One door, then review

1.  **Set up** with
    [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md),
    [`add_person()`](https://andjar.github.io/metawoRld/reference/read_people.md),
    and adjust the forms and criteria.
2.  **Identify** records:
    [`add_search()`](https://andjar.github.io/metawoRld/reference/add_search.md)
    and
    [`add_records()`](https://andjar.github.io/metawoRld/reference/read_records.md)
    (DataFindR’s `df_search_pubmed()` does both).
3.  **Screen**:
    [`add_screening()`](https://andjar.github.io/metawoRld/reference/add_screening.md)
    stores each person’s (or tool’s) decision;
    [`screening_status()`](https://andjar.github.io/metawoRld/reference/add_screening.md)
    shows conflicts;
    [`prisma_counts()`](https://andjar.github.io/metawoRld/reference/prisma_counts.md)
    feeds the PRISMA diagram.
4.  **Extract**: any channel calls
    [`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md).
    Invalid data are refused with every problem listed; plausibility
    checks from `_rules.yml` are recorded as warnings.
5.  **Accept**:
    [`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md)
    for single extraction, or
    [`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md)
    and
    [`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md)
    for dual extraction. `provenance.csv` records which extraction each
    value came from and which others agreed.
6.  **Verify** values against the papers with
    [`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md).
    Decisions are tied to a hash of the values, so a changed value
    returns to the queue.
7.  **Check** with
    [`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md),
    locally and in CI
    ([`use_github_action_check()`](https://andjar.github.io/metawoRld/reference/use_github_action_check.md)).
    It recomputes the file hashes in `datapackage.json`, so data edited
    outside metawoRld are caught.
8.  **Analyse and publish** with
    [`run_all_meta_analyses()`](https://andjar.github.io/metawoRld/reference/run_all_meta_analyses.md)
    and
    [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md);
    release versions as signed Git tags archived on Zenodo.
