# Conceptual overview: the living review pipeline

Systematic reviews go out of date quickly. A *living* review is updated
as new evidence appears. Two R packages work together to make that
practical:

- **metawoRld** owns the review: settings, screening criteria, the
  schema describing what is extracted, the validated study data, and the
  website. It has no AI dependencies and can be used on its own with
  manual data entry.
- **DataFindR** uses large language models (through `ellmer`) to screen
  studies and extract data *into* a metawoRld project.

DataFindR depends on metawoRld, never the other way around.

## The project

[`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
creates a folder (by default a Git repository):

    my_review/
    ├── _metawoRld.yml           # name, description, inclusion/exclusion criteria
    ├── _extraction_schema.yml   # what is extracted from each study
    ├── _assessment_schema.yml   # what a screening decision looks like
    ├── _extraction_prompt.txt   # LLM instructions (used by DataFindR)
    ├── _assessment_prompt.txt
    ├── data/
    │   └── <study id>/
    │       ├── metadata.yml     # design, population, methods, groups
    │       └── data.csv         # one row per reported statistic
    ├── .metawoRld_cache/        # DataFindR's cached LLM results (shareable)
    └── index.qmd, studies.qmd, data.qmd, _quarto.yml, ...   # website

Studies are stored under their DOI or PMID (normalized, and encoded for
the file system with
[`sanitize_id()`](https://andjar.github.io/metawoRld/reference/sanitize_id.md)),
so cached screening/extraction results and stored data always refer to
the same paper.

## The schema

The templates are generic: studies have `methods` and `groups`, and
every reported statistic is a data point with a `method_id`, `group_id`,
`variable_name`, `timepoint`, `sample_size`, `statistic_type` and up to
three values. Edit `_extraction_schema.yml` to fit your review: add
fields, tighten descriptions, or turn free-text fields into enums.

A schema node looks like this:

``` yaml
group_id:
  _type: string                 # object, array, string, integer, number, boolean, enum
  _description: "Shown to the LLM."
  _required: true               # default
  _ref: groups.group_id         # must match an id in metadata$groups
```

`_ref` links are checked by
[`validate_links()`](https://andjar.github.io/metawoRld/reference/validate_links.md)
and used by
[`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md)
to join, for example, `group_name` and `method_unit` onto the data.

## Pipeline

1.  **Set up** the project with
    [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
    and adjust the schema and criteria.
2.  **Identify** records: `DataFindR::df_search_pubmed()` (repeat it to
    pick up new publications), or `df_fetch_metadata()` /
    `df_add_metadata()`.
3.  **Screen** titles and abstracts: `DataFindR::df_assess_batch()` in
    parallel or with discounted batch APIs; review and override in
    `df_shiny_assess()`.
4.  **Extract** data from full texts of included studies:
    `DataFindR::df_extract_batch()`; check and correct in
    `df_shiny_extract()`.
5.  **Import**: `DataFindR::df_import_batch()` validates each study with
    [`add_study_data()`](https://andjar.github.io/metawoRld/reference/add_study_data.md)
    and stores it.
6.  **Check** with
    [`validate_world()`](https://andjar.github.io/metawoRld/reference/validate_world.md),
    follow progress with `DataFindR::df_status()` and
    `df_prisma_counts()`.
7.  **Publish** with
    [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md);
    [`use_github_action_quarto()`](https://andjar.github.io/metawoRld/reference/use_github_action_quarto.md)
    makes GitHub rebuild the site on every push.
8.  **Analyse** with
    [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md).

Every LLM result is cached in the project together with the model, token
use, cost and a fingerprint of the prompt, schema and criteria.
Re-running a step only processes new studies or studies whose results
were produced with an outdated setup. Manual decisions and corrections
are never overwritten.

## Without AI

All metawoRld functions work without DataFindR.
[`add_study_template()`](https://andjar.github.io/metawoRld/reference/add_study_template.md)
writes placeholder files for manual entry and
[`validate_study()`](https://andjar.github.io/metawoRld/reference/validate_study.md)
checks them.
