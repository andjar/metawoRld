# metawoRld: structured, validated data for living systematic reviews

A metawoRld project is a folder (usually a Git repository) holding the
review's settings, forms describing what is extracted, a record registry
and one folder per accepted study. The format is defined in `SPEC.md`.

## Details

Main functions:

- [`create_metawoRld()`](https://andjar.github.io/metawoRld/reference/create_metawoRld.md)
  creates a project;
  [`add_person()`](https://andjar.github.io/metawoRld/reference/read_people.md)
  registers the people and tools that contribute.

- [`add_search()`](https://andjar.github.io/metawoRld/reference/add_search.md),
  [`add_records()`](https://andjar.github.io/metawoRld/reference/read_records.md),
  [`add_screening()`](https://andjar.github.io/metawoRld/reference/add_screening.md)
  and
  [`prisma_counts()`](https://andjar.github.io/metawoRld/reference/prisma_counts.md)
  keep the search and screening log.

- [`submit_study()`](https://andjar.github.io/metawoRld/reference/submit_study.md)
  is the one door for data from any channel (entry form, CSV, DataFindR,
  ...).
  [`accept_submission()`](https://andjar.github.io/metawoRld/reference/accept_submission.md),
  [`compare_extractions()`](https://andjar.github.io/metawoRld/reference/compare_extractions.md)
  and
  [`reconcile()`](https://andjar.github.io/metawoRld/reference/reconcile.md)
  turn submissions into accepted data with provenance.

- [`review_values()`](https://andjar.github.io/metawoRld/reference/review_values.md)
  and
  [`set_verification()`](https://andjar.github.io/metawoRld/reference/set_verification.md)
  record human verification.

- [`check_project()`](https://andjar.github.io/metawoRld/reference/check_project.md)
  checks everything (schemas, hashes, provenance), locally or in CI.

- [`load_metawoRld()`](https://andjar.github.io/metawoRld/reference/load_metawoRld.md),
  [`run_all_meta_analyses()`](https://andjar.github.io/metawoRld/reference/run_all_meta_analyses.md)
  and
  [`generate_website()`](https://andjar.github.io/metawoRld/reference/generate_website.md)
  analyse and publish.

## See also

Useful links:

- <https://andjar.github.io/metawoRld/>

- <https://github.com/andjar/metawoRld>

- Report bugs at <https://github.com/andjar/metawoRld/issues>

## Author

**Maintainer**: Anders Hagen Jarmund <anders.h.jarmund@ntnu.no>
([ORCID](https://orcid.org/0000-0002-3923-1637))
