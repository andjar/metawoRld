#' metawoRld: structured, validated data for living systematic reviews
#'
#' A metawoRld project is a folder (usually a Git repository) holding the
#' review's settings, forms describing what is extracted, a record registry
#' and one folder per accepted study. The format is defined in `SPEC.md`.
#'
#' Main functions:
#' * [create_metawoRld()] creates a project; [add_person()] registers the
#'   people and tools that contribute.
#' * [add_search()], [add_records()], [add_screening()] and [prisma_counts()]
#'   keep the search and screening log.
#' * [submit_study()] is the one door for data from any channel (entry form,
#'   CSV, DataFindR, ...). [accept_submission()], [compare_extractions()] and
#'   [reconcile()] turn submissions into accepted data with provenance.
#' * [review_values()] and [set_verification()] record human verification.
#' * [check_project()] checks everything (schemas, hashes, provenance), locally
#'   or in CI.
#' * [load_metawoRld()], [run_all_meta_analyses()] and [generate_website()]
#'   analyse and publish.
#'
#' @keywords internal
#' @importFrom rlang %||%
"_PACKAGE"
