#' metawoRld: A Framework for Managing Data in Living Systematic Reviews
#'
#' @description
#' The `metawoRld` package provides a comprehensive suite of tools to establish
#' and maintain a structured, version-controlled environment for living systematic
#' reviews. It emphasizes FAIR principles (Findable, Accessible, Interoperable,
#' and Reusable) for review data.
#'
#' Key features include:
#' \\itemize{
#'   \\item **Project Initialization**: Create a standardized project structure with `create_metawoRld()`, including configuration files, data storage directories, and optional Git repository setup.
#'   \\item **Schema-Driven Data Management**: Define and enforce data structures for study metadata (YAML) and quantitative data (CSV) using a project-specific schema.
#'   \\item **Data Addition and Validation**: Add new study data with `add_study_data()`, which validates entries against the schema. Use `add_study_template()` to create placeholder files for manual data entry, and `validate_study()` or `validate_world()` to check data integrity.
#'   \\item **Data Aggregation**: Load and consolidate data from all studies in a project into convenient R objects using `load_metawoRld()`.
#'   \\item **Web Presence**: Generate a Quarto-based project website with `generate_study_webpage()` to display review progress, data summaries, and individual study details.
#'   \\item **Integration with DataFindR**: Designed to seamlessly work with the `DataFindR` package, which uses AI to help extract and structure data from scientific literature for population into a `metawoRld` project.
#' }
#'
#' By promoting a systematic and reproducible approach to data management,
#' `metawoRld` aims to streamline the living review process, enhance collaboration,
#' and facilitate timely updates as new evidence emerges.
#'
#' @docType package
#' @name metawoRld-package
#' @aliases metawoRld
#' @seealso
#' Useful links:
#' \\itemize{
#'   \\item \\url{https://github.com/andjar/metawoRld} (Development repository)
#'   \\item \\code{\\link{create_metawoRld}} for starting a new project.
#'   \\item \\code{\\link{add_study_data}} for adding data to a project.
#'   \\item \\code{\\link{load_metawoRld}} for loading all project data.
#'   \\item \\code{\\link{generate_study_webpage}} for creating a project website.
#' }
NULL

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom data.table data.table
## usethis namespace: end
NULL
