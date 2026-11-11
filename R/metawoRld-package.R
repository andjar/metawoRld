#' metawoRld: structured, validated data for living systematic reviews
#'
#' A metawoRld project is a folder (usually a Git repository) holding the
#' review's settings, a schema describing what is extracted from each study,
#' and one folder per study with `metadata.yml` and `data.csv`.
#'
#' Main functions:
#' * [create_metawoRld()] creates a project with generic, editable templates.
#' * [add_study_data()] validates and stores one study; [add_study_template()]
#'   creates files for manual entry; [validate_study()] / [validate_world()]
#'   check edited files.
#' * [load_metawoRld()] loads everything for analysis.
#' * [generate_website()] builds a Quarto website; [use_github_action_quarto()]
#'   publishes it automatically.
#'
#' The companion package DataFindR uses LLMs to screen studies and extract
#' data into a metawoRld project.
#'
#' @keywords internal
#' @importFrom rlang %||%
"_PACKAGE"
