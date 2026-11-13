#' Create a new metawoRld project
#'
#' Creates the project folder laid out as described in `SPEC.md`: settings
#' (`_metawoRld.yml`), forms, prompts, rules, the people list, the search
#' and screening log, `submissions/` and `data/`, a README, website templates
#' and (optionally) a Git repository.
#'
#' @param path Folder to create. Must not exist yet.
#' @param project_name Short name of the review.
#' @param project_description One or two sentences describing the scope.
#' @param inclusion_criteria,exclusion_criteria Character vectors of criteria
#'   used for screening.
#' @param git_init Initialise a Git repository? Defaults to `TRUE`.
#' @param website Add Quarto website templates? Defaults to `TRUE`.
#' @param license Data license, stored in `_metawoRld.yml` and in every
#'   study's `datapackage.json`. Defaults to `"CC-BY-4.0"`.
#' @param ... Further named values stored in `_metawoRld.yml`
#'   (e.g. `search_query = "..."`).
#'
#' @details
#' The project contains:
#' * `_metawoRld.yml`: project name, description, criteria and other settings.
#' * `_metawoRld.yml`: settings, criteria and `spec_version`.
#' * `forms/extraction.yml` (what is extracted) and `forms/screening.yml`.
#'   Edit these to fit your review.
#' * `prompts/`: LLM prompts used by DataFindR.
#' * `_rules.yml`, `_analyses.yml`: vocabulary, value checks and planned
#'   analyses.
#' * `people.yml`, `searches.yml`, `records.csv`, `screening.csv`: who works on
#'   the review, and the search and screening log.
#' * `submissions/`: staged extractions; `data/<study_id>/`: accepted data.
#'
#' @return The project path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' create_metawoRld(
#'   file.path(tempdir(), "my_review"),
#'   project_name = "Biomarker X in pregnancy",
#'   project_description = "Living review of maternal serum levels of X.",
#'   inclusion_criteria = c("Human pregnancy", "Serum or plasma levels of X reported"),
#'   exclusion_criteria = c("Animal study", "Review or case report")
#' )
#' }
create_metawoRld <- function(path,
                             project_name,
                             project_description,
                             inclusion_criteria = character(0),
                             exclusion_criteria = character(0),
                             git_init = TRUE,
                             website = TRUE,
                             license = "CC-BY-4.0",
                             ...) {
  for (arg in c("path", "project_name", "project_description")) {
    val <- get(arg)
    if (!rlang::is_scalar_character(val) || !nzchar(val)) {
      rlang::abort(paste0("`", arg, "` must be a single non-empty string."))
    }
  }
  if (!is.character(inclusion_criteria) || !is.character(exclusion_criteria)) {
    rlang::abort("`inclusion_criteria` and `exclusion_criteria` must be character vectors.")
  }
  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))))) {
    rlang::abort("Additional arguments in `...` must be named.")
  }

  proj_path <- fs::path_norm(path)
  if (fs::file_exists(proj_path)) rlang::abort(paste0("Path already exists: ", proj_path))

  fs::dir_create(fs::path(proj_path, c("data", "submissions")))
  for (d in c("data", "submissions")) file.create(fs::path(proj_path, d, ".gitkeep"))

  config <- c(
    list(
      spec_version = 1L,
      project_name = project_name,
      project_description = project_description,
      creation_date = as.character(Sys.Date()),
      metawoRld_version = as.character(utils::packageVersion("metawoRld")),
      inclusion_criteria = as.list(inclusion_criteria),
      exclusion_criteria = as.list(exclusion_criteria),
      license = license
    ),
    extra
  )
  yaml::write_yaml(config, fs::path(proj_path, "_metawoRld.yml"))

  use_templates(proj_path)

  yaml::write_yaml(list(people = list(), tools = list()), fs::path(proj_path, "people.yml"))
  yaml::write_yaml(list(searches = list()), fs::path(proj_path, "searches.yml"))
  writeLines(paste(.records_cols, collapse = ","), fs::path(proj_path, "records.csv"))
  writeLines(paste(.screening_cols, collapse = ","), fs::path(proj_path, "screening.csv"))
  writeLines(c("# News", "", paste0("## Unreleased"), "", paste0("* Project created on ", Sys.Date(), ".")),
             fs::path(proj_path, "NEWS.md"))

  writeLines(c(
    paste("#", project_name), "",
    project_description, "",
    paste0("Created with the metawoRld R package on ", Sys.Date(), ". ",
           "The data are licensed under ", license, "."), "",
    "* `_metawoRld.yml`: project settings and screening criteria",
    "* `forms/extraction.yml`: what is extracted from each study",
    "* `people.yml`, `searches.yml`, `records.csv`, `screening.csv`: team, searches and screening",
    "* `submissions/`: extractions waiting to be accepted",
    "* `data/<study_id>/`: accepted data (`metadata.yml`, `data.csv`), with `provenance.csv`,",
    "  `verification.csv` and `datapackage.json` (file hashes)",
    "",
    "Check the project with `metawoRld::check_project()`."
  ), fs::path(proj_path, "README.md"))

  writeLines(c(
    ".Rproj.user/", ".Rhistory", ".RData", ".Ruserdata", "",
    "# Rendered website and Quarto cache",
    "_site/", ".quarto/", "study/*.html", "*_files/", "",
    "# Generated by generate_website()",
    "figures/", "analyses.qmd", "results.qmd", "",
    "# Full texts are usually copyrighted; keep them out of version control",
    "fulltext/", "*.pdf", "",
    ".DS_Store", "*~"
  ), fs::path(proj_path, ".gitignore"))

  if (website) {
    tryCatch(
      .write_website_templates(proj_path, overwrite = FALSE),
      error = function(e) rlang::warn(paste("Could not add website templates:", conditionMessage(e)))
    )
  }

  if (isTRUE(git_init)) {
    tryCatch(
      git2r::init(proj_path),
      error = function(e) rlang::warn(paste("Could not initialise Git repository:", conditionMessage(e)))
    )
  }

  rlang::inform(paste0("Created metawoRld project at ", proj_path))
  invisible(proj_path)
}

#' Add or update the forms, prompts and rules templates in a project
#'
#' Copies the default `forms/`, `prompts/`, `_rules.yml` and `_analyses.yml`
#' into the project.
#'
#' @param path Project root.
#' @param overwrite Replace files that already exist? Defaults to `FALSE`, so
#'   your edits are kept.
#' @return Paths of the files that were written, invisibly.
#' @export
use_templates <- function(path = ".", overwrite = FALSE) {
  proj_path <- .check_project(path)
  src <- system.file("templates", package = "metawoRld")
  files <- fs::dir_ls(src, recurse = TRUE, type = "file")
  written <- character(0)
  for (f in files) {
    dest <- fs::path(proj_path, fs::path_rel(f, src))
    if (overwrite || !fs::file_exists(dest)) {
      fs::dir_create(fs::path_dir(dest))
      fs::file_copy(f, dest, overwrite = TRUE)
      written <- c(written, dest)
    }
  }
  invisible(written)
}

#' Read the project configuration
#'
#' @param path Project root.
#' @return The contents of `_metawoRld.yml` as a list. `inclusion_criteria`
#'   and `exclusion_criteria` are always character vectors.
#' @export
read_config <- function(path = ".") {
  proj_path <- .check_project(path)
  cfg <- yaml::read_yaml(fs::path(proj_path, "_metawoRld.yml"))
  cfg$inclusion_criteria <- as.character(unlist(cfg$inclusion_criteria))
  cfg$exclusion_criteria <- as.character(unlist(cfg$exclusion_criteria))
  cfg
}
