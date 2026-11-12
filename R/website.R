#' Generate (and render) the project website
#'
#' Builds a living meta-analysis dashboard: an overview page (studies,
#' verification progress, summary of findings), one dashboard page per
#' meta-analysis in `_analyses.yml` (see [generate_dashboard()]), a results
#' page per variable, plus the study list, the full data and one page per
#' study (pages of removed studies are deleted). Renders into `_site/`.
#'
#' Existing projects: use `update_templates = TRUE` once to get the new
#' `_quarto.yml` and overview page.
#'
#' @param path Project root.
#' @param render Render with Quarto? Requires the `quarto` R package and the
#'   Quarto CLI. Defaults to `TRUE`.
#' @param update_templates Overwrite `_quarto.yml`, `index.qmd`,
#'   `studies.qmd`, `data.qmd`, `styles.css` and `study.qmd.tmpl` with the
#'   current package versions? Defaults to `FALSE` so your edits are kept;
#'   missing files are always added.
#' @return The project path, invisibly.
#' @export
generate_website <- function(path = ".", render = TRUE, update_templates = FALSE) {
  proj_path <- .check_project(path)
  .write_website_templates(proj_path, overwrite = update_templates)
  generate_study_webpage(proj_path)
  generate_dashboard(proj_path)
  if (render) {
    rlang::check_installed("quarto", reason = "to render the website.")
    quarto::quarto_render(input = proj_path, as_job = FALSE)
  }
  invisible(proj_path)
}

#' Generate one web page per study
#'
#' Fills `study.qmd.tmpl` for each study and writes `study/<study>.qmd`.
#' Pages for studies that were removed are deleted.
#'
#' @param path Project root.
#' @return Paths of the generated pages, invisibly.
#' @export
generate_study_webpage <- function(path = ".") {
  proj_path <- .check_project(path)
  tmpl_file <- fs::path(proj_path, "study.qmd.tmpl")
  if (!fs::file_exists(tmpl_file)) .write_website_templates(proj_path, overwrite = FALSE)
  template <- readr::read_file(tmpl_file)

  out_dir <- fs::path(proj_path, "study")
  fs::dir_create(out_dir)
  world <- load_metawoRld(proj_path, verbose = FALSE)

  written <- character(0)
  for (id in names(world$studies_metadata)) {
    meta <- world$studies_metadata[[id]]
    dir_name <- sanitize_id(id)
    page <- glue::glue_data(
      list(
        front_matter = trimws(yaml::as.yaml(list(title = as.character(meta$title %||% id)))),
        summary = .md_summary(meta),
        sections = .md_sections(meta),
        data_path = paste0("../data/", dir_name, "/data.csv")
      ),
      template, .open = "{{", .close = "}}"
    )
    file <- fs::path(out_dir, paste0(dir_name, ".qmd"))
    readr::write_file(paste0(page, "\n"), file)
    written <- c(written, file)
  }

  stale <- setdiff(fs::dir_ls(out_dir, glob = "*.qmd"), written)
  if (length(stale)) fs::file_delete(stale)
  invisible(written)
}

#' Add a GitHub Actions workflow that publishes the website
#'
#' Writes `.github/workflows/metawoRld-website.yml`. On every push to the
#' default branch (and on manual dispatch) it installs metawoRld, renders the
#' site and deploys `_site/` to GitHub Pages. In the repository settings, set
#' *Pages > Source* to *GitHub Actions*.
#'
#' @param path Project root.
#' @param overwrite Replace an existing workflow file? Defaults to `FALSE`.
#' @return The workflow path, invisibly.
#' @export
use_github_action_quarto <- function(path = ".", overwrite = FALSE) {
  proj_path <- .check_project(path)
  wf <- fs::path(proj_path, ".github", "workflows", "metawoRld-website.yml")
  if (fs::file_exists(wf) && !overwrite) {
    rlang::abort(paste0(wf, " already exists. Use `overwrite = TRUE` to replace it."))
  }
  fs::dir_create(fs::path_dir(wf))
  fs::file_copy(system.file("github", "metawoRld-website.yml", package = "metawoRld"), wf, overwrite = TRUE)
  rlang::inform(c(paste0("Created ", wf), "i" = "Set Settings > Pages > Source to 'GitHub Actions'."))
  invisible(wf)
}

# ---- internals ---------------------------------------------------------------

.write_website_templates <- function(proj_path, overwrite = FALSE) {
  cfg <- read_config(proj_path)
  vals <- list(
    project_name = cfg$project_name %||% "metawoRld project",
    project_description = cfg$project_description %||% "",
    date = as.character(Sys.Date())
  )
  src <- fs::dir_ls(system.file("website", package = "metawoRld"))
  for (f in src) {
    dest <- fs::path(proj_path, fs::path_file(f))
    if (fs::file_exists(dest) && !overwrite) next
    if (grepl("\\.(qmd|yml)$", f)) {
      txt <- glue::glue_data(vals, readr::read_file(f), .open = "{{", .close = "}}")
      readr::write_file(paste0(txt, "\n"), dest)
    } else {
      fs::file_copy(f, dest, overwrite = TRUE)
    }
  }
  invisible(proj_path)
}

.md_escape <- function(x) gsub("\\|", "\\\\|", gsub("[\r\n]+", " ", x))

.is_obj_array <- function(v) is.list(v) && length(v) > 0 && is.null(names(v)) && all(vapply(v, is.list, logical(1)))

.md_summary <- function(meta) {
  skip <- c("title", "abstract", "study_id")
  lines <- character(0)
  for (f in setdiff(names(meta), skip)) {
    v <- meta[[f]]
    if (.is_obj_array(v) || is.null(v)) next
    val <- paste(unlist(v), collapse = ", ")
    if (f == "doi" && nzchar(val)) val <- paste0("[", val, "](https://doi.org/", val, ")")
    if (f == "pmid" && nzchar(val)) val <- paste0("[", val, "](https://pubmed.ncbi.nlm.nih.gov/", val, "/)")
    lines <- c(lines, paste0("- **", gsub("_", " ", f), ":** ", .md_escape(val)))
  }
  if (!is.null(meta$abstract)) lines <- c(lines, "", "### Abstract", "", .md_escape(meta$abstract))
  paste(lines, collapse = "\n")
}

.md_sections <- function(meta) {
  out <- character(0)
  for (f in names(meta)) {
    v <- meta[[f]]
    if (!.is_obj_array(v)) next
    df <- .rows_to_df(v)
    df <- df[, vapply(df, function(col) !is.list(col), logical(1)), drop = FALSE]
    if (!ncol(df)) next
    header <- paste0("| ", paste(names(df), collapse = " | "), " |")
    sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
    rows <- apply(df, 1, function(r) paste0("| ", paste(.md_escape(ifelse(is.na(r), "", r)), collapse = " | "), " |"))
    out <- c(out, paste0("### ", tools::toTitleCase(gsub("_", " ", f))), "", header, sep, rows, "")
  }
  paste(out, collapse = "\n")
}
