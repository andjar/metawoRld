# Data packages, file hashes and the project check (SPEC.md sections 8, 10).

.package_files <- c("study.yml", "metadata.yml", "data.csv", "provenance.csv", "verification.csv")

#' SHA-256 hash of a file
#'
#' @param file Path to a file.
#' @return `"sha256:<64 hex digits>"`.
#' @export
hash_file <- function(file) {
  paste0("sha256:", digest::digest(file = file, algo = "sha256"))
}

# Write data/<study_id>/datapackage.json (Frictionless Data Package).
.write_datapackage <- function(proj_path, study_id) {
  dir <- .study_dir(proj_path, study_id)
  schema <- get_schema(proj_path, "extraction")
  cfg <- read_config(proj_path)
  study <- yaml::read_yaml(fs::path(dir, "study.yml"))
  files <- .package_files[fs::file_exists(fs::path(dir, .package_files))]
  text_schema <- function(cols) list(fields = lapply(cols, function(c) list(name = c, type = "string")), missingValues = list(""))
  resources <- lapply(files, function(f) {
    path <- fs::path(dir, f)
    csv <- grepl("\\.csv$", f)
    r <- list(
      name = gsub("[^a-z0-9]+", "-", tolower(fs::path_ext_remove(f))),
      path = f,
      format = if (csv) "csv" else "yaml",
      mediatype = if (csv) "text/csv" else "application/yaml",
      encoding = "utf-8",
      bytes = as.integer(fs::file_size(path)),
      hash = hash_file(path)
    )
    if (f == "data.csv") r$schema <- table_schema(schema)
    if (f == "provenance.csv") r$schema <- text_schema(.prov_cols)
    if (f == "verification.csv") r$schema <- text_schema(.verif_cols)
    r
  })
  lic <- cfg$license %||% "CC-BY-4.0"
  pkg <- list(
    name = study_id,
    title = as.character(cfg$project_name %||% "metawoRld project"),
    profile = "data-package",
    licenses = list(list(name = lic, path = if (identical(lic, "CC-BY-4.0")) "https://creativecommons.org/licenses/by/4.0/")),
    resources = resources,
    metawoRld = list(spec_version = 1L, study_id = study_id, form = "extraction",
                     form_version = as.character(study$form_version %||% schema$`_form`$version))
  )
  pkg$licenses[[1]] <- Filter(Negate(is.null), pkg$licenses[[1]])
  jsonlite::write_json(pkg, fs::path(dir, "datapackage.json"), auto_unbox = TRUE, pretty = TRUE)
  invisible(fs::path(dir, "datapackage.json"))
}

#' Check a whole metawoRld project
#'
#' The single check to run locally and in continuous integration. It checks
#' the project files, every accepted study (schema and link errors, file
#' hashes in `datapackage.json`, provenance, form version, people) and the
#' submissions, records and screening log.
#'
#' Errors mean the data are invalid or were changed outside metawoRld (for
#' example a value edited in `data.csv` without updating provenance and
#' hashes). Warnings point at work left to do.
#'
#' @param path Project root.
#' @param ci Running in CI? If `TRUE`, the R session exits with status 1 when
#'   errors are found, so the CI job fails. Use
#'   `Rscript -e 'metawoRld::check_project(ci = TRUE)'`.
#' @return A tibble of issues (`level`, `where`, `message`), invisibly.
#' @export
check_project <- function(path = ".", ci = FALSE) {
  proj_path <- .check_project(path)
  issues <- list()
  add <- function(level, where, msg) {
    if (length(msg) && length(where)) issues[[length(issues) + 1]] <<- dplyr::tibble(level = level, where = where, message = msg)
  }

  cfg <- tryCatch(read_config(proj_path), error = function(e) { add("error", "_metawoRld.yml", conditionMessage(e)); list() })
  if (!identical(as.integer(cfg$spec_version %||% NA), 1L)) add("error", "_metawoRld.yml", "`spec_version: 1` is missing.")
  schema <- tryCatch(get_schema(proj_path, "extraction"), error = function(e) { add("error", "forms/extraction.yml", conditionMessage(e)); NULL })
  forms <- tryCatch(list_forms(proj_path), error = function(e) { add("error", "forms/", conditionMessage(e)); NULL })
  people <- tryCatch(read_people(proj_path), error = function(e) { add("error", "people.yml", conditionMessage(e)); NULL })
  actors <- people$id %||% character(0)
  if (!is.null(people) && !nrow(people)) add("warning", "people.yml", "No people registered; see add_person().")
  rules <- tryCatch(read_rules(proj_path), error = function(e) { add("error", "_rules.yml", conditionMessage(e)); NULL })

  bad_dirs <- setdiff(fs::path_file(fs::dir_ls(fs::path(proj_path, "data"), type = "directory")), list_studies(proj_path))
  if (length(bad_dirs)) add("error", paste0("data/", bad_dirs), "Folder name is not a valid study id.")

  if (!is.null(schema)) {
    form_major <- .major(schema$`_form`$version)
    for (id in list_studies(proj_path)) {
      where <- paste0("data/", id)
      dir <- .study_dir(proj_path, id)
      add("error", where, .study_errors(proj_path, id, check_links = TRUE))
      add("error", where, .hash_problems(dir))
      s <- tryCatch(.read_study_dir(dir, schema), error = function(e) NULL)
      if (is.null(s)) next
      if (!fs::file_exists(fs::path(dir, "study.yml"))) add("error", where, "study.yml is missing.")
      if (!identical(s$study$study_id, id)) add("error", where, "study.yml: study_id does not match the folder name.")
      fv <- s$study$form_version
      if (is.null(fv)) add("warning", where, "study.yml: form_version is missing.")
      else if (!identical(.major(fv), form_major)) {
        add("warning", where, paste0("Data follow form version ", fv, "; the current extraction form is ", schema$`_form`$version, "."))
      }
      need <- c("_metadata", as.character(s$data$measurement_id))
      miss <- setdiff(need, s$provenance$measurement_id)
      if (length(miss)) add("error", where, paste0("No provenance for: ", paste(utils::head(miss, 10), collapse = ", "),
                                                     if (length(miss) > 10) ", ..."))
      if (!is.null(people)) {
        unknown <- setdiff(unique(s$provenance$person[nzchar(s$provenance$person)]), actors)
        add("warning", where, if (length(unknown)) paste0("provenance.csv: unknown person/tool: ", paste(unknown, collapse = ", ")))
        noperson <- sum(!nzchar(s$provenance$person))
        add("warning", where, if (noperson) paste0("provenance.csv: ", noperson, " row(s) without a person."))
        v <- .read_verification(dir)
        unknown <- setdiff(unique(v$reviewer[nzchar(v$reviewer)]), actors)
        add("warning", where, if (length(unknown)) paste0("verification.csv: unknown reviewer: ", paste(unknown, collapse = ", ")))
      }
    }
  }

  subs <- tryCatch(list_submissions(proj_path), error = function(e) { add("error", "submissions/", conditionMessage(e)); NULL })
  if (!is.null(subs) && nrow(subs)) {
    pend <- subs[subs$status == "pending", , drop = FALSE]
    if (nrow(pend)) add("warning", paste0("submissions/", pend$study_id, "/", pend$extraction_id), "Submission is pending.")
    if (!is.null(people)) {
      unk <- subs[!subs$person %in% actors, , drop = FALSE]
      if (nrow(unk)) add("warning", paste0("submissions/", unk$study_id, "/", unk$extraction_id), paste0("Unknown person/tool: ", unk$person))
    }
  }

  rec <- tryCatch(read_records(proj_path), error = function(e) { add("error", "records.csv", conditionMessage(e)); NULL })
  if (!is.null(rec) && nrow(rec)) {
    dups <- unique(rec$record_id[duplicated(rec$record_id)])
    add("error", "records.csv", if (length(dups)) paste0("Duplicate record_id: ", paste(dups, collapse = ", ")))
    linked <- setdiff(unique(rec$study_id[nzchar(rec$study_id)]), c(list_studies(proj_path), subs$study_id))
    add("warning", "records.csv", if (length(linked)) paste0("Linked to unknown studies: ", paste(linked, collapse = ", ")))
    bad <- setdiff(unique(rec$duplicate_of[nzchar(rec$duplicate_of)]), rec$record_id)
    add("error", "records.csv", if (length(bad)) paste0("duplicate_of refers to unknown records: ", paste(bad, collapse = ", ")))
  }
  scr <- tryCatch(.read_char_csv(fs::path(proj_path, "screening.csv"), .screening_cols), error = function(e) { add("error", "screening.csv", conditionMessage(e)); NULL })
  if (!is.null(scr) && nrow(scr)) {
    bad <- setdiff(unique(scr$record_id), rec$record_id)
    add("error", "screening.csv", if (length(bad)) paste0("Unknown records: ", paste(utils::head(bad, 10), collapse = ", ")))
    if (!is.null(people)) {
      unk <- setdiff(unique(scr$person), actors)
      add("warning", "screening.csv", if (length(unk)) paste0("Unknown person/tool: ", paste(unk, collapse = ", ")))
    }
    conf <- tryCatch(screening_status(proj_path), error = function(e) NULL)
    n_conf <- sum(conf$outcome == "conflict")
    add("warning", "screening.csv", if (n_conf) paste0(n_conf, " screening conflict(s) without a consensus decision."))
  }

  out <- dplyr::bind_rows(dplyr::tibble(level = character(0), where = character(0), message = character(0)), issues)
  n_err <- sum(out$level == "error")
  n_warn <- sum(out$level == "warning")
  if (nrow(out)) {
    lines <- paste0(ifelse(out$level == "error", "x ", "! "), out$where, ": ", out$message)
    message(paste(lines, collapse = "\n"))
  }
  message(sprintf("check_project(): %d study(ies), %d error(s), %d warning(s).",
                  length(list_studies(proj_path)), n_err, n_warn))
  if (ci && n_err > 0) quit(save = "no", status = 1)
  invisible(out)
}

#' Add a GitHub Actions workflow that runs check_project()
#'
#' The workflow runs [check_project()] on every push and pull request, so
#' invalid data or files edited outside metawoRld fail the check. The command
#' is a plain `Rscript` call and works in any CI system.
#'
#' @param path Project root.
#' @param overwrite Replace an existing workflow? Defaults to `FALSE`.
#' @return The workflow file, invisibly.
#' @export
use_github_action_check <- function(path = ".", overwrite = FALSE) {
  proj_path <- .check_project(path)
  wf <- fs::path(proj_path, ".github", "workflows", "metawoRld-check.yml")
  if (fs::file_exists(wf) && !overwrite) rlang::abort(paste0(wf, " already exists. Use `overwrite = TRUE` to replace it."))
  fs::dir_create(fs::path_dir(wf))
  fs::file_copy(system.file("github", "metawoRld-check.yml", package = "metawoRld"), wf, overwrite = TRUE)
  rlang::inform(paste0("Created ", wf))
  invisible(wf)
}

# ---- internals ---------------------------------------------------------------

.major <- function(version) sub("\\..*$", "", as.character(version %||% ""))

.hash_problems <- function(dir) {
  dp <- fs::path(dir, "datapackage.json")
  if (!fs::file_exists(dp)) return("datapackage.json is missing.")
  pkg <- tryCatch(jsonlite::read_json(dp), error = function(e) NULL)
  if (is.null(pkg)) return("datapackage.json cannot be read.")
  listed <- vapply(pkg$resources, function(r) as.character(r$path), character(1))
  out <- character(0)
  for (r in pkg$resources) {
    f <- fs::path(dir, r$path)
    if (!fs::file_exists(f)) { out <- c(out, paste0(r$path, " is listed in datapackage.json but missing.")); next }
    if (!identical(hash_file(f), r$hash)) {
      out <- c(out, paste0(r$path, " changed outside metawoRld (hash differs from datapackage.json)."))
    }
  }
  extra <- setdiff(.package_files[fs::file_exists(fs::path(dir, .package_files))], listed)
  c(out, if (length(extra)) paste0(extra, " is not listed in datapackage.json."))
}
