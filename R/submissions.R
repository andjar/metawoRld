# Submissions: the one door into a project (SPEC.md section 9).

.sub_status <- c("pending", "accepted", "rejected", "superseded")

#' Submit an extraction of a study
#'
#' The one door for data from any channel (the entry form, CSV files,
#' DataFindR, scripts). The extraction is checked against the extraction form
#' (types, required fields, allowed values, links) and, if valid, written to
#' `submissions/<study_id>/<extraction_id>/` with a gate report. Value checks
#' from `_rules.yml` are recorded as warnings. Nothing is written to `data/`
#' until the submission is accepted with [accept_submission()] or
#' [reconcile()].
#'
#' @param path Project root.
#' @param study_id Study id, e.g. `"hansen2019"` (see [suggest_study_id()]).
#' @param metadata Named list matching the form's `metadata`, or the path to
#'   a `metadata.yml` file.
#' @param data Data frame of data points, or the path to a `data.csv` file.
#' @param person Id of the person or tool (from `people.yml`) who made the
#'   extraction.
#' @param channel How the data were produced, e.g. `"entry_form"`,
#'   `"csv_import"`, `"datafindr"`, `"script"`.
#' @param tool Producing software as `name@version`. Defaults to metawoRld's
#'   version.
#' @param model LLM model, if one was used.
#' @param source_sha256 Hash of the full text the values were read from (see
#'   [hash_file()]).
#' @param reports Reports of the study (list of lists with `doi`, `pmid`,
#'   `role`). Defaults to `metadata$doi`/`metadata$pmid`.
#' @param provenance Optional data frame with per-data-point provenance
#'   (`measurement_id` and any of the provenance columns), e.g. a model per
#'   value.
#' @param blind Hide this extraction from other extractors until reconciled
#'   (used by the entry form for dual extraction)?
#' @return The extraction id, invisibly. Validation errors abort with class
#'   `metawoRld_validation_error`; nothing is written then.
#' @export
submit_study <- function(path = ".", study_id, metadata, data = data.frame(), person,
                         channel = "script", tool = NULL, model = NULL, source_sha256 = NULL,
                         reports = NULL, provenance = NULL, blind = FALSE) {
  proj_path <- .check_project(path)
  check_study_id(study_id)
  .check_actor(proj_path, person)
  schema <- get_schema(proj_path, "extraction")
  if (rlang::is_scalar_character(metadata)) metadata <- yaml::read_yaml(metadata)
  if (rlang::is_scalar_character(data)) data <- .read_data_csv(data, schema)
  if (!is.list(metadata) || is.data.frame(metadata)) rlang::abort("`metadata` must be a named list or a path to a YAML file.")
  data <- .as_data_points(data)
  metadata$study_id <- study_id
  metadata <- .clean_for_yaml(metadata)

  errs <- .validate_parts(metadata, data, schema)
  if (length(errs)) .abort_validation(study_id, errs)
  warns <- .gate_warnings(proj_path, metadata, data)

  created <- Sys.time()
  ext_id <- .new_extraction_id(proj_path, study_id, person, created)
  dir <- fs::path(proj_path, "submissions", study_id, ext_id)
  fs::dir_create(dir)
  sub <- list(
    extraction_id = ext_id, study_id = study_id, form = "extraction",
    form_version = as.character(schema$`_form`$version),
    person = person, channel = channel, tool = tool %||% .tool_string(),
    model = model, source_sha256 = source_sha256,
    created = format(created, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = "pending", blind = isTRUE(blind),
    gate = list(checked = .now(), errors = list(), warnings = as.list(warns))
  )
  yaml::write_yaml(Filter(Negate(is.null), sub), fs::path(dir, "submission.yml"))
  yaml::write_yaml(list(study_id = study_id, reports = .clean_reports(reports %||% .reports_from_metadata(metadata))),
                   fs::path(dir, "study.yml"))
  yaml::write_yaml(metadata, fs::path(dir, "metadata.yml"))
  .write_data_csv(data, schema, fs::path(dir, "data.csv"))
  if (is.data.frame(provenance) && nrow(provenance)) .write_char_csv(provenance, fs::path(dir, "provenance.csv"))

  rlang::inform(c(paste0("Submitted ", study_id, " (", ext_id, "): ", nrow(data), " data point(s)."),
                  if (length(warns)) c("!" = paste0(length(warns), " value warning(s); see check_submission()."))))
  invisible(ext_id)
}

#' Check a submission
#'
#' Re-runs the intake gate on a submission folder, e.g. one written by a
#' program in another language, and records the result in its
#' `submission.yml`. A pending submission with errors is marked `rejected`.
#'
#' @param path Project root.
#' @param study_id,extraction_id The submission.
#' @return The gate report (`errors`, `warnings`), invisibly.
#' @export
check_submission <- function(path = ".", study_id, extraction_id) {
  proj_path <- .check_project(path)
  dir <- .submission_dir(proj_path, study_id, extraction_id)
  schema <- get_schema(proj_path, "extraction")
  s <- tryCatch(.read_submission_dir(dir, schema), error = function(e) e)
  sub_file <- fs::path(dir, "submission.yml")
  sub <- if (fs::file_exists(sub_file)) yaml::read_yaml(sub_file) else list()
  if (inherits(s, "error")) {
    errs <- conditionMessage(s); warns <- character(0)
  } else {
    errs <- c(.validate_parts(s$metadata, s$data, schema), .submission_header_errors(proj_path, sub, study_id, extraction_id))
    if (!identical(as.character(s$metadata$study_id %||% study_id), study_id)) errs <- c(errs, "metadata.yml: study_id does not match the folder.")
    warns <- .gate_warnings(proj_path, s$metadata, s$data)
  }
  sub$gate <- list(checked = .now(), errors = as.list(errs), warnings = as.list(warns))
  if (length(errs) && identical(sub$status %||% "pending", "pending")) sub$status <- "rejected"
  if (fs::dir_exists(dir)) yaml::write_yaml(sub, sub_file)
  out <- list(errors = errs, warnings = warns)
  if (length(errs)) rlang::warn(c(paste0("Submission ", extraction_id, " has ", length(errs), " error(s):"), rlang::set_names(utils::head(errs, 20), "x")))
  invisible(out)
}

#' List submissions
#'
#' @param path Project root.
#' @param study_id Only this study. Defaults to all.
#' @param status Only these statuses (`pending`, `accepted`, `rejected`,
#'   `superseded`). Defaults to all.
#' @return A tibble with one row per submission.
#' @export
list_submissions <- function(path = ".", study_id = NULL, status = NULL) {
  proj_path <- .check_project(path)
  empty <- dplyr::tibble(study_id = character(0), extraction_id = character(0), person = character(0),
                         channel = character(0), tool = character(0), form_version = character(0),
                         created = character(0), status = character(0), blind = logical(0),
                         n_data_points = integer(0), n_errors = integer(0), n_warnings = integer(0))
  root <- fs::path(proj_path, "submissions")
  if (!fs::dir_exists(root)) return(empty)
  studies <- study_id %||% fs::path_file(fs::dir_ls(root, type = "directory"))
  rows <- list()
  for (sid in studies) {
    sdir <- fs::path(root, sid)
    if (!fs::dir_exists(sdir)) next
    for (d in fs::dir_ls(sdir, type = "directory")) {
      f <- fs::path(d, "submission.yml")
      s <- if (fs::file_exists(f)) yaml::read_yaml(f) else list()
      dc <- fs::path(d, "data.csv")
      n <- if (fs::file_exists(dc)) max(0L, length(readLines(dc, warn = FALSE)) - 1L) else 0L
      rows[[length(rows) + 1]] <- dplyr::tibble(
        study_id = sid, extraction_id = fs::path_file(d),
        person = as.character(s$person %||% NA), channel = as.character(s$channel %||% NA),
        tool = as.character(s$tool %||% NA), form_version = as.character(s$form_version %||% NA),
        created = as.character(s$created %||% NA), status = as.character(s$status %||% "pending"),
        blind = isTRUE(s$blind), n_data_points = as.integer(n),
        n_errors = length(s$gate$errors), n_warnings = length(s$gate$warnings)
      )
    }
  }
  out <- dplyr::bind_rows(empty, rows)
  if (!is.null(status)) out <- out[out$status %in% status, , drop = FALSE]
  out[order(out$study_id, out$created), , drop = FALSE]
}

#' Read a submission
#'
#' @inheritParams check_submission
#' @return A list with `submission`, `study`, `metadata`, `data` and
#'   `provenance`.
#' @export
read_submission <- function(path = ".", study_id, extraction_id) {
  proj_path <- .check_project(path)
  .read_submission_dir(.submission_dir(proj_path, study_id, extraction_id), get_schema(proj_path, "extraction"))
}

#' Accept a submission
#'
#' Writes the submission to `data/<study_id>/` with provenance pointing to the
#' extraction, marks it `accepted` and marks earlier accepted submissions of
#' the study `superseded`. If the study already has data, they are replaced;
#' verification of unchanged data points is kept. Records whose DOI or PMID
#' match the study's reports are linked to the study.
#'
#' For dual extraction, use [compare_extractions()] and [reconcile()]
#' instead.
#'
#' @inheritParams check_submission
#' @param person Id of the person accepting (recorded in `submission.yml`).
#'   Defaults to the `metawoRld.person` option.
#' @return The study folder, invisibly.
#' @export
accept_submission <- function(path = ".", study_id, extraction_id, person = getOption("metawoRld.person")) {
  proj_path <- .check_project(path)
  if (!is.null(person)) .check_actor(proj_path, person)
  schema <- get_schema(proj_path, "extraction")
  s <- .read_submission_dir(.submission_dir(proj_path, study_id, extraction_id), schema)
  if (!identical(s$submission$status %||% "pending", "pending")) {
    rlang::abort(paste0("Submission ", extraction_id, " is ", s$submission$status, ", not pending."))
  }
  errs <- .validate_parts(s$metadata, s$data, schema)
  if (length(errs)) .abort_validation(study_id, errs)
  prov <- if (identical(s$submission$kind, "amendment")) {
    # keep the provenance of everything the amendment did not change
    sb <- s$submission
    list(extraction_id = sb$extraction_id, person = sb$person, channel = sb$channel, tool = sb$tool %||% "",
         model = sb$model %||% "", source_sha256 = sb$source_sha256 %||% "", timestamp = sb$created)
  } else {
    .submission_provenance(s, as.character(c("_metadata", s$data$measurement_id)))
  }
  dir <- add_study_data(proj_path, study_id, s$metadata, s$data, reports = s$study$reports,
                        provenance = prov, overwrite = fs::dir_exists(.study_dir(proj_path, study_id)))
  .set_submission_status(proj_path, study_id, extraction_id, person, "accepted")
  .link_reports(proj_path, study_id, s$study$reports)
  rlang::inform(paste0("Accepted ", extraction_id, " into data/", study_id, "."))
  invisible(dir)
}

#' Reject a submission
#'
#' @inheritParams accept_submission
#' @param reason Why it was rejected.
#' @return `NULL`, invisibly.
#' @export
reject_submission <- function(path = ".", study_id, extraction_id, reason = "", person = getOption("metawoRld.person")) {
  proj_path <- .check_project(path)
  dir <- .submission_dir(proj_path, study_id, extraction_id)
  f <- fs::path(dir, "submission.yml")
  sub <- yaml::read_yaml(f)
  sub$status <- "rejected"
  sub$rejected <- list(by = person, at = .now(), reason = reason)
  yaml::write_yaml(sub, f)
  invisible(NULL)
}

#' Compare extractions of the same study
#'
#' Lines up the data points of two or more extractions by their identifying
#' fields (by default the form's `_form$entry` rows, columns and context,
#' e.g. variable, group, method, time point and statistic type) and compares
#' every value; metadata are compared field by field. Extractors should use
#' the same group and method ids for data points to line up.
#'
#' @param path Project root.
#' @param study_id Study id.
#' @param extraction_ids Extractions to compare. Defaults to all pending
#'   submissions of the study.
#' @return A tibble with `part` (`metadata` or `data`), `key` (the data
#'   point's identifying values, or the top-level metadata field), `field`
#'   (`(row)` for presence of a data point), one column per extraction and
#'   `agree`.
#' @export
compare_extractions <- function(path = ".", study_id, extraction_ids = NULL) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  ext <- .load_extractions(proj_path, study_id, extraction_ids, schema)
  .compare(ext, schema)
}

#' Reconcile two or more extractions
#'
#' Combines extractions of the same study into the accepted data. Values on
#' which all extractions agree are taken as they are; disagreements must be
#' resolved with `choices` (or taken from `base` with
#' `unresolved = "base"`). `provenance.csv` records, for each data point,
#' which extraction it came from and which other extractions agreed with it;
#' values set by the arbiter are attributed to them with channel
#' `reconcile`.
#'
#' @inheritParams compare_extractions
#' @param base Extraction whose values are used where nothing else is chosen.
#'   Defaults to the first one.
#' @param choices Data frame resolving disagreements, with columns `part`,
#'   `key`, `field` (as in [compare_extractions()]) and either `from` (an
#'   extraction id to take the value from) or `value` (the value to use).
#'   For `field = "(row)"`, `from` adds or keeps the data point from that
#'   extraction and `value = "drop"` removes it. For metadata, `from` takes
#'   the whole top-level field (`key`) from that extraction.
#' @param person Id of the arbiter.
#' @param unresolved What to do with disagreements not covered by `choices`:
#'   `"error"` (default) or `"base"`.
#' @return The study folder, invisibly.
#' @export
reconcile <- function(path = ".", study_id, extraction_ids = NULL, base = NULL, choices = NULL,
                      person, unresolved = c("error", "base")) {
  proj_path <- .check_project(path)
  unresolved <- match.arg(unresolved)
  .check_actor(proj_path, person)
  schema <- get_schema(proj_path, "extraction")
  ext <- .load_extractions(proj_path, study_id, extraction_ids, schema)
  ids <- names(ext)
  base <- base %||% ids[1]
  if (!base %in% ids) rlang::abort("`base` must be one of the extractions.")
  cmp <- .compare(ext, schema)
  choices <- .check_choices(choices, ids)

  dis <- cmp[!cmp$agree, , drop = FALSE]
  covered <- vapply(seq_len(nrow(dis)), function(i) {
    if (dis$part[i] == "metadata") any(choices$part == "metadata" & choices$key == dis$key[i])
    else any(choices$part == "data" & choices$key == dis$key[i] & choices$field %in% c(dis$field[i], "(row)"))
  }, logical(1))
  if (any(!covered) && unresolved == "error") {
    left <- dis[!covered, , drop = FALSE]
    rlang::abort(c(paste0(nrow(left), " disagreement(s) are not resolved by `choices`:"),
                   rlang::set_names(utils::head(paste0(left$part, " ", left$key, " / ", left$field), 20), "x"),
                   "i" = "See compare_extractions(), or use `unresolved = \"base\"`."),
                 class = "metawoRld_unresolved")
  }

  # ---- metadata
  meta <- ext[[base]]$metadata
  for (i in which(choices$part == "metadata")) {
    k <- choices$key[i]
    if (!is.na(choices$from[i])) meta[k] <- list(ext[[choices$from[i]]]$metadata[[k]])
    else meta[[k]] <- choices$value[i]
  }
  meta <- .clean_for_yaml(meta)

  # ---- data points
  keyed <- lapply(ext, function(e) .keyed_data(e$data, schema))
  items <- schema$data_points$`_items`
  final <- keyed[[base]]
  for (i in which(choices$part == "data" & choices$field == "(row)")) {
    k <- choices$key[i]
    final <- final[final$.key != k, , drop = FALSE]
    if (!is.na(choices$from[i])) {
      row <- keyed[[choices$from[i]]][keyed[[choices$from[i]]]$.key == k, , drop = FALSE]
      if (nrow(row)) {
        if (row$measurement_id %in% final$measurement_id) row$measurement_id <- .unique_id(row$measurement_id, final$measurement_id)
        final <- dplyr::bind_rows(final, row)
      }
    }
  }
  for (i in which(choices$part == "data" & choices$field != "(row)")) {
    k <- choices$key[i]; f <- choices$field[i]
    j <- which(final$.key == k)
    if (!length(j)) next
    new <- if (!is.na(choices$from[i])) {
      src <- keyed[[choices$from[i]]]
      if (f %in% names(src) && any(src$.key == k)) src[[f]][src$.key == k][1] else NA
    } else .coerce_value(choices$value[i], items[[f]]$`_type` %||% "string")
    if (!f %in% names(final)) final[[f]] <- NA
    if (is.numeric(final[[f]]) && is.character(new)) final[[f]] <- as.character(final[[f]])
    final[[f]][j] <- new
  }

  # ---- provenance: which extractions match each final value
  vfields <- .value_fields(schema, names(final))
  sig <- function(d) stats::setNames(.row_signature(d, vfields), d$.key)
  final_sig <- sig(final)
  ext_sigs <- lapply(keyed, sig)
  meta_sig <- .meta_signature(meta)
  prov_rows <- list()
  attribute <- function(id, agree) {
    from <- if (base %in% agree) base else if (length(agree)) agree[1] else NA
    if (is.na(from)) {
      c(measurement_id = id, extraction_id = "", agreed_with = "", person = person, channel = "reconcile",
        tool = .tool_string(), model = "", source_sha256 = "", timestamp = .now())
    } else {
      s <- ext[[from]]$submission
      c(measurement_id = id, extraction_id = from, agreed_with = paste(setdiff(agree, from), collapse = "; "),
        person = as.character(s$person %||% ""), channel = as.character(s$channel %||% ""),
        tool = as.character(s$tool %||% ""), model = as.character(s$model %||% ""),
        source_sha256 = as.character(s$source_sha256 %||% ""), timestamp = as.character(s$created %||% ""))
    }
  }
  agree_meta <- ids[vapply(ids, function(x) identical(.meta_signature(.clean_for_yaml(ext[[x]]$metadata)), meta_sig), logical(1))]
  prov_rows[[1]] <- attribute("_metadata", agree_meta)
  for (r in seq_len(nrow(final))) {
    k <- final$.key[r]
    agree <- ids[vapply(ids, function(x) isTRUE(ext_sigs[[x]][k] == final_sig[k]), logical(1))]
    prov_rows[[r + 1]] <- attribute(as.character(final$measurement_id[r]), agree)
  }
  prov <- as.data.frame(do.call(rbind, prov_rows), stringsAsFactors = FALSE)
  final$.key <- NULL

  reports <- ext[[base]]$study$reports
  errs <- .validate_parts(meta, final, schema)
  if (length(errs)) .abort_validation(study_id, errs)
  dir <- add_study_data(proj_path, study_id, meta, final, reports = reports, provenance = prov,
                        overwrite = fs::dir_exists(.study_dir(proj_path, study_id)))
  for (x in ids) .set_submission_status(proj_path, study_id, x, person, "accepted", reconciled = TRUE)
  .link_reports(proj_path, study_id, reports)
  rlang::inform(paste0("Reconciled ", length(ids), " extractions into data/", study_id, " (",
                       sum(!cmp$agree), " disagreement(s), ", nrow(choices), " choice(s))."))
  invisible(dir)
}

#' Amend an accepted study
#'
#' Refinements during a review go through the same door as extractions: a
#' small submission that changes an accepted study, accepted with
#' [accept_submission()]. Use it to add a fact to groups (e.g. `onset` after
#' it was added to the form), link subgroups to their parent group, add a
#' group that was not extracted the first time, or add or correct data
#' points. When accepted, the provenance and verification of everything that
#' did not change are kept.
#'
#' @param path Project root.
#' @param study_id Accepted study to amend.
#' @param person Person making the amendment.
#' @param groups List of groups to change or add, each a list with
#'   `group_id` and the fields to set, e.g.
#'   `list(list(group_id = "pe_early", parent_group_id = "pe", onset = "early"))`.
#'   Fields of existing groups not mentioned are kept.
#' @param data Data frame (or list) of data points to add, or to replace
#'   (matched by `measurement_id`).
#' @param remove_data `measurement_id`s of data points to remove.
#' @param metadata Named list of other metadata fields to replace.
#' @param note Why the study was amended (stored in `submission.yml`).
#' @param channel Channel name. Defaults to `"amendment"`.
#' @return The extraction id of the amendment, invisibly.
#' @export
amend_study <- function(path = ".", study_id, person, groups = NULL, data = NULL, remove_data = NULL,
                        metadata = NULL, note = "", channel = "amendment") {
  proj_path <- .check_project(path)
  s <- read_study(proj_path, study_id)
  meta <- s$metadata
  for (nm in names(metadata)) meta[nm] <- list(metadata[[nm]])
  if (length(groups)) {
    g <- meta$groups %||% list()
    if (is.data.frame(g)) g <- lapply(seq_len(nrow(g)), function(k) .df_row(g, k))
    ids <- vapply(g, function(x) as.character(x$group_id %||% ""), character(1))
    for (ch in groups) {
      if (is.null(ch$group_id)) rlang::abort("Each entry of `groups` needs a `group_id`.")
      j <- match(ch$group_id, ids)
      if (is.na(j)) { g[[length(g) + 1]] <- ch; ids <- c(ids, ch$group_id) } else g[[j]] <- utils::modifyList(g[[j]], ch)
    }
    meta$groups <- g
  }
  d <- as.data.frame(s$data, stringsAsFactors = FALSE)
  if (length(remove_data)) d <- d[!d$measurement_id %in% remove_data, , drop = FALSE]
  if (!is.null(data)) {
    new <- as.data.frame(.as_data_points(data), stringsAsFactors = FALSE)
    if (!"measurement_id" %in% names(new)) rlang::abort("`data` needs a `measurement_id` column.")
    d <- d[!d$measurement_id %in% new$measurement_id, , drop = FALSE]
    d <- dplyr::bind_rows(d, new)
  }
  id <- submit_study(proj_path, study_id, meta, d, person = person, channel = channel, reports = s$study$reports)
  f <- fs::path(proj_path, "submissions", study_id, id, "submission.yml")
  sub <- yaml::read_yaml(f)
  sub$kind <- "amendment"
  sub$note <- note
  yaml::write_yaml(sub, f)
  invisible(id)
}

#' Write blank files for a submission
#'
#' Writes `metadata.yml` (a placeholder for every form field) and a
#' header-only `data.csv` to `dir`, for filling in with any text editor or
#' spreadsheet program that saves CSV. Submit them with
#' `submit_study(metadata = "<dir>/metadata.yml", data = "<dir>/data.csv")`.
#'
#' @param path Project root.
#' @param dir Folder to write to. Must not contain the files already.
#' @return `dir`, invisibly.
#' @export
submission_template <- function(path = ".", dir) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  files <- fs::path(dir, c("metadata.yml", "data.csv"))
  if (any(fs::file_exists(files))) rlang::abort(paste0("Template files already exist in ", dir, "."))
  fs::dir_create(dir)
  meta <- .template_value(schema$metadata)
  meta$study_id <- NULL
  yaml::write_yaml(meta, files[1])
  writeLines(paste(.schema_fields(schema$data_points$`_items`), collapse = ","), files[2])
  invisible(dir)
}

# ---- internals ---------------------------------------------------------------

.submission_dir <- function(proj_path, study_id, extraction_id) {
  check_study_id(study_id)
  if (!rlang::is_scalar_character(extraction_id) || !grepl("^[A-Za-z0-9_.-]+$", extraction_id)) rlang::abort("Invalid extraction id.")
  dir <- fs::path(proj_path, "submissions", study_id, extraction_id)
  if (!fs::dir_exists(dir)) rlang::abort(paste0("Submission not found: submissions/", study_id, "/", extraction_id))
  dir
}

.submission_study_ids <- function(proj_path) {
  root <- fs::path(proj_path, "submissions")
  if (!fs::dir_exists(root)) return(character(0))
  fs::path_file(fs::dir_ls(root, type = "directory"))
}

.new_extraction_id <- function(proj_path, study_id, person, time) {
  repeat {
    id <- paste0(format(time, "%Y%m%dT%H%M%S", tz = "UTC"), "-", person, "-extraction")
    if (!fs::dir_exists(fs::path(proj_path, "submissions", study_id, id))) return(id)
    time <- time + 1
  }
}

.read_data_csv <- function(file, schema) {
  header <- names(readr::read_csv(file, n_max = 0, col_types = readr::cols(.default = "c"), show_col_types = FALSE))
  readr::read_csv(file, col_types = schema_col_types(schema, header), na = c("", "NA"),
                  progress = FALSE, show_col_types = FALSE)
}

.read_submission_dir <- function(dir, schema) {
  if (!fs::dir_exists(dir)) rlang::abort(paste0("Submission folder not found: ", dir))
  f <- function(x) fs::path(dir, x)
  if (!fs::file_exists(f("metadata.yml"))) rlang::abort("metadata.yml not found.")
  metadata <- yaml::read_yaml(f("metadata.yml"))
  if (!is.list(metadata)) rlang::abort("metadata.yml is not a YAML mapping.")
  list(
    submission = if (fs::file_exists(f("submission.yml"))) yaml::read_yaml(f("submission.yml")) else list(),
    study = if (fs::file_exists(f("study.yml"))) yaml::read_yaml(f("study.yml")) else list(),
    metadata = metadata,
    data = if (fs::file_exists(f("data.csv"))) .read_data_csv(f("data.csv"), schema) else dplyr::tibble(),
    provenance = .read_char_csv(f("provenance.csv"), .prov_cols)
  )
}

.submission_header_errors <- function(proj_path, sub, study_id, extraction_id) {
  out <- character(0)
  for (k in c("extraction_id", "study_id", "person", "channel", "created")) {
    if (is.null(sub[[k]]) || !nzchar(as.character(sub[[k]]))) out <- c(out, paste0("submission.yml: `", k, "` is missing."))
  }
  if (!is.null(sub$extraction_id) && !identical(sub$extraction_id, extraction_id)) out <- c(out, "submission.yml: extraction_id does not match the folder.")
  if (!is.null(sub$study_id) && !identical(sub$study_id, study_id)) out <- c(out, "submission.yml: study_id does not match the folder.")
  if (!is.null(sub$person) && !sub$person %in% .known_actors(proj_path)) out <- c(out, paste0("submission.yml: unknown person/tool '", sub$person, "'."))
  if (!is.null(sub$status) && !sub$status %in% .sub_status) out <- c(out, "submission.yml: invalid status.")
  out
}

.gate_warnings <- function(proj_path, metadata, data) {
  if (!nrow(data)) return(character(0))
  rules <- tryCatch(read_rules(proj_path), error = function(e) NULL)
  if (is.null(rules)) return(character(0))
  fl <- .check_study_values(data, metadata, rules)
  if (!nrow(fl)) return(character(0))
  paste0(fl$measurement_id, ": ", fl$message, " [", fl$check, "]")
}

.submission_provenance <- function(s, ids) {
  sub <- s$submission
  base <- list(extraction_id = sub$extraction_id %||% "", agreed_with = "", person = sub$person %||% "",
               channel = sub$channel %||% "", tool = sub$tool %||% "", model = sub$model %||% "",
               source_sha256 = sub$source_sha256 %||% "", timestamp = sub$created %||% .now())
  out <- data.frame(measurement_id = ids, stringsAsFactors = FALSE)
  given <- s$provenance
  m <- match(ids, given$measurement_id)
  for (c in setdiff(.prov_cols, "measurement_id")) {
    v <- if (c %in% names(given)) given[[c]][m] else rep(NA_character_, length(ids))
    v[is.na(v) | !nzchar(v)] <- as.character(base[[c]])
    out[[c]] <- v
  }
  out$extraction_id <- base$extraction_id
  out
}

.set_submission_status <- function(proj_path, study_id, extraction_id, person, status, reconciled = FALSE) {
  f <- fs::path(proj_path, "submissions", study_id, extraction_id, "submission.yml")
  sub <- yaml::read_yaml(f)
  sub$status <- status
  sub[[status]] <- Filter(Negate(is.null), list(by = person, at = .now(), reconciled = if (reconciled) TRUE))
  yaml::write_yaml(sub, f)
  if (status == "accepted") {
    others <- list_submissions(proj_path, study_id, status = "accepted")
    keep <- others$extraction_id[others$extraction_id != extraction_id]
    if (reconciled) keep <- character(0)
    for (x in keep) {
      g <- fs::path(proj_path, "submissions", study_id, x, "submission.yml")
      s2 <- yaml::read_yaml(g)
      s2$status <- "superseded"
      s2$superseded <- list(by = extraction_id, at = .now())
      yaml::write_yaml(s2, g)
    }
  }
  invisible(NULL)
}

.link_reports <- function(proj_path, study_id, reports) {
  rec <- read_records(proj_path)
  if (!nrow(rec) || !length(reports)) return(invisible(NULL))
  dois <- unlist(lapply(reports, function(r) r$doi))
  pmids <- unlist(lapply(reports, function(r) r$pmid))
  hit <- (nzchar(rec$doi) & rec$doi %in% dois) | (nzchar(rec$pmid) & rec$pmid %in% pmids)
  if (any(hit & rec$study_id != study_id)) {
    rec$study_id[hit] <- study_id
    .write_char_csv(rec, fs::path(proj_path, "records.csv"))
  }
  invisible(NULL)
}

.load_extractions <- function(proj_path, study_id, extraction_ids, schema) {
  check_study_id(study_id)
  if (is.null(extraction_ids)) {
    extraction_ids <- list_submissions(proj_path, study_id, status = "pending")$extraction_id
  }
  if (length(extraction_ids) < 2) rlang::abort(paste0("Need at least two extractions of '", study_id, "' to compare."))
  ext <- lapply(extraction_ids, function(x) .read_submission_dir(.submission_dir(proj_path, study_id, x), schema))
  names(ext) <- extraction_ids
  ext
}

.key_fields <- function(schema, cols) {
  e <- schema$`_form`$entry
  k <- unlist(c(e$rows, e$columns, e$context))
  if (!length(k)) k <- c("variable_name", "group_id", "method_id", "timepoint", "statistic_type")
  intersect(setdiff(k, .hash_exclude), cols)
}

.value_fields <- function(schema, cols) {
  setdiff(intersect(.schema_fields(schema$data_points$`_items`), cols), c(.key_fields(schema, cols), .hash_exclude))
}

.fmt_values <- function(x) {
  # Element-wise, so a value is written the same whatever its neighbours are.
  out <- as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

.keyed_data <- function(d, schema) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  if (!nrow(d)) { d$.key <- character(0); return(d) }
  kf <- .key_fields(schema, names(d))
  key <- if (length(kf)) do.call(paste, c(lapply(kf, function(f) ifelse(is.na(d[[f]]), "", .fmt_values(d[[f]]))), sep = " | ")) else d$measurement_id
  dup <- stats::ave(seq_along(key), key, FUN = seq_along)
  key[dup > 1] <- paste0(key[dup > 1], " #", dup[dup > 1])
  d$.key <- key
  d
}

.row_signature <- function(d, fields) {
  if (!nrow(d)) return(character(0))
  fields <- intersect(fields, names(d))
  do.call(paste, c(lapply(fields, function(f) paste0(f, "=", ifelse(is.na(d[[f]]), "", .fmt_values(d[[f]])))), sep = "\n"))
}

.flatten_meta <- function(x, prefix = "") {
  if (is.data.frame(x)) x <- lapply(seq_len(nrow(x)), function(i) .df_row(x, i))
  if (is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))) {
    out <- character(0)
    for (nm in names(x)) out <- c(out, .flatten_meta(x[[nm]], if (nzchar(prefix)) paste0(prefix, ".", nm) else nm))
    return(out)
  }
  if (is.list(x) && length(x) && all(vapply(x, is.list, logical(1)))) {
    out <- character(0)
    for (i in seq_along(x)) {
      el <- x[[i]]
      idf <- grep("_id$", names(el), value = TRUE)[1]
      lab <- if (!is.na(idf)) as.character(el[[idf]]) else as.character(i)
      out <- c(out, .flatten_meta(el, paste0(prefix, "[", lab, "]")))
    }
    return(out)
  }
  v <- unlist(x)
  stats::setNames(if (length(v)) paste(.fmt_values(v), collapse = "; ") else NA_character_, prefix)
}

.meta_signature <- function(m) {
  m$study_id <- NULL
  f <- .flatten_meta(m)
  paste(names(f), f, sep = "=", collapse = "\n")
}

.compare <- function(ext, schema) {
  ids <- names(ext)
  # metadata
  flat <- lapply(ext, function(e) { m <- e$metadata; m$study_id <- NULL; .flatten_meta(.clean_for_yaml(m)) })
  fields <- unique(unlist(lapply(flat, names)))
  meta <- dplyr::tibble(part = rep("metadata", length(fields)), key = sub("[.\\[].*$", "", fields), field = fields)
  for (x in ids) meta[[x]] <- unname(flat[[x]][fields])
  # data
  keyed <- lapply(ext, function(e) .keyed_data(e$data, schema))
  keys <- unique(unlist(lapply(keyed, function(d) d$.key)))
  cols <- unique(unlist(lapply(keyed, names)))
  vf <- .value_fields(schema, cols)
  rows <- list()
  for (k in keys) {
    pres <- dplyr::tibble(part = "data", key = k, field = "(row)")
    for (x in ids) pres[[x]] <- if (k %in% keyed[[x]]$.key) "present" else NA_character_
    vals <- dplyr::tibble(part = rep("data", length(vf)), key = k, field = vf)
    for (x in ids) {
      d <- keyed[[x]]
      j <- match(k, d$.key)
      vals[[x]] <- vapply(vf, function(f) if (is.na(j) || !f %in% names(d)) NA_character_ else .fmt_values(d[[f]][j]), character(1), USE.NAMES = FALSE)
    }
    present <- !is.na(unlist(pres[1, ids]))
    vals <- vals[vapply(seq_len(nrow(vals)), function(i) any(!is.na(unlist(vals[i, ids[present]]))), logical(1)), , drop = FALSE]
    rows <- c(rows, list(pres, vals))
  }
  out <- dplyr::bind_rows(meta, rows)
  same <- function(v) { v[is.na(v)] <- "\001NA"; length(unique(v)) == 1 }
  out$agree <- vapply(seq_len(nrow(out)), function(i) same(unlist(out[i, ids])), logical(1))
  out
}

.check_choices <- function(choices, ids) {
  empty <- data.frame(part = character(0), key = character(0), field = character(0),
                      from = character(0), value = character(0), stringsAsFactors = FALSE)
  if (is.null(choices) || !nrow(choices)) return(empty)
  ch <- as.data.frame(choices, stringsAsFactors = FALSE)
  if (!all(c("part", "key") %in% names(ch))) rlang::abort("`choices` needs columns `part`, `key`, `field` and `from` or `value`.")
  if (!"field" %in% names(ch)) ch$field <- NA_character_
  if (!"from" %in% names(ch)) ch$from <- NA_character_
  if (!"value" %in% names(ch)) ch$value <- NA_character_
  ch <- ch[, names(empty)]
  ch[] <- lapply(ch, as.character)
  ch$from[!is.na(ch$from) & !nzchar(ch$from)] <- NA
  bad <- setdiff(stats::na.omit(ch$from), ids)
  if (length(bad)) rlang::abort(paste0("`choices$from` refers to unknown extractions: ", paste(bad, collapse = ", ")))
  if (any(is.na(ch$from) & is.na(ch$value))) rlang::abort("Each choice needs `from` or `value`.")
  drop <- ch$field %in% "(row)" & is.na(ch$from) & !ch$value %in% "drop"
  if (any(drop)) rlang::abort("For `field = \"(row)\"` use `from` or `value = \"drop\"`.")
  ch
}

.coerce_value <- function(v, type) {
  if (is.na(v) || !nzchar(v)) return(NA)
  switch(type, integer = , number = as.numeric(v), boolean = as.logical(v), v)
}

.unique_id <- function(id, taken) {
  i <- 2
  while (paste0(id, "_", i) %in% taken) i <- i + 1
  paste0(id, "_", i)
}
