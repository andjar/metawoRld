# Verification is stored per study in data/<study>/verification.csv:
#   measurement_id, status (verified | rejected), reviewer, timestamp,
#   row_hash, note
# `row_hash` fingerprints the data point's values when it was reviewed. If the
# values change afterwards (re-extraction, manual edit), the status becomes
# "changed" and the data point needs to be reviewed again.

.verif_cols <- c("measurement_id", "status", "reviewer", "timestamp", "row_hash", "note")

# Fields that do not affect the value itself; editing them keeps a verification.
.hash_exclude <- c("measurement_id", "source", "source_page", "source_quote", "notes")

#' Verification status of data points
#'
#' @param path Project root.
#' @param study_ids Studies to include. Defaults to all.
#' @return A tibble with `study_id`, `measurement_id`, `status`
#'   (`"unverified"`, `"verified"`, `"rejected"` or `"changed"`, the latter
#'   meaning the values changed after review), `reviewer`, `timestamp` and
#'   `note`.
#' @export
verification_status <- function(path = ".", study_ids = NULL) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  ids <- study_ids %||% list_studies(proj_path)
  out <- lapply(ids, function(id) {
    dir <- .study_dir(proj_path, id)
    s <- tryCatch(.read_study_dir(dir, schema), error = function(e) NULL)
    if (is.null(s) || nrow(s$data) == 0) return(NULL)
    st <- .status_for(s$data, .read_verification(dir), schema)
    st$study_id <- id
    st
  })
  res <- dplyr::bind_rows(
    dplyr::tibble(study_id = character(0), measurement_id = character(0), status = character(0),
                  reviewer = character(0), timestamp = character(0), note = character(0)),
    dplyr::bind_rows(out)
  )
  dplyr::relocate(res, "study_id")
}

#' Record the verification of data points
#'
#' @param path Project root.
#' @param study_id Study identifier.
#' @param measurement_id One or more `measurement_id`s in the study.
#' @param status `"verified"`, `"rejected"` (the value is wrong or should not
#'   be used) or `"unverified"` (removes an earlier decision).
#' @param reviewer Reviewer name or initials. Defaults to the
#'   `metawoRld.reviewer` option, then the system user name.
#' @param note Optional note, e.g. why a value was rejected.
#' @return The updated verification table of the study, invisibly.
#' @export
set_verification <- function(path = ".", study_id, measurement_id,
                             status = c("verified", "rejected", "unverified"),
                             reviewer = NULL, note = NULL) {
  status <- match.arg(status)
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  dir <- .study_dir(proj_path, study_id)
  if (!fs::dir_exists(dir)) rlang::abort(paste0("Study not found: ", study_id))
  data <- .read_study_dir(dir, schema)$data
  unknown <- setdiff(measurement_id, data$measurement_id)
  if (length(unknown)) rlang::abort(paste0("Unknown measurement_id(s) in '", study_id, "': ", paste(unknown, collapse = ", ")))

  v <- .read_verification(dir)
  v <- v[!v$measurement_id %in% measurement_id, , drop = FALSE]
  if (status != "unverified") {
    hashes <- .row_hashes(data, schema)
    new <- data.frame(
      measurement_id = measurement_id,
      status = status,
      reviewer = reviewer %||% .default_reviewer(),
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      row_hash = unname(hashes[measurement_id]),
      note = note %||% "",
      stringsAsFactors = FALSE
    )
    v <- rbind(v, new)
  }
  v <- v[order(match(v$measurement_id, data$measurement_id)), , drop = FALSE]
  data.table::fwrite(v, fs::path(dir, "verification.csv"), na = "")
  invisible(v)
}

#' Correct a data point
#'
#' Changes one data point, re-validates the study against the schema and, by
#' default, marks the data point as verified with a note listing the changes
#' (e.g. `"value1: 5.1 -> 5.2"`). Use this (or [review_values()]) rather than
#' editing `data.csv` by hand, so corrections are documented.
#'
#' @param path Project root.
#' @param study_id Study identifier.
#' @param measurement_id The data point to change.
#' @param ... Named new values, e.g. `value1 = 5.2, unit = "pg/mL"`.
#' @param verify Mark the corrected data point as verified? Defaults to `TRUE`.
#' @param reviewer,note See [set_verification()]. `note` is appended to the
#'   list of changes.
#' @return A character vector describing the changes, invisibly.
#' @export
update_data_point <- function(path = ".", study_id, measurement_id, ..., verify = TRUE,
                              reviewer = NULL, note = NULL) {
  proj_path <- .check_project(path)
  changes <- list(...)
  if (!length(changes) || is.null(names(changes)) || any(!nzchar(names(changes)))) {
    rlang::abort("Give the new values as named arguments, e.g. `value1 = 5.2`.")
  }
  schema <- get_schema(proj_path, "extraction")
  s <- read_study(proj_path, study_id)
  d <- as.data.frame(s$data)
  i <- which(d$measurement_id == measurement_id)
  if (length(i) != 1) rlang::abort(paste0("measurement_id '", measurement_id, "' must occur exactly once in '", study_id, "'."))

  items <- schema$data_points$`_items`
  log <- character(0)
  for (f in names(changes)) {
    new <- changes[[f]] %||% NA
    changes[f] <- list(new)
    if (length(new) != 1) rlang::abort(paste0("`", f, "` must be a single value."))
    type <- items[[f]]$`_type` %||% "string"
    new <- if (is.na(new) || identical(new, "")) NA else switch(
      type,
      integer = , number = suppressWarnings(as.numeric(new)),
      boolean = as.logical(new),
      as.character(new)
    )
    if (length(new) == 1 && is.na(new) && !is.na(changes[[f]]) && !identical(changes[[f]], "")) {
      rlang::abort(paste0("`", f, "` must be a ", type, "; got '", changes[[f]], "'."))
    }
    old <- if (f %in% names(d)) d[[f]][i] else NA
    if (identical(as.character(old), as.character(new)) || (is.na(old) && is.na(new))) next
    if (!f %in% names(d)) d[[f]] <- if (type %in% c("integer", "number")) NA_real_ else NA_character_
    if (is.numeric(d[[f]]) && is.character(new)) d[[f]] <- as.character(d[[f]])
    d[[f]][i] <- new
    log <- c(log, paste0(f, ": ", .show(old), " -> ", .show(new)))
  }
  if (!length(log)) {
    if (verify) set_verification(proj_path, study_id, measurement_id, "verified", reviewer, note)
    return(invisible(character(0)))
  }
  add_study_data(proj_path, study_id, s$metadata, d, overwrite = TRUE)
  if (verify) {
    set_verification(proj_path, study_id, measurement_id, "verified", reviewer,
                     paste(c(paste0("Corrected: ", paste(log, collapse = "; ")), note), collapse = ". "))
  }
  invisible(log)
}

# ---- internals ---------------------------------------------------------------

.default_reviewer <- function() {
  r <- getOption("metawoRld.reviewer")
  if (is.null(r)) r <- Sys.getenv("USER", Sys.getenv("USERNAME", "unknown"))
  r
}

.show <- function(x) if (length(x) == 0 || is.na(x)) "(empty)" else format(x)

.read_verification <- function(study_dir) {
  f <- fs::path(study_dir, "verification.csv")
  empty <- as.data.frame(stats::setNames(rep(list(character(0)), length(.verif_cols)), .verif_cols), stringsAsFactors = FALSE)
  if (!fs::file_exists(f)) return(empty)
  v <- utils::read.csv(f, colClasses = "character", na.strings = character(0), check.names = FALSE)
  for (c in setdiff(.verif_cols, names(v))) v[[c]] <- if (nrow(v)) "" else character(0)
  v[, .verif_cols, drop = FALSE]
}

.row_hashes <- function(data, schema) {
  fields <- setdiff(.schema_fields(schema$data_points$`_items`), .hash_exclude)
  fields <- intersect(fields, names(data))
  h <- vapply(seq_len(nrow(data)), function(i) {
    vals <- vapply(fields, function(f) {
      x <- data[[f]][i]
      if (is.na(x)) "" else if (is.numeric(x)) format(x, digits = 15) else as.character(x)
    }, character(1))
    rlang::hash(paste(fields, vals, sep = "=", collapse = "\n"))
  }, character(1))
  stats::setNames(h, as.character(data$measurement_id))
}

.status_for <- function(data, v, schema) {
  hashes <- .row_hashes(data, schema)
  m <- match(as.character(data$measurement_id), v$measurement_id)
  status <- ifelse(is.na(m), "unverified", v$status[m])
  changed <- !is.na(m) & v$row_hash[m] != hashes
  status[changed] <- "changed"
  dplyr::tibble(
    measurement_id = as.character(data$measurement_id),
    status = status,
    reviewer = ifelse(is.na(m), NA_character_, v$reviewer[m]),
    timestamp = ifelse(is.na(m), NA_character_, v$timestamp[m]),
    note = ifelse(is.na(m), NA_character_, v$note[m])
  )
}
