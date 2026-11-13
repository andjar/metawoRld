#' Write accepted study data (low level)
#'
#' Validates the metadata and data points against the extraction form and
#' writes `data/<study_id>/` (`study.yml`, `metadata.yml`, `data.csv`,
#' `provenance.csv` and `datapackage.json`; see `SPEC.md` section 8). All
#' validation problems are reported at once.
#'
#' This bypasses the submission workflow. Input channels should use
#' [submit_study()] and [accept_submission()], which call this function and
#' record where each value came from.
#'
#' @param path Project root.
#' @param study_id Study id (see [check_study_id()]), e.g. `"hansen2019"`.
#' @param metadata Named list matching the `metadata` part of the form.
#'   Arrays of objects may be given as data frames.
#' @param data Data frame (or list of objects) matching `data_points`. May
#'   have zero rows.
#' @param reports Reports (publications) of the study: a list of lists with
#'   `doi`, `pmid` and `role` (`primary` or `secondary`). Defaults to the
#'   existing reports, else `metadata$doi`/`metadata$pmid`.
#' @param provenance Where the values came from: either a list of defaults
#'   (`extraction_id`, `person`, `channel`, `tool`, `model`, `source_sha256`,
#'   `timestamp`) applied to every data point, or a data frame with a
#'   `measurement_id` column (and `_metadata` for the metadata). When
#'   overwriting, the provenance of data points whose values did not change
#'   is kept.
#' @param overwrite Replace an existing study? Defaults to `FALSE`.
#' @param validate Validate before writing? Defaults to `TRUE`.
#'
#' @return The study folder, invisibly.
#' @export
add_study_data <- function(path = ".",
                           study_id,
                           metadata,
                           data,
                           reports = NULL,
                           provenance = list(channel = "manual_edit"),
                           overwrite = FALSE,
                           validate = TRUE) {
  proj_path <- .check_project(path)
  if (missing(study_id)) rlang::abort("`study_id` is required.")
  check_study_id(study_id)
  if (missing(metadata) || !is.list(metadata) || is.data.frame(metadata)) {
    rlang::abort("`metadata` must be a named list.")
  }
  if (missing(data)) data <- data.frame()
  data <- .as_data_points(data)
  metadata$study_id <- study_id
  metadata <- .clean_for_yaml(metadata)

  schema <- get_schema(proj_path, "extraction")
  if (validate) {
    errs <- .validate_parts(metadata, data, schema)
    if (length(errs)) .abort_validation(study_id, errs)
  }

  study_dir <- .study_dir(proj_path, study_id)
  verification <- NULL
  old <- NULL
  if (fs::dir_exists(study_dir)) {
    if (!overwrite) {
      rlang::abort(paste0("Study '", study_id, "' already exists. Use `overwrite = TRUE` to replace it."))
    }
    old <- tryCatch(read_study(proj_path, study_id), error = function(e) NULL)
    # Keep reviewers' decisions; changed values show up as "changed".
    vf <- fs::path(study_dir, "verification.csv")
    if (fs::file_exists(vf)) verification <- readLines(vf, warn = FALSE)
    fs::dir_delete(study_dir)
  }
  fs::dir_create(study_dir)
  if (!is.null(verification)) writeLines(verification, fs::path(study_dir, "verification.csv"))

  study <- list(
    study_id = study_id,
    reports = reports %||% old$study$reports %||% .reports_from_metadata(metadata),
    status = old$study$status %||% "included",
    form_version = as.character(schema$`_form`$version),
    notes = old$study$notes %||% ""
  )
  study$reports <- .clean_reports(study$reports)
  yaml::write_yaml(study, fs::path(study_dir, "study.yml"))
  yaml::write_yaml(metadata, fs::path(study_dir, "metadata.yml"))
  .write_data_csv(data, schema, fs::path(study_dir, "data.csv"))
  prov <- .build_provenance(data, metadata, provenance, old, schema)
  .write_char_csv(prov, fs::path(study_dir, "provenance.csv"))
  .write_datapackage(proj_path, study_id)

  invisible(study_dir)
}

#' Read one study from a metawoRld project
#'
#' @param path Project root.
#' @param study_id Study id.
#' @return A list with `study` (contents of `study.yml`), `metadata`
#'   (list), `data` (tibble; column types from the form) and `provenance`
#'   (tibble).
#' @export
read_study <- function(path = ".", study_id) {
  proj_path <- .check_project(path)
  study_dir <- .study_dir(proj_path, study_id)
  if (!fs::dir_exists(study_dir)) rlang::abort(paste0("Study not found: ", study_id))
  .read_study_dir(study_dir, get_schema(proj_path, "extraction"))
}

#' List the studies in a project
#'
#' @param path Project root.
#' @return Character vector of study ids.
#' @export
list_studies <- function(path = ".") {
  proj_path <- .check_project(path)
  data_path <- fs::path(proj_path, "data")
  if (!fs::dir_exists(data_path)) return(character(0))
  ids <- fs::path_file(fs::dir_ls(data_path, type = "directory"))
  sort(ids[is_study_id(ids)])
}

#' Load all studies of a metawoRld project
#'
#' @param path Project root.
#' @param verbose Report skipped studies? Defaults to `TRUE`.
#' @param drop_rejected Leave out data points a reviewer rejected? Defaults
#'   to `TRUE`.
#' @param verified_only Keep only data points whose current values were
#'   verified? Defaults to `FALSE`.
#'
#' @return A list with
#' \describe{
#'   \item{`studies`}{One row per study with the simple (scalar or
#'     string-vector) metadata fields; vectors are collapsed with `"; "`.}
#'   \item{`studies_data`}{All data points with a `study_id` column. For each
#'     `_ref` field in the schema the scalar fields of the referenced object
#'     are joined, prefixed with the field name without `_id` (e.g.
#'     `method_unit`, `group_name`). `verification_status` (see
#'     [verification_status()]) and, when `_rules.yml` defines variables,
#'     `variable_canonical` are added.}
#'   \item{`studies_metadata`}{Named list of each study's metadata.}
#' }
#' @export
load_metawoRld <- function(path = ".", verbose = TRUE, drop_rejected = TRUE, verified_only = FALSE) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  rules <- read_rules(proj_path)
  ids <- list_studies(proj_path)

  metas <- list()
  datas <- list()
  for (id in ids) {
    res <- tryCatch(.read_study_dir(.study_dir(proj_path, id), schema), error = function(e) e)
    if (inherits(res, "error")) {
      if (verbose) rlang::warn(paste0("Skipping '", id, "': ", conditionMessage(res)))
      next
    }
    metas[[id]] <- res$metadata
    d <- res$data
    if (nrow(d) > 0) {
      vs <- .status_for(d, .read_verification(.study_dir(proj_path, id)), schema)
      d$verification_status <- vs$status
      if (drop_rejected) d <- d[d$verification_status != "rejected", , drop = FALSE]
      if (verified_only) d <- d[d$verification_status == "verified", , drop = FALSE]
      if (length(rules$variables) && "variable_name" %in% names(d)) {
        d$variable_canonical <- canonical_variable(d$variable_name, rules)
      }
    }
    if (nrow(d) > 0) {
      d <- .join_refs(d, res$metadata, schema)
      d <- dplyr::mutate(d, study_id = id, .before = 1)
      datas[[id]] <- d
    }
  }

  data <- if (length(datas)) {
    dplyr::as_tibble(data.table::rbindlist(datas, fill = TRUE, use.names = TRUE))
  } else {
    dplyr::tibble(study_id = character(0))
  }
  if (verbose) rlang::inform(paste0("Loaded ", length(metas), " studies and ", nrow(data), " data points."))

  list(
    studies = .studies_table(metas),
    studies_data = data,
    studies_metadata = metas
  )
}

#' Validate one study
#'
#' Reads the study's files and checks them against the schema, including the
#' `_ref` links between data points and metadata. Useful after editing files
#' by hand.
#'
#' @param study_id Study identifier.
#' @param path Project root.
#' @param check_links Check `_ref` links? Defaults to `TRUE`.
#' @return `TRUE` invisibly if valid; otherwise an error listing all problems.
#' @export
validate_study <- function(study_id, path = ".", check_links = TRUE) {
  errs <- .study_errors(.check_project(path), study_id, check_links)
  if (length(errs)) .abort_validation(study_id, errs)
  invisible(TRUE)
}

#' Validate all studies in a project
#'
#' @param path Project root.
#' @param check_links Check `_ref` links? Defaults to `TRUE`.
#' @return A tibble with one row per study: `study_id`, `valid`, `n_errors`
#'   and `errors` (list of messages). A summary is printed.
#' @export
validate_world <- function(path = ".", check_links = TRUE) {
  proj_path <- .check_project(path)
  ids <- list_studies(proj_path)
  errs <- lapply(ids, function(id) {
    tryCatch(.study_errors(proj_path, id, check_links), error = function(e) conditionMessage(e))
  })
  out <- dplyr::tibble(
    study_id = ids,
    valid = lengths(errs) == 0,
    n_errors = lengths(errs),
    errors = errs
  )
  rlang::inform(sprintf("%d studies checked: %d valid, %d with errors.",
                        nrow(out), sum(out$valid), sum(!out$valid)))
  out
}

# ---- internals ---------------------------------------------------------------

.study_dir <- function(proj_path, study_id) {
  check_study_id(study_id)
  fs::path(proj_path, "data", study_id)
}

.read_study_dir <- function(study_dir, schema) {
  meta_file <- fs::path(study_dir, "metadata.yml")
  data_file <- fs::path(study_dir, "data.csv")
  if (!fs::file_exists(meta_file)) rlang::abort("metadata.yml not found.")
  metadata <- yaml::read_yaml(meta_file)
  if (!is.list(metadata)) rlang::abort("metadata.yml is not a YAML mapping.")
  data <- if (fs::file_exists(data_file)) {
    header <- names(readr::read_csv(data_file, n_max = 0, col_types = readr::cols(.default = "c"),
                                    show_col_types = FALSE))
    readr::read_csv(data_file, col_types = schema_col_types(schema, header), na = c("", "NA"),
                    progress = FALSE, show_col_types = FALSE)
  } else {
    dplyr::tibble()
  }
  sf <- fs::path(study_dir, "study.yml")
  study <- if (fs::file_exists(sf)) yaml::read_yaml(sf) else list(study_id = fs::path_file(study_dir))
  list(study = study, metadata = metadata, data = data,
       provenance = .read_char_csv(fs::path(study_dir, "provenance.csv"), .prov_cols))
}

.study_errors <- function(proj_path, study_id, check_links) {
  schema <- get_schema(proj_path, "extraction")
  study_dir <- .study_dir(proj_path, study_id)
  if (!fs::dir_exists(study_dir)) return(paste0("Study folder not found: ", study_dir))
  if (!fs::file_exists(fs::path(study_dir, "data.csv"))) return("data.csv not found.")
  s <- tryCatch(.read_study_dir(study_dir, schema), error = function(e) e)
  if (inherits(s, "error")) return(conditionMessage(s))
  .validate_parts(s$metadata, s$data, schema, check_links)
}

.validate_parts <- function(metadata, data, schema, check_links = TRUE) {
  c(
    validate_against_schema(metadata, schema$metadata, path = "metadata"),
    validate_against_schema(data, schema$data_points, path = "data_points"),
    if (check_links) validate_links(metadata, data, schema),
    if (check_links) .group_tree_errors(metadata$groups)
  )
}

.as_data_points <- function(data) {
  if (is.data.frame(data)) return(data)
  if (!is.list(data)) rlang::abort("`data` must be a data frame or a list of objects.")
  .rows_to_df(data)
}

.reports_from_metadata <- function(metadata) {
  doi <- normalize_identifier(as.character(metadata$doi %||% ""))
  pmid <- normalize_identifier(as.character(metadata$pmid %||% ""))
  if (!nzchar(doi) && !nzchar(pmid)) return(list())
  list(Filter(nzchar, list(doi = doi, pmid = pmid, role = "primary")))
}

.clean_reports <- function(reports) {
  if (is.data.frame(reports)) reports <- lapply(seq_len(nrow(reports)), function(i) .df_row(reports, i))
  lapply(reports, function(r) {
    r <- lapply(r, function(v) as.character(v %||% ""))
    if (!is.null(r$doi)) r$doi <- normalize_identifier(r$doi)
    if (!is.null(r$pmid)) r$pmid <- normalize_identifier(r$pmid)
    r$role <- r$role %||% "primary"
    Filter(function(v) length(v) == 1 && !is.na(v) && nzchar(v), r)
  })
}

# ---- provenance --------------------------------------------------------------

.prov_cols <- c("measurement_id", "extraction_id", "agreed_with", "person", "channel",
                "tool", "model", "source_sha256", "timestamp")

.tool_string <- function() paste0("metawoRld@", utils::packageVersion("metawoRld"))

.build_provenance <- function(data, metadata, provenance, old, schema) {
  ids <- c("_metadata", as.character(data$measurement_id %||% character(0)))
  defaults <- list(extraction_id = "", agreed_with = "", person = getOption("metawoRld.person", ""),
                   channel = "manual_edit", tool = .tool_string(), model = "", source_sha256 = "",
                   timestamp = .now())
  if (is.data.frame(provenance)) {
    given <- as.data.frame(provenance, stringsAsFactors = FALSE)
    if (!"measurement_id" %in% names(given)) rlang::abort("A `provenance` data frame needs a `measurement_id` column.")
    out <- data.frame(measurement_id = ids, stringsAsFactors = FALSE)
    m <- match(ids, as.character(given$measurement_id))
    for (c in setdiff(.prov_cols, "measurement_id")) {
      v <- if (c %in% names(given)) as.character(given[[c]])[m] else rep(NA_character_, length(ids))
      v[is.na(v)] <- as.character(defaults[[c]])
      out[[c]] <- v
    }
    return(out)
  }
  provenance <- utils::modifyList(defaults, as.list(provenance %||% list()))
  out <- data.frame(measurement_id = ids, stringsAsFactors = FALSE)
  for (c in setdiff(.prov_cols, "measurement_id")) out[[c]] <- as.character(provenance[[c]] %||% "")
  # Keep the provenance of values that did not change.
  if (!is.null(old) && nrow(old$provenance) && nrow(data) && nrow(old$data)) {
    new_h <- .row_hashes(data, schema)
    old_h <- .row_hashes(old$data, schema)
    same <- names(new_h)[names(new_h) %in% names(old_h) & new_h == old_h[names(new_h)]]
    if (identical(.meta_signature(.clean_for_yaml(old$metadata)), .meta_signature(metadata))) same <- c("_metadata", same)
    for (id in same) {
      j <- match(id, old$provenance$measurement_id)
      if (!is.na(j)) out[match(id, out$measurement_id), .prov_cols] <- old$provenance[j, .prov_cols]
    }
  }
  out
}

.abort_validation <- function(study_id, errs) {
  shown <- utils::head(errs, 30)
  more <- if (length(errs) > 30) paste0("... and ", length(errs) - 30, " more.") else NULL
  rlang::abort(
    c(paste0("Validation failed for study '", study_id, "' (", length(errs), " problem(s)):"),
      rlang::set_names(c(shown, more), rep("x", length(c(shown, more))))),
    class = "metawoRld_validation_error",
    validation_errors = errs
  )
}

.write_data_csv <- function(data, schema, file) {
  cols <- .schema_fields(schema$data_points$`_items`)
  if (ncol(data) == 0) {
    writeLines(paste(cols, collapse = ","), file)
    return(invisible(file))
  }
  data <- as.data.frame(data)
  # Flatten list columns so they survive the CSV round trip
  for (nm in names(data)) {
    if (is.list(data[[nm]])) {
      data[[nm]] <- vapply(data[[nm]], function(v) paste(unlist(v), collapse = "; "), character(1))
    }
  }
  data <- data[, c(intersect(cols, names(data)), setdiff(names(data), cols)), drop = FALSE]
  data.table::fwrite(data, file, na = "")
  invisible(file)
}

.join_refs <- function(data, metadata, schema) {
  refs <- schema_refs(schema)
  for (i in seq_len(nrow(refs))) {
    r <- refs[i, ]
    if (!r$field %in% names(data)) next
    objs <- metadata[[r$array]]
    if (is.null(objs) || length(objs) == 0) next
    lookup <- .rows_to_df(if (is.data.frame(objs)) lapply(seq_len(nrow(objs)), function(k) .df_row(objs, k)) else objs)
    if (!r$id_field %in% names(lookup)) next
    keep <- vapply(lookup, function(col) !is.list(col), logical(1))
    lookup <- lookup[, keep, drop = FALSE]
    prefix <- paste0(sub("_id$", "", r$field), "_")
    other <- setdiff(names(lookup), r$id_field)
    names(lookup)[match(other, names(lookup))] <- paste0(prefix, other)
    lookup <- lookup[, setdiff(names(lookup), names(data)), drop = FALSE] |>
      cbind(.key = as.character(lookup[[r$id_field]]))
    lookup <- lookup[!duplicated(lookup$.key), , drop = FALSE]
    data$.key <- as.character(data[[r$field]])
    data <- dplyr::left_join(data, lookup, by = ".key")
    data$.key <- NULL
  }
  data
}

.studies_table <- function(metas) {
  if (!length(metas)) return(dplyr::tibble(study_id = character(0)))
  rows <- lapply(names(metas), function(id) {
    m <- metas[[id]]
    simple <- Filter(function(v) is.atomic(v) || (is.list(v) && all(vapply(v, function(e) is.atomic(e) && length(e) == 1, logical(1)))), m)
    simple <- lapply(simple, function(v) paste(unlist(v), collapse = "; "))
    simple$study_id <- id
    simple
  })
  out <- dplyr::as_tibble(data.table::rbindlist(rows, fill = TRUE, use.names = TRUE))
  dplyr::relocate(out, "study_id")
}

.template_value <- function(node) {
  desc <- node$`_description` %||% ""
  req <- if (.is_required(node)) "REQUIRED" else "OPTIONAL"
  switch(
    node$`_type`,
    object = {
      out <- lapply(.schema_fields(node), function(f) .template_value(node[[f]]))
      rlang::set_names(out, .schema_fields(node))
    },
    array = list(.template_value(node$`_items`)),
    enum = paste0(req, ": one of ", paste(unlist(node$`_values`), collapse = " | ")),
    paste0(req, " (", node$`_type`, "): ", desc)
  )
}
