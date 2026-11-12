#' Add a study to a metawoRld project
#'
#' Validates the metadata and data points against the project's extraction
#' schema (types, required fields, enum values and `_ref` links) and writes
#' `data/<study>/metadata.yml` and `data/<study>/data.csv`. All validation
#' problems are reported at once.
#'
#' @param path Project root.
#' @param study_id Study identifier, preferably the DOI or PMID. It is
#'   normalized with [normalize_identifier()], stored as `metadata$study_id`
#'   and used (encoded with [sanitize_id()]) as the folder name.
#' @param metadata Named list matching the `metadata` part of the schema.
#'   Arrays of objects may be given as data frames.
#' @param data Data frame (or list of objects) matching `data_points`. May
#'   have zero rows.
#' @param overwrite Replace an existing study? Defaults to `FALSE`.
#' @param validate Validate before writing? Defaults to `TRUE`.
#'
#' @return The study folder, invisibly.
#' @export
add_study_data <- function(path = ".",
                           study_id,
                           metadata,
                           data,
                           overwrite = FALSE,
                           validate = TRUE) {
  proj_path <- .check_project(path)
  if (missing(study_id) || !rlang::is_scalar_character(study_id) || !nzchar(study_id)) {
    rlang::abort("`study_id` must be a single non-empty string.")
  }
  if (missing(metadata) || !is.list(metadata) || is.data.frame(metadata)) {
    rlang::abort("`metadata` must be a named list.")
  }
  if (missing(data)) data <- data.frame()
  if (!is.data.frame(data)) {
    if (!is.list(data)) rlang::abort("`data` must be a data frame or a list of objects.")
    data <- .rows_to_df(data)
  }

  study_id <- normalize_identifier(study_id)
  metadata$study_id <- study_id
  metadata <- .clean_for_yaml(metadata)

  schema <- get_schema(proj_path, "extraction")
  if (validate) {
    errs <- c(
      validate_against_schema(metadata, schema$metadata, path = "metadata"),
      validate_against_schema(data, schema$data_points, path = "data_points"),
      validate_links(metadata, data, schema)
    )
    if (length(errs)) .abort_validation(study_id, errs)
  }

  study_dir <- .study_dir(proj_path, study_id)
  verification <- NULL
  if (fs::dir_exists(study_dir)) {
    if (!overwrite) {
      rlang::abort(paste0("Study '", study_id, "' already exists. Use `overwrite = TRUE` to replace it."))
    }
    # Keep reviewers' decisions; changed values show up as "changed".
    vf <- fs::path(study_dir, "verification.csv")
    if (fs::file_exists(vf)) verification <- readLines(vf, warn = FALSE)
    fs::dir_delete(study_dir)
  }
  fs::dir_create(study_dir)
  if (!is.null(verification)) writeLines(verification, fs::path(study_dir, "verification.csv"))

  yaml::write_yaml(metadata, fs::path(study_dir, "metadata.yml"))
  .write_data_csv(data, schema, fs::path(study_dir, "data.csv"))

  invisible(study_dir)
}

#' Read one study from a metawoRld project
#'
#' @param path Project root.
#' @param study_id Study identifier (as given to [add_study_data()]).
#' @return A list with `metadata` (list) and `data` (tibble). `data.csv` is
#'   read with column types taken from the schema.
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
  desanitize_id(fs::path_file(fs::dir_ls(data_path, type = "directory")))
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

#' Create template files for manual data entry
#'
#' Writes `metadata.yml` with a placeholder for every schema field (one
#' example element for arrays of objects) and a header-only `data.csv`.
#'
#' @param path Project root.
#' @param study_id Study identifier.
#' @param overwrite Replace an existing study folder? Defaults to `FALSE`.
#' @return The study folder, invisibly.
#' @export
add_study_template <- function(path = ".", study_id, overwrite = FALSE) {
  proj_path <- .check_project(path)
  if (missing(study_id) || !rlang::is_scalar_character(study_id) || !nzchar(study_id)) {
    rlang::abort("`study_id` must be a single non-empty string.")
  }
  study_id <- normalize_identifier(study_id)
  schema <- get_schema(proj_path, "extraction")
  study_dir <- .study_dir(proj_path, study_id)
  if (fs::dir_exists(study_dir) && !overwrite) {
    rlang::abort(paste0("Study '", study_id, "' already exists. Use `overwrite = TRUE` to replace it."))
  }
  fs::dir_create(study_dir)

  meta <- .template_value(schema$metadata)
  meta$study_id <- study_id
  yaml::write_yaml(meta, fs::path(study_dir, "metadata.yml"))

  cols <- .schema_fields(schema$data_points$`_items`)
  writeLines(paste(cols, collapse = ","), fs::path(study_dir, "data.csv"))
  invisible(study_dir)
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
  fs::path(proj_path, "data", sanitize_id(normalize_identifier(study_id)))
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
  list(metadata = metadata, data = data)
}

.study_errors <- function(proj_path, study_id, check_links) {
  schema <- get_schema(proj_path, "extraction")
  study_dir <- .study_dir(proj_path, study_id)
  if (!fs::dir_exists(study_dir)) return(paste0("Study folder not found: ", study_dir))
  if (!fs::file_exists(fs::path(study_dir, "data.csv"))) return("data.csv not found.")
  s <- tryCatch(.read_study_dir(study_dir, schema), error = function(e) e)
  if (inherits(s, "error")) return(conditionMessage(s))
  c(
    validate_against_schema(s$metadata, schema$metadata, path = "metadata"),
    validate_against_schema(s$data, schema$data_points, path = "data_points"),
    if (check_links) validate_links(s$metadata, s$data, schema)
  )
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
