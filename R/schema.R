#' Validate data against a metawoRld schema
#'
#' Checks types, required fields and enum values recursively. Missing values
#' (`NULL` or `NA`) in required fields are reported; empty arrays are allowed.
#' Data frames are accepted wherever the schema expects an array of objects
#' (each row is validated as an object), which is what `ellmer` and
#' `readr` return.
#'
#' @param data The data to validate (list, data frame or scalar).
#' @param schema A schema node, e.g. `get_schema(path)` or a sub-node such as
#'   `get_schema(path)$data_points`.
#' @param path Label used as the root of the error locations.
#' @param strict If `TRUE`, fields that are not defined in the schema are
#'   reported as errors.
#' @return A character vector of error messages; empty if the data are valid.
#' @export
#' @examples
#' schema <- list(`_type` = "object",
#'                n = list(`_type` = "integer"),
#'                label = list(`_type` = "string", `_required` = FALSE))
#' validate_against_schema(list(n = 3), schema)
#' validate_against_schema(list(n = 2.5, label = 1), schema)
validate_against_schema <- function(data, schema, path = "root", strict = FALSE) {
  if (!is.list(schema)) rlang::abort("`schema` must be a list.")
  .validate_node(data, schema, path, strict)
}

.validate_node <- function(x, node, path, strict) {
  type <- node$`_type`
  if (is.null(type)) return(paste0(path, ": schema node has no `_type`."))

  if (.is_missing(x, type)) {
    if (.is_required(node)) return(paste0(path, ": required value is missing."))
    return(character(0))
  }

  switch(
    type,
    object = {
      if (is.data.frame(x)) {
        if (nrow(x) != 1) {
          return(paste0(path, ": expected one object but got a data frame with ", nrow(x), " rows."))
        }
        x <- .df_row(x, 1)
      }
      if (!is.list(x)) return(paste0(path, ": expected an object but got ", .describe(x), "."))
      fields <- .schema_fields(node)
      errs <- character(0)
      for (f in fields) {
        errs <- c(errs, .validate_node(x[[f]], node[[f]], paste0(path, ".", f), strict))
      }
      if (strict) {
        extra <- setdiff(names(x), fields)
        if (length(extra)) errs <- c(errs, paste0(path, ": unknown field(s): ", paste(extra, collapse = ", ")))
      }
      errs
    },
    array = {
      items <- node$`_items`
      if (is.null(items)) return(paste0(path, ": array schema has no `_items`."))
      if (is.data.frame(x)) {
        n <- nrow(x)
        get <- function(i) .df_row(x, i)
      } else if (is.list(x) || is.atomic(x)) {
        n <- length(x)
        get <- function(i) x[[i]]
      } else {
        return(paste0(path, ": expected an array but got ", .describe(x), "."))
      }
      unlist(lapply(seq_len(n), function(i) {
        .validate_node(get(i), items, paste0(path, "[", i, "]"), strict)
      }), use.names = FALSE) %||% character(0)
    },
    string = if (!.is_scalar(x, is.character)) paste0(path, ": expected a string but got ", .describe(x), "."),
    integer = if (!.is_scalar(x, is.numeric) || !isTRUE(all.equal(x, round(x)))) {
      paste0(path, ": expected an integer but got ", .describe(x), ".")
    },
    number = if (!.is_scalar(x, is.numeric)) paste0(path, ": expected a number but got ", .describe(x), "."),
    boolean = if (!.is_scalar(x, is.logical)) paste0(path, ": expected TRUE/FALSE but got ", .describe(x), "."),
    enum = {
      values <- unlist(node$`_values`)
      if (!.is_scalar(x, is.character)) {
        paste0(path, ": expected one of [", paste(values, collapse = ", "), "] but got ", .describe(x), ".")
      } else if (!x %in% values) {
        paste0(path, ": '", x, "' is not one of [", paste(values, collapse = ", "), "].")
      }
    },
    paste0(path, ": unknown schema type '", type, "'.")
  ) %||% character(0)
}

#' Validate links between data points and metadata
#'
#' Uses the `_ref` keys in the `data_points` part of the schema (e.g.
#' `_ref: groups.group_id`) to check that every id used in the data is defined
#' in the corresponding metadata array.
#'
#' @param metadata Study metadata (list).
#' @param data Data points (data frame or list of objects).
#' @param schema The full extraction schema.
#' @return A character vector of error messages; empty if all links are valid.
#' @export
validate_links <- function(metadata, data, schema) {
  refs <- schema_refs(schema)
  if (nrow(refs) == 0 || .n_rows(data) == 0) return(character(0))
  if (!is.data.frame(data)) data <- .rows_to_df(data)
  errs <- character(0)
  for (i in seq_len(nrow(refs))) {
    r <- refs[i, ]
    if (!r$field %in% names(data)) next
    used <- unique(as.character(data[[r$field]]))
    used <- used[!is.na(used) & nzchar(used)]
    defined <- .ids_in(metadata[[r$array]], r$id_field)
    bad <- setdiff(used, c(defined, unlist(r$allow)))
    if (length(bad)) {
      errs <- c(errs, paste0(
        "data_points.", r$field, ": value(s) ", paste0("'", bad, "'", collapse = ", "),
        " not defined in metadata.", r$array, "[].", r$id_field,
        if (length(defined)) paste0(" (defined: ", paste(defined, collapse = ", "), ")") else " (none defined)"
      ))
    }
  }
  errs
}

#' List the cross-references defined in a schema
#'
#' @param schema The full extraction schema.
#' @return A data frame with columns `field`, `array`, `id_field` and `allow`
#'   (a list column of extra allowed values).
#' @export
schema_refs <- function(schema) {
  items <- schema$data_points$`_items`
  out <- data.frame(field = character(0), array = character(0), id_field = character(0))
  out$allow <- list()
  if (is.null(items)) return(out)
  for (f in .schema_fields(items)) {
    ref <- items[[f]]$`_ref`
    if (is.null(ref)) next
    parts <- strsplit(ref, ".", fixed = TRUE)[[1]]
    if (length(parts) != 2) rlang::abort(paste0("Invalid `_ref` for '", f, "': use '<array>.<id_field>'."))
    row <- data.frame(field = f, array = parts[1], id_field = parts[2])
    row$allow <- list(as.character(unlist(items[[f]]$`_ref_allow`)))
    out <- rbind(out, row)
  }
  out
}

#' Column types for a study's data.csv derived from the schema
#'
#' Used when reading `data.csv` files so that, for example, a string field
#' containing `"0.03"` is not silently turned into a number.
#'
#' @param schema The full extraction schema.
#' @param columns Optional column names present in the file; only these get
#'   a specification.
#' @return A [readr::cols()] specification.
#' @export
schema_col_types <- function(schema, columns = NULL) {
  items <- schema$data_points$`_items`
  spec <- list()
  fields <- .schema_fields(items)
  if (!is.null(columns)) fields <- intersect(fields, columns)
  for (f in fields) {
    spec[[f]] <- switch(
      items[[f]]$`_type`,
      string = , enum = readr::col_character(),
      integer = , number = readr::col_double(),
      boolean = readr::col_logical(),
      readr::col_character()
    )
  }
  do.call(readr::cols, c(spec, list(.default = readr::col_guess())))
}

# ---- helpers ---------------------------------------------------------------

.schema_fields <- function(node) {
  nms <- names(node)
  nms[!startsWith(nms, "_")]
}

.is_required <- function(node) !identical(node$`_required`, FALSE)

.is_missing <- function(x, type) {
  if (is.null(x)) return(TRUE)
  if (type == "array") return(FALSE)
  if (is.data.frame(x)) return(nrow(x) == 0)
  if (is.list(x)) return(length(x) == 0)
  length(x) == 1 && is.na(x)
}

.is_scalar <- function(x, pred) length(x) == 1 && pred(x) && !is.na(x)

.describe <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.data.frame(x)) return(paste0("a data frame (", nrow(x), " rows)"))
  if (is.list(x)) return(paste0("a list of length ", length(x)))
  if (length(x) != 1) return(paste0("a ", class(x)[1], " vector of length ", length(x)))
  paste0(class(x)[1], " '", format(x), "'")
}

# One data-frame row as a plain list, unwrapping list-columns.
.df_row <- function(df, i) {
  lapply(df, function(col) if (is.list(col)) col[[i]] else col[i])
}

.n_rows <- function(x) if (is.data.frame(x)) nrow(x) else length(x)

# List of objects (rows) -> data frame; NULLs become NA, non-scalars list-cols.
.rows_to_df <- function(rows) {
  if (length(rows) == 0) return(data.frame())
  rows <- lapply(rows, function(r) {
    lapply(r, function(v) if (is.null(v)) NA else if (length(v) == 1 && !is.list(v)) v else list(v))
  })
  as.data.frame(data.table::rbindlist(rows, fill = TRUE, use.names = TRUE))
}

# Ids defined in a metadata array (list of objects or data frame).
.ids_in <- function(x, id_field) {
  if (is.null(x)) return(character(0))
  ids <- if (is.data.frame(x)) x[[id_field]] else vapply(x, function(o) {
    v <- if (is.list(o)) o[[id_field]] else NULL
    if (is.null(v) || length(v) != 1) NA_character_ else as.character(v)
  }, character(1))
  ids <- as.character(ids)
  unique(ids[!is.na(ids) & nzchar(ids)])
}

# Recursively convert data frames to lists of rows and drop missing scalars so
# YAML files stay row-oriented and free of `.na` markers.
.clean_for_yaml <- function(x) {
  if (is.data.frame(x)) {
    x <- lapply(seq_len(nrow(x)), function(i) .df_row(x, i))
  }
  if (is.list(x)) {
    named <- !is.null(names(x))
    x <- lapply(x, .clean_for_yaml)
    if (named) x <- x[!vapply(x, is.null, logical(1))]
    return(x)
  }
  if (length(x) == 1 && is.na(x)) return(NULL)
  if (is.factor(x)) x <- as.character(x)
  x
}

.check_project <- function(path) {
  if (!rlang::is_scalar_character(path) || !nzchar(path)) {
    rlang::abort("`path` must be a single non-empty string.")
  }
  proj_path <- fs::path_norm(path)
  if (!fs::file_exists(fs::path(proj_path, "_metawoRld.yml"))) {
    rlang::abort(paste0("Not a metawoRld project (no _metawoRld.yml): ", proj_path))
  }
  proj_path
}
