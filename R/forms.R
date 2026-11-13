#' Read a form from a metawoRld project
#'
#' Forms live in `forms/<name>.yml` and use the node format described in
#' `SPEC.md` (section 7). `forms/extraction.yml` is required; others (e.g.
#' `screening`, `rob2`) are optional.
#'
#' @param path Project root. Defaults to the working directory.
#' @param form Form name, e.g. `"extraction"` (default) or `"screening"`.
#'   `"assessment"` is accepted as an alias of `"screening"`.
#' @return The form as a nested list, including its `_form` header.
#' @export
get_schema <- function(path = ".", form = "extraction") {
  proj_path <- .check_project(path)
  if (identical(form, "assessment")) form <- "screening"
  if (!grepl("^[a-z0-9_-]+$", form)) rlang::abort("`form` must be a form name such as \"extraction\".")
  file <- fs::path(proj_path, "forms", paste0(form, ".yml"))
  if (!fs::file_exists(file)) {
    rlang::abort(c(paste0("Form not found: ", file),
                   "i" = "Run `use_templates()` to add the default forms to the project."))
  }
  out <- tryCatch(yaml::read_yaml(file), error = function(e) {
    rlang::abort(paste0("Could not parse ", file, ": ", conditionMessage(e)))
  })
  if (!is.list(out) || is.null(out$`_type`)) {
    rlang::abort(paste0(file, " is not a valid form (missing top-level `_type`)."))
  }
  bad <- .boolean_keys(out)
  if (length(bad)) {
    rlang::abort(c(
      paste0(file, " has field names that YAML reads as TRUE/FALSE: ", paste(bad, collapse = ", ")),
      "i" = "Rename fields such as `n`, `y`, `yes`, `no`, `on` or `off` (e.g. `n` -> `sample_size`)."
    ))
  }
  out$`_form` <- utils::modifyList(list(name = form, kind = "extraction", version = "1.0.0"), out$`_form` %||% list())
  out
}

#' @rdname get_schema
#' @export
read_form <- get_schema

#' List the forms of a project
#'
#' @param path Project root.
#' @return A tibble with `name`, `kind` and `version` of each form.
#' @export
list_forms <- function(path = ".") {
  proj_path <- .check_project(path)
  files <- fs::dir_ls(fs::path(proj_path, "forms"), glob = "*.yml")
  rows <- lapply(files, function(f) {
    nm <- fs::path_ext_remove(fs::path_file(f))
    h <- get_schema(proj_path, nm)$`_form`
    dplyr::tibble(name = nm, kind = as.character(h$kind), version = as.character(h$version))
  })
  dplyr::bind_rows(c(list(dplyr::tibble(name = character(0), kind = character(0), version = character(0))), unname(rows)))
}

#' Write JSON Schemas and Table Schemas for the project's forms
#'
#' Writes `forms/<name>.schema.json` (JSON Schema 2020-12) for every form,
#' and `forms/extraction.data.tableschema.json` (Frictionless Table Schema
#' for `data.csv`). Tools in any language can validate submissions with these
#' files. They are generated: do not edit them by hand.
#'
#' @param path Project root.
#' @return Paths of the written files, invisibly.
#' @export
export_schemas <- function(path = ".") {
  proj_path <- .check_project(path)
  out <- character(0)
  for (nm in list_forms(proj_path)$name) {
    f <- get_schema(proj_path, nm)
    js <- c(list(`$schema` = "https://json-schema.org/draft/2020-12/schema",
                 title = paste0("metawoRld form: ", nm),
                 `x-metawoRld-form-version` = f$`_form`$version),
            .json_schema(f))
    file <- fs::path(proj_path, "forms", paste0(nm, ".schema.json"))
    jsonlite::write_json(js, file, auto_unbox = TRUE, pretty = TRUE, null = "null")
    out <- c(out, file)
  }
  ts_file <- fs::path(proj_path, "forms", "extraction.data.tableschema.json")
  jsonlite::write_json(table_schema(get_schema(proj_path, "extraction")), ts_file, auto_unbox = TRUE, pretty = TRUE)
  invisible(c(out, ts_file))
}

#' Frictionless Table Schema for the data points of a form
#'
#' @param form A form from [get_schema()] (with `data_points`).
#' @return A list that serializes to a Table Schema.
#' @export
table_schema <- function(form) {
  items <- form$data_points$`_items`
  fields <- lapply(.schema_fields(items), function(f) {
    node <- items[[f]]
    type <- switch(node$`_type`, integer = "integer", number = "number", boolean = "boolean", "string")
    fld <- list(name = f, type = type, description = node$`_description` %||% "")
    cons <- list()
    if (.is_required(node)) cons$required <- TRUE
    if (identical(node$`_type`, "enum")) cons$enum <- as.list(as.character(unlist(node$`_values`)))
    if (length(cons)) fld$constraints <- cons
    fld
  })
  list(fields = fields, missingValues = list(""))
}

# ---- internals ---------------------------------------------------------------

.json_schema <- function(node) {
  desc <- node$`_description`
  base <- switch(
    node$`_type`,
    object = {
      fields <- .schema_fields(node)
      props <- lapply(fields, function(f) .json_schema(node[[f]]))
      names(props) <- fields
      req <- fields[vapply(fields, function(f) .is_required(node[[f]]), logical(1))]
      list(type = "object", properties = props, required = I(req))
    },
    array = list(type = "array", items = .json_schema(node$`_items`)),
    enum = list(type = "string", enum = I(as.character(unlist(node$`_values`)))),
    integer = list(type = "integer"),
    number = list(type = "number"),
    boolean = list(type = "boolean"),
    list(type = "string")
  )
  if (!.is_required(node) && node$`_type` != "array") {
    base$type <- I(c(base$type, "null"))
    if (!is.null(base$enum)) base$enum <- I(c(base$enum, NA))
  }
  if (!is.null(desc)) base <- c(list(description = desc), base)
  base
}

.boolean_keys <- function(x, path = "") {
  if (!is.list(x) || is.null(names(x))) return(character(0))
  out <- character(0)
  for (nm in names(x)) {
    p <- if (nzchar(path)) paste0(path, ".", nm) else nm
    if (nm %in% c("TRUE", "FALSE")) out <- c(out, p)
    out <- c(out, .boolean_keys(x[[nm]], p))
  }
  out
}
