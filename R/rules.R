.default_checks <- list(
  statistic_consistency = "error",
  missing_dispersion = "warning",
  sample_size = "warning",
  duplicate = "warning",
  unknown_variable = "warning",
  unit_mismatch = "warning",
  out_of_range = "warning"
)

#' Read the project's value rules
#'
#' Reads `_rules.yml` (see the comments in the file created by
#' [create_metawoRld()] or [use_templates()]). Missing files or sections fall
#' back to the defaults: no variable vocabulary and all checks enabled.
#'
#' @param path Project root.
#' @return A list with `variables` (named list) and `checks` (named list of
#'   severities: `"error"`, `"warning"` or `"off"`).
#' @export
read_rules <- function(path = ".") {
  proj_path <- .check_project(path)
  f <- fs::path(proj_path, "_rules.yml")
  r <- if (fs::file_exists(f)) yaml::read_yaml(f) %||% list() else list()
  vars <- r$variables %||% list()
  if (length(vars) && is.null(names(vars))) rlang::abort("`variables` in _rules.yml must be a mapping of variable names.")
  checks <- utils::modifyList(.default_checks, r$checks %||% list())
  bad <- setdiff(unlist(checks), c("error", "warning", "off"))
  if (length(bad)) rlang::abort(paste0("Check severities must be error, warning or off; got: ", paste(bad, collapse = ", ")))
  list(variables = vars, checks = checks)
}

#' Map variable names to their canonical names
#'
#' @param x Character vector of variable names as reported.
#' @param rules Rules from [read_rules()].
#' @return Character vector of canonical names (`NA` where unknown).
#' @export
canonical_variable <- function(x, rules) {
  lookup <- .variable_lookup(rules)
  unname(lookup[tolower(trimws(as.character(x)))])
}

#' Add a variable or synonym to the project's rules
#'
#' Used by [review_values()] when a reviewer maps an unknown variable name.
#' Creates the variable if it does not exist yet. Comments at the top of
#' `_rules.yml` are kept; other comments are lost.
#'
#' @param path Project root.
#' @param synonym The name as reported in a paper.
#' @param variable The canonical variable name.
#' @param unit Optional expected unit (only set when the variable is new).
#' @return The updated rules, invisibly.
#' @export
add_variable_synonym <- function(path = ".", synonym, variable, unit = NULL) {
  proj_path <- .check_project(path)
  for (a in c("synonym", "variable")) {
    v <- get(a)
    if (!rlang::is_scalar_character(v) || !nzchar(trimws(v))) rlang::abort(paste0("`", a, "` must be a single non-empty string."))
  }
  f <- fs::path(proj_path, "_rules.yml")
  if (!fs::file_exists(f)) fs::file_copy(system.file("templates", "_rules.yml", package = "metawoRld"), f)
  rules <- read_rules(proj_path)
  existing <- canonical_variable(synonym, rules)
  if (!is.na(existing) && existing != variable) {
    rlang::abort(paste0("'", synonym, "' is already mapped to '", existing, "'."))
  }
  v <- rules$variables[[variable]] %||% list()
  if (is.null(rules$variables[[variable]]) && !is.null(unit) && nzchar(unit)) v$unit <- unit
  if (!identical(tolower(synonym), tolower(variable))) {
    v$synonyms <- as.list(unique(c(unlist(v$synonyms), synonym)))
  }
  rules$variables[[variable]] <- if (length(v)) v else list(synonyms = list())
  .write_rules(f, rules)
  invisible(rules)
}

#' Check extracted values for plausibility and consistency
#'
#' Runs the built-in checks configured in `_rules.yml` on every data point:
#' internal consistency of each statistic type (e.g. Q1 <= median <= Q3,
#' SD >= 0, n/N matching the reported percentage), missing dispersion,
#' sample sizes larger than the group, duplicates, unknown variable names,
#' unit mismatches and implausible values. Nothing is changed; the flags guide
#' human review in [review_values()].
#'
#' @param path Project root.
#' @param study_ids Studies to check. Defaults to all.
#' @param rules Rules to use. Defaults to [read_rules()].
#' @return A tibble with `study_id`, `measurement_id`, `check`, `severity`
#'   and `message` (one row per flag).
#' @export
check_values <- function(path = ".", study_ids = NULL, rules = read_rules(path)) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  ids <- study_ids %||% list_studies(proj_path)
  flags <- lapply(ids, function(id) {
    s <- tryCatch(.read_study_dir(.study_dir(proj_path, id), schema), error = function(e) NULL)
    if (is.null(s) || nrow(s$data) == 0) return(NULL)
    f <- .check_study_values(s$data, s$metadata, rules)
    if (nrow(f)) f$study_id <- id
    f
  })
  out <- dplyr::bind_rows(.empty_flags(), dplyr::bind_rows(flags))
  dplyr::relocate(out, "study_id")
}

# ---- internals ---------------------------------------------------------------

.empty_flags <- function() {
  dplyr::tibble(study_id = character(0), measurement_id = character(0), check = character(0),
                severity = character(0), message = character(0))
}

.variable_lookup <- function(rules) {
  vars <- rules$variables
  if (!length(vars)) return(stats::setNames(character(0), character(0)))
  keys <- character(0)
  vals <- character(0)
  for (v in names(vars)) {
    syn <- c(v, as.character(unlist(vars[[v]]$synonyms)))
    keys <- c(keys, tolower(trimws(syn)))
    vals <- c(vals, rep(v, length(syn)))
  }
  stats::setNames(vals, keys)
}

.write_rules <- function(file, rules) {
  lines <- readLines(file, warn = FALSE)
  first <- which(!grepl("^\\s*#", lines) & nzchar(trimws(lines)))[1]
  header <- if (is.na(first)) lines else lines[seq_len(first - 1)]
  vars <- rules$variables
  body <- yaml::as.yaml(list(checks = rules$checks))
  vars_yaml <- if (length(vars)) yaml::as.yaml(list(variables = vars)) else "variables: {}\n"
  writeLines(c(header, sub("\n$", "", vars_yaml), "", sub("\n$", "", body)), file)
}

.check_study_values <- function(d, meta, rules) {
  sev <- rules$checks
  out <- list()
  add <- function(i, check, msg) {
    if (identical(sev[[check]], "off")) return()
    out[[length(out) + 1]] <<- list(measurement_id = as.character(d$measurement_id[i]), check = check,
                                    severity = sev[[check]], message = msg)
  }
  col <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
  num <- function(nm) suppressWarnings(as.numeric(col(nm)))
  v1 <- num("value1"); v2 <- num("value2"); v3 <- num("value3"); n <- num("sample_size")
  st <- as.character(col("statistic_type"))
  group_n <- .lookup_field(meta$groups, "group_id", "sample_size")
  method_unit <- .lookup_field(meta$methods, "method_id", "unit")
  canon <- if (length(rules$variables)) canonical_variable(col("variable_name"), rules) else rep(NA_character_, nrow(d))
  fmt <- function(x) format(x, digits = 6)

  for (i in seq_len(nrow(d))) {
    s <- st[i]
    # statistic consistency and missing dispersion
    if (s %in% c("mean_sd", "mean_se", "geometric_mean_sd")) {
      if (!is.na(v2[i]) && v2[i] < 0) add(i, "statistic_consistency", paste0("Dispersion (value2 = ", fmt(v2[i]), ") is negative."))
      if (s == "geometric_mean_sd" && !is.na(v1[i]) && v1[i] <= 0) add(i, "statistic_consistency", "Geometric mean must be positive.")
      if (is.na(v2[i])) add(i, "missing_dispersion", "No SD/SE reported (value2 is empty).")
    }
    if (s %in% c("median_iqr", "median_range", "mean_ci95")) {
      if (is.na(v2[i]) || is.na(v3[i])) {
        add(i, "missing_dispersion", "Lower and upper bounds (value2, value3) should both be reported.")
      } else if (!is.na(v1[i]) && !(v2[i] <= v1[i] && v1[i] <= v3[i])) {
        add(i, "statistic_consistency", sprintf("Expected lower <= central <= upper, got %s, %s, %s.", fmt(v2[i]), fmt(v1[i]), fmt(v3[i])))
      }
    }
    if (s == "n_percent") {
      if (!is.na(v2[i]) && (v2[i] < 0 || v2[i] > 100)) add(i, "statistic_consistency", paste0("Percentage ", fmt(v2[i]), " is outside 0-100."))
      if (!is.na(v1[i]) && !is.na(n[i])) {
        if (v1[i] > n[i]) add(i, "statistic_consistency", sprintf("Count %s is larger than n = %s.", fmt(v1[i]), fmt(n[i])))
        else if (!is.na(v2[i]) && n[i] > 0 && abs(100 * v1[i] / n[i] - v2[i]) > 1) {
          add(i, "statistic_consistency", sprintf("%s/%s = %.1f%%, but %s%% is reported.", fmt(v1[i]), fmt(n[i]), 100 * v1[i] / n[i], fmt(v2[i])))
        }
      }
    }
    if (s == "count" && !is.na(v1[i])) {
      if (v1[i] < 0 || v1[i] != round(v1[i])) add(i, "statistic_consistency", "A count must be a non-negative whole number.")
      if (!is.na(n[i]) && v1[i] > n[i]) add(i, "statistic_consistency", sprintf("Count %s is larger than n = %s.", fmt(v1[i]), fmt(n[i])))
    }
    if (s == "proportion" && !is.na(v1[i]) && (v1[i] < 0 || v1[i] > 1)) {
      add(i, "statistic_consistency", paste0("Proportion ", fmt(v1[i]), " is outside 0-1",
                                            if (v1[i] > 1 && v1[i] <= 100) " (a percentage? use n_percent)" else "", "."))
    }
    # sample size vs group
    g <- as.character(col("group_id")[i])
    gn <- suppressWarnings(as.numeric(group_n[g]))
    if (!is.na(n[i]) && n[i] <= 0) add(i, "sample_size", "Sample size must be positive.")
    if (!is.na(n[i]) && length(gn) && !is.na(gn) && n[i] > gn) {
      add(i, "sample_size", sprintf("n = %s is larger than the group's sample size (%s).", fmt(n[i]), fmt(gn)))
    }
    # variables, units and ranges
    if (length(rules$variables)) {
      cv <- canon[i]
      if (is.na(cv)) {
        add(i, "unknown_variable", paste0("'", col("variable_name")[i], "' is not a known variable or synonym in _rules.yml."))
      } else {
        rule <- rules$variables[[cv]]
        unit <- as.character(col("unit")[i])
        if (is.na(unit) || !nzchar(unit)) unit <- unname(method_unit[as.character(col("method_id")[i])])
        factor <- 1
        if (!is.null(rule$unit) && length(unit) && !is.na(unit) && nzchar(unit) && tolower(unit) != tolower(rule$unit)) {
          idx <- match(tolower(unit), tolower(names(rule$convert)))
          conv <- if (is.na(idx)) NULL else rule$convert[[idx]]
          if (is.null(conv)) {
            add(i, "unit_mismatch", sprintf("Unit '%s' differs from the expected '%s' and no conversion is defined.", unit, rule$unit))
            factor <- NA
          } else factor <- as.numeric(conv)
        }
        rng <- as.numeric(unlist(rule$range))
        central <- s %in% c("mean_sd", "mean_se", "mean_ci95", "median_iqr", "median_range", "geometric_mean_sd")
        if (length(rng) == 2 && central && !is.na(v1[i]) && !is.na(factor)) {
          val <- v1[i] * factor
          if ((!is.na(rng[1]) && val < rng[1]) || (!is.na(rng[2]) && val > rng[2])) {
            add(i, "out_of_range", sprintf("%s %s is outside the plausible range [%s, %s]%s.", fmt(val), rule$unit %||% "",
                                           fmt(rng[1]), fmt(rng[2]), if (factor != 1) paste0(" (converted from ", fmt(v1[i]), " ", unit, ")") else ""))
          }
        }
      }
    }
  }
  # duplicates
  key_cols <- intersect(c("method_id", "group_id", "variable_name", "timepoint", "statistic_type"), names(d))
  if (length(key_cols)) {
    key <- do.call(paste, c(lapply(d[key_cols], function(x) tolower(trimws(ifelse(is.na(x), "", as.character(x))))), sep = "\r"))
    dup <- key %in% key[duplicated(key)]
    for (i in which(dup)) {
      others <- setdiff(d$measurement_id[key == key[i]], d$measurement_id[i])
      add(i, "duplicate", paste0("Same group, variable, timepoint and statistic as ", paste(others, collapse = ", "), "."))
    }
  }
  if (!length(out)) return(.empty_flags()[, -1])
  dplyr::bind_rows(out)
}

.lookup_field <- function(objs, id_field, field) {
  if (is.null(objs) || !length(objs)) return(stats::setNames(character(0), character(0)))
  df <- .rows_to_df(if (is.data.frame(objs)) lapply(seq_len(nrow(objs)), function(k) .df_row(objs, k)) else objs)
  if (!all(c(id_field, field) %in% names(df))) return(stats::setNames(character(0), character(0)))
  stats::setNames(as.character(df[[field]]), as.character(df[[id_field]]))
}
