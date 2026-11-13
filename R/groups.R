# Groups, subgroups and categories (SPEC.md section 7.1).
#
# A study's groups form a tree: `parent_group_id` says that a group is part
# of a larger group (early-onset PE within PE; PE within the whole cohort).
# Groups have facts: the fields of `groups` in the extraction form (e.g.
# `category`, `onset`). A subgroup inherits the facts of its parent unless it
# sets its own. Categories in `_rules.yml` are rules over these facts, so one
# group can belong to several categories (early-onset PE is both "PE" and
# "PE_early"). Analyses pick exactly one level per study and arm, so no
# participant is counted twice.

# Fields of a group that are structure or bookkeeping, not facts.
.group_structural <- c("group_id", "name", "definition", "sample_size", "parent_group_id", "notes")

# Groups as a data frame with character columns.
.groups_df <- function(groups) {
  if (is.null(groups) || !length(groups)) {
    return(data.frame(group_id = character(0), name = character(0), parent_group_id = character(0), stringsAsFactors = FALSE))
  }
  df <- .rows_to_df(if (is.data.frame(groups)) lapply(seq_len(nrow(groups)), function(k) .df_row(groups, k)) else groups)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (c in c("group_id", "name", "parent_group_id")) if (!c %in% names(df)) df[[c]] <- NA_character_
  df[] <- lapply(df, function(x) if (is.list(x)) vapply(x, function(v) paste(unlist(v), collapse = "; "), character(1)) else x)
  df$group_id <- as.character(df$group_id)
  df$parent_group_id <- as.character(df$parent_group_id)
  df$parent_group_id[!is.na(df$parent_group_id) & !nzchar(df$parent_group_id)] <- NA
  df
}

# Ancestors of a group, nearest first (stops at cycles).
.ancestors <- function(df, id) {
  out <- character(0)
  p <- df$parent_group_id[match(id, df$group_id)]
  while (length(p) && !is.na(p) && !p %in% c(out, id)) {
    out <- c(out, p)
    p <- df$parent_group_id[match(p, df$group_id)]
  }
  out
}

.descendants <- function(df, id) {
  kids <- df$group_id[!is.na(df$parent_group_id) & df$parent_group_id == id]
  unique(c(kids, unlist(lapply(kids, function(k) .descendants(df, k)))))
}

# Are two groups the same, or is one inside the other?
.overlap <- function(df, a, b) a == b || a %in% .ancestors(df, b) || b %in% .ancestors(df, a)

# Problems with the group tree (validation errors).
.group_tree_errors <- function(groups) {
  df <- .groups_df(groups)
  if (!nrow(df)) return(character(0))
  out <- character(0)
  bad <- setdiff(stats::na.omit(df$parent_group_id), df$group_id)
  if (length(bad)) out <- c(out, paste0("metadata.groups[].parent_group_id: '", bad, "' is not a group_id of this study."))
  self <- df$group_id[!is.na(df$parent_group_id) & df$parent_group_id == df$group_id]
  if (length(self)) out <- c(out, paste0("metadata.groups: '", self, "' is its own parent_group_id."))
  for (id in setdiff(df$group_id, self)) {
    seen <- id; p <- df$parent_group_id[match(id, df$group_id)]
    while (!is.na(p)) {
      if (p %in% seen) { out <- c(out, paste0("metadata.groups: parent_group_id forms a loop through '", id, "'.")); break }
      seen <- c(seen, p); p <- df$parent_group_id[match(p, df$group_id)]
    }
  }
  unique(out)
}

# Facts of each group, with inherited values filled in.
.group_facts <- function(groups) {
  df <- .groups_df(groups)
  facts <- setdiff(names(df), .group_structural)
  out <- df[, c("group_id", "name", "parent_group_id", facts), drop = FALSE]
  for (f in facts) {
    own <- as.character(df[[f]])
    own[!is.na(own) & !nzchar(own)] <- NA
    val <- own
    for (i in which(is.na(own))) {
      for (a in .ancestors(df, df$group_id[i])) {
        v <- own[match(a, df$group_id)]
        if (!is.na(v)) { val[i] <- v; break }
      }
    }
    out[[f]] <- val
  }
  out
}

#' Which categories each study group belongs to
#'
#' Categories are defined in `_rules.yml`. A category with `where` matches
#' groups whose facts (fields of `groups` in the extraction form, inherited
#' from the parent group unless set on the subgroup) have the given values:
#'
#' ```yaml
#' group_categories:
#'   PE:       {where: {category: PE}, synonyms: [Preeclampsia]}
#'   PE_early: {where: {category: PE, onset: early}}
#' ```
#'
#' A category without `where` matches groups whose (inherited) `category`
#' field, or own `name`, is the category name or one of its synonyms. A
#' group can belong to several categories.
#'
#' @param groups The `groups` array of a study's metadata.
#' @param rules Rules from [read_rules()].
#' @return A tibble with one row per group and category it belongs to:
#'   `group_id`, `category`.
#' @export
group_memberships <- function(groups, rules) {
  facts <- .group_facts(groups)
  empty <- dplyr::tibble(group_id = character(0), category = character(0))
  cats <- rules$group_categories
  if (!nrow(facts) || !length(cats)) return(empty)
  lookup <- .category_lookup(rules)
  norm <- function(x) tolower(trimws(as.character(x)))
  # the `category` fact mapped through names and synonyms
  cat_fact <- if ("category" %in% names(facts)) unname(lookup[norm(facts$category)]) else rep(NA_character_, nrow(facts))
  cat_fact[is.na(cat_fact) & "category" %in% names(facts)] <- facts$category[is.na(cat_fact) & "category" %in% names(facts)]
  by_name <- unname(lookup[norm(facts$name)])
  rows <- list()
  for (cn in names(cats)) {
    w <- cats[[cn]]$where
    hit <- if (length(w)) {
      ok <- rep(TRUE, nrow(facts))
      for (f in names(w)) {
        allowed <- norm(unlist(w[[f]]))
        have <- if (f == "category") cat_fact else if (f %in% names(facts)) facts[[f]] else rep(NA_character_, nrow(facts))
        if (f == "category") allowed <- unique(c(allowed, norm(unname(lookup[allowed]))))
        ok <- ok & !is.na(have) & norm(have) %in% allowed
      }
      ok
    } else {
      (!is.na(cat_fact) & cat_fact == cn) | (is.na(cat_fact) & !is.na(by_name) & by_name == cn)
    }
    if (any(hit)) rows[[length(rows) + 1]] <- dplyr::tibble(group_id = facts$group_id[hit], category = cn)
  }
  dplyr::bind_rows(c(list(empty), rows))
}

#' Assign study groups to group categories
#'
#' The most specific category of each group (see [group_memberships()]):
#' a category defined with more conditions wins.
#'
#' @inheritParams group_memberships
#' @return A character vector of categories named by `group_id` (`NA` where
#'   no category applies).
#' @export
group_category <- function(groups, rules) {
  df <- .groups_df(groups)
  out <- stats::setNames(rep(NA_character_, nrow(df)), df$group_id)
  m <- group_memberships(groups, rules)
  if (!nrow(m)) return(out)
  spec <- vapply(m$category, function(c) length(rules$group_categories[[c]]$where), integer(1))
  m <- m[order(-spec), , drop = FALSE]
  m <- m[!duplicated(m$group_id), , drop = FALSE]
  out[m$group_id] <- m$category
  out
}

# Pick the groups an analysis arm uses in one study: exactly one level, so no
# participant is counted twice.
#   members:  group ids that belong to the arm's category
#   has_data: group ids with a usable value for this variable and time point
#   policy:   "reported" (use a group's own value, else pool its subgroups)
#             or "pool" (prefer pooling complete subgroups)
# Returns list(units = list of list(groups, how)), or a reason when nothing is
# usable.
.select_level <- function(df, members, has_data, policy = "reported") {
  members <- intersect(members, df$group_id)
  top <- members[!vapply(members, function(g) any(.ancestors(df, g) %in% members), logical(1))]
  facts <- .group_facts(df)
  own_set <- function(g) {
    row <- df[match(g, df$group_id), setdiff(names(df), .group_structural), drop = FALSE]
    nm <- names(row)[!is.na(unlist(row)) & nzchar(unlist(row))]
    paste(sort(nm), collapse = "+")
  }
  n_of <- function(g) suppressWarnings(as.numeric(df$sample_size[match(g, df$group_id)]))
  resolve <- function(g) {
    reported <- if (g %in% has_data) list(groups = g, how = "reported") else NULL
    if (identical(policy, "reported") && !is.null(reported)) return(reported)
    kids <- df$group_id[!is.na(df$parent_group_id) & df$parent_group_id == g]
    if (length(kids) && "sample_size" %in% names(df)) {
      dims <- split(kids, vapply(kids, own_set, character(1)))
      for (k in dims) {
        if (!isTRUE(all.equal(sum(n_of(k)), n_of(g))) || anyNA(n_of(k))) next
        parts <- lapply(k, resolve)
        if (any(vapply(parts, is.null, logical(1)))) next
        return(list(groups = unlist(lapply(parts, `[[`, "groups")),
                    how = paste0("pooled subgroups (", paste(k, collapse = " + "), ")")))
      }
    }
    reported
  }
  units <- lapply(top, resolve)
  names(units) <- top
  units <- Filter(Negate(is.null), units)
  list(units = units, missing = setdiff(top, names(units)))
}

# Combine selected units of one arm into one set of participants.
.arm_rows <- function(dt, units) {
  ids <- unlist(lapply(units, `[[`, "groups"))
  how <- unname(vapply(units, `[[`, character(1), "how"))
  rows <- dt[dt$group_id %in% ids, , drop = FALSE]
  rows <- rows[!duplicated(rows$group_id), , drop = FALSE]
  desc <- if (length(units) > 1) paste0("combined groups: ", paste(paste0(names(units), " (", how, ")"), collapse = ", ")) else how[1]
  list(rows = rows, how = desc, groups = ids)
}

#' Groups waiting for a fact
#'
#' When a fact is added to the groups of the extraction form during the
#' review (for example `onset` with values early/late), studies extracted
#' before have no value for it. This lists the groups where the fact is not
#' assessed (neither set on the group nor inherited from its parent), so
#' they can be revisited, e.g. with [amend_study()].
#'
#' @param path Project root.
#' @param field The group field (fact) in `forms/extraction.yml`.
#' @param category Only groups belonging to this category (see
#'   [group_memberships()]), e.g. `"PE"`.
#' @return A tibble with `study_id`, `group_id`, `name`, `parent_group_id`.
#' @export
refinement_queue <- function(path = ".", field, category = NULL) {
  proj_path <- .check_project(path)
  schema <- get_schema(proj_path, "extraction")
  if (!field %in% .schema_fields(schema$metadata$groups$`_items`)) {
    rlang::abort(c(paste0("`", field, "` is not a field of groups in forms/extraction.yml."),
                   "i" = "Add it to the form first (and bump the form's version)."))
  }
  rules <- read_rules(proj_path)
  out <- list()
  for (sid in list_studies(proj_path)) {
    g <- read_study(proj_path, sid)$metadata$groups
    f <- .group_facts(g)
    if (!nrow(f)) next
    val <- if (field %in% names(f)) f[[field]] else rep(NA_character_, nrow(f))
    keep <- is.na(val)
    if (!is.null(category)) keep <- keep & f$group_id %in% group_memberships(g, rules)$group_id[group_memberships(g, rules)$category == category]
    if (any(keep)) out[[length(out) + 1]] <- dplyr::tibble(study_id = sid, group_id = f$group_id[keep], name = f$name[keep],
                                                           parent_group_id = f$parent_group_id[keep])
  }
  dplyr::bind_rows(c(list(dplyr::tibble(study_id = character(0), group_id = character(0), name = character(0),
                                        parent_group_id = character(0))), out))
}

# ---- consistency of subgroups with their total --------------------------------

# Flags (check_values format) for one study: subgroup sizes per split, and
# subgroups that do not reproduce the reported total.
.subgroup_flags <- function(d, meta, rules) {
  sev <- rules$checks
  df <- .groups_df(meta$groups)
  out <- list()
  add <- function(id, check, msg) {
    if (identical(sev[[check]] %||% "warning", "off")) return()
    out[[length(out) + 1]] <<- list(measurement_id = id, check = check, severity = sev[[check]] %||% "warning", message = msg)
  }
  if (!nrow(df) || !"sample_size" %in% names(df)) return(out)
  n_of <- function(g) suppressWarnings(as.numeric(df$sample_size[match(g, df$group_id)]))
  own_set <- function(g) {
    row <- df[match(g, df$group_id), setdiff(names(df), .group_structural), drop = FALSE]
    paste(sort(names(row)[!is.na(unlist(row)) & nzchar(unlist(row))]), collapse = "+")
  }
  for (p in unique(stats::na.omit(df$parent_group_id))) {
    if (!p %in% df$group_id) next
    kids <- df$group_id[!is.na(df$parent_group_id) & df$parent_group_id == p]
    for (k in split(kids, vapply(kids, own_set, character(1)))) {
      tot <- sum(n_of(k)); np <- n_of(p)
      if (!anyNA(c(tot, np)) && tot > np) {
        add("_metadata", "subgroup_size", sprintf("Subgroups %s have %s participants in total, more than '%s' (n = %s).",
                                                  paste(k, collapse = ", "), format(tot), p, format(np)))
      }
      if (anyNA(c(tot, np)) || tot != np || !nrow(d)) next
      # values: subgroups pooled vs the reported total
      key <- intersect(c("variable_name", "method_id", "timepoint"), names(d))
      dp <- d[d$group_id == p, , drop = FALSE]
      for (i in seq_len(nrow(dp))) {
        same <- Reduce(`&`, lapply(key, function(c) (is.na(d[[c]]) & is.na(dp[[c]][i])) | (!is.na(d[[c]]) & d[[c]] %in% dp[[c]][i])))
        kd <- d[same & d$group_id %in% k, , drop = FALSE]
        if (!setequal(kd$group_id, k) || any(duplicated(kd$group_id))) next
        tot_ms <- to_mean_sd(dp$statistic_type[i], dp$value1[i], dp$value2[i], dp$value3 %||% NA, dp$sample_size[i] %||% np)
        if (is.na(tot_ms$mean) || !identical(tot_ms$conversion, "reported")) next
        ms <- to_mean_sd(kd$statistic_type, kd$value1, kd$value2, if ("value3" %in% names(kd)) kd$value3 else NA,
                         ifelse(is.na(kd$sample_size), n_of(kd$group_id), kd$sample_size))
        if (anyNA(ms$mean) || !all(ms$conversion == "reported")) next
        pooled <- .combine_groups(data.frame(group_id = kd$group_id, sample_size = n_of(kd$group_id), mean = ms$mean, sd = ms$sd), FALSE)
        off <- function(a, b, rel) !is.na(a) && !is.na(b) && abs(a - b) > max(rel * abs(b), 0.0051)
        if (off(pooled$mean, tot_ms$mean, 0.01) || off(pooled$sd, tot_ms$sd, 0.02)) {
          add(as.character(dp$measurement_id[i]), "subgroup_total",
              sprintf("Subgroups %s pool to mean %.3g (SD %.3g), but the total '%s' reports %.3g (SD %.3g).",
                      paste(k, collapse = " + "), pooled$mean, pooled$sd, p, tot_ms$mean, tot_ms$sd))
        }
      }
    }
  }
  out
}

#' Add a fact to the groups of the extraction form
#'
#' For refining groups during a review, e.g. when early- and late-onset
#' preeclampsia turn out to matter. Adds an optional field to `groups` in
#' `forms/extraction.yml` and bumps the form's minor version. Studies
#' accepted before stay valid; [refinement_queue()] lists the groups where
#' the fact is not yet assessed, and [amend_study()] fills it in. Define
#' categories that use the fact with `where` in `_rules.yml` (see
#' [group_memberships()]).
#'
#' @param path Project root.
#' @param name Field name, e.g. `"onset"`.
#' @param values Allowed values (makes the field an enum), e.g.
#'   `c("early", "late", "not reported")`. `NULL` for free text.
#' @param description Description shown in the entry form and to LLMs.
#' @return The new form version, invisibly.
#' @export
add_group_fact <- function(path = ".", name, values = NULL, description) {
  proj_path <- .check_project(path)
  if (!rlang::is_scalar_character(name) || !grepl("^[a-z][a-z0-9_]*$", name)) {
    rlang::abort("`name` must be lowercase letters, digits and underscores, starting with a letter.")
  }
  if (tolower(name) %in% c("y", "n", "yes", "no", "on", "off", "true", "false")) rlang::abort(paste0("`", name, "` reads as TRUE/FALSE in YAML; choose another name."))
  schema <- get_schema(proj_path, "extraction")
  if (name %in% .schema_fields(schema$metadata$groups$`_items`)) rlang::abort(paste0("Groups already have a field `", name, "`."))
  f <- fs::path(proj_path, "forms", "extraction.yml")
  x <- readLines(f, warn = FALSE)
  g <- grep("^  groups:\\s*$", x)
  it <- g + grep("^    _items:\\s*$", x[(g + 1):length(x)])[1]
  if (!length(g) || is.na(it)) rlang::abort("Could not find metadata.groups._items in forms/extraction.yml.")
  end <- it
  while (end < length(x) && (grepl("^\\s*$", x[end + 1]) || grepl("^      ", x[end + 1]) || grepl("^\\s*#", x[end + 1]))) end <- end + 1
  while (end > it && grepl("^\\s*$", x[end])) end <- end - 1
  q <- function(s) paste0("\"", gsub("\"", "'", s), "\"")
  block <- c(paste0("      ", name, ":"),
             if (length(values)) c("        _type: enum", paste0("        _values: [", paste(vapply(values, q, ""), collapse = ", "), "]"))
             else "        _type: string",
             paste0("        _description: ", q(description)),
             "        _required: false")
  x <- append(x, block, after = end)
  v <- grep("^  version:", x)[1]
  new_version <- NA_character_
  if (!is.na(v)) {
    parts <- as.integer(strsplit(sub("^  version:\\s*", "", x[v]), ".", fixed = TRUE)[[1]])
    parts <- c(parts, 0L, 0L)[1:3]
    new_version <- paste(parts[1], parts[2] + 1L, 0L, sep = ".")
    x[v] <- paste0("  version: ", new_version)
  }
  writeLines(x, f)
  get_schema(proj_path, "extraction")   # fail loudly if the result does not parse
  invisible(new_version)
}
