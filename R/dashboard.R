#' Generate the dashboard pages of the website
#'
#' Runs every meta-analysis in `_analyses.yml` and writes two Quarto
#' dashboard pages with their figures (in `figures/`):
#' * `analyses.qmd`: one page per meta-analysis with the pooled estimate,
#'   heterogeneity, prediction interval, a forest plot, a cumulative
#'   meta-analysis by publication year (how the evidence has evolved) and
#'   the studies that could not be included, with reasons.
#' * `results.qmd`: every value reported for each variable, across studies
#'   and groups (descriptive, no pooling).
#'
#' Called by [generate_website()]; the overview page (`index.qmd`) is a
#' template that summarises the same results.
#'
#' @param path Project root.
#' @param max_variables Maximum number of variables on the results page
#'   (those reported by most studies first).
#' @return Paths of the generated pages, invisibly.
#' @export
generate_dashboard <- function(path = ".", max_variables = 40) {
  proj_path <- .check_project(path)
  fig_dir <- fs::path(proj_path, "figures")
  if (fs::dir_exists(fig_dir)) fs::dir_delete(fig_dir)
  fs::dir_create(fig_dir)
  world <- load_metawoRld(proj_path, verbose = FALSE)
  analyses <- read_analyses(proj_path)
  has_metafor <- rlang::is_installed("metafor")
  results <- if (has_metafor) run_all_meta_analyses(proj_path, world) else list()

  # ---- analyses.qmd
  lines <- c("---", "title: Meta-analyses", "format:", "  dashboard:", "    scrolling: true", "    css: styles.css", "---", "")
  if (!length(analyses)) {
    lines <- c(lines, "# Meta-analyses", "", "::: {.card title=\"No meta-analyses defined yet\"}",
               "Define meta-analyses in `_analyses.yml` (see the comments in that file) and rebuild the site.", ":::")
  } else if (!has_metafor) {
    lines <- c(lines, "# Meta-analyses", "", "::: {.card}", "Install the **metafor** package to run the meta-analyses.", ":::")
  }
  for (i in seq_along(results)) {
    lines <- c(lines, .dashboard_analysis_page(results[[i]], names(results)[i], i, proj_path, fig_dir))
  }
  analyses_file <- fs::path(proj_path, "analyses.qmd")
  writeLines(enc2utf8(lines), analyses_file, useBytes = TRUE)

  # ---- results.qmd
  vars <- .variables_by_studies(world, read_rules(proj_path))
  vars <- utils::head(vars, max_variables)
  lines <- c("---", "title: Results by variable", "format:", "  dashboard:", "    scrolling: true", "    css: styles.css", "---", "")
  if (!length(vars)) {
    lines <- c(lines, "::: {.card}", "No continuous results yet.", ":::")
  } else {
    lines <- c(lines, "## Row {.tabset}", "")
    for (j in seq_along(vars)) {
      v <- vars[j]
      d <- .variable_rows(world, names(v), read_rules(proj_path))
      f <- fs::path(fig_dir, paste0("variable_", j, ".png"))
      .png(f, width = 11, height = 1.9 + 0.22 * max(1, nrow(d)), {
        plot_variable(proj_path, names(v), world)
      })
      lines <- c(lines, paste0("::: {.card title=\"", .md_attr(names(v)), " (", v, " stud", if (v == 1) "y" else "ies", ")\"}"),
                 paste0("![](figures/", fs::path_file(f), "){fig-alt=\"Reported values of ", .md_attr(names(v)), "\"}"), ":::", "")
    }
  }
  results_file <- fs::path(proj_path, "results.qmd")
  writeLines(enc2utf8(lines), results_file, useBytes = TRUE)
  invisible(c(analyses_file, results_file))
}

# ---- internals ---------------------------------------------------------------

.dashboard_analysis_page <- function(r, name, i, proj_path, fig_dir) {
  out <- c(paste0("# ", .md_escape(name)), "")
  if (inherits(r, "error")) {
    return(c(out, "::: {.card title=\"Could not run this analysis\"}", .md_escape(conditionMessage(r)), ":::", ""))
  }
  s <- r$summary
  a <- r$analysis
  what <- if (a$measure %in% .measures_two) paste0(a$compare[1], " vs ", a$compare[2]) else a$group
  vb <- function(title, value, icon, color) c(paste0("::: {.valuebox icon=\"", icon, "\" color=\"", color, "\"}"), title, "", value, ":::", "")
  est <- if (is.na(s$est) && nrow(r$data) == 1) as.numeric(.measure_transf(a$measure)(r$data$yi)) else s$est
  ci <- regmatches(s$estimate, regexpr("\\[[^]]*\\]", s$estimate))
  fmt <- function(x) formatC(x, digits = 2, format = "f")
  out <- c(out, "## Row {height=180px}", "",
           vb(paste0("Pooled ", .measure_short(a$measure), if (length(ci)) paste0(", 95% CI ", ci) else "",
                     if (s$studies == 1) " (single study)" else ""),
              if (is.na(est)) "-" else fmt(est), "bullseye", "primary"),
           vb(paste0("Studies (", format(s$participants, big.mark = ","), " participants)"), s$studies, "journal-text", "secondary"),
           vb("Heterogeneity (I^2^)", s$I2 %|NA|% "-", "distribute-vertical", "light"),
           vb("95% prediction interval", s$prediction_interval %|NA|% "-", "arrows-expand", "light"))

  ff <- fs::path(fig_dir, paste0("forest_", i, ".png"))
  .png(ff, width = 9, height = 2.2 + 0.28 * max(1, nrow(r$data)), plot_forest(r))
  cf <- fs::path(fig_dir, paste0("cumulative_", i, ".png"))
  .png(cf, width = 6, height = 4, plot_cumulative(r))

  verified <- sum(r$data$verified)
  info <- c(
    paste0("**", .md_escape(a$variable), "**, ", .md_escape(what), if (!is.na(a$timepoint)) paste0(", timepoint matching `", a$timepoint, "`") else "", "."),
    "",
    paste0("Random-effects model (", a$method, "). ", verified, " of ", nrow(r$data), " included studies have all their values verified",
           if (a$verified_only) " (only verified values are used)" else "", "."),
    if (any(r$data$converted)) c("", paste0(sum(r$data$converted), " stud", if (sum(r$data$converted) == 1) "y uses" else "ies use", " means/SDs estimated from medians, ranges, SEs or CIs.")),
    if (any(nzchar(r$data$note))) c("", paste0("Notes: ", paste(.md_escape(paste0(r$data$label, ": ", r$data$note)[nzchar(r$data$note)]), collapse = "; "), ".")),
    if (length(a$subgroups) && nrow(r$subgroups)) c("", "**Subgroups:**", "",
      paste0("- ", .md_escape(r$subgroups$subgroup), ": ", .md_escape(r$subgroups$estimate), " (", r$subgroups$studies, " stud",
             ifelse(r$subgroups$studies == 1, "y", "ies"), ifelse(is.na(r$subgroups$I2), "", paste0(", I^2^ ", r$subgroups$I2)), ")"),
      if (!is.na(r$subgroup_test)) c("", .md_escape(r$subgroup_test))),
    if (nrow(r$excluded)) c("", "**Not included:**", "", paste0("- ", .md_escape(r$excluded$study_id), ": ", .md_escape(r$excluded$reason)))
  )
  contrib <- .contribution_table(r)
  c(out, "## Row", "",
    "### Column {width=60%}", "",
    "::: {.card title=\"Forest plot\"}", paste0("![](figures/", fs::path_file(ff), "){fig-alt=\"Forest plot for ", .md_attr(name), "\"}"), ":::", "",
    "### Column {width=40%}", "",
    "::: {.card title=\"How the evidence has evolved\"}", paste0("![](figures/", fs::path_file(cf), "){fig-alt=\"Cumulative meta-analysis for ", .md_attr(name), "\"}"), ":::", "",
    "::: {.card title=\"Details\"}", info, ":::", "",
    if (length(contrib)) c("## Row", "", "::: {.card title=\"How each study contributes\"}", contrib, ":::", ""))
}

# Markdown table: which groups each study contributes, at which level.
.contribution_table <- function(r) {
  s <- r$selection
  if (is.null(s) || !nrow(s)) return(character(0))
  lab <- r$data$label[match(s$study_id, r$data$study_id)]
  lab <- sub(" \\[[^]]*\\]$", "", ifelse(is.na(lab), s$study_id, lab))
  how <- ifelse(s$how == "reported", "as reported", s$how)
  conv <- ifelse(is.na(s$conversion) | s$conversion %in% c("", "reported"), "", paste0(" (", s$conversion, ")"))
  has_sg <- any(!is.na(s$subgroup))
  head <- c(paste0("| Study |", if (has_sg) " Subgroup |" else "", " Arm | Groups used | Level | n | Time point | Statistic |"),
            paste0("|---|", if (has_sg) "---|" else "", "---|---|---|---:|---|---|"))
  rows <- paste0("| ", .md_escape(lab), " |", if (has_sg) paste0(" ", .md_escape(ifelse(is.na(s$subgroup), "", s$subgroup)), " |") else "",
                 " ", .md_escape(s$arm), " | ", .md_escape(s$group_names), " | ", .md_escape(how), " | ",
                 formatC(s$n, format = "fg", digits = 4), " | ", .md_escape(s$timepoint), " | ", .md_escape(s$statistic), conv, " |")
  c(head, rows, "", "Each study contributes one set of participants per arm: a group and its subgroups are never both used.")
}

.measure_short <- function(m) switch(m, SMD = "SMD (Hedges' g)", MD = "mean difference", ROM = "ratio of means",
                                       OR = "odds ratio", MN = "mean", PLO = "proportion", m)

`%|NA|%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

.md_attr <- function(x) gsub("\"", "'", .md_escape(x))

.png <- function(file, width, height, expr) {
  grDevices::png(file, width = width, height = height, units = "in", res = 110)
  on.exit(grDevices::dev.off())
  force(expr)
  invisible(file)
}

.variables_by_studies <- function(world, rules) {
  d <- as.data.frame(world$studies_data)
  if (!nrow(d)) return(stats::setNames(integer(0), character(0)))
  d <- d[d$statistic_type %in% c("mean_sd", "mean_se", "mean_ci95", "median_iqr", "median_range", "geometric_mean_sd"), , drop = FALSE]
  if (!nrow(d)) return(stats::setNames(integer(0), character(0)))
  key <- if ("variable_canonical" %in% names(d)) ifelse(is.na(d$variable_canonical), d$variable_name, d$variable_canonical) else d$variable_name
  n <- tapply(d$study_id, key, function(x) length(unique(x)))
  n <- sort(n, decreasing = TRUE)
  stats::setNames(as.integer(n), names(n))
}
