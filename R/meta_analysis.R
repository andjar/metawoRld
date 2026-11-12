.measures_two <- c("SMD", "MD", "ROM", "OR")
.measures_one <- c("MN", "PLO")
.measures_binary <- c("OR", "PLO")

#' Read the project's meta-analysis definitions
#'
#' Reads `_analyses.yml` (see the comments in the template added by
#' [create_metawoRld()] or [use_templates()]) and checks each entry.
#'
#' @param path Project root.
#' @return A list of analyses (each a list with `name`, `variable`,
#'   `measure`, `compare` or `group`, `timepoint`, `verified_only`, `method`).
#' @export
read_analyses <- function(path = ".") {
  proj_path <- .check_project(path)
  f <- fs::path(proj_path, "_analyses.yml")
  if (!fs::file_exists(f)) return(list())
  a <- yaml::read_yaml(f)$analyses %||% list()
  out <- lapply(seq_along(a), function(i) .check_analysis(a[[i]], i))
  nms <- vapply(out, `[[`, character(1), "name")
  if (anyDuplicated(nms)) rlang::abort(paste0("Duplicate analysis names in _analyses.yml: ", paste(unique(nms[duplicated(nms)]), collapse = ", ")))
  stats::setNames(out, nms)
}

.check_analysis <- function(a, i = 1) {
  where <- paste0("_analyses.yml, analysis ", i, if (!is.null(a$name)) paste0(" ('", a$name, "')"))
  if (!is.list(a)) rlang::abort(paste0(where, ": must be a mapping."))
  for (f in c("name", "variable", "measure")) {
    if (!rlang::is_scalar_character(a[[f]]) || !nzchar(a[[f]])) rlang::abort(paste0(where, ": `", f, "` is required."))
  }
  a$measure <- toupper(a$measure)
  if (!a$measure %in% c(.measures_two, .measures_one)) {
    rlang::abort(paste0(where, ": `measure` must be one of ", paste(c(.measures_two, .measures_one), collapse = ", "), "."))
  }
  if (a$measure %in% .measures_two) {
    a$compare <- as.character(unlist(a$compare))
    if (length(a$compare) != 2) rlang::abort(paste0(where, ": `compare` must list two group categories for ", a$measure, "."))
  } else {
    if (!rlang::is_scalar_character(a$group)) rlang::abort(paste0(where, ": `group` (one group category) is required for ", a$measure, "."))
  }
  a$timepoint <- a$timepoint %||% NA_character_
  a$verified_only <- isTRUE(a$verified_only)
  a$method <- a$method %||% "REML"
  a
}

#' Convert summary statistics to mean and SD
#'
#' Means with SE or 95% CI are converted exactly; medians with IQR or range
#' are converted with the methods of Wan et al. (2014, BMC Med Res Methodol
#' 14:135). Other statistic types give `NA`.
#'
#' @param statistic_type Character vector (see the schema's enum).
#' @param value1,value2,value3 Numeric vectors as stored in `data.csv`.
#' @param n Sample sizes.
#' @return A data frame with `mean`, `sd` and `conversion` (`"reported"`,
#'   `"SE to SD"`, `"CI to SD"`, `"Wan 2014 (IQR)"`, `"Wan 2014 (range)"` or
#'   `NA`).
#' @export
#' @examples
#' to_mean_sd(c("mean_sd", "median_iqr"), c(5, 4), c(1, 3), c(NA, 6), c(20, 30))
to_mean_sd <- function(statistic_type, value1, value2, value3, n) {
  k <- length(statistic_type)
  v1 <- suppressWarnings(as.numeric(value1)); v2 <- suppressWarnings(as.numeric(value2))
  v3 <- suppressWarnings(as.numeric(value3)); n <- suppressWarnings(as.numeric(n))
  mean <- rep(NA_real_, k); sd <- rep(NA_real_, k); conv <- rep(NA_character_, k)
  st <- as.character(statistic_type)
  i <- st %in% "mean_sd"; mean[i] <- v1[i]; sd[i] <- v2[i]; conv[i] <- "reported"
  i <- st %in% "mean_se"; mean[i] <- v1[i]; sd[i] <- v2[i] * sqrt(n[i]); conv[i] <- "SE to SD"
  i <- st %in% "mean_ci95" & n > 1
  mean[i] <- v1[i]; sd[i] <- sqrt(n[i]) * (v3[i] - v2[i]) / (2 * stats::qt(0.975, n[i] - 1)); conv[i] <- "CI to SD"
  i <- st %in% "median_iqr" & n > 0
  mean[i] <- (v2[i] + v1[i] + v3[i]) / 3
  sd[i] <- (v3[i] - v2[i]) / (2 * stats::qnorm((0.75 * n[i] - 0.125) / (n[i] + 0.25))); conv[i] <- "Wan 2014 (IQR)"
  i <- st %in% "median_range" & n > 0
  mean[i] <- (v2[i] + 2 * v1[i] + v3[i]) / 4
  sd[i] <- (v3[i] - v2[i]) / (2 * stats::qnorm((n[i] - 0.375) / (n[i] + 0.25))); conv[i] <- "Wan 2014 (range)"
  conv[is.na(mean) | is.na(sd)] <- NA_character_
  data.frame(mean = mean, sd = sd, conversion = conv, stringsAsFactors = FALSE)
}

#' Data used in a meta-analysis
#'
#' Selects the data points for an analysis, assigns groups to categories,
#' converts statistics and units, combines subgroups within a study (e.g.
#' mild and severe disease, using the formulas in the Cochrane Handbook
#' section 6.5.2.10) and returns one row per study.
#'
#' @param path Project root.
#' @param analysis An analysis from [read_analyses()], or its name.
#' @param world Optional result of [load_metawoRld()] (with
#'   `drop_rejected = TRUE`) to avoid re-reading the project.
#' @return A list with `data` (one row per included study) and `excluded`
#'   (studies reporting the variable but not usable, with a reason).
#' @export
analysis_data <- function(path = ".", analysis, world = NULL) {
  proj_path <- .check_project(path)
  analysis <- .resolve_analysis(proj_path, analysis)
  rules <- read_rules(proj_path)
  world <- world %||% load_metawoRld(proj_path, verbose = FALSE)
  d <- as.data.frame(world$studies_data)
  if (analysis$verified_only && nrow(d)) d <- d[d$verification_status == "verified", , drop = FALSE]

  key <- if ("variable_canonical" %in% names(d)) ifelse(is.na(d$variable_canonical), d$variable_name, d$variable_canonical) else d$variable_name
  d <- d[!is.na(key) & tolower(key) == tolower(analysis$variable), , drop = FALSE]
  if (!is.na(analysis$timepoint) && nrow(d)) {
    tp <- if ("timepoint" %in% names(d)) d$timepoint else rep(NA_character_, nrow(d))
    d <- d[!is.na(tp) & grepl(analysis$timepoint, tp, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
  }
  binary <- analysis$measure %in% .measures_binary
  cats <- if (analysis$measure %in% .measures_two) analysis$compare else analysis$group
  empty <- list(data = .empty_analysis_rows(analysis), excluded = dplyr::tibble(study_id = character(0), reason = character(0)))
  if (!nrow(d)) return(empty)

  # categories
  d$category <- NA_character_
  for (sid in unique(d$study_id)) {
    gc <- group_category(world$studies_metadata[[sid]]$groups, rules)
    i <- d$study_id == sid
    d$category[i] <- unname(gc[as.character(d$group_id[i])])
  }
  # values
  for (c in c("unit", "method_unit", "timepoint", "value3")) if (!c %in% names(d)) d[[c]] <- NA
  if (binary) {
    d$events <- .to_events(d$statistic_type, d$value1, d$value2, d$sample_size)
    d$usable <- !is.na(d$events) & !is.na(d$sample_size) & d$sample_size > 0
    d$why <- ifelse(d$usable, NA, "no count, percentage or proportion")
    d$conversion <- NA_character_
  } else {
    ms <- to_mean_sd(d$statistic_type, d$value1, d$value2, d$value3, d$sample_size)
    target <- rules$variables[[analysis$variable]]$unit
    unit <- ifelse(is.na(d$unit) | d$unit == "", d$method_unit, d$unit)
    factor <- .unit_factor(unit, target, rules$variables[[analysis$variable]]$convert)
    d$mean <- ms$mean * factor; d$sd <- ms$sd * factor; d$conversion <- ms$conversion
    d$usable <- !is.na(d$mean) & !is.na(d$sd) & d$sd > 0 & !is.na(d$sample_size) & d$sample_size > 1
    d$why <- ifelse(d$usable, NA,
                    ifelse(is.na(factor), paste0("unit '", unit, "' cannot be converted to '", target, "'"),
                           ifelse(is.na(ms$conversion), paste0("'", d$statistic_type, "' without the values needed for a mean and SD"),
                                  "missing or invalid SD or sample size")))
  }
  d$verified <- d$verification_status == "verified"

  rows <- list(); excluded <- list()
  for (sid in unique(d$study_id)) {
    ds <- d[d$study_id == sid, , drop = FALSE]
    tps <- unique(ifelse(is.na(ds$timepoint), "", ds$timepoint))
    picked <- NULL
    for (tp in tps) {
      dt <- ds[ifelse(is.na(ds$timepoint), "", ds$timepoint) == tp & ds$usable, , drop = FALSE]
      parts <- lapply(cats, function(cg) dt[!is.na(dt$category) & dt$category == cg, , drop = FALSE])
      if (all(vapply(parts, nrow, integer(1)) > 0)) { picked <- list(tp = tp, parts = parts); break }
    }
    if (is.null(picked)) {
      have <- unique(stats::na.omit(ds$category[ds$usable]))
      reason <- if (!any(ds$usable)) paste(unique(stats::na.omit(ds$why)), collapse = "; ")
      else if (all(is.na(ds$category))) "groups not assigned to a category (set `category` or add synonyms in _rules.yml)"
      else paste0("no usable data for: ", paste(setdiff(cats, have), collapse = ", "))
      excluded[[length(excluded) + 1]] <- list(study_id = sid, reason = reason)
      next
    }
    meta <- world$studies_metadata[[sid]]
    combined <- lapply(picked$parts, function(p) .combine_groups(p, binary))
    row <- list(study_id = sid, label = .study_label(meta, sid), year = suppressWarnings(as.integer(.val(meta$year, NA))),
                timepoint = picked$tp,
                converted = any(unlist(lapply(picked$parts, function(p) !p$conversion %in% c("reported", NA)))),
                verified = all(unlist(lapply(picked$parts, `[[`, "verified"))),
                measurement_ids = paste(unlist(lapply(picked$parts, `[[`, "measurement_id")), collapse = ", "),
                note = paste(stats::na.omit(vapply(combined, `[[`, character(1), "note")), collapse = "; "))
    for (j in seq_along(combined)) {
      sfx <- if (length(combined) == 2) j else ""
      row[[paste0("n", sfx)]] <- combined[[j]]$n
      if (binary) row[[paste0("events", sfx)]] <- combined[[j]]$events
      else { row[[paste0("mean", sfx)]] <- combined[[j]]$mean; row[[paste0("sd", sfx)]] <- combined[[j]]$sd }
    }
    rows[[length(rows) + 1]] <- row
  }
  list(
    data = dplyr::bind_rows(.empty_analysis_rows(analysis), dplyr::bind_rows(rows)),
    excluded = dplyr::bind_rows(dplyr::tibble(study_id = character(0), reason = character(0)), dplyr::bind_rows(excluded))
  )
}

#' Run a meta-analysis
#'
#' Random-effects meta-analysis with [metafor::rma()] of the data from
#' [analysis_data()], plus a cumulative meta-analysis by publication year
#' showing how the pooled estimate has evolved.
#'
#' @inheritParams analysis_data
#' @return An object of class `metawoRld_meta`: a list with `analysis`,
#'   `data` (with effect sizes `yi`, `vi`), `excluded`, `model` (the `rma`
#'   fit, or `NULL` with fewer than two studies), `summary` (one-row tibble)
#'   and `cumulative` (pooled estimate after each publication year).
#' @export
run_meta_analysis <- function(path = ".", analysis, world = NULL) {
  rlang::check_installed("metafor", reason = "to run meta-analyses.")
  proj_path <- .check_project(path)
  analysis <- .resolve_analysis(proj_path, analysis)
  ad <- analysis_data(proj_path, analysis, world)
  dat <- ad$data
  m <- analysis$measure
  if (nrow(dat)) {
    es <- switch(
      m,
      SMD = , MD = , ROM = metafor::escalc(m, m1i = dat$mean1, sd1i = dat$sd1, n1i = dat$n1, m2i = dat$mean2, sd2i = dat$sd2, n2i = dat$n2),
      OR = metafor::escalc("OR", ai = dat$events1, n1i = dat$n1, ci = dat$events2, n2i = dat$n2, add = 0.5, to = "only0"),
      MN = metafor::escalc("MN", mi = dat$mean, sdi = dat$sd, ni = dat$n),
      PLO = metafor::escalc("PLO", xi = dat$events, ni = dat$n, add = 0.5, to = "only0")
    )
    dat$yi <- as.numeric(es$yi); dat$vi <- as.numeric(es$vi)
    dat <- dat[!is.na(dat$yi) & !is.na(dat$vi), , drop = FALSE]
  } else {
    dat$yi <- numeric(0); dat$vi <- numeric(0)
  }
  dat <- dat[order(dat$year, dat$label), , drop = FALSE]
  model <- if (nrow(dat) >= 2) {
    tryCatch(metafor::rma(yi = dat$yi, vi = dat$vi, method = analysis$method, slab = dat$label),
             error = function(e) { rlang::warn(paste0(analysis$name, ": ", conditionMessage(e))); NULL })
  }
  tf <- .measure_transf(m)
  summ <- .summarise_model(model, dat, tf, analysis)
  cumulative <- if (!is.null(model)) {
    cm <- metafor::cumul(model, order = seq_len(nrow(dat)))
    cdf <- data.frame(year = dat$year, k = seq_len(nrow(dat)), estimate = tf(cm$estimate), ci_lb = tf(cm$ci.lb), ci_ub = tf(cm$ci.ub))
    cdf[!duplicated(cdf$year, fromLast = TRUE) & !is.na(cdf$year), , drop = FALSE]
  } else data.frame(year = integer(0), k = integer(0), estimate = numeric(0), ci_lb = numeric(0), ci_ub = numeric(0))
  structure(list(analysis = analysis, data = dplyr::as_tibble(dat), excluded = ad$excluded,
                 model = model, summary = summ, cumulative = cumulative,
                 unit = read_rules(proj_path)$variables[[analysis$variable]]$unit %||% ""),
            class = "metawoRld_meta")
}

#' Run all meta-analyses of a project
#'
#' @param path Project root.
#' @param world Optional result of [load_metawoRld()].
#' @return A named list of `metawoRld_meta` objects (or error conditions for
#'   analyses that failed).
#' @export
run_all_meta_analyses <- function(path = ".", world = NULL) {
  proj_path <- .check_project(path)
  world <- world %||% load_metawoRld(proj_path, verbose = FALSE)
  lapply(read_analyses(proj_path), function(a) tryCatch(run_meta_analysis(proj_path, a, world), error = function(e) e))
}

#' Summary of findings table
#'
#' @param results Output of [run_all_meta_analyses()].
#' @return A tibble with one row per analysis.
#' @export
summary_of_findings <- function(results) {
  rows <- lapply(names(results), function(n) {
    r <- results[[n]]
    if (inherits(r, "error")) return(dplyr::tibble(analysis = n, measure = NA_character_, studies = NA_integer_, participants = NA_real_,
                                                   estimate = paste("Error:", conditionMessage(r)), I2 = NA_character_,
                                                   prediction_interval = NA_character_, latest_study = NA_integer_))
    r$summary
  })
  dplyr::bind_rows(rows)
}

#' @export
print.metawoRld_meta <- function(x, ...) {
  s <- x$summary
  cat("Meta-analysis:", x$analysis$name, "\n")
  cat(sprintf("  %s, %d studies, %s participants\n", .measure_label(x$analysis$measure), s$studies, format(s$participants)))
  cat("  Pooled estimate:", s$estimate, "\n")
  if (!is.na(s$I2)) cat("  Heterogeneity: I2 =", s$I2, " Prediction interval:", s$prediction_interval, "\n")
  if (nrow(x$excluded)) cat("  Excluded studies:", nrow(x$excluded), "(see $excluded)\n")
  invisible(x)
}

#' Forest plot of a meta-analysis
#'
#' Studies whose values are not yet verified are drawn in grey; studies whose
#' mean/SD were estimated (from median/IQR, range, SE or CI) are marked `(c)`.
#'
#' @param ma A `metawoRld_meta` object from [run_meta_analysis()].
#' @param ... Passed to [metafor::forest()].
#' @return Draws the plot; returns `ma` invisibly.
#' @export
plot_forest <- function(ma, ...) {
  dat <- ma$data
  if (!nrow(dat)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No studies with usable data yet.")
    return(invisible(ma))
  }
  m <- ma$analysis$measure
  lab <- paste0(dat$label, ifelse(dat$converted, " (c)", ""))
  col <- ifelse(dat$verified, "black", "grey55")
  atr <- switch(m, ROM = , OR = exp, PLO = metafor::transf.ilogit, NULL)
  args <- list(slab = lab, colout = col, header = c("Study", "Estimate [95% CI]"),
               xlab = .measure_label(m, ma), atransf = atr, ...)
  ilab_lab <- if (m %in% .measures_two) c("n1", "n2") else "n"
  args$ilab <- if (m %in% .measures_two) cbind(dat$n1, dat$n2) else cbind(dat$n)
  if (is.null(atr)) args$atransf <- NULL
  op <- graphics::par(mar = c(5.5, 4, 1, 2)); on.exit(graphics::par(op))
  res <- if (!is.null(ma$model)) {
    do.call(metafor::forest, c(list(ma$model, addpred = TRUE), args))
  } else {
    args$col <- args$colout; args$colout <- NULL
    do.call(metafor::forest, c(list(dat$yi, dat$vi), args))
  }
  if (!is.null(res$ilab.xpos)) graphics::text(res$ilab.xpos, res$ylim[2] - 1, ilab_lab, font = 2, cex = res$cex)
  graphics::mtext(paste0(if (m %in% .measures_two) paste0("n1: ", ma$analysis$compare[1], ", n2: ", ma$analysis$compare[2], ".  ") else "",
                         "(c) mean/SD estimated from median, range, SE or CI.  Grey: not yet verified."),
                  side = 1, line = 4.2, adj = 0, cex = 0.75, col = "grey30")
  invisible(ma)
}

#' Cumulative meta-analysis plot
#'
#' Shows how the pooled estimate and its 95% CI changed as studies were
#' published.
#'
#' @param ma A `metawoRld_meta` object.
#' @return Draws the plot; returns `ma` invisibly.
#' @export
plot_cumulative <- function(ma) {
  cm <- ma$cumulative
  if (nrow(cm) < 2) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "Needs studies from at least two years.")
    return(invisible(ma))
  }
  ref <- if (ma$analysis$measure %in% c("ROM", "OR")) 1 else if (ma$analysis$measure %in% c("SMD", "MD")) 0 else NA
  yl <- range(c(cm$ci_lb, cm$ci_ub, ref), na.rm = TRUE)
  op <- graphics::par(mar = c(4, 4.5, 2, 1)); on.exit(graphics::par(op))
  graphics::plot(cm$year, cm$estimate, type = "n", ylim = yl, xlab = "Publication year (studies included so far)",
                 ylab = paste("Pooled", .measure_short(ma$analysis$measure)), las = 1,
                 log = if (ma$analysis$measure %in% c("ROM", "OR")) "y" else "")
  graphics::polygon(c(cm$year, rev(cm$year)), c(cm$ci_lb, rev(cm$ci_ub)), col = grDevices::adjustcolor("steelblue", 0.2), border = NA)
  if (!is.na(ref)) graphics::abline(h = ref, lty = 2, col = "grey40")
  graphics::lines(cm$year, cm$estimate, lwd = 2, col = "steelblue4")
  graphics::points(cm$year, cm$estimate, pch = 19, col = "steelblue4")
  graphics::text(cm$year, cm$ci_ub, paste0("k=", cm$k), pos = 3, cex = 0.7, col = "grey30", xpd = NA)
  invisible(ma)
}

#' Plot all reported values of a variable
#'
#' A descriptive overview (no pooling): every group-level value reported for
#' the variable, with its reported spread (SD, SE, IQR, range or CI), coloured
#' by group category.
#'
#' @param path Project root.
#' @param variable Variable name (canonical or as extracted).
#' @param world Optional result of [load_metawoRld()].
#' @return Draws the plot; returns the plotted data invisibly.
#' @export
plot_variable <- function(path = ".", variable, world = NULL) {
  proj_path <- .check_project(path)
  rules <- read_rules(proj_path)
  world <- world %||% load_metawoRld(proj_path, verbose = FALSE)
  d <- .variable_rows(world, variable, rules)
  if (!nrow(d)) { graphics::plot.new(); graphics::text(0.5, 0.5, "No values."); return(invisible(d)) }
  cats <- unique(d$category_lab)
  pal <- grDevices::hcl.colors(max(3, length(cats)), "Dark 3")[seq_along(cats)]
  col <- pal[match(d$category_lab, cats)]
  n <- nrow(d)
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
  graphics::plot.new()
  left <- min(4.5, max(graphics::strwidth(d$row_label, "inches", cex = 0.8))) + 0.25
  graphics::par(mai = c(0.8, left, 0.5, 0.2), new = TRUE)
  xr <- range(c(d$lo, d$hi, d$v1), na.rm = TRUE)
  graphics::plot(d$v1, rev(seq_len(n)), xlim = xr, ylim = c(0.5, n + 0.5), yaxt = "n", ylab = "", pch = ifelse(grepl("^median", d$statistic_type), 15, 19),
                 col = col, xlab = paste0(variable, if (nzchar(d$unit_lab[1])) paste0(" (", d$unit_lab[1], ")") else ""), las = 1)
  graphics::segments(d$lo, rev(seq_len(n)), d$hi, rev(seq_len(n)), col = col, lwd = 1.5)
  graphics::axis(2, at = rev(seq_len(n)), labels = d$row_label, las = 1, cex.axis = 0.8, tick = FALSE)
  graphics::abline(h = which(diff(as.integer(factor(rev(d$study_id), levels = unique(rev(d$study_id))))) != 0) + 0.5, col = "grey85")
  graphics::legend("bottom", inset = c(0, 1), xpd = NA, horiz = TRUE, legend = c(cats, "mean (SD/SE/CI)", "median (IQR/range)"),
                   col = c(pal, "grey30", "grey30"), pch = c(rep(19, length(cats)), 19, 15), lty = c(rep(1, length(cats)), NA, NA),
                   bty = "n", cex = 0.8)
  invisible(d)
}

# ---- internals ---------------------------------------------------------------

.resolve_analysis <- function(proj_path, analysis) {
  if (rlang::is_scalar_character(analysis)) {
    all <- read_analyses(proj_path)
    if (!analysis %in% names(all)) rlang::abort(paste0("No analysis named '", analysis, "' in _analyses.yml."))
    return(all[[analysis]])
  }
  .check_analysis(analysis)
}

.empty_analysis_rows <- function(analysis) {
  base <- dplyr::tibble(study_id = character(0), label = character(0), year = integer(0), timepoint = character(0),
                        converted = logical(0), verified = logical(0), measurement_ids = character(0), note = character(0))
  m <- analysis$measure
  extra <- if (m %in% c("SMD", "MD", "ROM")) c("n1", "mean1", "sd1", "n2", "mean2", "sd2")
           else if (m == "OR") c("n1", "events1", "n2", "events2")
           else if (m == "MN") c("n", "mean", "sd") else c("n", "events")
  for (e in extra) base[[e]] <- numeric(0)
  base
}

.to_events <- function(statistic_type, value1, value2, n) {
  v1 <- suppressWarnings(as.numeric(value1)); v2 <- suppressWarnings(as.numeric(value2)); n <- suppressWarnings(as.numeric(n))
  st <- as.character(statistic_type)
  ev <- rep(NA_real_, length(st))
  i <- st %in% "n_percent"; ev[i] <- ifelse(is.na(v1[i]), round(v2[i] / 100 * n[i]), v1[i])
  i <- st %in% "count"; ev[i] <- v1[i]
  i <- st %in% "proportion"; ev[i] <- round(v1[i] * n[i])
  ev
}

.unit_factor <- function(unit, target, convert) {
  if (is.null(target) || !nzchar(target)) return(rep(1, length(unit)))
  vapply(unit, function(u) {
    if (is.na(u) || !nzchar(u) || tolower(u) == tolower(target)) return(1)
    idx <- match(tolower(u), tolower(names(convert)))
    if (is.na(idx)) NA_real_ else as.numeric(convert[[idx]])
  }, numeric(1), USE.NAMES = FALSE)
}

# Combine several groups of one category in a study (Cochrane Handbook 6.5.2.10).
.combine_groups <- function(p, binary) {
  p <- p[!duplicated(p$group_id), , drop = FALSE]
  note <- if (nrow(p) > 1) paste0("combined ", nrow(p), " groups (", paste(p$group_id, collapse = ", "), ")") else NA_character_
  if (binary) return(list(n = sum(p$sample_size), events = sum(p$events), note = note))
  n <- p$sample_size[1]; m <- p$mean[1]; s <- p$sd[1]
  for (k in seq_len(nrow(p))[-1]) {
    n2 <- p$sample_size[k]; m2 <- p$mean[k]; s2 <- p$sd[k]
    s <- sqrt(((n - 1) * s^2 + (n2 - 1) * s2^2 + n * n2 / (n + n2) * (m^2 + m2^2 - 2 * m * m2)) / (n + n2 - 1))
    m <- (n * m + n2 * m2) / (n + n2)
    n <- n + n2
  }
  list(n = n, mean = m, sd = s, note = note)
}

.measure_transf <- function(m) switch(m, ROM = , OR = exp, PLO = metafor::transf.ilogit, identity)

.measure_label <- function(m, ma = NULL) {
  unit <- if (!is.null(ma)) ma$unit %||% "" else ""
  lab <- switch(m, SMD = "Standardized mean difference (Hedges' g)", MD = "Mean difference", ROM = "Ratio of means",
                OR = "Odds ratio", MN = "Mean", PLO = "Proportion", m)
  if (m %in% c("MD", "MN") && nzchar(unit)) lab <- paste0(lab, " (", unit, ")")
  lab
}

.summarise_model <- function(model, dat, tf, analysis) {
  a_name <- analysis$name
  a_measure <- analysis$measure
  f <- function(x) formatC(x, digits = 2, format = "f")
  n_part <- if (analysis$measure %in% .measures_two) sum(dat$n1 + dat$n2) else sum(dat$n)
  latest <- if (nrow(dat)) suppressWarnings(max(dat$year, na.rm = TRUE)) else NA_integer_
  if (!is.finite(latest)) latest <- NA_integer_
  if (is.null(model)) {
    est <- if (nrow(dat) == 1) paste0(f(tf(dat$yi)), " [", f(tf(dat$yi - 1.96 * sqrt(dat$vi))), ", ", f(tf(dat$yi + 1.96 * sqrt(dat$vi))), "] (single study)") else "No studies yet"
    return(dplyr::tibble(analysis = a_name, measure = a_measure, studies = nrow(dat), participants = n_part,
                         estimate = est, I2 = NA_character_, prediction_interval = NA_character_, latest_study = latest,
                         est = NA_real_, ci_lb = NA_real_, ci_ub = NA_real_, pval = NA_real_))
  }
  pr <- stats::predict(model, transf = tf)
  dplyr::tibble(
    analysis = a_name, measure = a_measure, studies = model$k, participants = n_part,
    estimate = paste0(f(pr$pred), " [", f(pr$ci.lb), ", ", f(pr$ci.ub), "]"),
    I2 = paste0(formatC(model$I2, digits = 0, format = "f"), "%"),
    prediction_interval = paste0("[", f(pr$pi.lb), ", ", f(pr$pi.ub), "]"),
    latest_study = latest,
    est = as.numeric(pr$pred), ci_lb = as.numeric(pr$ci.lb), ci_ub = as.numeric(pr$ci.ub), pval = as.numeric(model$pval)
  )
}

.variable_rows <- function(world, variable, rules) {
  d <- as.data.frame(world$studies_data)
  if (!nrow(d)) return(d)
  key <- if ("variable_canonical" %in% names(d)) ifelse(is.na(d$variable_canonical), d$variable_name, d$variable_canonical) else d$variable_name
  d <- d[!is.na(key) & tolower(key) == tolower(variable), , drop = FALSE]
  d <- d[d$statistic_type %in% c("mean_sd", "mean_se", "mean_ci95", "median_iqr", "median_range", "geometric_mean_sd"), , drop = FALSE]
  if (!nrow(d)) return(d)
  for (c in c("unit", "method_unit", "timepoint", "value2", "value3", "group_name")) if (!c %in% names(d)) d[[c]] <- NA
  unit <- ifelse(is.na(d$unit) | d$unit == "", d$method_unit, d$unit)
  target <- rules$variables[[variable]]$unit
  factor <- .unit_factor(unit, target, rules$variables[[variable]]$convert)
  keep <- !is.na(factor)
  d <- d[keep, , drop = FALSE]; factor <- factor[keep]; unit <- unit[keep]
  d$v1 <- d$value1 * factor
  sym <- d$statistic_type %in% c("mean_sd", "mean_se", "geometric_mean_sd")
  d$lo <- ifelse(sym, d$value1 - d$value2, d$value2) * factor
  d$hi <- ifelse(sym, d$value1 + d$value2, d$value3) * factor
  d$category_lab <- "Other"
  for (sid in unique(d$study_id)) {
    gc <- group_category(world$studies_metadata[[sid]]$groups, rules)
    i <- d$study_id == sid
    d$category_lab[i] <- ifelse(is.na(gc[as.character(d$group_id[i])]), "Other", gc[as.character(d$group_id[i])])
  }
  lab <- vapply(d$study_id, function(s) .study_label(world$studies_metadata[[s]], s), character(1))
  d$row_label <- paste0(lab, " | ", ifelse(is.na(d$group_name), d$group_id, d$group_name),
                        ifelse(is.na(d$timepoint) | d$timepoint == "", "", paste0(" | ", d$timepoint)))
  d$unit_lab <- if (!is.null(target)) target else ifelse(is.na(unit), "", unit)
  d$year <- vapply(d$study_id, function(s) suppressWarnings(as.integer(.val(world$studies_metadata[[s]]$year, NA))), integer(1))
  d[order(d$year, d$study_id), , drop = FALSE]
}
