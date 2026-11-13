skip_if_not_installed("metafor")

test_that("conversions to mean and SD", {
  x <- to_mean_sd(c("mean_sd", "mean_se", "mean_ci95", "median_iqr", "median_range", "count"),
                  c(5, 5, 5, 4, 4, 1), c(1, 0.2, 4, 3, 1, NA), c(NA, NA, 6, 6, 9, NA), c(25, 25, 25, 30, 30, 30))
  expect_equal(x$sd[1:2], c(1, 1))
  expect_equal(x$sd[3], 5 * 2 / (2 * qt(0.975, 24)))
  expect_equal(x$mean[4], (3 + 4 + 6) / 3)
  expect_equal(x$sd[4], 3 / (2 * qnorm((0.75 * 30 - 0.125) / 30.25)))
  expect_equal(x$mean[5], (1 + 8 + 9) / 4)
  expect_equal(x$conversion, c("reported", "SE to SD", "CI to SD", "Wan 2014 (IQR)", "Wan 2014 (range)", NA))
})

test_that("analysis data: categories, units, subgroup combination and exclusions", {
  p <- ma_project()
  expect_equal(names(read_analyses(p)), c("IL-6 PE vs control", "IL-6 in controls", "Smoking PE vs control"))
  ad <- analysis_data(p, "IL-6 PE vs control")
  expect_equal(ad$data$study_id, c("study-a", "study-b", "study-c"))
  b <- ad$data[ad$data$study_id == "study-b", ]
  expect_equal(b$mean1, 6, tolerance = 1e-8)                       # (4+6+8)/3 ng/mL * 1000
  expect_true(b$converted)
  cc <- ad$data[ad$data$study_id == "study-c", ]
  expect_equal(cc$n1, 20); expect_equal(cc$mean1, 5)               # combined mild + severe
  expect_equal(cc$sd1, sqrt((9 * 1 + 9 * 1 + 10 * 10 / 20 * (16 + 36 - 48)) / 19))
  expect_equal(cc$sd2, 0.2 * 5)                                     # SE -> SD
  expect_match(cc$note, "combined groups: g1 \\(reported\\), g2 \\(reported\\)")
  expect_equal(ad$excluded$study_id, "study-d")
  expect_match(ad$excluded$reason, "no usable data for: PE")
})

test_that("run_meta_analysis matches metafor and produces cumulative results", {
  p <- ma_project()
  ma <- run_meta_analysis(p, "IL-6 PE vs control")
  ad <- analysis_data(p, "IL-6 PE vs control")$data
  es <- metafor::escalc("SMD", m1i = ad$mean1, sd1i = ad$sd1, n1i = ad$n1, m2i = ad$mean2, sd2i = ad$sd2, n2i = ad$n2)
  ref <- metafor::rma(es$yi, es$vi, method = "REML")
  expect_equal(ma$summary$est, as.numeric(ref$b))
  expect_equal(ma$summary$studies, 3)
  expect_equal(ma$summary$participants, 50 + 80 + 45)
  expect_equal(ma$cumulative$year, c(2015L, 2018L, 2020L))          # pooled estimate after each year
  expect_equal(ma$cumulative$k, 1:3)
  expect_equal(ma$cumulative$estimate[3], ma$summary$est)
  expect_output(print(ma), "3 studies")

  mn <- run_meta_analysis(p, "IL-6 in controls")
  expect_equal(mn$summary$studies, 4)
  expect_equal(mn$unit, "pg/mL")

  or <- run_meta_analysis(p, "Smoking PE vs control")               # single study: no model
  expect_null(or$model)
  expect_match(or$summary$estimate, "single study")
  expect_equal(exp(or$data$yi), (4 / 16) / (3 / 27))

  # verified_only
  set_verification(p, "study-a", c("d1", "d2"), "verified")
  a <- read_analyses(p)[[1]]; a$verified_only <- TRUE
  expect_equal(run_meta_analysis(p, a)$data$study_id, "study-a")

  # plots draw without error
  f <- tempfile(fileext = ".png"); grDevices::png(f, 900, 600)
  plot_forest(ma); plot_cumulative(ma); plot_forest(or); plot_variable(p, "IL6")
  grDevices::dev.off()
  expect_true(file.size(f) > 0)

  sof <- summary_of_findings(run_all_meta_analyses(p))
  expect_equal(sof$studies, c(3, 4, 1))
})

test_that("group categories and vocabulary for prompts", {
  p <- ma_project()
  r <- read_rules(p)
  gc <- group_category(list(list(group_id = "a", name = "mild pe"), list(group_id = "b", name = "x", category = "control"),
                            list(group_id = "c", name = "Other")), r)
  expect_equal(unname(gc), c("PE", "Control", NA))
  v <- rules_for_prompt(p)
  expect_equal(v$known_variables, "- IL6 (IL-6); unit: pg/mL")
  expect_match(v$group_categories, "- PE (also: Preeclampsia, Mild PE, Severe PE)\n- Control (also: Controls)", fixed = TRUE)
})

test_that("invalid analyses are reported", {
  p <- local_project()
  writeLines(c("analyses:", "  - name: x", "    variable: v", "    measure: SMD", "    compare: [A]"), file.path(p, "_analyses.yml"))
  expect_error(read_analyses(p), "two group categories")
  writeLines(c("analyses:", "  - name: x", "    variable: v", "    measure: XYZ"), file.path(p, "_analyses.yml"))
  expect_error(read_analyses(p), "must be one of")
})
