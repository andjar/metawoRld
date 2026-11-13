test_that("example_project builds a clean, complete project", {
  p <- example_project(file.path(withr::local_tempdir(), "il6"), website = FALSE)
  expect_equal(list_studies(p), c("afshari2005", "dewi2026", "gencheva2022", "vitoratos2010"))
  expect_equal(nrow(suppressMessages(check_project(p))), 0)
  pc <- prisma_counts(p)
  n <- stats::setNames(pc$n, pc$stage)
  expect_equal(unname(n[c("Records identified", "Records excluded (title/abstract)", "Reports excluded (full text)",
                          "Studies included", "Reports of included studies")]), c(6, 1, 1, 4, 4))
  # dual extraction: the arbiter kept Table 2 (mean +/- SE); ext2 agreed on the antepartum values
  v <- read_study(p, "vitoratos2010")
  expect_equal(unique(v$data$statistic_type), "mean_se")
  expect_true(all(nzchar(v$provenance$agreed_with[v$provenance$measurement_id %in% c("dp1", "dp2")])))
  expect_equal(nrow(suppressMessages(check_values(p))), 0)          # incl. Gencheva subgroups vs total
  # refinement: Gencheva's subgroups are linked; the main analysis still uses its total
  g <- read_study(p, "gencheva2022")$metadata$groups
  expect_equal(vapply(g, function(x) x$parent_group_id %||% NA_character_, "")[4:5], c("pe", "pe"))
  ma <- run_meta_analysis(p, "IL-6 in preeclampsia vs normotensive pregnancy")
  sel <- ma$selection[ma$selection$study_id == "gencheva2022", ]
  expect_equal(sel$how, c("reported", "reported"))
  expect_equal(sel$n, c(37, 50))
  expect_equal(get_schema(p)$`_form`$version, "1.1.0")
})

test_that("example values match the published articles", {
  src <- system.file("extdata", "il6-preeclampsia", "extractions", package = "metawoRld")
  il6 <- function(f) {
    d <- dplyr::bind_rows(lapply(yaml::read_yaml(file.path(src, f))$data_points, as.data.frame))
    d[d$variable_name == "IL-6", c("group_id", "timepoint", "sample_size", "statistic_type", "value1", "value2")]
  }
  a <- il6("afshari2005.yml")       # BMC Pregnancy Childbirth 2005;5:14, Results
  expect_equal(a$value1, c(5.8, 3.01)); expect_equal(a$value2, c(4.85, 2.45)); expect_equal(a$sample_size, c(24, 18))
  v <- il6("vitoratos2010-ext1.yml")  # Mediators Inflamm 2010:908649, Table 2 (mean +/- SE)
  expect_equal(v$value1, c(2.66, 2.44, 3.53, 1.69)); expect_equal(v$value2, c(1.37, 0.85, 0.52, 0.48))
  g <- il6("gencheva2022.yml")      # Cardiovasc J Afr 2022;33:65-73, p. 67
  expect_equal(g$value1[1:3], c(8.06, 5.08, 2.77)); expect_equal(g$value2[1:3], c(12.48, 5.16, 2.43))
  expect_equal(round((31 * 6.46 + 6 * 16.34) / 37, 2), 8.06)   # subgroups pool to the overall mean
  d <- il6("dewi2026.yml")          # Narra J 2026;6:e3016, Table 2 (median, min-max)
  expect_equal(d$value1, c(16.05, 3.71)); expect_equal(d$sample_size, c(34, 34))
})
