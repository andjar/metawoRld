flag_checks <- function(f, id) sort(f$check[f$measurement_id == id])

test_that("built-in consistency checks flag the right data points", {
  p <- local_project()
  d <- example_data()
  d <- rbind(d, data.frame(
    measurement_id = c("dp4", "dp5", "dp6", "dp7", "dp8"),
    method_id = c("m1", "m1", "POP_CHAR", "POP_CHAR", "m1"),
    group_id = c("g1", "g2", "g1", "g1", "g1"),
    variable_name = c("X", "X", "Smokers", "Smokers2", "X"),
    timepoint = c("Week 30", "Week 30", NA, NA, "Week 24"),
    sample_size = c(20, 30, 20, 20, 20),
    statistic_type = c("median_iqr", "mean_sd", "n_percent", "proportion", "mean_sd"),
    value1 = c(4, 5, 5, 25, 5.1), value2 = c(6, -1, 40, NA, 1.2), value3 = c(8, NA, NA, NA, NA),
    p_value = NA, source = "Table 3"
  ))
  add_study_data(p, "s1", example_metadata(), d)
  f <- check_values(p)
  expect_equal(flag_checks(f, "dp4"), "statistic_consistency")         # median below Q1
  expect_equal(flag_checks(f, "dp5"), c("sample_size", "statistic_consistency"))  # SD < 0, n 30 > 25
  expect_match(f$message[f$measurement_id == "dp6"], "5/20 = 25.0%, but 40% is reported")
  expect_match(f$message[f$measurement_id == "dp7"], "a percentage")
  expect_equal(flag_checks(f, "dp8"), "duplicate")                     # same as dp1
  expect_equal(flag_checks(f, "dp1"), "duplicate")
  expect_length(flag_checks(f, "dp2"), 0)
  expect_true(all(f$severity[f$check == "statistic_consistency"] == "error"))
})

test_that("variables, synonyms, units, conversions and ranges", {
  p <- local_project()
  d <- example_data()
  d$variable_name <- c("x-level", "X", "Age")
  d$unit <- c(NA, "ng/mL", NA)
  d$value1[2] <- 4.2          # 4.2 ng/mL = 4200 pg/mL -> out of range
  add_study_data(p, "s1", example_metadata(), d)

  # no variables defined -> no vocabulary checks
  expect_false(any(check_values(p)$check == "unknown_variable"))

  add_variable_synonym(p, "X", "X")
  add_variable_synonym(p, "x-level", "X")
  r <- read_rules(p)
  r$variables$X$unit <- "pg/mL"
  r$variables$X$convert <- list(`ng/mL` = 1000)
  r$variables$X$range <- list(0, 100)
  f <- check_values(p, rules = r)
  expect_equal(f$check[f$measurement_id == "dp3"], "unknown_variable")   # Age not defined
  expect_match(f$message[f$measurement_id == "dp2" & f$check == "out_of_range"], "4200 pg/mL is outside .* \\(converted from 4.2 ng/mL\\)")
  expect_false(any(f$measurement_id == "dp1"))                           # synonym + method unit pg/mL

  r$variables$X$convert <- NULL
  expect_true(any(check_values(p, rules = r)$check == "unit_mismatch"))
  r$checks$unit_mismatch <- "off"
  expect_false(any(check_values(p, rules = r)$check == "unit_mismatch"))

  expect_error(add_variable_synonym(p, "x-level", "Y"), "already mapped to 'X'")
  # header comments are kept
  expect_match(readLines(file.path(p, "_rules.yml"))[1], "^# metawoRld value rules")
  expect_equal(canonical_variable(c("X-LEVEL", "nope"), read_rules(p)), c("X", NA))
})
