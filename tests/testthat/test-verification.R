test_that("verification status follows decisions and value changes", {
  p <- local_project()
  add_study_data(p, "smith2020", example_metadata(), example_data())
  st <- verification_status(p)
  expect_equal(st$status, rep("unverified", 3))

  set_verification(p, "smith2020", c("dp1", "dp2"), "verified", reviewer = "AHJ")
  set_verification(p, "smith2020", "dp3", "rejected", reviewer = "AHJ", note = "Wrong group")
  st <- verification_status(p)
  expect_equal(st$status, c("verified", "verified", "rejected"))
  expect_equal(st$note[3], "Wrong group")
  expect_error(set_verification(p, "smith2020", "dp9"), "Unknown measurement_id")

  # editing notes/source keeps the verification; editing a value invalidates it
  update_data_point(p, "smith2020", "dp1", source = "Table 2, row 3", verify = FALSE)
  expect_equal(verification_status(p)$status[1], "verified")
  d <- read_study(p, "smith2020")$data; d$value1[2] <- 9.9
  add_study_data(p, "smith2020", read_study(p, "smith2020")$metadata, d, overwrite = TRUE)   # e.g. re-import
  expect_equal(verification_status(p)$status, c("verified", "changed", "rejected"))

  set_verification(p, "smith2020", "dp2", "unverified")
  expect_equal(verification_status(p)$status[2], "unverified")
})

test_that("update_data_point validates, documents and verifies corrections", {
  p <- local_project()
  add_study_data(p, "smith2020", example_metadata(), example_data())
  log <- update_data_point(p, "smith2020", "dp1", value1 = "5.2", value3 = NA, reviewer = "AHJ")
  expect_equal(log, "value1: 5.1 -> 5.2")
  expect_equal(read_study(p, "smith2020")$data$value1[1], 5.2)
  st <- verification_status(p)
  expect_equal(st$status[1], "verified")
  expect_equal(st$note[1], "Corrected: value1: 5.1 -> 5.2")

  expect_error(update_data_point(p, "smith2020", "dp1", value1 = "abc"), "must be a number")
  expect_error(update_data_point(p, "smith2020", "dp1", group_id = "g9"), "not defined")
  expect_equal(read_study(p, "smith2020")$data$group_id[1], "g1")                # unchanged after failed edit
})

test_that("load_metawoRld adds status, drops rejected and can keep verified only", {
  p <- local_project()
  add_study_data(p, "smith2020", example_metadata(), example_data())
  set_verification(p, "smith2020", "dp1", "verified")
  set_verification(p, "smith2020", "dp3", "rejected")
  w <- suppressMessages(load_metawoRld(p))
  expect_equal(w$studies_data$measurement_id, c("dp1", "dp2"))
  expect_equal(w$studies_data$verification_status, c("verified", "unverified"))
  expect_equal(nrow(suppressMessages(load_metawoRld(p, drop_rejected = FALSE))$studies_data), 3)
  expect_equal(suppressMessages(load_metawoRld(p, verified_only = TRUE))$studies_data$measurement_id, "dp1")

  add_variable_synonym(p, "X", "Biomarker X")
  expect_equal(suppressMessages(load_metawoRld(p))$studies_data$variable_canonical, c("Biomarker X", "Biomarker X"))
})
