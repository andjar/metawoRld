test_that("valid example passes and links are checked", {
  p <- local_project()
  s <- get_schema(p)
  m <- example_metadata(); m$study_id <- "x"
  expect_length(validate_against_schema(m, s$metadata), 0)
  expect_length(validate_against_schema(example_data(), s$data_points), 0)
  expect_length(validate_links(m, example_data(), s), 0)

  d <- example_data(); d$group_id[1] <- "nope"
  expect_match(validate_links(m, d, s), "'nope' not defined in metadata.groups")
})

test_that("type, enum, NA and empty-array rules", {
  p <- local_project()
  s <- get_schema(p)
  d <- example_data()
  d$value1[2] <- NA            # required number missing
  d$statistic_type[3] <- "sd"  # not in enum
  errs <- validate_against_schema(d, s$data_points)
  expect_length(errs, 2)
  expect_match(errs[1], "\\[2\\]\\.value1: required")
  expect_match(errs[2], "'sd' is not one of")

  # zero data points are allowed
  expect_length(validate_against_schema(list(), s$data_points), 0)
  expect_length(validate_against_schema(d[0, ], s$data_points), 0)

  # non-integer n
  d <- example_data(); d$sample_size[1] <- 2.5
  expect_match(validate_against_schema(d, s$data_points), "expected an integer")

  # strict mode
  sch <- list(`_type` = "object", a = list(`_type` = "string"))
  expect_match(validate_against_schema(list(a = "x", b = 1), sch, strict = TRUE), "unknown field")
})

test_that("data frames from ellmer (list columns) validate as arrays of objects", {
  p <- local_project()
  s <- get_schema(p)
  m <- example_metadata(); m$study_id <- "x"
  m$methods <- data.frame(method_id = "m1", name = "X", unit = "pg/mL")
  m$authors <- c("A", "B")
  expect_length(validate_against_schema(m, s$metadata), 0)
})
