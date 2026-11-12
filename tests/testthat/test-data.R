test_that("add, read, load round trip keeps types and joins references", {
  p <- local_project()
  expect_silent(add_study_data(p, "https://doi.org/10.1000/ABC", example_metadata(), example_data()))
  expect_equal(list_studies(p), "10.1000/abc")
  expect_true(dir.exists(file.path(p, "data", "10.1000_fslash_abc")))

  s <- read_study(p, "10.1000/abc")
  expect_equal(s$metadata$study_id, "10.1000/abc")
  expect_type(s$data$p_value, "character")          # stays a string after CSV round trip
  expect_true(validate_study("10.1000/abc", p))

  w <- suppressMessages(load_metawoRld(p))
  expect_equal(nrow(w$studies), 1)
  expect_equal(w$studies$authors, "Smith J; Doe A")
  d <- w$studies_data
  expect_equal(nrow(d), 3)
  expect_equal(d$method_unit[1], "pg/mL")
  expect_equal(d$group_name, c("Cases", "Controls", "Cases"))
  expect_true(is.na(d$method_name[3]))              # POP_CHAR has no method
})

test_that("add_study_data reports all problems and respects overwrite", {
  p <- local_project()
  m <- example_metadata(); m$year <- "twenty"; m$title <- NULL
  d <- example_data(); d$group_id[1] <- "bad"
  err <- expect_error(add_study_data(p, "s1", m, d), class = "metawoRld_validation_error")
  expect_length(err$validation_errors, 3)

  add_study_data(p, "s1", example_metadata(), example_data())
  expect_error(add_study_data(p, "s1", example_metadata(), example_data()), "already exists")
  # overwrite removes stale files
  writeLines("x", file.path(p, "data", "s1", "old.txt"))
  add_study_data(p, "s1", example_metadata(), example_data()[1, ], overwrite = TRUE)
  expect_false(file.exists(file.path(p, "data", "s1", "old.txt")))
  expect_equal(nrow(read_study(p, "s1")$data), 1)
})

test_that("studies with zero data points and data-frame metadata are stored", {
  p <- local_project()
  m <- example_metadata()
  m$groups <- data.frame(group_id = c("g1", "g2"), name = c("A", "B"))
  add_study_data(p, "s0", m, data.frame())
  y <- yaml::read_yaml(file.path(p, "data", "s0", "metadata.yml"))
  expect_equal(y$groups[[2]]$name, "B")             # row-oriented YAML
  expect_true(validate_study("s0", p))
})

test_that("templates and validate_world", {
  p <- local_project()
  add_study_template(p, "manual1")
  expect_true(file.exists(file.path(p, "data", "manual1", "metadata.yml")))
  add_study_data(p, "good", example_metadata(), example_data())
  res <- suppressMessages(validate_world(p))
  expect_equal(res$valid[res$study_id == "good"], TRUE)
  expect_equal(res$valid[res$study_id == "manual1"], FALSE)   # placeholders fail type checks
})
