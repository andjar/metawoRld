test_that("add, read, load round trip keeps types and joins references", {
  p <- local_project()
  m <- example_metadata(); m$doi <- "https://doi.org/10.1000/ABC"
  expect_silent(add_study_data(p, "smith2020", m, example_data()))
  expect_equal(list_studies(p), "smith2020")
  dir <- file.path(p, "data", "smith2020")
  expect_true(all(file.exists(file.path(dir, c("study.yml", "metadata.yml", "data.csv", "provenance.csv", "datapackage.json")))))

  s <- read_study(p, "smith2020")
  expect_equal(s$metadata$study_id, "smith2020")
  expect_equal(s$study$reports[[1]]$doi, "10.1000/abc")   # reports from metadata, normalized
  expect_equal(s$study$form_version, "1.0.0")
  expect_equal(s$provenance$measurement_id, c("_metadata", "dp1", "dp2", "dp3"))
  expect_type(s$data$p_value, "character")          # stays a string after CSV round trip
  expect_true(validate_study("smith2020", p))

  w <- suppressMessages(load_metawoRld(p))
  expect_equal(nrow(w$studies), 1)
  expect_equal(w$studies$authors, "Smith J; Doe A")
  d <- w$studies_data
  expect_equal(nrow(d), 3)
  expect_equal(d$method_unit[1], "pg/mL")
  expect_equal(d$group_name, c("Cases", "Controls", "Cases"))
  expect_true(is.na(d$method_name[3]))              # POP_CHAR has no method
})

test_that("study ids are checked and suggested", {
  expect_equal(is_study_id(c("hansen2019", "hansen2019b", "a-b", "AB12", "ab", "10.1000/x", NA)),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  p <- local_project()
  expect_error(add_study_data(p, "10.1000/abc", example_metadata(), example_data()), "Invalid study id")
  expect_equal(suggest_study_id("Hansen A", 2019, p), "hansen2019")
  expect_equal(suggest_study_id(list("Ødegård K", "B"), "2020", p), "odegard2020")
  add_study_data(p, "hansen2019", example_metadata(), example_data())
  expect_equal(suggest_study_id("Hansen, A.", 2019, p), "hansen2019b")
})

test_that("add_study_data reports all problems and respects overwrite", {
  p <- local_project()
  m <- example_metadata(); m$year <- "twenty"; m$title <- NULL
  d <- example_data(); d$group_id[1] <- "bad"
  err <- expect_error(add_study_data(p, "smith2020", m, d), class = "metawoRld_validation_error")
  expect_length(err$validation_errors, 3)

  add_study_data(p, "smith2020", example_metadata(), example_data())
  expect_error(add_study_data(p, "smith2020", example_metadata(), example_data()), "already exists")
  # overwrite removes stale files
  writeLines("x", file.path(p, "data", "smith2020", "old.txt"))
  add_study_data(p, "smith2020", example_metadata(), example_data()[1, ], overwrite = TRUE)
  expect_false(file.exists(file.path(p, "data", "smith2020", "old.txt")))
  expect_equal(nrow(read_study(p, "smith2020")$data), 1)
})

test_that("overwriting keeps the provenance of unchanged values", {
  p <- local_project()
  add_study_data(p, "smith2020", example_metadata(), example_data(),
                 provenance = list(person = "ahj", channel = "entry_form", extraction_id = "e1"))
  d <- example_data(); d$value1[2] <- 9.9; d$source[1] <- "Table 3"   # source does not count as a value
  add_study_data(p, "smith2020", example_metadata(), d, overwrite = TRUE,
                 provenance = list(person = "kb", channel = "manual_edit"))
  pr <- read_study(p, "smith2020")$provenance
  expect_equal(pr$person, c("ahj", "ahj", "kb", "ahj"))
  expect_equal(pr$channel, c("entry_form", "entry_form", "manual_edit", "entry_form"))
})

test_that("studies with zero data points and data-frame metadata are stored", {
  p <- local_project()
  m <- example_metadata()
  m$groups <- data.frame(group_id = c("g1", "g2"), name = c("A", "B"))
  add_study_data(p, "empty2020", m, data.frame())
  y <- yaml::read_yaml(file.path(p, "data", "empty2020", "metadata.yml"))
  expect_equal(y$groups[[2]]$name, "B")             # row-oriented YAML
  expect_true(validate_study("empty2020", p))
})

test_that("validate_world reports studies edited into an invalid state", {
  p <- local_project()
  add_study_data(p, "good2020", example_metadata(), example_data())
  add_study_data(p, "bad2020", example_metadata(), example_data())
  f <- file.path(p, "data", "bad2020", "metadata.yml")
  y <- yaml::read_yaml(f); y$year <- "unknown"; yaml::write_yaml(y, f)
  res <- suppressMessages(validate_world(p))
  expect_equal(res$valid[res$study_id == "good2020"], TRUE)
  expect_equal(res$valid[res$study_id == "bad2020"], FALSE)
})
