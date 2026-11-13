test_that("submit_study validates, stages and records gate warnings", {
  p <- team_project()
  m <- example_metadata(); m$doi <- "10.1000/xyz"
  d <- example_data(); d$value2[1] <- -1                     # negative SD -> value warning
  id <- suppressMessages(submit_study(p, "smith2020", m, d, person = "ahj", channel = "entry_form"))
  expect_match(id, "^\\d{8}T\\d{6}-ahj-extraction$")
  expect_equal(list_studies(p), character(0))                # nothing accepted yet
  s <- read_submission(p, "smith2020", id)
  expect_equal(s$submission$status, "pending")
  expect_equal(s$submission$form_version, "1.0.0")
  expect_true(length(s$submission$gate$warnings) >= 1)
  expect_equal(s$study$reports[[1]]$doi, "10.1000/xyz")

  bad <- example_data(); bad$group_id[1] <- "nope"
  expect_error(submit_study(p, "smith2020", m, bad, person = "ahj"), class = "metawoRld_validation_error")
  expect_error(submit_study(p, "smith2020", m, d, person = "stranger"), "Unknown person")
  expect_error(submit_study(p, "Smith 2020", m, d, person = "ahj"), "Invalid study id")
  expect_equal(nrow(list_submissions(p)), 1)
})

test_that("CSV channel: template files round-trip through submit_study", {
  p <- team_project()
  dir <- file.path(tempfile(), "entry")
  submission_template(p, dir)
  expect_true(all(file.exists(file.path(dir, c("metadata.yml", "data.csv")))))
  expect_error(submission_template(p, dir), "already exist")
  yaml::write_yaml(example_metadata(), file.path(dir, "metadata.yml"))
  utils::write.csv(example_data(), file.path(dir, "data.csv"), row.names = FALSE, na = "")
  id <- suppressMessages(submit_study(p, "smith2020", file.path(dir, "metadata.yml"), file.path(dir, "data.csv"),
                                      person = "kb", channel = "csv_import"))
  expect_equal(nrow(read_submission(p, "smith2020", id)$data), 3)
})

test_that("accept_submission writes data with provenance and links records", {
  p <- team_project()
  add_records(p, data.frame(title = "A study of X", doi = "10.1000/xyz"))
  m <- example_metadata(); m$doi <- "10.1000/xyz"
  id1 <- suppressMessages(submit_study(p, "smith2020", m, example_data(), person = "datafindr",
                                       channel = "datafindr", tool = "DataFindR@0.3.0", model = "llama3.1",
                                       source_sha256 = "sha256:abc"))
  suppressMessages(accept_submission(p, "smith2020", id1, person = "ahj"))
  s <- read_study(p, "smith2020")
  expect_equal(unique(s$provenance$extraction_id), id1)
  expect_equal(unique(s$provenance$model), "llama3.1")
  expect_equal(unique(s$provenance$tool), "DataFindR@0.3.0")
  expect_equal(read_records(p)$study_id, "smith2020")
  expect_equal(list_submissions(p)$status, "accepted")
  expect_error(accept_submission(p, "smith2020", id1), "not pending")

  # a corrected re-extraction replaces the data; verification of unchanged rows is kept
  set_verification(p, "smith2020", c("dp1", "dp2"), reviewer = "ahj")
  Sys.sleep(1)
  d <- example_data(); d$value1[2] <- 4.4
  id2 <- suppressMessages(submit_study(p, "smith2020", m, d, person = "kb", channel = "entry_form"))
  suppressMessages(accept_submission(p, "smith2020", id2, person = "ahj"))
  expect_equal(verification_status(p)$status, c("verified", "changed", "unverified"))
  subs <- list_submissions(p)
  expect_equal(subs$status[subs$extraction_id == id1], "superseded")
  expect_equal(unique(read_study(p, "smith2020")$provenance$person), "kb")
})

test_that("dual extraction: compare and reconcile with provenance", {
  p <- team_project()
  m <- example_metadata()
  id1 <- suppressMessages(submit_study(p, "smith2020", m, example_data(), person = "ahj", blind = TRUE))
  Sys.sleep(1)
  d2 <- example_data(); d2$value1[1] <- 5.2; d2$measurement_id <- paste0("k", 1:3)
  d2 <- rbind(d2, transform(d2[1, ], measurement_id = "k4", variable_name = "Y"))
  m2 <- m; m2$journal <- "J Testing"
  id2 <- suppressMessages(submit_study(p, "smith2020", m2, d2, person = "kb", blind = TRUE))
  expect_true(all(list_submissions(p)$blind))

  cmp <- compare_extractions(p, "smith2020")
  dis <- cmp[!cmp$agree, ]
  expect_equal(sort(unique(dis$field)), sort(c("journal", "value1", "(row)", "sample_size", "value2", "p_value")))
  expect_true(all(cmp$agree[cmp$part == "data" & grepl("^Age", cmp$key)]))   # different measurement_ids still line up

  expect_error(reconcile(p, "smith2020", person = "ahj"), class = "metawoRld_unresolved")
  keyX <- dis$key[dis$field == "value1" & grepl("^X", dis$key)]
  keyY <- unique(dis$key[grepl("^Y", dis$key)])
  choices <- data.frame(part = c("metadata", "data", "data"), key = c("journal", keyX, keyY),
                        field = c("journal", "value1", "(row)"), from = c(id2, NA, id2), value = c(NA, "5.15", NA))
  suppressMessages(reconcile(p, "smith2020", choices = choices, person = "ahj"))
  s <- read_study(p, "smith2020")
  expect_equal(s$metadata$journal, "J Testing")
  expect_equal(s$data$value1[s$data$measurement_id == "dp1"], 5.15)
  expect_true("k4" %in% s$data$measurement_id)
  pr <- s$provenance
  expect_equal(pr$channel[pr$measurement_id == "dp1"], "reconcile")          # arbiter's value
  expect_equal(pr$person[pr$measurement_id == "dp1"], "ahj")
  expect_equal(pr$extraction_id[pr$measurement_id == "dp2"], id1)
  expect_equal(pr$agreed_with[pr$measurement_id == "dp2"], id2)               # both extractors agreed
  expect_equal(pr$extraction_id[pr$measurement_id == "k4"], id2)
  expect_equal(pr$extraction_id[pr$measurement_id == "_metadata"], id2)
  expect_equal(unique(list_submissions(p)$status), "accepted")
})

test_that("check_submission re-checks folders written by other tools", {
  p <- team_project()
  id <- suppressMessages(submit_study(p, "smith2020", example_metadata(), example_data(), person = "ahj"))
  dc <- file.path(p, "submissions", "smith2020", id, "data.csv")
  x <- readLines(dc); x[2] <- sub(",g1,", ",g7,", x[2]); writeLines(x, dc)
  expect_warning(check_submission(p, "smith2020", id), "error")
  res <- suppressWarnings(check_submission(p, "smith2020", id))
  expect_true(any(grepl("g7", res$errors)))
  expect_equal(list_submissions(p)$status, "rejected")
  expect_error(accept_submission(p, "smith2020", id), "not pending")
})

test_that("compare_with_accepted shows what a re-extraction would change", {
  p <- team_project()
  id <- suppressMessages(submit_study(p, "smith2020", example_metadata(), example_data(), person = "ahj"))
  expect_null(compare_with_accepted(p, "smith2020", id))                 # not accepted yet
  suppressMessages(accept_submission(p, "smith2020", id, person = "ahj"))
  d <- example_data(); d$value2[2] <- 2.54; d <- d[-3, ]
  m <- example_metadata(); m$journal <- "J Other"
  id2 <- suppressMessages(submit_study(p, "smith2020", m, d, person = "kb"))
  ch <- compare_with_accepted(p, "smith2020", id2)
  expect_equal(ch$field[ch$part == "data"], c("value2", "(data point)"))
  expect_equal(ch$submitted[ch$field == "value2"], "2.54")
  expect_equal(ch$id[ch$part == "metadata"], "journal")
  same <- suppressMessages(submit_study(p, "smith2020", example_metadata(), example_data(), person = "kb"))
  expect_equal(nrow(compare_with_accepted(p, "smith2020", same)), 0)
})
