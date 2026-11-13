test_that("datapackage.json lists every file with its hash", {
  p <- team_project()
  add_study_data(p, "smith2020", example_metadata(), example_data(), provenance = list(person = "ahj"))
  set_verification(p, "smith2020", "dp1", reviewer = "ahj")
  dir <- file.path(p, "data", "smith2020")
  pkg <- jsonlite::read_json(file.path(dir, "datapackage.json"))
  paths <- vapply(pkg$resources, function(r) r$path, "")
  expect_setequal(paths, c("study.yml", "metadata.yml", "data.csv", "provenance.csv", "verification.csv"))
  r <- pkg$resources[[match("verification.csv", paths)]]
  expect_equal(r$hash, hash_file(file.path(dir, "verification.csv")))       # refreshed by set_verification()
  expect_match(r$hash, "^sha256:[0-9a-f]{64}$")
  expect_equal(pkg$metawoRld$study_id, "smith2020")
  expect_equal(pkg$licenses[[1]]$name, "CC-BY-4.0")
})

test_that("check_project passes a clean project and catches tampering", {
  p <- team_project()
  id <- suppressMessages(submit_study(p, "smith2020", example_metadata(), example_data(), person = "ahj"))
  res <- suppressMessages(check_project(p))
  expect_equal(res$level, "warning")                                         # pending submission
  suppressMessages(accept_submission(p, "smith2020", id, person = "ahj"))
  expect_equal(nrow(suppressMessages(check_project(p))), 0)

  dc <- file.path(p, "data", "smith2020", "data.csv")
  x <- readLines(dc); x[2] <- sub("5.1", "9.9", x[2], fixed = TRUE); writeLines(x, dc)
  res <- suppressMessages(check_project(p))
  expect_true(any(res$level == "error" & grepl("changed outside metawoRld", res$message)))

  # restore through metawoRld: the check passes again
  add_study_data(p, "smith2020", example_metadata(), example_data(), overwrite = TRUE, provenance = list(person = "ahj"))
  expect_equal(sum(suppressMessages(check_project(p))$level == "error"), 0)
})

test_that("check_project reports provenance, people and form version problems", {
  p <- team_project()
  add_study_data(p, "smith2020", example_metadata(), example_data(), provenance = list(person = "ghost"))
  dir <- file.path(p, "data", "smith2020")
  pr <- read.csv(file.path(dir, "provenance.csv"), colClasses = "character")
  write.csv(pr[pr$measurement_id != "dp2", ], file.path(dir, "provenance.csv"), row.names = FALSE)
  metawoRld:::.write_datapackage(p, "smith2020")
  f <- file.path(p, "forms", "extraction.yml")
  writeLines(sub("version: 1.0.0", "version: 2.0.0", readLines(f), fixed = TRUE), f)
  dir.create(file.path(p, "data", "Bad Folder"))
  res <- suppressMessages(check_project(p))
  expect_true(any(grepl("No provenance for: dp2", res$message)))
  expect_true(any(grepl("unknown person/tool: ghost", res$message)))
  expect_true(any(grepl("form version 1.0.0", res$message)))
  expect_true(any(res$where == "data/Bad Folder"))
})

test_that("use_github_action_check writes the workflow", {
  p <- local_project()
  wf <- suppressMessages(use_github_action_check(p))
  expect_true(any(grepl("check_project", readLines(wf))))
  expect_error(use_github_action_check(p), "already exists")
})
