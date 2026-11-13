skip_if(!nzchar(Sys.which("git")), "git not installed")

git <- function(dir, ...) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE))
  invisible(out)
}

test_that("create_metawoRld commits, and sync shares submissions both ways", {
  withr::local_envvar(GIT_AUTHOR_NAME = "Test", GIT_AUTHOR_EMAIL = "t@example.org",
                      GIT_COMMITTER_NAME = "Test", GIT_COMMITTER_EMAIL = "t@example.org")
  root <- withr::local_tempdir()
  remote <- file.path(root, "remote.git")
  system2("git", c("init", "-q", "--bare", shQuote(remote)))

  # admin creates the project and publishes it
  a <- file.path(root, "admin")
  suppressMessages(create_metawoRld(a, "Shared review", "Testing sync", website = FALSE))
  expect_true(sync_status(a)$repo)
  expect_equal(sync_status(a)$changes, 0L)                       # initial commit made
  expect_true("screening.csv merge=union" %in% readLines(file.path(a, ".gitattributes")))
  add_person(a, "ahj", "Anders", roles = c("admin", "arbiter", "extractor"))
  add_person(a, "kb", "Kari", roles = c("extractor"))
  git(a, "remote", "add", "origin", shQuote(remote))
  res <- sync_project(a, "Team")
  expect_true(res$committed); expect_true(res$pushed)

  # a colleague clones, submits a study and syncs
  k <- file.path(root, "kari")
  system2("git", c("clone", "-q", shQuote(remote), shQuote(k)))
  options(metawoRld.person = "kb"); on.exit(options(metawoRld.person = NULL), add = TRUE)
  id <- suppressMessages(submit_study(k, "smith2020", example_metadata(), example_data(), person = "kb"))
  r2 <- sync_project(k, "Submit smith2020")
  expect_true(r2$pushed)

  # the admin syncs: the submission arrives in the inbox
  r3 <- sync_project(a)
  expect_equal(r3$new_submissions$extraction_id, id)
  expect_match(r3$message, "1 new submission from the team \\(kb\\)")
  expect_equal(list_submissions(a, status = "pending")$extraction_id, id)

  # parallel screening by both is merged line by line, not as a conflict
  add_records(a, data.frame(title = "A", doi = "10.1000/a")); sync_project(a); sync_project(k)
  add_screening(a, "10.1000/a", "title_abstract", "include", person = "ahj")
  add_screening(k, "10.1000/a", "title_abstract", "exclude", person = "kb")
  sync_project(a)
  expect_silent(r4 <- sync_project(k))
  expect_setequal(read.csv(file.path(k, "screening.csv"))$person, c("ahj", "kb"))
})

test_that("sync explains when there is no repository or remote", {
  p <- local_project()                          # created without git
  expect_false(sync_status(p)$repo)
  expect_error(sync_project(p), "not a Git repository")
  expect_false(commit_project(p, "x"))
  withr::local_envvar(GIT_AUTHOR_NAME = "T", GIT_AUTHOR_EMAIL = "t@x.org", GIT_COMMITTER_NAME = "T", GIT_COMMITTER_EMAIL = "t@x.org")
  q <- file.path(withr::local_tempdir(), "solo")
  suppressMessages(create_metawoRld(q, "Solo", "No remote", website = FALSE))
  add_person(q, "ahj", "Anders")
  res <- sync_project(q)
  expect_true(res$committed); expect_false(res$pushed)
  expect_match(res$message, "No shared repository")
})
