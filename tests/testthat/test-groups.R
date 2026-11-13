# A study like Gencheva 2022: PE total, early/late subgroups, controls.
gencheva_meta <- function(link = TRUE) {
  m <- example_metadata()
  m$groups <- list(
    list(group_id = "pe", name = "Pre-eclampsia", sample_size = 37L, category = "PE"),
    list(group_id = "pe_early", name = "Early onset", sample_size = 31L),
    list(group_id = "pe_late", name = "Late onset", sample_size = 6L),
    list(group_id = "ctrl", name = "Controls", sample_size = 50L, category = "Control")
  )
  if (link) {
    m$groups[[2]]$parent_group_id <- "pe"; m$groups[[2]]$onset <- "early"
    m$groups[[3]]$parent_group_id <- "pe"; m$groups[[3]]$onset <- "late"
  }
  m
}
gencheva_data <- function(total = TRUE) {
  d <- data.frame(measurement_id = paste0("dp", 1:4), method_id = "m1", group_id = c("pe", "ctrl", "pe_early", "pe_late"),
                  variable_name = "IL-6", timepoint = "Antepartum", sample_size = c(37L, 50L, 31L, 6L),
                  statistic_type = "mean_sd", value1 = c(8.06, 2.77, 6.46, 16.34), value2 = c(12.48, 2.43, 7.73, 25.78),
                  source = "Table 1")
  if (!total) d <- d[d$group_id != "pe", ]
  d
}
group_project <- function(env = parent.frame()) {
  p <- team_project(env)
  f <- file.path(p, "forms", "extraction.yml")
  x <- readLines(f)
  at <- grep("^      parent_group_id:", x)
  x <- append(x, c("      onset:", "        _type: enum", "        _values: [early, late, not reported]",
                   "        _description: \"Onset of preeclampsia.\"", "        _required: false"), after = at + 3)
  writeLines(x, f)
  r <- read_rules(p)
  r$group_categories <- list(PE = list(synonyms = list("Preeclampsia")), Control = list(),
                             PE_early = list(where = list(category = "PE", onset = "early")),
                             PE_late = list(where = list(category = "PE", onset = "late")),
                             Pregnant = list(where = list(category = "All")))
  metawoRld:::.write_rules(file.path(p, "_rules.yml"), r)
  p
}

test_that("subgroups inherit facts; a group can belong to several categories", {
  p <- group_project()
  rules <- read_rules(p)
  mem <- group_memberships(gencheva_meta()$groups, rules)
  expect_setequal(mem$category[mem$group_id == "pe_early"], c("PE", "PE_early"))
  expect_equal(mem$category[mem$group_id == "pe"], "PE")
  expect_equal(unname(group_category(gencheva_meta()$groups, rules)[c("pe", "pe_early", "ctrl")]), c("PE", "PE_early", "Control"))
  # unlinked subgroups belong to no category
  expect_false(any(c("pe_early", "pe_late") %in% group_memberships(gencheva_meta(link = FALSE)$groups, rules)$group_id))
})

test_that("each analysis uses one level per study: never a total and its subgroups", {
  p <- group_project()
  add_study_data(p, "gencheva2022", gencheva_meta(), gencheva_data())
  a <- list(name = "PE vs Control", variable = "IL-6", measure = "SMD", compare = c("PE", "Control"))
  ad <- analysis_data(p, a)
  expect_equal(ad$data$n1, 37)                         # the reported total, not 37 + 31 + 6
  expect_equal(ad$data$mean1, 8.06)
  expect_equal(ad$selection$how[ad$selection$arm == "PE"], "reported")

  ad <- analysis_data(p, utils::modifyList(a, list(levels = "pool")))
  expect_equal(ad$data$n1, 37)
  expect_equal(round(ad$data$mean1, 2), 8.06)          # subgroups pooled reproduce the total
  expect_equal(round(ad$data$sd1, 2), 12.48)
  expect_match(ad$selection$how[ad$selection$arm == "PE"], "pooled subgroups \\(pe_early \\+ pe_late\\)")

  ad <- analysis_data(p, utils::modifyList(a, list(compare = c("PE_early", "Control"))))
  expect_equal(ad$data$n1, 31)
})

test_that("a missing total is replaced by its complete subgroups", {
  p <- group_project()
  add_study_data(p, "gencheva2022", gencheva_meta(), gencheva_data(total = FALSE))
  ad <- analysis_data(p, list(name = "x", variable = "IL-6", measure = "SMD", compare = c("PE", "Control")))
  expect_equal(ad$data$n1, 37)
  expect_match(ad$data$note, "PE: pooled subgroups")
  # incomplete subgroups are not passed off as the total
  m <- gencheva_meta(); m$groups[[3]]$sample_size <- 5L
  d <- gencheva_data(total = FALSE); d$sample_size[d$group_id == "pe_late"] <- 5L
  add_study_data(p, "gencheva2022", m, d, overwrite = TRUE)
  ad <- analysis_data(p, list(name = "x", variable = "IL-6", measure = "SMD", compare = c("PE", "Control")))
  expect_equal(nrow(ad$data), 0)
  expect_match(ad$excluded$reason, "PE: 'pe' has no usable value and its subgroups \\(pe_early, pe_late\\) do not add up")
})

test_that("old-style tagging of a total and its subgroups does not double count", {
  p <- group_project()
  m <- gencheva_meta(); for (j in 2:3) m$groups[[j]]$category <- "PE"
  add_study_data(p, "gencheva2022", m, gencheva_data())
  ad <- analysis_data(p, list(name = "x", variable = "IL-6", measure = "SMD", compare = c("PE", "Control")))
  expect_equal(ad$data$n1, 37)
})

test_that("arms that overlap are refused", {
  p <- group_project()
  m <- gencheva_meta()
  m$groups <- c(list(list(group_id = "all", name = "Whole cohort", sample_size = 87L, category = "All")),
                lapply(m$groups, function(g) { if (g$group_id %in% c("pe", "ctrl")) g$parent_group_id <- "all"; g }))
  d <- rbind(gencheva_data(), transform(gencheva_data()[1, ], measurement_id = "dp5", group_id = "all", sample_size = 87L, value1 = 5))
  add_study_data(p, "cohort2020", m, d)
  ad <- analysis_data(p, list(name = "x", variable = "IL-6", measure = "SMD", compare = c("PE", "Pregnant")))
  expect_equal(nrow(ad$data), 0)
  expect_match(ad$excluded$reason, "overlap")
})

test_that("subgroup analyses split a shared control group and test differences", {
  skip_if_not_installed("metafor")
  p <- group_project()
  add_study_data(p, "gencheva2022", gencheva_meta(), gencheva_data())
  d2 <- gencheva_data(); d2$value1 <- d2$value1 * 0.8
  add_study_data(p, "other2021", gencheva_meta(), d2)
  a <- list(name = "By onset", variable = "IL-6", measure = "SMD", compare = c("PE", "Control"), subgroups = c("PE_early", "PE_late"))
  ma <- run_meta_analysis(p, a)
  expect_equal(nrow(ma$data), 4)                                      # 2 studies x 2 subgroups
  expect_equal(unique(ma$data$n2), 25)                                # 50 controls split between 2 subgroups
  expect_true(all(grepl("shared by 2 subgroups", ma$data$note)))
  expect_equal(ma$subgroups$subgroup, c("PE_early", "PE_late"))
  expect_match(ma$subgroup_test, "Test for subgroup differences")
  expect_equal(sum(ma$selection$n[ma$selection$arm == "Control"]), 100)  # each control woman counted once
})

test_that("subgroups are checked against their total", {
  p <- group_project()
  add_study_data(p, "gencheva2022", gencheva_meta(), gencheva_data())
  expect_equal(nrow(suppressMessages(check_values(p))), 0)            # 31 + 6 = 37; pooled 8.06 (12.48)
  d <- gencheva_data(); d$value1[1] <- 9.5
  m <- gencheva_meta(); m$groups[[2]]$sample_size <- 40L
  add_study_data(p, "gencheva2022", m, d, overwrite = TRUE)
  fl <- check_values(p)
  expect_true("subgroup_size" %in% fl$check)
  m$groups[[2]]$sample_size <- 31L
  add_study_data(p, "gencheva2022", m, d, overwrite = TRUE)
  fl <- check_values(p)
  expect_equal(fl$check, "subgroup_total")
  expect_equal(fl$measurement_id, "dp1")
  expect_match(fl$message, "pool to mean 8.06")
})

test_that("group trees are validated", {
  p <- group_project()
  m <- gencheva_meta(); m$groups[[1]]$parent_group_id <- "pe_early"
  err <- expect_error(add_study_data(p, "loop2020", m, gencheva_data()), class = "metawoRld_validation_error")
  expect_true(any(grepl("loop", err$validation_errors)))
  m <- gencheva_meta(); m$groups[[2]]$parent_group_id <- "nope"
  err <- expect_error(add_study_data(p, "bad2020", m, gencheva_data()), class = "metawoRld_validation_error")
  expect_true(any(grepl("'nope' is not a group_id", err$validation_errors)))
})

test_that("refinement: queue of unassessed groups, amendment keeps provenance and verification", {
  p <- group_project()
  id <- suppressMessages(submit_study(p, "gencheva2022", gencheva_meta(link = FALSE), gencheva_data(), person = "kb"))
  suppressMessages(accept_submission(p, "gencheva2022", id, person = "ahj"))
  set_verification(p, "gencheva2022", c("dp1", "dp3"), reviewer = "ahj")
  q <- refinement_queue(p, "onset")
  expect_setequal(q$group_id, c("pe", "pe_early", "pe_late", "ctrl"))
  expect_equal(refinement_queue(p, "onset", category = "PE")$group_id, "pe")
  expect_error(refinement_queue(p, "trimester"), "not a field of groups")

  am <- suppressMessages(amend_study(p, "gencheva2022", person = "ahj", note = "Link onset subgroups",
                                     groups = list(list(group_id = "pe_early", parent_group_id = "pe", onset = "early"),
                                                   list(group_id = "pe_late", parent_group_id = "pe", onset = "late"))))
  expect_equal(read_submission(p, "gencheva2022", am)$submission$kind, "amendment")
  suppressMessages(accept_submission(p, "gencheva2022", am, person = "ahj"))
  s <- read_study(p, "gencheva2022")
  expect_equal(s$metadata$groups[[2]]$onset, "early")
  pr <- s$provenance
  expect_equal(pr$person[pr$measurement_id == "dp1"], "kb")           # value unchanged: original provenance
  expect_equal(pr$extraction_id[pr$measurement_id == "_metadata"], am) # metadata changed by the amendment
  expect_equal(verification_status(p)$status[verification_status(p)$measurement_id == "dp1"], "verified")
  expect_equal(refinement_queue(p, "onset", category = "PE")$group_id, "pe")  # only the (mixed-onset) total is left
})

test_that("add_group_fact adds an optional field and bumps the minor version", {
  p <- local_project()
  expect_equal(add_group_fact(p, "onset", c("early", "late"), "Onset."), "1.1.0")
  s <- get_schema(p)
  expect_equal(s$metadata$groups$`_items`$onset$`_values`, c("early", "late"))
  expect_false(s$metadata$groups$`_items`$onset$`_required`)
  expect_error(add_group_fact(p, "onset", NULL, "again"), "already have")
  expect_error(add_group_fact(p, "no", NULL, "x"), "TRUE/FALSE")
  expect_equal(add_group_fact(p, "severity", NULL, "Free text."), "1.2.0")
})
