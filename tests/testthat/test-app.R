skip_if_not_installed("shiny"); skip_if_not_installed("bslib"); skip_if_not_installed("DT")
options(metawoRld.remember_recent = FALSE)

test_that("new_review sets up a working project in one step", {
  p <- new_review(file.path(withr::local_tempdir(), "r"), "IL-6 in preeclampsia", "Is IL-6 higher?",
                  groups = c("Preeclampsia", "Normotensive"), variable = "IL-6", unit = "pg/mL",
                  inclusion_criteria = c("Human", ""), person_id = "ahj", person_name = "Anders", git = FALSE)
  expect_equal(read_people(p)$id, "ahj")
  expect_equal(names(read_rules(p)$group_categories), c("Preeclampsia", "Normotensive"))
  expect_equal(read_rules(p)$variables$`IL-6`$unit, "pg/mL")
  a <- read_analyses(p)[[1]]
  expect_equal(a$compare, c("Preeclampsia", "Normotensive")); expect_equal(a$measure, "SMD")
  expect_equal(read_config(p)$inclusion_criteria, "Human")
  expect_true(any(grepl("^# metawoRld meta-analyses", readLines(file.path(p, "_analyses.yml")))))   # comments kept
  expect_equal(nrow(suppressMessages(check_project(p))), 0)
  expect_error(new_review(file.path(withr::local_tempdir(), "x"), "T", design = "two_groups", groups = "Only one",
                          person_id = "a1", person_name = "A", git = FALSE), "name 2 groups")
  q <- new_review(file.path(withr::local_tempdir(), "one"), "Prevalence", design = "one_group", groups = "Pregnant",
                  variable = "Anaemia", person_id = "a1", person_name = "A", git = FALSE)
  expect_equal(read_analyses(q)[[1]]$measure, "MN")
})

test_that("write_analyses validates and round-trips", {
  p <- local_project()
  write_analyses(p, list(list(name = "A", variable = "IL-6", measure = "smd", compare = c("PE", "Control"), levels = "pool")))
  a <- read_analyses(p)$A
  expect_equal(a$measure, "SMD"); expect_equal(a$levels, "pool")
  expect_error(write_analyses(p, list(list(name = "B", variable = "x", measure = "SMD", compare = "PE"))), "two group categories")
})

test_that("reconcile choices are built from one pick per disagreement", {
  dis <- data.frame(part = c("metadata", "metadata", "data", "data", "data"),
                    key = c("groups", "groups", "k1", "k2", "k2"), field = c("groups[g1].name", "groups[g1].n", "(row)", "(row)", "value1"),
                    e1 = c("A", "1", "present", NA, NA), e2 = c("B", "2", NA, "present", "5"), stringsAsFactors = FALSE)
  ch <- metawoRld:::.choices_from_picks(dis, c("e2", "e2", "e2", "e2", "e2"))
  expect_equal(nrow(ch), 4)                                       # metadata key once
  expect_equal(ch$value[ch$key == "k1"], "drop")                   # picked e2, where the row is absent
  expect_equal(ch$from[ch$key == "k2" & ch$field == "(row)"], "e2")
  expect_equal(metawoRld:::.parse_where("category = PE; onset = early|late"), list(category = list("PE"), onset = list("early", "late")))
  expect_equal(metawoRld:::.parse_where(""), list())
})

test_that("the app UI builds", {
  ui <- htmltools::renderTags(metawoRld:::.app_ui())$html
  for (x in c("Start a new review", "Inbox", "Setup", "Publish", "Sync")) expect_match(ui, x)
})

test_that("app server: overview, inbox accept/reconcile, setup", {
  p <- team_project()
  m <- example_metadata()
  id1 <- suppressMessages(submit_study(p, "smith2020", m, example_data(), person = "ahj"))
  Sys.sleep(1)
  d2 <- example_data(); d2$value1[1] <- 5.2
  id2 <- suppressMessages(submit_study(p, "smith2020", m, d2, person = "kb"))
  id3 <- suppressMessages(submit_study(p, "jones2021", m, example_data(), person = "kb"))
  app <- launch_app(p, person = "ahj")
  on.exit(rm(list = ls(metawoRld:::.app_state), envir = metawoRld:::.app_state), add = TRUE)

  shiny::testServer(app, {
    s <- status()
    expect_equal(s$pending, 3); expect_equal(s$studies, 0)
    expect_equal(nrow(pending()), 3)

    # accept the single extraction of jones2021
    i <- which(pending()$study_id == "jones2021")
    session$setInputs(inbox_table_rows_selected = i)
    expect_equal(sel_sub()$extraction_id, id3)
    session$setInputs(inbox_accept = 1)
    expect_equal(list_studies(p), "jones2021")
    expect_equal(nrow(pending()), 2)

    # reconcile smith2020: keep kb's value for the disagreement
    session$setInputs(inbox_table_rows_selected = 1)
    session$setInputs(inbox_compare = 1)
    st <- cmp_state()
    expect_equal(st$ids, c(id1, id2))
    expect_equal(nrow(st$dis), 1)
    session$setInputs(pick_1 = id2)
    session$setInputs(reconcile_go = 1)
    expect_equal(read_study(p, "smith2020")$data$value1[1], 5.2)
    expect_equal(status()$pending, 0)

    # setup: variable, category with a rule, analysis
    session$setInputs(var_name = "IL-6", var_unit = "pg/mL", var_syn = "IL6, Interleukin-6", var_add = 1)
    expect_equal(unlist(read_rules(p)$variables$`IL-6`$synonyms), c("IL6", "Interleukin-6"))
    session$setInputs(cat_name = "Cases", cat_syn = "Preeclampsia", cat_where = "", cat_add = 1)
    session$setInputs(cat_name = "Controls", cat_syn = "", cat_where = "category = Control", cat_add = 2)
    expect_equal(unlist(read_rules(p)$group_categories$Controls$where$category), "Control")
    session$setInputs(an_name = "", an_variable = "IL-6", an_measure = "SMD", an_g1 = "Cases", an_g2 = "Controls",
                      an_timepoint = "", an_levels = "reported", an_add = 1)
    expect_equal(names(read_analyses(p)), "IL-6: Cases vs Controls")
    session$setInputs(an_table_rows_selected = 1, an_delete = 1)
    expect_length(read_analyses(p), 0)

    # people and check
    session$setInputs(pp_id = "EX", pp_name = "Example", pp_roles = c("verifier"), pp_add = 1)
    expect_true("ex" %in% read_people(p)$id)
    session$setInputs(check_run = 1)
    expect_equal(sum(check_res()$project$level == "error"), 0)
  })
})
