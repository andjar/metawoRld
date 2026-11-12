skip_if_not_installed("shiny")
skip_if_not_installed("bslib")
skip_if_not_installed("DT")

test_that("review app verifies, corrects, rejects and maps variables", {
  p <- local_project()
  add_study_data(p, "10.1000/abc", example_metadata(), example_data())
  app <- review_values_app(p, reviewer = "AHJ")

  shiny::testServer(app, {
    session$setInputs(reviewer = "AHJ", status_filter = c("unverified", "changed"),
                      flagged_only = FALSE, study_filter = "")
    expect_equal(current()$measurement_id, "dp1")
    expect_equal(nrow(queue()), 3)

    # plain verify
    set_form(session, cur()$row)
    expect_length(changed_fields(), 0)
    session$setInputs(verify = 1)
    expect_equal(verification_status(p)$status[1], "verified")
    expect_equal(current()$measurement_id, "dp2")          # advanced
    expect_equal(nrow(queue()), 2)

    # correction: change value2, verify -> logged correction
    set_form(session, cur()$row, f_value2 = 3.3)
    expect_equal(changed_fields(), "value2")
    expect_match(output$verify_button$html, "Save correction")
    session$setInputs(verify = 2)
    st <- verification_status(p)
    expect_equal(st$status[2], "verified")
    expect_equal(st$note[2], "Corrected: value2: 3.1 -> 3.3")
    expect_equal(read_study(p, "10.1000/abc")$data$value2[2], 3.3)

    # invalid correction is refused and nothing changes
    set_form(session, cur()$row, f_group_id = "g9")
    session$setInputs(verify = 3)
    expect_equal(verification_status(p)$status[3], "unverified")

    # reject via keyboard with a note
    set_form(session, cur()$row, note = "Not in the paper")
    session$setInputs(key = list(k = "r", t = 1))
    st <- verification_status(p)
    expect_equal(st$status[3], "rejected")
    expect_equal(st$note[3], "Not in the paper")
    expect_equal(nrow(queue()), 0)

    # progress counts everything
    expect_match(output$progress$html, "2 of 3 values verified")

    # show all, flagged only after defining a vocabulary
    add_variable_synonym(p, "X", "X")
    rules(read_rules(p))
    for (id in names(shiny::reactiveValuesToList(store))) store[[id]] <- load_one(id)
    session$setInputs(status_filter = c("unverified", "verified", "rejected", "changed"), flagged_only = TRUE)
    expect_equal(queue()$variable, "Age")                  # unknown variable
    current(list(study_id = "10.1000/abc", measurement_id = "dp3"))
    expect_false(is.null(output$dp_map))
    session$setInputs(map_to = "Maternal age", map = 1)
    expect_equal(canonical_variable("Age", read_rules(p)), "Maternal age")
    expect_equal(nrow(queue()), 0)
  })
})

test_that("statistics are described in plain language", {
  r <- list(statistic_type = "median_iqr", value1 = 4.2, value2 = 3.1, value3 = NA)
  expect_equal(metawoRld:::.describe_statistic(r, "pg/mL"),
               "median (IQR) = <b>4.2 (3.1 to <span class='missing'>?</span>)</b> <span class='unit'>pg/mL</span>")
  expect_match(metawoRld:::.describe_statistic(list(statistic_type = "n_percent", value1 = 5, value2 = 25), ""), "n \\(%\\) = <b>5 \\(25%\\)</b>")
})
