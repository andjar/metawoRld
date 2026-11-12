local_project <- function(env = parent.frame(), ...) {
  path <- file.path(tempfile("mw"), "proj")
  suppressMessages(create_metawoRld(
    path, "Test review", "A test project.",
    inclusion_criteria = c("Human", "Biomarker X reported"),
    exclusion_criteria = c("Animal study"),
    git_init = FALSE, ...
  ))
  withr::defer(unlink(dirname(path), recursive = TRUE), envir = env)
  path
}

example_metadata <- function() {
  list(
    title = "A study of X", authors = list("Smith J", "Doe A"), year = 2020L,
    journal = "J Test", country = list("Norway"), study_design = "Cohort",
    population = "Pregnant women",
    methods = list(list(method_id = "m1", name = "Serum X by ELISA", unit = "pg/mL")),
    groups = list(list(group_id = "g1", name = "Cases", sample_size = 20L),
                  list(group_id = "g2", name = "Controls", sample_size = 25L))
  )
}

example_data <- function() {
  data.frame(
    measurement_id = c("dp1", "dp2", "dp3"),
    method_id = c("m1", "m1", "POP_CHAR"),
    group_id = c("g1", "g2", "g1"),
    variable_name = c("X", "X", "Age"),
    timepoint = c("Week 24", "Week 24", NA),
    sample_size = c(20, 25, 20),
    statistic_type = c("mean_sd", "median_iqr", "mean_sd"),
    value1 = c(5.1, 4.2, 31),
    value2 = c(1.2, 3.1, 4),
    value3 = c(NA, 5.5, NA),
    p_value = c("0.03", NA, NA),
    source = c("Table 2", "Table 2", "Table 1")
  )
}

# Set the review form inputs to the current data point's values, as the
# browser would after updateXInput().
set_form <- function(session, row, ...) {
  v <- function(x) if (is.null(x) || is.na(x)) "" else as.character(x)
  n <- function(x) if (is.null(x)) NA else x
  vals <- list(f_variable_name = v(row$variable_name), f_group_id = v(row$group_id), f_timepoint = v(row$timepoint),
               f_statistic_type = v(row$statistic_type), f_sample_size = n(row$sample_size), f_unit = v(row$unit),
               f_value1 = n(row$value1), f_value2 = n(row$value2), f_value3 = n(row$value3), f_p_value = v(row$p_value),
               note = "")
  do.call(session$setInputs, utils::modifyList(vals, list(...)))
}
