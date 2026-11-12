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

ma_project <- function(env = parent.frame()) {
  p <- local_project(env)
  r <- read_rules(p)
  r$variables <- list(IL6 = list(synonyms = list("IL-6"), unit = "pg/mL", convert = list(`ng/mL` = 1000)))
  r$group_categories <- list(PE = list(synonyms = list("Preeclampsia", "Mild PE", "Severe PE")),
                             Control = list(synonyms = list("Controls")))
  metawoRld:::.write_rules(file.path(p, "_rules.yml"), r)
  writeLines(c("analyses:",
               "  - name: IL-6 PE vs control", "    variable: IL6", "    measure: SMD", "    compare: [PE, Control]",
               "  - name: IL-6 in controls", "    variable: IL6", "    measure: MN", "    group: Control",
               "  - name: Smoking PE vs control", "    variable: Smoking", "    measure: OR", "    compare: [PE, Control]"),
             file.path(p, "_analyses.yml"))
  m <- function(year, groups) list(title = paste("Study", year), authors = list(paste("Author", year)), year = year,
    country = list("NO"), study_design = "Cohort", population = "Pregnant",
    methods = list(list(method_id = "m1", name = "ELISA", unit = "pg/mL")), groups = groups)
  g <- function(id, name, n, category = NULL) Filter(Negate(is.null), list(group_id = id, name = name, sample_size = n, category = category))
  dp <- function(id, grp, var, st, v1, v2, v3 = NA, n, unit = NA, tp = "T3")
    data.frame(measurement_id = id, method_id = "m1", group_id = grp, variable_name = var, timepoint = tp, sample_size = n,
               statistic_type = st, value1 = v1, value2 = v2, value3 = v3, unit = unit, source = "Table 2")
  # A: mean/SD; B: median/IQR in ng/mL with category field; C: two PE subgroups; D: controls only; E: smoking counts
  add_study_data(p, "A", m(2015L, list(g("g1", "Preeclampsia", 20L), g("g2", "Controls", 30L))),
                 rbind(dp("d1", "g1", "IL-6", "mean_sd", 5, 2, n = 20), dp("d2", "g2", "IL-6", "mean_sd", 3, 1.5, n = 30),
                       dp("d3", "g1", "Smoking", "n_percent", 4, 20, n = 20), dp("d4", "g2", "Smoking", "n_percent", 3, 10, n = 30)))
  add_study_data(p, "B", m(2018L, list(g("a", "Cases", 40L, "PE"), g("b", "Healthy women", 40L, "Control"))),
                 rbind(dp("d1", "a", "IL6", "median_iqr", 0.006, 0.004, 0.008, n = 40, unit = "ng/mL"),
                       dp("d2", "b", "IL6", "median_iqr", 0.004, 0.003, 0.005, n = 40, unit = "ng/mL")))
  add_study_data(p, "C", m(2020L, list(g("g1", "Mild PE", 10L), g("g2", "Severe PE", 10L), g("g3", "Controls", 25L))),
                 rbind(dp("d1", "g1", "IL-6", "mean_sd", 4, 1, n = 10), dp("d2", "g2", "IL-6", "mean_sd", 6, 1, n = 10),
                       dp("d3", "g3", "IL-6", "mean_se", 3, 0.2, n = 25)))
  add_study_data(p, "D", m(2021L, list(g("g1", "Controls", 50L))), dp("d1", "g1", "IL-6", "mean_sd", 2.8, 1.1, n = 50))
  p
}

