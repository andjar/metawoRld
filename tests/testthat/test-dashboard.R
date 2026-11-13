skip_if_not_installed("metafor")

test_that("generate_dashboard writes analysis and results pages with figures", {
  p <- ma_project()
  files <- generate_dashboard(p)
  expect_equal(basename(files), c("analyses.qmd", "results.qmd"))
  a <- readLines(file.path(p, "analyses.qmd"))
  expect_true(any(a == "# IL-6 PE vs control"))
  expect_true(any(grepl("{.valuebox", a, fixed = TRUE)))
  expect_true(any(grepl("![](figures/forest_1.png)", a, fixed = TRUE)))
  expect_true(any(grepl("- study-d: no usable data for: PE", a, fixed = TRUE)))     # excluded studies listed
  expect_true(all(file.exists(file.path(p, "figures", c("forest_1.png", "cumulative_1.png", "variable_1.png")))))
  r <- readLines(file.path(p, "results.qmd"))
  expect_true(any(grepl("IL6 (4 studies)", r, fixed = TRUE)))
})

test_that("dashboard explains how to add analyses when none are defined", {
  p <- local_project()
  add_study_data(p, "smith2020", example_metadata(), example_data())
  generate_dashboard(p)
  expect_true(any(grepl("No meta-analyses defined yet", readLines(file.path(p, "analyses.qmd")))))
})

test_that("generate_website wires the dashboard into the site", {
  p <- ma_project()
  generate_website(p, render = FALSE)
  q <- yaml::read_yaml(file.path(p, "_quarto.yml"))
  expect_true(all(c("index.qmd", "analyses.qmd", "results.qmd") %in% q$project$render))
  expect_equal(readLines(file.path(p, "index.qmd"))[3:5], c("format:", "  dashboard:", "    css: styles.css"))
})
