test_that("create_metawoRld writes config, templates and website files", {
  p <- local_project()
  files <- list.files(p, all.files = TRUE)
  expect_true(all(c("_metawoRld.yml", "_extraction_schema.yml", "_assessment_schema.yml",
                    "_extraction_prompt.txt", "_assessment_prompt.txt", "data", "README.md",
                    ".gitignore", "_quarto.yml", "index.qmd", "styles.css", "study.qmd.tmpl") %in% files))
  cfg <- read_config(p)
  expect_equal(cfg$inclusion_criteria, c("Human", "Biomarker X reported"))
  expect_true(any(grepl("Test review", readLines(file.path(p, "_quarto.yml")))))
  expect_error(create_metawoRld(p, "a", "b"), "already exists")
})

test_that("both schemas are valid metawoRld schemas", {
  p <- local_project()
  s <- get_schema(p)
  expect_equal(s$`_type`, "object")
  expect_equal(get_schema(p, "assessment")$decision$`_type`, "enum")
  refs <- schema_refs(s)
  expect_equal(refs$field, c("method_id", "group_id", "comparison_group_id"))
  expect_equal(refs$allow[[1]], "POP_CHAR")
})

test_that("use_templates keeps edits unless overwrite = TRUE", {
  p <- local_project()
  f <- file.path(p, "_assessment_prompt.txt")
  writeLines("custom", f)
  use_templates(p)
  expect_equal(readLines(f), "custom")
  use_templates(p, overwrite = TRUE)
  expect_false(identical(readLines(f), "custom"))
})
