test_that("create_metawoRld writes the layout from SPEC.md", {
  p <- local_project()
  files <- list.files(p, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
  expect_true(all(c("_metawoRld.yml", "_rules.yml", "_analyses.yml", "people.yml", "searches.yml",
                    "records.csv", "screening.csv", "NEWS.md", "README.md", ".gitignore",
                    "forms/extraction.yml", "forms/screening.yml", "prompts/extraction.txt",
                    "prompts/screening.txt", "data", "submissions",
                    "_quarto.yml", "index.qmd", "styles.css", "study.qmd.tmpl") %in% files))
  cfg <- read_config(p)
  expect_equal(cfg$spec_version, 1L)
  expect_equal(cfg$license, "CC-BY-4.0")
  expect_equal(cfg$inclusion_criteria, c("Human", "Biomarker X reported"))
  expect_true(any(grepl("Test review", readLines(file.path(p, "_quarto.yml")))))
  expect_error(create_metawoRld(p, "a", "b"), "already exists")
  expect_equal(nrow(read_records(p)), 0)
  expect_equal(nrow(read_people(p)), 0)
})

test_that("forms are valid metawoRld forms with headers", {
  p <- local_project()
  s <- get_schema(p)
  expect_equal(s$`_type`, "object")
  expect_equal(s$`_form`$name, "extraction")
  expect_equal(s$`_form`$version, "1.0.0")
  expect_equal(get_schema(p, "screening")$decision$`_type`, "enum")
  expect_equal(get_schema(p, "assessment")$`_form`$kind, "assessment")   # alias
  expect_equal(list_forms(p)$name, c("extraction", "screening"))
  refs <- schema_refs(s)
  expect_equal(refs$field, c("method_id", "group_id", "comparison_group_id"))
  expect_equal(refs$allow[[1]], "POP_CHAR")
  expect_error(get_schema(p, "rob2"), "Form not found")
})

test_that("forms with YAML-boolean field names are refused", {
  p <- local_project()
  f <- file.path(p, "forms", "extraction.yml")
  writeLines(sub("  sample_size:", "  n:", readLines(f)), f)
  expect_error(get_schema(p), "YAML reads as TRUE/FALSE")
})

test_that("export_schemas writes JSON Schema and Table Schema", {
  p <- local_project()
  files <- export_schemas(p)
  expect_true(all(file.exists(file.path(p, "forms", c("extraction.schema.json", "screening.schema.json",
                                                      "extraction.data.tableschema.json")))))
  js <- jsonlite::read_json(file.path(p, "forms", "extraction.schema.json"))
  expect_equal(js$`$schema`, "https://json-schema.org/draft/2020-12/schema")
  expect_equal(js$properties$data_points$type, "array")
  expect_true("measurement_id" %in% unlist(js$properties$data_points$items$required))
  ts <- jsonlite::read_json(file.path(p, "forms", "extraction.data.tableschema.json"))
  st <- Filter(function(f) f$name == "statistic_type", ts$fields)[[1]]
  expect_true("mean_sd" %in% unlist(st$constraints$enum))
})

test_that("use_templates keeps edits unless overwrite = TRUE", {
  p <- local_project()
  f <- file.path(p, "prompts", "screening.txt")
  writeLines("custom", f)
  unlink(file.path(p, "forms", "screening.yml"))
  written <- use_templates(p)
  expect_equal(basename(written), "screening.yml")
  expect_equal(readLines(f), "custom")
  use_templates(p, overwrite = TRUE)
  expect_false(identical(readLines(f), "custom"))
})
