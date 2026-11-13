test_that("people and tools are registered and checked", {
  p <- team_project()
  pp <- read_people(p)
  expect_equal(pp$id, c("ahj", "kb", "datafindr"))
  expect_equal(pp$type, c("person", "person", "tool"))
  add_person(p, "kb", "Kari B", roles = "verifier")          # update in place
  expect_equal(read_people(p)$name[2], "Kari B")
  expect_error(add_person(p, "X Y", "Bad"), "lowercase")
  expect_error(add_person(p, "zz", "Z", roles = "boss"), "Unknown role")
  expect_error(add_search(p, "s-1", "PubMed", "q", person = "nobody"), "Unknown person")
})

test_that("record ids follow the spec", {
  expect_equal(record_id(c("https://doi.org/10.1000/AB", NA, NA), c("1", "PMID: 123", NA), c("t", "t", "A Title!"), c(2020, 2020, 2020))[1:2],
               c("10.1000/ab", "pmid:123"))
  r3 <- record_id(NA, NA, "A Title!", 2020)
  expect_match(r3, "^rec-[0-9a-f]{12}$")
  expect_equal(r3, record_id("", "", "a title", "2020"))      # normalized title
})

test_that("searches, records, screening and PRISMA counts", {
  p <- team_project()
  add_search(p, "pubmed-1", "PubMed", "x AND y", 3, "ahj", date = as.Date("2026-11-01"))
  expect_error(add_search(p, "pubmed-1", "PubMed", "again", person = "ahj"), "already exists")
  expect_equal(yaml::read_yaml(file.path(p, "searches.yml"))$searches[[1]]$results, 3L)

  recs <- data.frame(title = c("A", "B", "C"), doi = c("10.1000/X", NA, NA), pmid = c(NA, "123", NA), year = 2020)
  res <- add_records(p, recs, "pubmed-1")
  expect_true(all(res$new))
  res <- add_records(p, recs[1, ], "manual")
  expect_false(res$new)
  r <- read_records(p)
  expect_equal(r$record_id[1:2], c("10.1000/x", "pmid:123"))
  expect_equal(r$searches[1], "pubmed-1; manual")

  add_screening(p, r$record_id, "title_abstract", c("include", "exclude", "include"), c("", "Animal study", ""), "ahj")
  add_screening(p, r$record_id[c(1, 3)], "title_abstract", c("include", "exclude"), person = "kb")
  st <- screening_status(p)
  expect_equal(st$outcome, c("include", "exclude", "conflict"))
  expect_equal(st$n_decisions, c(2L, 1L, 2L))
  add_screening(p, r$record_id[3], "title_abstract", "include", person = "ahj", kind = "consensus")
  expect_equal(screening_status(p)$outcome[3], "include")
  expect_error(add_screening(p, "10.9/none", "title_abstract", "include", person = "ahj"), "Unknown record")
  expect_error(add_screening(p, r$record_id[1], "title_abstract", "maybe", person = "ahj"), "include, exclude or unsure")

  add_screening(p, r$record_id[c(1, 3)], "full_text", c("include", "exclude"), c("", "No data"), person = "ahj")
  link_record(p, r$record_id[1], "smith2020")
  pc <- prisma_counts(p)
  n <- stats::setNames(pc$n, pc$stage)
  expect_equal(unname(n[c("Records identified", "Records screened (title/abstract)", "Records excluded (title/abstract)",
                          "Reports assessed (full text)", "Reports excluded (full text)", "  Excluded: No data",
                          "Reports of included studies")]),
               c(3, 3, 1, 2, 1, 1, 1))
})
